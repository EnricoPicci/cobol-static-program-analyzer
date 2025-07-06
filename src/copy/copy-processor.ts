/**
 * Core COPY statement processor implementation
 */

import { CharStream, CommonTokenStream } from 'antlr4ng';
import { Cobol85PreprocessorLexer } from '../generated/preprocessor/Cobol85PreprocessorLexer';
import { Cobol85PreprocessorParser } from '../generated/preprocessor/Cobol85PreprocessorParser';
import {
  CopyProcessor as ICopyProcessor,
  CopybookConfiguration,
  PreprocessedSource,
  CopyStatementInfo,
  ProcessingResult,
  CopyProcessingError,
  DEFAULT_COPYBOOK_CONFIG,
  SourceMapping,
  ResolvedCopybook
} from './types';
import { CopybookResolver } from './copybook-resolver';
import { ReplacingProcessor } from './replacing-processor';
import { DependencyTracker } from './dependency-tracker';
import { CopyStatementVisitor } from './copy-visitor';
import { CopyErrorHandler } from './error-handler';

export class CopyProcessor implements ICopyProcessor {
  private config: CopybookConfiguration;
  private resolver: CopybookResolver;
  private replacingProcessor: ReplacingProcessor;
  private dependencyTracker: DependencyTracker;
  private errorHandler: CopyErrorHandler;
  private currentNestingLevel: number = 0;
  private resolvedCopybooks: ResolvedCopybook[] = [];
  private currentParent: string = 'main';
  private processCache: Map<string, PreprocessedSource> = new Map();

  constructor(config?: Partial<CopybookConfiguration>) {
    this.config = { ...DEFAULT_COPYBOOK_CONFIG, ...config };
    this.resolver = new CopybookResolver(this.config);
    this.replacingProcessor = new ReplacingProcessor();
    this.dependencyTracker = new DependencyTracker();
    this.errorHandler = new CopyErrorHandler();
  }

  async process(source: string, config?: Partial<CopybookConfiguration>): Promise<PreprocessedSource> {
    if (config) {
      this.updateConfiguration(config);
    }

    // Check cache first for performance optimization
    const cacheKey = this.generateCacheKey(source, config);
    if (this.config.cacheEnabled && this.processCache.has(cacheKey)) {
      return this.processCache.get(cacheKey)!;
    }

    this.errorHandler.clear();
    this.dependencyTracker.clear();
    this.currentNestingLevel = 0;
    this.resolvedCopybooks = [];
    this.currentParent = 'main';

    try {
      const processedContent = await this.processNestedCopies(source);
      const sourceMap = this.buildSourceMap(source, processedContent);
      const includedCopybooks = this.getIncludedCopybooks();

      const result = {
        originalSource: source,
        processedSource: processedContent,
        includedCopybooks,
        sourceMap,
        errors: this.errorHandler.getErrors()
      };

      // Cache the result for future use
      if (this.config.cacheEnabled) {
        this.processCache.set(cacheKey, result);
      }

      return result;
    } catch (error) {
      this.errorHandler.handleUnexpectedError(error as Error, { line: 1, column: 1 });
      
      const errorResult = {
        originalSource: source,
        processedSource: source, // Return original source on failure
        includedCopybooks: [],
        sourceMap: [],
        errors: this.errorHandler.getErrors()
      };

      // Don't cache error results
      return errorResult;
    }
  }

  async processNestedCopies(content: string, currentDepth: number = 0): Promise<string> {
    if (currentDepth >= this.config.maxNestingLevel) {
      this.errorHandler.handleMaxNestingLevelExceeded(
        this.config.maxNestingLevel,
        { line: 1, column: 1 }
      );
      return content;
    }

    const previousNestingLevel = this.currentNestingLevel;
    this.currentNestingLevel = currentDepth;

    try {
      const copyStatements = await this.findCopyStatements(content);
      
      if (copyStatements.length === 0) {
        return content; // No COPY statements to process
      }

      let processedContent = content;
      
      // Process COPY statements in reverse order to maintain correct positions
      for (const copyStatement of copyStatements.reverse()) {
        try {
          const result = await this.processSingleCopy(copyStatement);
          
          if (result.success && result.processedContent) {
            processedContent = this.replaceCopyStatement(
              processedContent,
              copyStatement,
              result.processedContent
            );
          }
        } catch (error) {
          this.errorHandler.handleCopyProcessingError(
            error as Error,
            copyStatement.sourceLocation,
            copyStatement.name
          );
        }
      }
      
      // Recursively process any newly introduced COPY statements
      if (processedContent !== content) {
        return await this.processNestedCopies(processedContent, currentDepth + 1);
      }
      
      return processedContent;
    } catch (error) {
      this.errorHandler.handleUnexpectedError(
        error as Error,
        { line: 1, column: 1 }
      );
      return content;
    } finally {
      this.currentNestingLevel = previousNestingLevel;
    }
  }

  private async findCopyStatements(content: string): Promise<CopyStatementInfo[]> {
    try {
      const copyStatements: CopyStatementInfo[] = [];
      const lines = content.split('\n');
      
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();
        
        // Skip comment lines and empty lines
        if (line.startsWith('*') || line.startsWith('//') || line === '') {
          continue;
        }
        
        // Look for COPY statements (optimized for performance)
        if (line.toUpperCase().includes('COPY ')) {
          const copyMatch = line.match(/COPY\s+(.+?)\.$/i);
          if (copyMatch) {
            const copyInfo = this.parseCopyStatement(copyMatch[1], i + 1);
            if (copyInfo) {
              copyStatements.push(copyInfo);
            }
          }
        }
      }
      
      return copyStatements;
    } catch (error) {
      this.errorHandler.handleParsingError(error as Error, { line: 1, column: 1 });
      return [];
    }
  }

  private parseCopyStatement(copyContent: string, lineNumber: number): CopyStatementInfo | null {
    try {
      // Parse: "COPYBOOK-NAME [OF/IN LIBRARY] [REPLACING ...]"
      let remainingContent = copyContent.trim();
      
      // Extract copybook name (first word)
      const parts = remainingContent.split(/\s+/);
      const name = parts[0];
      if (!name) return null;
      
      let library: string | undefined;
      let replacingClause: any = undefined;
      
      // Check for OF/IN library
      const libraryMatch = remainingContent.match(/\b(OF|IN)\s+([A-Z0-9-]+)/i);
      if (libraryMatch) {
        library = libraryMatch[2];
      }
      
      // Check for REPLACING clause
      const replacingMatch = remainingContent.match(/REPLACING\s+(.+)$/i);
      if (replacingMatch) {
        replacingClause = this.parseReplacingClause(replacingMatch[1]);
      }
      
      return {
        name,
        library,
        replacingClause,
        sourceLocation: {
          line: lineNumber,
          column: 1
        }
      };
    } catch (error) {
      console.warn('Error parsing COPY statement:', error);
      return null;
    }
  }

  private parseReplacingClause(replacingText: string): any {
    try {
      const patterns: any[] = [];
      
      // Simple regex to match ==OLD== BY ==NEW== patterns
      const regex = /==([^=]*)==\s+BY\s+==([^=]*)==/gi;
      let match;
      
      while ((match = regex.exec(replacingText)) !== null) {
        patterns.push({
          oldText: { content: match[1], type: 'pseudotext' },
          newText: { content: match[2], type: 'pseudotext' }
        });
      }
      
      return patterns.length > 0 ? { patterns } : undefined;
    } catch (error) {
      console.warn('Error parsing REPLACING clause:', error);
      return undefined;
    }
  }

  private async processSingleCopy(copyInfo: CopyStatementInfo): Promise<ProcessingResult> {
    try {
      // 1. Resolve copybook location
      const copybook = await this.resolver.resolve(copyInfo);
      
      // 2. Check for circular dependencies
      this.dependencyTracker.addDependency(this.currentParent, copybook.name);
      const circularDeps = this.dependencyTracker.detectCircularDependencies();
      
      if (circularDeps.length > 0) {
        this.errorHandler.handleCircularDependency(
          circularDeps[0].cycle,
          copyInfo.sourceLocation
        );
        return {
          success: false,
          originalStatement: copyInfo,
          dependencies: [],
          errors: [this.errorHandler.getErrors().slice(-1)[0]]
        };
      }
      
      // 3. Process nested COPY statements
      const previousParent = this.currentParent;
      this.currentParent = copybook.name;
      const processedContent = await this.processNestedCopies(
        copybook.content,
        this.currentNestingLevel + 1
      );
      this.currentParent = previousParent;
      
      // 4. Apply REPLACING clause if present
      const finalContent = copyInfo.replacingClause
        ? this.replacingProcessor.applyReplacing(processedContent, copyInfo.replacingClause)
        : processedContent;
      
      // Track successfully resolved copybook
      this.resolvedCopybooks.push(copybook);
      
      return {
        success: true,
        originalStatement: copyInfo,
        resolvedCopybook: copybook,
        processedContent: finalContent,
        dependencies: copybook.dependencies,
        errors: []
      };
    } catch (error) {
      // Check if this is a copybook not found error
      if (error instanceof Error && error.message.includes('not found')) {
        // Check if we already have this error to avoid duplicates
        const existingErrors = this.errorHandler.getErrors();
        const isDuplicate = existingErrors.some(e => 
          e.type === 'COPYBOOK_NOT_FOUND' && 
          e.copybook === copyInfo.name &&
          e.location.line === copyInfo.sourceLocation.line
        );
        
        if (!isDuplicate) {
          this.errorHandler.handleCopybookNotFound(
            copyInfo.name,
            copyInfo.library,
            copyInfo.sourceLocation
          );
        }
      } else {
        this.errorHandler.handleCopyProcessingError(
          error as Error,
          copyInfo.sourceLocation,
          copyInfo.name
        );
      }
      
      return {
        success: false,
        originalStatement: copyInfo,
        dependencies: [],
        errors: [this.errorHandler.getErrors().slice(-1)[0]]
      };
    }
  }

  private replaceCopyStatement(
    content: string,
    copyStatement: CopyStatementInfo,
    replacement: string
  ): string {
    const lines = content.split('\n');
    
    // Find the line with the COPY statement
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (line.includes(`COPY ${copyStatement.name}`)) {
        // Replace the entire line with the copybook content
        lines[i] = replacement;
        break;
      }
    }
    
    return lines.join('\n');
  }

  private buildSourceMap(original: string, processed: string): SourceMapping[] {
    // Simplified source mapping implementation
    const mappings: SourceMapping[] = [];
    const originalLines = original.split('\n');
    const processedLines = processed.split('\n');
    
    for (let i = 0; i < Math.min(originalLines.length, processedLines.length); i++) {
      mappings.push({
        originalLine: i + 1,
        originalColumn: 1,
        processedLine: i + 1,
        processedColumn: 1,
        source: 'main'
      });
    }
    
    return mappings;
  }

  private getIncludedCopybooks(): ResolvedCopybook[] {
    // Return the copybooks that were successfully resolved during processing
    return this.resolvedCopybooks;
  }

  /**
   * Generate a cache key for the given source and configuration
   */
  private generateCacheKey(source: string, config?: Partial<CopybookConfiguration>): string {
    const configStr = config ? JSON.stringify(config) : '';
    const sourceHash = this.simpleHash(source);
    return `${sourceHash}-${this.simpleHash(configStr)}`;
  }

  /**
   * Simple hash function for cache keys
   */
  private simpleHash(str: string): string {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32bit integer
    }
    return Math.abs(hash).toString(36);
  }

  getConfiguration(): CopybookConfiguration {
    return { ...this.config };
  }

  updateConfiguration(config: Partial<CopybookConfiguration>): void {
    this.config = { ...this.config, ...config };
    this.resolver.configure(config);
  }

  getDependencyTracker(): DependencyTracker {
    return this.dependencyTracker;
  }

  getErrorHandler(): CopyErrorHandler {
    return this.errorHandler;
  }
}
