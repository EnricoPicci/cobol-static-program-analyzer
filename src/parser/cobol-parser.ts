/**
 * Main COBOL Parser - Two-stage parsing pipeline with error handling
 * Integrates ANTLR generated parsers with custom AST builder
 */

import { CharStream, CommonTokenStream, ParseTree, RecognitionException, Token } from 'antlr4ng';

// ANTLR Generated classes
import { Cobol85Lexer } from '../generated/parser/Cobol85Lexer';
import { Cobol85Parser } from '../generated/parser/Cobol85Parser';
import { Cobol85PreprocessorLexer } from '../generated/preprocessor/Cobol85PreprocessorLexer';
import { Cobol85PreprocessorParser } from '../generated/preprocessor/Cobol85PreprocessorParser';

// Our custom AST components
import { CobolASTBuilder, ASTBuilderConfig, DEFAULT_AST_BUILDER_CONFIG } from '../ast/builder';
import { CobolProgram } from '../ast/nodes/CobolProgram';

// Types and error handling
import { DiagnosticMessage, PerformanceMetrics } from '../core/types';
import { ParsingError, PreprocessingError, ASTConstructionError } from './error-handler';

/**
 * Configuration for COBOL parser
 */
export interface CobolParserConfig {
  /** AST builder configuration */
  astBuilder: ASTBuilderConfig;
  
  /** Enable preprocessing stage */
  enablePreprocessing: boolean;
  
  /** Copybook search paths */
  copybookPaths: string[];
  
  /** Maximum errors before stopping */
  maxErrors: number;
  
  /** Error recovery mode */
  errorRecovery: 'strict' | 'lenient';
  
  /** Enable performance monitoring */
  enableProfiling: boolean;
  
  /** Source encoding */
  encoding: string;
  
  /** COBOL dialect support */
  dialect: 'cobol85' | 'cobol2002' | 'cobol2014' | 'enterprise';
}

/**
 * Default parser configuration
 */
export const DEFAULT_COBOL_PARSER_CONFIG: CobolParserConfig = {
  astBuilder: DEFAULT_AST_BUILDER_CONFIG,
  enablePreprocessing: true,
  copybookPaths: ['./copybooks'],
  maxErrors: 10,
  errorRecovery: 'lenient',
  enableProfiling: false,
  encoding: 'utf8',
  dialect: 'cobol85'
};

/**
 * Result of parsing operation
 */
export interface ParseResult {
  /** Successfully parsed AST */
  ast?: CobolProgram;
  
  /** Parsing errors encountered */
  errors: DiagnosticMessage[];
  
  /** Parsing warnings */
  warnings: DiagnosticMessage[];
  
  /** Success status */
  success: boolean;
  
  /** Performance metrics */
  performance?: PerformanceMetrics;
  
  /** Preprocessing result */
  preprocessedSource?: string;
  
  /** Source file information */
  sourceInfo: {
    originalLength: number;
    processedLength: number;
    lineCount: number;
    copybooksIncluded: string[];
  };
}

/**
 * High-performance COBOL parser with two-stage processing
 */
export class CobolParser {
  private config: CobolParserConfig;
  private astBuilder: CobolASTBuilder;
  private errors: DiagnosticMessage[] = [];
  private warnings: DiagnosticMessage[] = [];

  constructor(config: CobolParserConfig = DEFAULT_COBOL_PARSER_CONFIG) {
    this.config = config;
    this.astBuilder = new CobolASTBuilder(config.astBuilder);
  }

  /**
   * Parse COBOL source code to AST
   */
  async parse(source: string, fileName?: string): Promise<ParseResult> {
    const startTime = performance.now();
    this.clearDiagnostics();

    try {
      // Stage 1: Preprocessing (COPY/REPLACE statements)
      let processedSource = source;
      let copybooksIncluded: string[] = [];
      
      if (this.config.enablePreprocessing) {
        const preprocessResult = await this.preprocess(source, fileName);
        processedSource = preprocessResult.processedSource;
        copybooksIncluded = preprocessResult.copybooksIncluded;
      }

      // Stage 2: Main parsing
      const parseTree = this.parseToTree(processedSource, fileName);
      
      // Stage 3: AST construction
      let ast: CobolProgram | undefined;
      try {
        ast = this.astBuilder.build(parseTree as any);
      } catch (error) {
        this.addError('AST_CONSTRUCTION_ERROR', `Failed to build AST: ${error instanceof Error ? error.message : String(error)}`, {
          line: 1,
          column: 1,
          endLine: 1,
          endColumn: 1
        });
      }

      // Calculate performance metrics
      const endTime = performance.now();
      const perfMetrics: PerformanceMetrics = {
        parseTime: endTime - startTime,
        memoryUsage: ast ? this.estimateMemoryUsage(ast) : 0,
        nodeCount: ast ? this.countASTNodes(ast) : 0,
        copybookCount: copybooksIncluded.length
      };

      return {
        ast,
        errors: [...this.errors],
        warnings: [...this.warnings],
        success: this.errors.length === 0,
        performance: this.config.enableProfiling ? perfMetrics : undefined,
        preprocessedSource: this.config.astBuilder.preserveSourceText ? processedSource : undefined,
        sourceInfo: {
          originalLength: source.length,
          processedLength: processedSource.length,
          lineCount: processedSource.split('\n').length,
          copybooksIncluded
        }
      };

    } catch (error) {
      this.addError('PARSING_FAILED', error instanceof Error ? error.message : String(error), {
        line: 1,
        column: 1,
        endLine: 1,
        endColumn: 1
      });

      return {
        errors: [...this.errors],
        warnings: [...this.warnings],
        success: false,
        sourceInfo: {
          originalLength: source.length,
          processedLength: 0,
          lineCount: source.split('\n').length,
          copybooksIncluded: []
        }
      };
    }
  }

  /**
   * Parse only to ANTLR parse tree (for testing/debugging)
   */
  parseToTree(source: string, fileName?: string): ParseTree {
    try {
      // Create input stream
      const inputStream = CharStream.fromString(source);
      
      // Lexical analysis
      const lexer = new Cobol85Lexer(inputStream);
      const tokenStream = new CommonTokenStream(lexer);
      
      // Syntactic analysis
      const parser = new Cobol85Parser(tokenStream);
      
      // Configure error handling
      this.configureParser(parser);
      
      // Parse from start rule
      const parseTree = parser.startRule();
      
      return parseTree;
      
    } catch (error) {
      if (error instanceof RecognitionException) {
        this.addError('SYNTAX_ERROR', `Syntax error: ${(error as any).message}`, {
          line: (error as any).offendingToken?.line || 1,
          column: (error as any).offendingToken?.charPositionInLine || 1,
          endLine: (error as any).offendingToken?.line || 1,
          endColumn: ((error as any).offendingToken?.charPositionInLine || 1) + (((error as any).offendingToken?.text?.length as number) || 1)
        });
      } else {
        this.addError('PARSE_ERROR', error instanceof Error ? error.message : String(error), {
          line: 1,
          column: 1,
          endLine: 1,
          endColumn: 1
        });
      }
      throw error;
    }
  }

  /**
   * Preprocessing stage - handle COPY/REPLACE statements
   */
  private async preprocess(source: string, fileName?: string): Promise<{
    processedSource: string;
    copybooksIncluded: string[];
  }> {
    try {
      // Create preprocessor
      const inputStream = CharStream.fromString(source);
      const lexer = new Cobol85PreprocessorLexer(inputStream);
      const tokenStream = new CommonTokenStream(lexer);
      const parser = new Cobol85PreprocessorParser(tokenStream);
      
      // Configure error handling for preprocessor
      this.configureParser(parser);
      
      // For now, return source as-is (COPY processing will be implemented in Phase 3)
      return {
        processedSource: source,
        copybooksIncluded: []
      };
      
    } catch (error) {
      throw new PreprocessingError(`Preprocessing failed: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  /**
   * Configure parser for error handling
   */
  private configureParser(parser: Cobol85Parser | Cobol85PreprocessorParser): void {
    // Remove default error listeners
    parser.removeErrorListeners();
    
    // Add custom error listener
    const errorListener = {
      syntaxError: (recognizer: any, offendingSymbol: any, line: number, charPositionInLine: number, msg: string, e: any) => {
        this.addError('SYNTAX_ERROR', msg, {
          line,
          column: charPositionInLine + 1,
          endLine: line,
          endColumn: charPositionInLine + ((offendingSymbol as Token)?.text?.length || 1) + 1
        });
        
        // Stop parsing if too many errors
        if (this.errors.length >= this.config.maxErrors) {
          throw new ParsingError(`Too many errors (${this.config.maxErrors}), stopping parsing`);
        }
      },
      reportAmbiguity: () => {
        // Optional: handle ambiguity reports
      },
      reportAttemptingFullContext: () => {
        // Optional: handle full context attempts
      },
      reportContextSensitivity: () => {
        // Optional: handle context sensitivity reports
      }
    };
    parser.addErrorListener(errorListener);

    // Configure error recovery strategy
    if (this.config.errorRecovery === 'strict') {
      // Bail out on first error - antlr4ng has different error handling
      // We'll handle this through the error listener above
    }
  }

  /**
   * Add parsing error
   */
  private addError(code: string, message: string, location: any): void {
    this.errors.push({
      severity: 'error',
      code,
      message,
      location
    });
  }

  /**
   * Add parsing warning
   */
  private addWarning(code: string, message: string, location: any): void {
    this.warnings.push({
      severity: 'warning',
      code,
      message,
      location
    });
  }

  /**
   * Clear diagnostic messages
   */
  private clearDiagnostics(): void {
    this.errors = [];
    this.warnings = [];
    this.astBuilder.clearErrors();
  }

  /**
   * Estimate memory usage of AST (rough calculation)
   */
  private estimateMemoryUsage(ast: CobolProgram): number {
    // Rough estimation: each node ~1KB average
    const nodeCount = this.countASTNodes(ast);
    return nodeCount * 1024; // bytes
  }

  /**
   * Count AST nodes recursively
   */
  private countASTNodes(node: any): number {
    let count = 1; // Current node
    
    if (node.children && Array.isArray(node.children)) {
      for (const child of node.children) {
        count += this.countASTNodes(child);
      }
    }
    
    return count;
  }

  /**
   * Update parser configuration
   */
  updateConfig(config: Partial<CobolParserConfig>): void {
    this.config = { ...this.config, ...config };
    this.astBuilder = new CobolASTBuilder(this.config.astBuilder);
  }

  /**
   * Get current configuration
   */
  getConfig(): CobolParserConfig {
    return { ...this.config };
  }

  /**
   * Validate COBOL source for basic syntax
   */
  async validate(source: string, fileName?: string): Promise<DiagnosticMessage[]> {
    const result = await this.parse(source, fileName);
    return [...result.errors, ...result.warnings];
  }
}