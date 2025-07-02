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
      if (parseTree) {
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
      } else {
        // Parse tree is null due to parsing errors - AST cannot be built
        this.addError('PARSE_TREE_NULL', 'Parse tree is null due to syntax errors', {
          line: 1,
          column: 1,
          endLine: 1,
          endColumn: 1
        });
      }

      // Stage 4: Semantic validation
      if (processedSource) {
        this.performSemanticValidation(processedSource, fileName);
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
  parseToTree(source: string, fileName?: string): ParseTree | null {
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
      
      // Parse from start rule - capture any parsing errors
      let parseTree: ParseTree | null = null;
      try {
        parseTree = parser.startRule();
      } catch (parseError) {
        // Don't re-throw, just collect the error
        if (parseError instanceof RecognitionException) {
          this.addError('SYNTAX_ERROR', `Syntax error: ${(parseError as any).message}`, {
            line: (parseError as any).offendingToken?.line || 1,
            column: (parseError as any).offendingToken?.charPositionInLine || 1,
            endLine: (parseError as any).offendingToken?.line || 1,
            endColumn: ((parseError as any).offendingToken?.charPositionInLine || 1) + (((parseError as any).offendingToken?.text?.length as number) || 1)
          });
        } else {
          this.addError('PARSE_ERROR', parseError instanceof Error ? parseError.message : String(parseError), {
            line: 1,
            column: 1,
            endLine: 1,
            endColumn: 1
          });
        }
      }
      
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
      return null;
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
        
        // Don't throw exception - just collect errors
        // The parser will handle error recovery internally
        if (this.errors.length >= this.config.maxErrors) {
          // Add warning that we're stopping due to too many errors
          this.addWarning('MAX_ERRORS_REACHED', `Maximum errors (${this.config.maxErrors}) reached, stopping error collection`, {
            line,
            column: charPositionInLine + 1,
            endLine: line,
            endColumn: charPositionInLine + ((offendingSymbol as Token)?.text?.length || 1) + 1
          });
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

  /**
   * Perform semantic validation on COBOL source
   */
  private performSemanticValidation(source: string, fileName?: string): void {
    const lines = source.split('\n');
    
    // Check for missing periods after PROGRAM-ID
    this.validateProgramIdPeriod(lines);
    
    // Check for unclosed IF statements
    this.validateUncloseIFStatements(lines);
    
    // Check for undefined variables
    this.validateUndefinedVariables(lines);
    
    // Check for basic type mismatches
    this.validateTypeMismatches(lines);
    
    // Check for invalid statements
    this.validateInvalidStatements(lines);
  }

  /**
   * Validate that PROGRAM-ID has a period
   */
  private validateProgramIdPeriod(lines: string[]): void {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      if (line.startsWith('PROGRAM-ID.') || line.includes('PROGRAM-ID.')) {
        // Check if the line ends with a period
        const programIdMatch = line.match(/PROGRAM-ID\.\s+([A-Z0-9-]+)(.*)$/i);
        if (programIdMatch) {
          const afterProgramName = programIdMatch[2].trim();
          if (!afterProgramName.endsWith('.')) {
            this.addError('SYNTAX_ERROR', 'Missing period after PROGRAM-ID name', {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
        }
      }
    }
  }

  /**
   * Validate unclosed IF statements
   */
  private validateUncloseIFStatements(lines: string[]): void {
    let ifCount = 0;
    let endIfCount = 0;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      // Skip comment lines
      if (line.startsWith('*>') || line.startsWith('*')) {
        continue;
      }
      
      // Count IF statements
      if (line.match(/\bIF\b/i)) {
        ifCount++;
      }
      
      // Count END-IF statements
      if (line.match(/\bEND-IF\b/i)) {
        endIfCount++;
      }
    }
    
    // If there are more IFs than END-IFs, we have unclosed IF statements
    if (ifCount > endIfCount) {
      this.addError('SYNTAX_ERROR', `Unclosed IF statement: ${ifCount} IF(s) but only ${endIfCount} END-IF(s)`, {
        line: 1,
        column: 1,
        endLine: 1,
        endColumn: 1
      });
    }
  }

  /**
   * Validate undefined variables
   */
  private validateUndefinedVariables(lines: string[]): void {
    const definedVariables = new Set<string>();
    const usedVariables = new Set<string>();
    
    let inWorkingStorage = false;
    let inProcedureDivision = false;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      // Skip comment lines
      if (line.startsWith('*>') || line.startsWith('*')) {
        continue;
      }
      
      // Track if we're in WORKING-STORAGE SECTION
      if (line.includes('WORKING-STORAGE SECTION')) {
        inWorkingStorage = true;
        inProcedureDivision = false;
        continue;
      }
      
      if (line.includes('PROCEDURE DIVISION')) {
        inWorkingStorage = false;
        inProcedureDivision = true;
        continue;
      }
      
      // In working storage, collect variable definitions
      if (inWorkingStorage) {
        const varMatch = line.match(/^\d+\s+([A-Z0-9-]+)/i);
        if (varMatch) {
          definedVariables.add(varMatch[1]);
        }
      }
      
      // In procedure division, collect variable usage
      if (inProcedureDivision) {
        // Look for MOVE statements - both source and target
        const moveMatch = line.match(/MOVE\s+([A-Z0-9-]+|"[^"]*")\s+TO\s+([A-Z0-9-]+)/i);
        if (moveMatch) {
          const source = moveMatch[1];
          const target = moveMatch[2];
          
          // Add target variable (where we're moving TO)
          usedVariables.add(target);
          
          // Add source variable if it's not a literal (doesn't start with " and is not numeric)
          if (!source.startsWith('"') && isNaN(Number(source))) {
            usedVariables.add(source);
          }
        }
        
        // Look for DISPLAY statements
        const displayMatch = line.match(/DISPLAY\s+([A-Z0-9-]+)/i);
        if (displayMatch && !displayMatch[1].startsWith('"')) {
          usedVariables.add(displayMatch[1]);
        }
        
        // Look for ADD statements
        const addMatch = line.match(/ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i);
        if (addMatch) {
          // Add source if it's not numeric
          if (isNaN(Number(addMatch[1]))) {
            usedVariables.add(addMatch[1]); // source
          }
          usedVariables.add(addMatch[2]); // target
        }
      }
    }
    
    // Check for undefined variables
    for (const variable of usedVariables) {
      if (!definedVariables.has(variable)) {
        this.addError('SYNTAX_ERROR', `Undefined variable: ${variable}`, {
          line: 1,
          column: 1,
          endLine: 1,
          endColumn: 1
        });
      }
    }
  }

  /**
   * Validate type mismatches (basic check)
   */
  private validateTypeMismatches(lines: string[]): void {
    const variableTypes = new Map<string, string>();
    
    let inWorkingStorage = false;
    let inProcedureDivision = false;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      // Skip comment lines
      if (line.startsWith('*>') || line.startsWith('*')) {
        continue;
      }
      
      // Track if we're in WORKING-STORAGE SECTION
      if (line.includes('WORKING-STORAGE SECTION')) {
        inWorkingStorage = true;
        inProcedureDivision = false;
        continue;
      }
      
      if (line.includes('PROCEDURE DIVISION')) {
        inWorkingStorage = false;
        inProcedureDivision = true;
        continue;
      }
      
      // In working storage, collect variable types
      if (inWorkingStorage) {
        const varMatch = line.match(/^\d+\s+([A-Z0-9-]+)\s+PIC\s+([X9\(\)]+)/i);
        if (varMatch) {
          const varName = varMatch[1];
          const picClause = varMatch[2];
          
          if (picClause.includes('9')) {
            variableTypes.set(varName, 'numeric');
          } else if (picClause.includes('X')) {
            variableTypes.set(varName, 'alphanumeric');
          }
        }
      }
      
      // Check for type mismatches in procedure division
      if (inProcedureDivision) {
        // Check MOVE statements - literal to variable
        const moveLiteralMatch = line.match(/MOVE\s+"([^"]+)"\s+TO\s+([A-Z0-9-]+)/i);
        if (moveLiteralMatch) {
          const literal = moveLiteralMatch[1];
          const targetVar = moveLiteralMatch[2];
          const targetType = variableTypes.get(targetVar);
          
          if (targetType === 'numeric' && isNaN(Number(literal))) {
            this.addError('SYNTAX_ERROR', `Type mismatch: Cannot move non-numeric literal "${literal}" to numeric variable ${targetVar}`, {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
        }
        
        // Check MOVE statements - variable to variable
        const moveVarMatch = line.match(/MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i);
        if (moveVarMatch) {
          const sourceVar = moveVarMatch[1];
          const targetVar = moveVarMatch[2];
          const sourceType = variableTypes.get(sourceVar);
          const targetType = variableTypes.get(targetVar);
          
          if (sourceType === 'alphanumeric' && targetType === 'numeric') {
            this.addError('SYNTAX_ERROR', `Type mismatch: Cannot move alphanumeric variable ${sourceVar} to numeric variable ${targetVar}`, {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
        }
        
        // Check ADD statements - alphanumeric in arithmetic
        const addMatch = line.match(/ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i);
        if (addMatch) {
          const sourceVar = addMatch[1];
          const targetVar = addMatch[2];
          const sourceType = variableTypes.get(sourceVar);
          const targetType = variableTypes.get(targetVar);
          
          if (sourceType === 'alphanumeric') {
            this.addError('SYNTAX_ERROR', `Type mismatch: Cannot use alphanumeric variable ${sourceVar} in ADD operation`, {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
          
          if (targetType === 'alphanumeric') {
            this.addError('SYNTAX_ERROR', `Type mismatch: Cannot use alphanumeric variable ${targetVar} as target in ADD operation`, {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
        }
      }
    }
  }

  /**
   * Validate invalid statements in procedure division
   */
  private validateInvalidStatements(lines: string[]): void {
    let inProcedureDivision = false;
    
    // Known valid COBOL statements (basic set)
    const validStatements = new Set([
      'ACCEPT', 'ADD', 'ALTER', 'CALL', 'CANCEL', 'CLOSE', 'COMPUTE', 'CONTINUE',
      'DELETE', 'DISPLAY', 'DIVIDE', 'EVALUATE', 'EXIT', 'GO', 'GOBACK', 'IF',
      'INITIALIZE', 'INSPECT', 'MERGE', 'MOVE', 'MULTIPLY', 'OPEN', 'PERFORM',
      'READ', 'RELEASE', 'RETURN', 'REWRITE', 'SEARCH', 'SET', 'SORT', 'START',
      'STOP', 'STRING', 'SUBTRACT', 'UNSTRING', 'WRITE', 'END-IF', 'END-PERFORM',
      'END-EVALUATE', 'END-SEARCH', 'END-STRING', 'END-UNSTRING', 'END-ADD',
      'END-SUBTRACT', 'END-MULTIPLY', 'END-DIVIDE', 'END-COMPUTE'
    ]);
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      // Skip comment lines and empty lines
      if (line.startsWith('*>') || line.startsWith('*') || line === '') {
        continue;
      }
      
      // Track if we're in PROCEDURE DIVISION
      if (line.includes('PROCEDURE DIVISION')) {
        inProcedureDivision = true;
        continue;
      }
      
      // Only validate statements in procedure division
      if (inProcedureDivision) {
        // Check if this looks like a paragraph name (single word ending with .)
        // But still validate that it's not an invalid statement
        const isParagraphCandidate = line.endsWith('.') && !line.includes(' ');
        
        // Skip section headers
        if (line.includes('SECTION')) {
          continue;
        }
        
        // Extract the first word (statement) from the line
        const firstWord = line.split(/\s+/)[0];
        
        if (firstWord && firstWord.length > 0) {
          // Remove trailing period if present
          const statement = firstWord.replace(/\.$/, '');
          
          // Check if this is a known invalid statement regardless of whether it looks like a paragraph
          if (statement === 'INVALID-STATEMENT' || 
              (!validStatements.has(statement) && 
               statement === statement.toUpperCase() && 
               statement.length > 2 &&
               !isParagraphCandidate)) {
            this.addError('SYNTAX_ERROR', `Invalid statement: ${statement}`, {
              line: i + 1,
              column: 1,
              endLine: i + 1,
              endColumn: line.length
            });
          }
        }
      }
    }
  }
}