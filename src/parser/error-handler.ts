/**
 * Three-tier error handling framework for COBOL parsing
 * Implements comprehensive error recovery and reporting
 */

import { RecognitionException, Token } from 'antlr4ng';
import { DiagnosticMessage, SourceLocation } from '../core/types';

/**
 * Base parsing error class
 */
export abstract class CobolParsingError extends Error {
  public readonly location?: SourceLocation;
  public readonly code: string;
  public readonly severity: 'error' | 'warning' | 'info';

  constructor(message: string, code: string, severity: 'error' | 'warning' | 'info' = 'error', location?: SourceLocation) {
    super(message);
    this.name = this.constructor.name;
    this.code = code;
    this.severity = severity;
    this.location = location;
  }

  toDiagnostic(): DiagnosticMessage {
    return {
      severity: this.severity,
      code: this.code,
      message: this.message,
      location: this.location || {
        line: 1,
        column: 1,
        endLine: 1,
        endColumn: 1
      }
    };
  }
}

/**
 * Tier 1: Syntax Errors - ANTLR parse-time errors
 */
export class SyntaxError extends CobolParsingError {
  public readonly offendingToken?: Token;
  public readonly expectedTokens?: string[];

  constructor(
    message: string,
    offendingToken?: Token,
    expectedTokens?: string[],
    location?: SourceLocation
  ) {
    super(message, 'SYNTAX_ERROR', 'error', location);
    this.offendingToken = offendingToken;
    this.expectedTokens = expectedTokens;
  }

  static fromRecognitionException(exception: RecognitionException): SyntaxError {
    const offendingToken = (exception as any).offendingToken;
    const location: SourceLocation = {
      line: offendingToken?.line || 1,
      column: (offendingToken as any)?.column || 1,
      endLine: offendingToken?.line || 1,
      endColumn: ((offendingToken as any)?.column || 1) + (offendingToken?.text?.length || 1)
    };

    return new SyntaxError(
      exception.message || 'Syntax error',
      offendingToken,
      undefined,
      location
    );
  }
}

/**
 * Tier 2: Semantic Errors - COBOL language rule violations
 */
export class SemanticError extends CobolParsingError {
  public readonly relatedLocations?: SourceLocation[];

  constructor(
    message: string,
    code: string,
    location: SourceLocation,
    relatedLocations?: SourceLocation[]
  ) {
    super(message, code, 'error', location);
    this.relatedLocations = relatedLocations;
  }

  toDiagnostic(): DiagnosticMessage {
    return {
      ...super.toDiagnostic(),
      relatedLocations: this.relatedLocations
    };
  }
}

/**
 * Tier 3: Analysis Errors - Static analysis specific issues
 */
export class AnalysisError extends CobolParsingError {
  public readonly analysisType: string;
  public readonly suggestions?: string[];

  constructor(
    message: string,
    analysisType: string,
    code: string,
    location: SourceLocation,
    suggestions?: string[]
  ) {
    super(message, code, 'warning', location);
    this.analysisType = analysisType;
    this.suggestions = suggestions;
  }

  toDiagnostic(): DiagnosticMessage {
    return {
      ...super.toDiagnostic(),
      suggestions: this.suggestions
    };
  }
}

/**
 * Preprocessing-specific errors
 */
export class PreprocessingError extends CobolParsingError {
  public readonly copybookName?: string;
  public readonly includeChain?: string[];

  constructor(
    message: string,
    copybookName?: string,
    includeChain?: string[],
    location?: SourceLocation
  ) {
    super(message, 'PREPROCESSING_ERROR', 'error', location);
    this.copybookName = copybookName;
    this.includeChain = includeChain;
  }
}

/**
 * AST construction errors
 */
export class ASTConstructionError extends CobolParsingError {
  public readonly nodeType?: string;
  public readonly context?: string;

  constructor(
    message: string,
    nodeType?: string,
    context?: string,
    location?: SourceLocation
  ) {
    super(message, 'AST_CONSTRUCTION_ERROR', 'error', location);
    this.nodeType = nodeType;
    this.context = context;
  }
}

/**
 * General parsing errors
 */
export class ParsingError extends CobolParsingError {
  constructor(message: string, location?: SourceLocation) {
    super(message, 'PARSING_ERROR', 'error', location);
  }
}

/**
 * Error recovery strategies
 */
export enum ErrorRecoveryStrategy {
  /** Stop parsing on first error */
  FAIL_FAST = 'fail_fast',
  
  /** Continue parsing, collecting all errors */
  COLLECT_ALL = 'collect_all',
  
  /** Try to recover and continue parsing */
  RECOVER_AND_CONTINUE = 'recover_and_continue',
  
  /** Skip problematic sections and continue */
  SKIP_AND_CONTINUE = 'skip_and_continue'
}

/**
 * Comprehensive error handler for COBOL parsing
 */
export class CobolErrorHandler {
  private errors: CobolParsingError[] = [];
  private warnings: CobolParsingError[] = [];
  private maxErrors: number;
  private strategy: ErrorRecoveryStrategy;

  constructor(
    maxErrors: number = 10,
    strategy: ErrorRecoveryStrategy = ErrorRecoveryStrategy.COLLECT_ALL
  ) {
    this.maxErrors = maxErrors;
    this.strategy = strategy;
  }

  /**
   * Add a parsing error
   */
  addError(error: CobolParsingError): void {
    if (error.severity === 'error') {
      if (this.strategy === ErrorRecoveryStrategy.FAIL_FAST) {
        throw error;
      }
      
      // Check if adding this error would exceed the limit
      if (this.errors.length >= this.maxErrors) {
        throw new ParsingError(`Too many errors (${this.maxErrors}), stopping parsing`);
      }
      
      this.errors.push(error);
    } else {
      this.warnings.push(error);
    }
  }

  /**
   * Add syntax error from ANTLR
   */
  addSyntaxError(
    message: string,
    offendingToken?: Token,
    line?: number,
    column?: number
  ): void {
    const location: SourceLocation = {
      line: line || offendingToken?.line || 1,
      column: (column || (offendingToken as any)?.column || 0) + 1,
      endLine: line || offendingToken?.line || 1,
      endColumn: (column || (offendingToken as any)?.column || 0) + (offendingToken?.text?.length || 1) + 1
    };

    const error = new SyntaxError(message, offendingToken, undefined, location);
    this.addError(error);
  }

  /**
   * Add semantic error
   */
  addSemanticError(
    message: string,
    code: string,
    location: SourceLocation,
    relatedLocations?: SourceLocation[]
  ): void {
    const error = new SemanticError(message, code, location, relatedLocations);
    this.addError(error);
  }

  /**
   * Add analysis warning
   */
  addAnalysisWarning(
    message: string,
    analysisType: string,
    code: string,
    location: SourceLocation,
    suggestions?: string[]
  ): void {
    const warning = new AnalysisError(message, analysisType, code, location, suggestions);
    this.addError(warning);
  }

  /**
   * Get all errors
   */
  getErrors(): CobolParsingError[] {
    return [...this.errors];
  }

  /**
   * Get all warnings
   */
  getWarnings(): CobolParsingError[] {
    return [...this.warnings];
  }

  /**
   * Get all diagnostics
   */
  getAllDiagnostics(): DiagnosticMessage[] {
    return [
      ...this.errors.map(e => e.toDiagnostic()),
      ...this.warnings.map(w => w.toDiagnostic())
    ];
  }

  /**
   * Check if parsing was successful
   */
  hasErrors(): boolean {
    return this.errors.length > 0;
  }

  /**
   * Check if there are warnings
   */
  hasWarnings(): boolean {
    return this.warnings.length > 0;
  }

  /**
   * Clear all errors and warnings
   */
  clear(): void {
    this.errors = [];
    this.warnings = [];
  }

  /**
   * Get error summary
   */
  getSummary(): {
    errorCount: number;
    warningCount: number;
    hasErrors: boolean;
    hasWarnings: boolean;
  } {
    return {
      errorCount: this.errors.length,
      warningCount: this.warnings.length,
      hasErrors: this.hasErrors(),
      hasWarnings: this.hasWarnings()
    };
  }

  /**
   * Generate error report
   */
  generateReport(): string {
    const lines: string[] = [];
    
    if (this.errors.length > 0) {
      lines.push(`Errors (${this.errors.length}):`);
      for (const error of this.errors) {
        const loc = error.location;
        lines.push(`  ${error.code}: ${error.message} (${loc?.line}:${loc?.column})`);
      }
    }
    
    if (this.warnings.length > 0) {
      lines.push(`Warnings (${this.warnings.length}):`);
      for (const warning of this.warnings) {
        const loc = warning.location;
        lines.push(`  ${warning.code}: ${warning.message} (${loc?.line}:${loc?.column})`);
      }
    }
    
    if (this.errors.length === 0 && this.warnings.length === 0) {
      lines.push('No errors or warnings');
    }
    
    return lines.join('\n');
  }
}