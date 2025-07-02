/**
 * Error Coordinator - Integrates three-tier error handling system
 * Coordinates syntax, semantic, and static analysis error detection
 */

import { CobolProgram } from '../ast/nodes/CobolProgram';
import { DiagnosticMessage } from '../core/types';
import { 
  CobolErrorHandler, 
  ErrorRecoveryStrategy,
  SyntaxError,
  SemanticError,
  AnalysisError
} from '../parser/error-handler';
import { SemanticValidator, SemanticValidationConfig, DEFAULT_SEMANTIC_CONFIG } from './semantic-validator';
import { StaticAnalyzer, StaticAnalysisConfig, DEFAULT_STATIC_ANALYSIS_CONFIG } from './static-analyzer';

/**
 * Error coordination configuration
 */
export interface ErrorCoordinationConfig {
  /** Semantic validation configuration */
  semantic: SemanticValidationConfig;
  
  /** Static analysis configuration */
  staticAnalysis: StaticAnalysisConfig;
  
  /** Error recovery strategy */
  recoveryStrategy: ErrorRecoveryStrategy;
  
  /** Maximum errors before stopping analysis */
  maxErrors: number;
  
  /** Continue analysis after syntax errors */
  continueOnSyntaxError: boolean;
  
  /** Continue analysis after semantic errors */
  continueOnSemanticError: boolean;
  
  /** Enable error correlation */
  enableErrorCorrelation: boolean;
  
  /** Suppress duplicate errors */
  suppressDuplicates: boolean;
}

export const DEFAULT_ERROR_COORDINATION_CONFIG: ErrorCoordinationConfig = {
  semantic: DEFAULT_SEMANTIC_CONFIG,
  staticAnalysis: DEFAULT_STATIC_ANALYSIS_CONFIG,
  recoveryStrategy: ErrorRecoveryStrategy.COLLECT_ALL,
  maxErrors: 100,
  continueOnSyntaxError: true,
  continueOnSemanticError: true,
  enableErrorCorrelation: true,
  suppressDuplicates: true
};

/**
 * Error analysis result
 */
export interface ErrorAnalysisResult {
  /** Total error count by tier */
  tierCounts: {
    syntax: number;
    semantic: number;
    analysis: number;
  };
  
  /** All diagnostics combined */
  diagnostics: DiagnosticMessage[];
  
  /** Syntax errors only */
  syntaxErrors: SyntaxError[];
  
  /** Semantic errors only */
  semanticErrors: SemanticError[];
  
  /** Analysis errors/warnings only */
  analysisIssues: AnalysisError[];
  
  /** Success status */
  hasErrors: boolean;
  hasWarnings: boolean;
  
  /** Error correlation information */
  correlatedErrors?: ErrorCorrelation[];
  
  /** Performance metrics */
  analysisTime: number;
}

/**
 * Error correlation information
 */
export interface ErrorCorrelation {
  /** Primary error */
  primaryError: DiagnosticMessage;
  
  /** Related errors that may be caused by the primary */
  relatedErrors: DiagnosticMessage[];
  
  /** Correlation confidence (0-1) */
  confidence: number;
  
  /** Suggested fix for all related errors */
  suggestedFix?: string;
}

/**
 * Central error coordinator for three-tier error handling
 */
export class ErrorCoordinator {
  private config: ErrorCoordinationConfig;
  private masterErrorHandler: CobolErrorHandler;
  private semanticValidator: SemanticValidator;
  private staticAnalyzer: StaticAnalyzer;
  private seenErrors: Set<string> = new Set();

  constructor(config: ErrorCoordinationConfig = DEFAULT_ERROR_COORDINATION_CONFIG) {
    this.config = config;
    this.masterErrorHandler = new CobolErrorHandler(config.maxErrors, config.recoveryStrategy);
    this.semanticValidator = new SemanticValidator(config.semantic);
    this.staticAnalyzer = new StaticAnalyzer(config.staticAnalysis);
  }

  /**
   * Perform comprehensive error analysis on COBOL program
   */
  analyzeErrors(program: CobolProgram, syntaxErrors: SyntaxError[] = []): ErrorAnalysisResult {
    const startTime = performance.now();
    
    this.clearAll();
    
    // Add any syntax errors from parsing phase
    syntaxErrors.forEach(error => this.masterErrorHandler.addError(error));
    
    const result: ErrorAnalysisResult = {
      tierCounts: { syntax: 0, semantic: 0, analysis: 0 },
      diagnostics: [],
      syntaxErrors: [],
      semanticErrors: [],
      analysisIssues: [],
      hasErrors: false,
      hasWarnings: false,
      analysisTime: 0
    };

    try {
      // Tier 1: Syntax errors are already captured during parsing
      result.syntaxErrors = syntaxErrors;
      result.tierCounts.syntax = syntaxErrors.length;

      // Tier 2: Semantic validation
      if (this.config.continueOnSyntaxError || syntaxErrors.length === 0) {
        this.performSemanticValidation(program);
        result.semanticErrors = this.semanticValidator.getErrors();
        result.tierCounts.semantic = result.semanticErrors.length;
      }

      // Tier 3: Static analysis
      if ((this.config.continueOnSemanticError || result.semanticErrors.length === 0) &&
          (this.config.continueOnSyntaxError || syntaxErrors.length === 0)) {
        this.performStaticAnalysis(program);
        result.analysisIssues = [
          ...this.staticAnalyzer.getErrors(),
          ...this.staticAnalyzer.getWarnings()
        ];
        result.tierCounts.analysis = result.analysisIssues.length;
      }

      // Combine all diagnostics
      result.diagnostics = this.getAllDiagnostics();
      
      // Apply deduplication if enabled
      if (this.config.suppressDuplicates) {
        result.diagnostics = this.deduplicateErrors(result.diagnostics);
      }

      // Perform error correlation if enabled
      if (this.config.enableErrorCorrelation) {
        result.correlatedErrors = this.correlateErrors(result.diagnostics);
      }

      // Set status flags
      result.hasErrors = result.diagnostics.some(d => d.severity === 'error');
      result.hasWarnings = result.diagnostics.some(d => d.severity === 'warning');

      result.analysisTime = performance.now() - startTime;

    } catch (error) {
      // Capture analysis failures
      this.masterErrorHandler.addError({
        name: 'ErrorAnalysisError',
        message: `Error analysis failed: ${error instanceof Error ? error.message : String(error)}`,
        code: 'ERROR_ANALYSIS_FAILED',
        severity: 'error' as const,
        location: { line: 1, column: 1, endLine: 1, endColumn: 1 },
        toDiagnostic: function() {
          return {
            severity: this.severity,
            code: this.code,
            message: this.message,
            location: this.location
          };
        }
      } as any);

      result.diagnostics = this.getAllDiagnostics();
      result.hasErrors = true;
      result.analysisTime = performance.now() - startTime;
    }

    return result;
  }

  /**
   * Perform semantic validation
   */
  private performSemanticValidation(program: CobolProgram): void {
    try {
      this.semanticValidator.validate(program);
      
      // Merge semantic validation results
      const semanticHandler = this.semanticValidator.getErrorHandler();
      semanticHandler.getErrors().forEach(error => this.masterErrorHandler.addError(error));
      semanticHandler.getWarnings().forEach(warning => this.masterErrorHandler.addError(warning));
      
    } catch (error) {
      this.masterErrorHandler.addSemanticError(
        `Semantic validation failed: ${error instanceof Error ? error.message : String(error)}`,
        'SEMANTIC_VALIDATION_FAILED',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
    }
  }

  /**
   * Perform static analysis
   */
  private performStaticAnalysis(program: CobolProgram): void {
    try {
      this.staticAnalyzer.analyze(program);
      
      // Merge static analysis results
      const analysisHandler = this.staticAnalyzer.getErrorHandler();
      analysisHandler.getErrors().forEach(error => this.masterErrorHandler.addError(error));
      analysisHandler.getWarnings().forEach(warning => this.masterErrorHandler.addError(warning));
      
    } catch (error) {
      this.masterErrorHandler.addAnalysisWarning(
        `Static analysis failed: ${error instanceof Error ? error.message : String(error)}`,
        'static-analysis',
        'STATIC_ANALYSIS_FAILED',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
    }
  }

  /**
   * Get all diagnostics from all error handlers
   */
  private getAllDiagnostics(): DiagnosticMessage[] {
    return this.masterErrorHandler.getAllDiagnostics();
  }

  /**
   * Deduplicate errors based on message and location
   */
  private deduplicateErrors(diagnostics: DiagnosticMessage[]): DiagnosticMessage[] {
    const seen = new Set<string>();
    return diagnostics.filter(diagnostic => {
      const key = `${diagnostic.code}:${diagnostic.message}:${diagnostic.location.line}:${diagnostic.location.column}`;
      if (seen.has(key)) {
        return false;
      }
      seen.add(key);
      return true;
    });
  }

  /**
   * Correlate errors to identify root causes
   */
  private correlateErrors(diagnostics: DiagnosticMessage[]): ErrorCorrelation[] {
    const correlations: ErrorCorrelation[] = [];
    
    // Group errors by location proximity
    const errorGroups = this.groupErrorsByProximity(diagnostics);
    
    errorGroups.forEach(group => {
      if (group.length > 1) {
        // Identify primary error (usually the first or most severe)
        const primaryError = this.identifyPrimaryError(group);
        const relatedErrors = group.filter(e => e !== primaryError);
        
        if (relatedErrors.length > 0) {
          correlations.push({
            primaryError,
            relatedErrors,
            confidence: this.calculateCorrelationConfidence(primaryError, relatedErrors),
            suggestedFix: this.generateCorrelatedFix(primaryError, relatedErrors)
          });
        }
      }
    });

    return correlations;
  }

  /**
   * Group errors by location proximity
   */
  private groupErrorsByProximity(diagnostics: DiagnosticMessage[]): DiagnosticMessage[][] {
    const groups: DiagnosticMessage[][] = [];
    const processed = new Set<DiagnosticMessage>();
    
    diagnostics.forEach(diagnostic => {
      if (processed.has(diagnostic)) return;
      
      const group: DiagnosticMessage[] = [diagnostic];
      processed.add(diagnostic);
      
      // Find nearby errors (within 5 lines)
      diagnostics.forEach(other => {
        if (processed.has(other)) return;
        
        const lineDiff = Math.abs(diagnostic.location.line - other.location.line);
        if (lineDiff <= 5) {
          group.push(other);
          processed.add(other);
        }
      });
      
      groups.push(group);
    });
    
    return groups;
  }

  /**
   * Identify primary error in a group
   */
  private identifyPrimaryError(group: DiagnosticMessage[]): DiagnosticMessage {
    // Prioritize by severity, then by tier, then by position
    return group.sort((a, b) => {
      // Errors before warnings
      if (a.severity === 'error' && b.severity !== 'error') return -1;
      if (b.severity === 'error' && a.severity !== 'error') return 1;
      
      // Syntax errors before semantic before analysis
      const tierPriority = (code: string): number => {
        if (code.includes('SYNTAX')) return 1;
        if (code.includes('SEMANTIC')) return 2;
        return 3;
      };
      
      const aTier = tierPriority(a.code);
      const bTier = tierPriority(b.code);
      if (aTier !== bTier) return aTier - bTier;
      
      // Earlier in file first
      return a.location.line - b.location.line;
    })[0];
  }

  /**
   * Calculate correlation confidence
   */
  private calculateCorrelationConfidence(primary: DiagnosticMessage, related: DiagnosticMessage[]): number {
    let confidence = 0.5; // Base confidence
    
    // Higher confidence if errors are on same line
    const sameLine = related.some(r => r.location.line === primary.location.line);
    if (sameLine) confidence += 0.3;
    
    // Higher confidence if similar error codes
    const similarCodes = related.some(r => 
      r.code.split('_')[0] === primary.code.split('_')[0]
    );
    if (similarCodes) confidence += 0.2;
    
    return Math.min(confidence, 1.0);
  }

  /**
   * Generate fix suggestion for correlated errors
   */
  private generateCorrelatedFix(primary: DiagnosticMessage, related: DiagnosticMessage[]): string | undefined {
    // Generate context-aware fix suggestions
    if (primary.code.includes('SYNTAX') && related.some(r => r.code.includes('SEMANTIC'))) {
      return 'Fix the syntax error first, which may resolve the related semantic issues';
    }
    
    if (primary.code.includes('MISSING') && related.some(r => r.code.includes('UNDEFINED'))) {
      return 'Add the missing declaration to resolve undefined references';
    }
    
    return undefined;
  }

  /**
   * Clear all error states
   */
  private clearAll(): void {
    this.masterErrorHandler.clear();
    this.semanticValidator.clear();
    this.staticAnalyzer.clear();
    this.seenErrors.clear();
  }

  /**
   * Get master error handler
   */
  getErrorHandler(): CobolErrorHandler {
    return this.masterErrorHandler;
  }

  /**
   * Get semantic validator
   */
  getSemanticValidator(): SemanticValidator {
    return this.semanticValidator;
  }

  /**
   * Get static analyzer
   */
  getStaticAnalyzer(): StaticAnalyzer {
    return this.staticAnalyzer;
  }

  /**
   * Update configuration
   */
  updateConfig(config: Partial<ErrorCoordinationConfig>): void {
    this.config = { ...this.config, ...config };
    
    // Update child components
    this.masterErrorHandler = new CobolErrorHandler(this.config.maxErrors, this.config.recoveryStrategy);
    this.semanticValidator = new SemanticValidator(this.config.semantic);
    this.staticAnalyzer = new StaticAnalyzer(this.config.staticAnalysis);
  }

  /**
   * Generate comprehensive error report
   */
  generateErrorReport(result: ErrorAnalysisResult): string {
    const lines: string[] = [];
    
    lines.push('=== COBOL Error Analysis Report ===\n');
    
    // Summary
    lines.push('SUMMARY:');
    lines.push(`  Tier 1 (Syntax): ${result.tierCounts.syntax} errors`);
    lines.push(`  Tier 2 (Semantic): ${result.tierCounts.semantic} errors`);
    lines.push(`  Tier 3 (Analysis): ${result.tierCounts.analysis} issues`);
    lines.push(`  Total Diagnostics: ${result.diagnostics.length}`);
    lines.push(`  Analysis Time: ${result.analysisTime.toFixed(2)}ms\n`);
    
    // Errors by severity
    const errors = result.diagnostics.filter(d => d.severity === 'error');
    const warnings = result.diagnostics.filter(d => d.severity === 'warning');
    
    if (errors.length > 0) {
      lines.push(`ERRORS (${errors.length}):`);
      errors.forEach(error => {
        lines.push(`  ${error.code}: ${error.message} (${error.location.line}:${error.location.column})`);
      });
      lines.push('');
    }
    
    if (warnings.length > 0) {
      lines.push(`WARNINGS (${warnings.length}):`);
      warnings.forEach(warning => {
        lines.push(`  ${warning.code}: ${warning.message} (${warning.location.line}:${warning.location.column})`);
      });
      lines.push('');
    }
    
    // Correlations
    if (result.correlatedErrors && result.correlatedErrors.length > 0) {
      lines.push('ERROR CORRELATIONS:');
      result.correlatedErrors.forEach((correlation, index) => {
        lines.push(`  ${index + 1}. Primary: ${correlation.primaryError.message}`);
        lines.push(`     Related: ${correlation.relatedErrors.length} issues`);
        lines.push(`     Confidence: ${(correlation.confidence * 100).toFixed(0)}%`);
        if (correlation.suggestedFix) {
          lines.push(`     Suggestion: ${correlation.suggestedFix}`);
        }
        lines.push('');
      });
    }
    
    if (result.diagnostics.length === 0) {
      lines.push('No errors or warnings found.');
    }
    
    return lines.join('\n');
  }
}