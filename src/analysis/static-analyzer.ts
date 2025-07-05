/**
 * Static Analyzer - Tier 3 Error Detection
 * Detects static analysis issues like dead code, unused variables, etc.
 */

import { CobolProgram } from '../ast/nodes/CobolProgram';
import { SectionNode } from '../ast/nodes/SectionNode';
import { ParagraphNode } from '../ast/nodes/ParagraphNode';
import { StatementNode } from '../ast/nodes/StatementNode';
import { DataDivision } from '../ast/nodes/DataDivision';
import { ProcedureDivision } from '../ast/nodes/ProcedureDivision';
import { SourceLocation, VariableDefinition, CallReference, StatementType } from '../core/types';
import { AnalysisError, CobolErrorHandler } from '../parser/error-handler';

/**
 * Static analysis configuration
 */
export interface StaticAnalysisConfig {
  /** Detect dead code */
  detectDeadCode: boolean;
  
  /** Detect unreachable paragraphs */
  detectUnreachableParagraphs: boolean;
  
  /** Detect unused variables */
  detectUnusedVariables: boolean;
  
  /** Detect circular dependencies */
  detectCircularDependencies: boolean;
  
  /** Detect infinite loops */
  detectInfiniteLoops: boolean;
  
  /** Detect unreferenced sections */
  detectUnreferencedSections: boolean;
  
  /** Check for missing GO TO targets */
  checkMissingGoToTargets: boolean;
  
  /** Analyze variable usage patterns */
  analyzeVariableUsage: boolean;
  
  /** Calculate code quality metrics */
  calculateCodeQualityMetrics: boolean;
  
  /** Analyze performance bottlenecks */
  analyzePerformanceBottlenecks: boolean;
  
  /** Detect security vulnerabilities */
  detectSecurityVulnerabilities: boolean;
  
  /** Perform control flow analysis */
  performControlFlowAnalysis: boolean;
  
  /** Perform data flow analysis */
  performDataFlowAnalysis: boolean;
  
  /** Analyze code maintainability */
  analyzeMaintainability: boolean;
  
  /** Detect code smells */
  detectCodeSmells: boolean;
  
  /** Analyze complexity metrics */
  analyzeComplexityMetrics: boolean;
}

export const DEFAULT_STATIC_ANALYSIS_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: true,
  calculateCodeQualityMetrics: true,
  analyzePerformanceBottlenecks: true,
  detectSecurityVulnerabilities: true,
  performControlFlowAnalysis: true,
  performDataFlowAnalysis: true,
  analyzeMaintainability: true,
  detectCodeSmells: true,
  analyzeComplexityMetrics: true
};

/**
 * Call graph node for dependency analysis
 */
interface CallGraphNode {
  name: string;
  type: 'section' | 'paragraph';
  location: SourceLocation;
  callers: string[];
  callees: string[];
  visited: boolean;
  reachable: boolean;
}

/**
 * Variable usage information
 */
interface VariableUsage {
  name: string;
  definition: VariableDefinition;
  references: SourceLocation[];
  isRead: boolean;
  isWritten: boolean;
  isInitialized: boolean;
  readCount: number;
  writeCount: number;
  firstUse?: SourceLocation;
  lastUse?: SourceLocation;
  dataFlowPaths: DataFlowPath[];
}

/**
 * Code quality metrics
 */
interface CodeQualityMetrics {
  cyclomaticComplexity: number;
  nestingDepth: number;
  linesOfCode: number;
  halsteadMetrics: HalsteadMetrics;
  maintainabilityIndex: number;
  technicalDebt: number;
  codeSmells: string[];
}

/**
 * Halstead metrics
 */
interface HalsteadMetrics {
  operators: number;
  operands: number;
  uniqueOperators: number;
  uniqueOperands: number;
  difficulty: number;
  volume: number;
  effort: number;
  bugs: number;
  time: number;
}

/**
 * Performance bottleneck
 */
interface PerformanceBottleneck {
  type: 'nested-loop' | 'file-io' | 'string-manipulation' | 'inefficient-search';
  location: SourceLocation;
  description: string;
  severity: 'low' | 'medium' | 'high';
  suggestion: string;
}

/**
 * Security vulnerability
 */
interface SecurityVulnerability {
  type: 'hardcoded-credential' | 'unsafe-file-operation' | 'sql-injection' | 'buffer-overflow';
  location: SourceLocation;
  description: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  recommendation: string;
}

/**
 * Control flow graph node
 */
interface ControlFlowNode {
  id: string;
  type: 'statement' | 'condition' | 'loop' | 'branch';
  statement?: StatementNode;
  predecessors: string[];
  successors: string[];
  dominators: string[];
  postDominators: string[];
}

/**
 * Data flow path
 */
interface DataFlowPath {
  variable: string;
  definition: SourceLocation;
  uses: SourceLocation[];
  reachable: boolean;
  hasUse: boolean;
  isLive: boolean;
}

/**
 * Static analyzer for COBOL programs
 */
export class StaticAnalyzer {
  private config: StaticAnalysisConfig;
  private errorHandler: CobolErrorHandler;
  private currentProgram?: CobolProgram;
  private callGraph: Map<string, CallGraphNode> = new Map();
  private variableUsage: Map<string, VariableUsage> = new Map();
  private codeQualityMetrics?: CodeQualityMetrics;
  private performanceBottlenecks: PerformanceBottleneck[] = [];
  private securityVulnerabilities: SecurityVulnerability[] = [];
  private controlFlowGraph: Map<string, ControlFlowNode> = new Map();
  private dataFlowPaths: Map<string, DataFlowPath[]> = new Map();

  constructor(config: StaticAnalysisConfig = DEFAULT_STATIC_ANALYSIS_CONFIG) {
    this.config = config;
    this.errorHandler = new CobolErrorHandler();
  }

  /**
   * Analyze entire COBOL program
   */
  analyze(program: CobolProgram): void {
    this.currentProgram = program;
    this.errorHandler.clear();
    this.callGraph.clear();
    this.variableUsage.clear();
    this.codeQualityMetrics = undefined;
    this.performanceBottlenecks = [];
    this.securityVulnerabilities = [];
    this.controlFlowGraph.clear();
    this.dataFlowPaths.clear();

    try {
      // Build call graph and usage maps
      this.buildCallGraph(program);
      this.buildVariableUsageMap(program);

      // Tier 3 Static Analysis
      if (this.config.detectDeadCode) {
        this.detectDeadCode();
      }

      if (this.config.detectUnreachableParagraphs) {
        this.detectUnreachableParagraphs();
      }

      if (this.config.detectUnusedVariables) {
        this.detectUnusedVariables();
      }

      if (this.config.detectCircularDependencies) {
        this.detectCircularDependencies();
      }

      if (this.config.detectInfiniteLoops) {
        this.detectInfiniteLoops();
      }

      if (this.config.detectUnreferencedSections) {
        this.detectUnreferencedSections();
      }

      if (this.config.checkMissingGoToTargets) {
        this.checkMissingGoToTargets();
      }

      if (this.config.analyzeVariableUsage) {
        this.analyzeVariableUsage();
      }

      if (this.config.calculateCodeQualityMetrics) {
        this.calculateCodeQualityMetrics();
      }

      if (this.config.analyzePerformanceBottlenecks) {
        this.analyzePerformanceBottlenecks();
      }

      if (this.config.detectSecurityVulnerabilities) {
        this.detectSecurityVulnerabilities();
      }

      if (this.config.performControlFlowAnalysis) {
        this.performControlFlowAnalysis();
      }

      if (this.config.performDataFlowAnalysis) {
        this.performDataFlowAnalysis();
      }

      if (this.config.analyzeMaintainability) {
        this.analyzeMaintainability();
      }

      if (this.config.detectCodeSmells) {
        this.detectCodeSmells();
      }

      if (this.config.analyzeComplexityMetrics) {
        this.analyzeComplexityMetrics();
      }

    } catch (error) {
      // Create an AnalysisError and add it to the error handler
      const analysisError = new AnalysisError(
        `Static analysis failed: ${error}`,
        'GENERAL',
        'STATIC_ANALYSIS_FAILED',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
      this.errorHandler.addError(analysisError);
    }
  }

  /**
   * Build call graph from program
   */
  private buildCallGraph(program: CobolProgram): void {
    // Implementation here
  }

  /**
   * Build variable usage map
   */
  private buildVariableUsageMap(program: CobolProgram): void {
    this.variableUsage.clear();
    
    // Initialize variable usage from data division
    if (program.dataDivision?.workingStorage) {
      for (const variable of program.dataDivision.workingStorage) {
        this.variableUsage.set(variable.name, {
          name: variable.name,
          definition: variable,
          references: [],
          isRead: false,
          isWritten: false,
          isInitialized: !!variable.initialValue,
          readCount: 0,
          writeCount: 0,
          dataFlowPaths: []
        });
      }
    }
    
    // Analyze statements in procedure division
    if (program.procedureDivision?.paragraphs) {
      for (const paragraph of program.procedureDivision.paragraphs) {
        if ((paragraph as any).statements) {
          for (const statement of (paragraph as any).statements) {
            if (statement instanceof StatementNode) {
              this.analyzeStatementForVariableUsage(statement);
            }
          }
        }
      }
    }
  }

  private analyzeStatementForVariableUsage(statement: StatementNode): void {
    const sourceText = statement.sourceText || '';
    
    switch (statement.statementType) {
      case 'MOVE':
        this.analyzeMoveStatement(statement, sourceText);
        break;
      case 'STRING':
        this.analyzeStringStatement(statement, sourceText);
        break;
      case 'DISPLAY':
        this.analyzeDisplayStatement(statement, sourceText);
        break;
      case 'COMPUTE':
        this.analyzeComputeStatement(statement, sourceText);
        break;
      case 'ADD':
      case 'SUBTRACT':
      case 'MULTIPLY':
      case 'DIVIDE':
        this.analyzeArithmeticStatement(statement, sourceText);
        break;
      default:
        // For other statements, try to extract variable references
        this.analyzeGenericStatement(statement, sourceText);
        break;
    }
  }

  private analyzeMoveStatement(statement: StatementNode, sourceText: string): void {
    // MOVE source TO destination
    const moveMatch = sourceText.match(/MOVE\s+([A-Z0-9\-]+)\s+TO\s+([A-Z0-9\-]+)/i);
    if (moveMatch) {
      const source = moveMatch[1];
      const destination = moveMatch[2];
      
      if (this.variableUsage.has(source)) {
        this.markVariableAsRead(source, statement.location);
      }
      if (this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
    }
  }

  private analyzeStringStatement(statement: StatementNode, sourceText: string): void {
    // STRING var1 DELIMITED BY ... var2 DELIMITED BY ... INTO result
    const stringMatch = sourceText.match(/STRING\s+(.*?)\s+INTO\s+([A-Z0-9\-]+)/i);
    if (stringMatch) {
      const sourcesPart = stringMatch[1];
      const destination = stringMatch[2];
      
      // Extract variables from the sources part
      const variableMatches = sourcesPart.match(/([A-Z0-9\-]+)/g);
      if (variableMatches) {
        for (const variable of variableMatches) {
          if (this.variableUsage.has(variable) && !['DELIMITED', 'BY', 'SIZE', 'SPACE'].includes(variable)) {
            this.markVariableAsRead(variable, statement.location);
          }
        }
      }
      
      // Mark destination as written
      if (this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
    }
  }

  private analyzeDisplayStatement(statement: StatementNode, sourceText: string): void {
    // DISPLAY variable
    const displayMatch = sourceText.match(/DISPLAY\s+([A-Z0-9\-]+)/i);
    if (displayMatch) {
      const variable = displayMatch[1];
      if (this.variableUsage.has(variable)) {
        this.markVariableAsRead(variable, statement.location);
      }
    }
  }

  private analyzeComputeStatement(statement: StatementNode, sourceText: string): void {
    // COMPUTE result = expression
    const computeMatch = sourceText.match(/COMPUTE\s+([A-Z0-9\-]+)\s*=\s*(.*)/i);
    if (computeMatch) {
      const destination = computeMatch[1];
      const expression = computeMatch[2];
      
      // Mark destination as written
      if (this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
      
      // Extract variables from expression
      const variableMatches = expression.match(/([A-Z0-9\-]+)/g);
      if (variableMatches) {
        for (const variable of variableMatches) {
          if (this.variableUsage.has(variable)) {
            this.markVariableAsRead(variable, statement.location);
          }
        }
      }
    }
  }

  private analyzeArithmeticStatement(statement: StatementNode, sourceText: string): void {
    // ADD/SUBTRACT/MULTIPLY/DIVIDE operations
    const variableMatches = sourceText.match(/([A-Z0-9\-]+)/g);
    if (variableMatches) {
      for (const variable of variableMatches) {
        if (this.variableUsage.has(variable)) {
          this.markVariableAsRead(variable, statement.location);
        }
      }
    }
  }

  private analyzeGenericStatement(statement: StatementNode, sourceText: string): void {
    // Generic variable extraction for other statements
    const variableMatches = sourceText.match(/([A-Z0-9\-]+)/g);
    if (variableMatches) {
      for (const variable of variableMatches) {
        if (this.variableUsage.has(variable)) {
          this.markVariableAsRead(variable, statement.location);
        }
      }
    }
  }

  private markVariableAsRead(variableName: string, location: SourceLocation): void {
    const usage = this.variableUsage.get(variableName);
    if (usage) {
      usage.isRead = true;
      usage.readCount++;
      usage.references.push(location);
      if (!usage.firstUse) {
        usage.firstUse = location;
      }
      usage.lastUse = location;
    }
  }

  private markVariableAsWritten(variableName: string, location: SourceLocation): void {
    const usage = this.variableUsage.get(variableName);
    if (usage) {
      usage.isWritten = true;
      usage.writeCount++;
      usage.references.push(location);
      if (!usage.firstUse) {
        usage.firstUse = location;
      }
      usage.lastUse = location;
    }
  }

  /**
   * Detect dead code
   */
  private detectDeadCode(): void {
    // Implementation here
  }

  /**
   * Detect unreachable paragraphs
   */
  private detectUnreachableParagraphs(): void {
    // Implementation here
  }

  /**
   * Detect unused variables
   */
  private detectUnusedVariables(): void {
    // Implementation here
  }

  /**
   * Detect circular dependencies
   */
  private detectCircularDependencies(): void {
    // Implementation here
  }

  /**
   * Detect infinite loops
   */
  private detectInfiniteLoops(): void {
    // Implementation here
  }

  /**
   * Detect unreferenced sections
   */
  private detectUnreferencedSections(): void {
    // Implementation here
  }

  /**
   * Check for missing GO TO targets
   */
  private checkMissingGoToTargets(): void {
    if (!this.currentProgram?.procedureDivision) {
      return;
    }

    // Get all available targets (paragraphs and sections)
    const availableTargets = new Set<string>();
    
    // Add paragraph names
    for (const paragraph of this.currentProgram.procedureDivision.paragraphs || []) {
      availableTargets.add(paragraph.name);
    }
    
    // Add section names
    for (const section of this.currentProgram.procedureDivision.sections || []) {
      availableTargets.add(section.name);
    }

    // Check GO TO statements for missing targets
    for (const paragraph of this.currentProgram.procedureDivision.paragraphs || []) {
      if ((paragraph as any).statements) {
        for (const statement of (paragraph as any).statements) {
          if (statement instanceof StatementNode && statement.statementType === 'GO') {
            // Extract targets from GO TO statement
            const sourceText = statement.sourceText || '';
            
            // Handle both simple GO TO and GO TO ... DEPENDING ON patterns
            const dependingOnMatch = sourceText.match(/GO\s+TO\s+((?:[A-Z0-9\-]+\s*)+)\s+DEPENDING\s+ON/i);
            const simpleGoToMatch = sourceText.match(/GO\s+TO\s+([A-Z0-9\-]+)(?!\s+\w+\s+DEPENDING)/i);
            
            if (dependingOnMatch) {
              // Handle GO TO PARA-A PARA-B PARA-C DEPENDING ON format
              const targetsText = dependingOnMatch[1];
              const targets = targetsText.trim().split(/\s+/);
              
              for (const target of targets) {
                if (target && !availableTargets.has(target)) {
                  this.errorHandler.addError(
                    new AnalysisError(
                      `GO TO target '${target}' not found`,
                      'static-analysis',
                      'MISSING_GOTO_TARGET',
                      statement.location
                    )
                  );
                }
              }
            } else if (simpleGoToMatch && simpleGoToMatch[1]) {
              // Handle simple GO TO format
              const target = simpleGoToMatch[1];
              if (!availableTargets.has(target)) {
                this.errorHandler.addError(
                  new AnalysisError(
                    `GO TO target '${target}' not found`,
                    'static-analysis',
                    'MISSING_GOTO_TARGET',
                    statement.location
                  )
                );
              }
            }
          }
        }
      }
    }
  }

  /**
   * Analyze variable usage patterns
   */
  private analyzeVariableUsage(): void {
    // Implementation here
  }

  /**
   * Calculate code quality metrics
   */
  private calculateCodeQualityMetrics(): void {
    // Implementation here
  }

  /**
   * Analyze performance bottlenecks
   */
  private analyzePerformanceBottlenecks(): void {
    // Implementation here
  }

  /**
   * Detect security vulnerabilities
   */
  private detectSecurityVulnerabilities(): void {
    // Implementation here
  }

  /**
   * Perform control flow analysis
   */
  private performControlFlowAnalysis(): void {
    // Implementation here
  }

  /**
   * Perform data flow analysis
   */
  private performDataFlowAnalysis(): void {
    // Implementation here
  }

  /**
   * Analyze maintainability
   */
  private analyzeMaintainability(): void {
    // Implementation here
  }

  /**
   * Detect code smells
   */
  private detectCodeSmells(): void {
    // Implementation here
  }

  /**
   * Analyze complexity metrics
   */
  private analyzeComplexityMetrics(): void {
    // Implementation here
  }

  /**
   * Get analysis errors
   */
  getErrors(): AnalysisError[] {
    return this.errorHandler.getErrors().filter(e => e instanceof AnalysisError) as AnalysisError[];
  }

  /**
   * Get analysis warnings
   */
  getWarnings(): AnalysisError[] {
    return this.errorHandler.getWarnings().filter(w => w instanceof AnalysisError) as AnalysisError[];
  }

  /**
   * Get error handler for integration
   */
  getErrorHandler(): CobolErrorHandler {
    return this.errorHandler;
  }

  /**
   * Get variable usage information
   */
  getVariableUsage(): Map<string, VariableUsage> {
    return new Map(this.variableUsage);
  }

  /**
   * Get call graph
   */
  getCallGraph(): Map<string, CallGraphNode> {
    return new Map(this.callGraph);
  }

  /**
   * Get code quality metrics
   */
  getCodeQualityMetrics(): CodeQualityMetrics | undefined {
    return this.codeQualityMetrics;
  }

  /**
   * Get performance bottlenecks
   */
  getPerformanceBottlenecks(): PerformanceBottleneck[] {
    return [...this.performanceBottlenecks];
  }

  /**
   * Get security vulnerabilities
   */
  getSecurityVulnerabilities(): SecurityVulnerability[] {
    return [...this.securityVulnerabilities];
  }

  /**
   * Get control flow graph
   */
  getControlFlowGraph(): Map<string, ControlFlowNode> {
    return new Map(this.controlFlowGraph);
  }

  /**
   * Get data flow paths
   */
  getDataFlowPaths(): Map<string, DataFlowPath[]> {
    return new Map(this.dataFlowPaths);
  }

  /**
   * Get current configuration
   */
  getConfiguration(): StaticAnalysisConfig {
    return { ...this.config };
  }

  /**
   * Clear all analysis data
   */
  clear(): void {
    this.errorHandler.clear();
    this.callGraph.clear();
    this.variableUsage.clear();
    this.codeQualityMetrics = undefined;
    this.performanceBottlenecks = [];
    this.securityVulnerabilities = [];
    this.controlFlowGraph.clear();
    this.dataFlowPaths.clear();
    this.currentProgram = undefined;
  }

  /**
   * Get analysis summary
   */
  getSummary(): any {
    return {
      errorsCount: this.getErrors().length,
      warningsCount: this.getWarnings().length,
      variablesAnalyzed: this.variableUsage.size,
      callGraphNodes: this.callGraph.size,
      performanceIssues: this.performanceBottlenecks.length,
      securityIssues: this.securityVulnerabilities.length,
      controlFlowNodes: this.controlFlowGraph.size,
      dataFlowPaths: this.dataFlowPaths.size,
      codeQualityMetrics: this.codeQualityMetrics
    };
  }
}