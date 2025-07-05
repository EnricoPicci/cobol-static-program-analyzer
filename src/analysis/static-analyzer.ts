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
    // Handle null or undefined program gracefully
    if (!program) {
      this.errorHandler.addError(
        new AnalysisError(
          'Program is null or undefined',
          'static-analysis',
          'INVALID_PROGRAM',
          { line: 1, column: 1, endLine: 1, endColumn: 1 }
        )
      );
      return;
    }
    
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

      if (this.config.detectUnreachableParagraphs && this.config.detectDeadCode) {
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
    this.callGraph.clear();
    
    if (!program.procedureDivision) {
      return;
    }
    
    // Initialize call graph nodes for paragraphs
    if (program.procedureDivision.paragraphs) {
      for (const paragraph of program.procedureDivision.paragraphs) {
        this.callGraph.set(paragraph.name, {
          name: paragraph.name,
          type: 'paragraph',
          location: paragraph.location,
          callers: [],
          callees: [],
          visited: false,
          reachable: false
        });
      }
    }
    
    // Initialize call graph nodes for sections
    if (program.procedureDivision.sections) {
      for (const section of program.procedureDivision.sections) {
        this.callGraph.set(section.name, {
          name: section.name,
          type: 'section',
          location: section.location,
          callers: [],
          callees: [],
          visited: false,
          reachable: false
        });
        
        // Add paragraphs within sections
        if (section.paragraphs) {
          for (const paragraph of section.paragraphs) {
            this.callGraph.set(paragraph.name, {
              name: paragraph.name,
              type: 'paragraph',
              location: paragraph.location,
              callers: [],
              callees: [],
              visited: false,
              reachable: false
            });
          }
        }
      }
    }
    
    // Build call relationships
    this.buildCallRelationships(program);
    
    // Mark reachable nodes
    this.markReachableNodes();
  }
  
  /**
   * Build call relationships between paragraphs and sections
   */
  private buildCallRelationships(program: CobolProgram): void {
    if (!program.procedureDivision) {
      return;
    }
    
    // Analyze paragraphs
    if (program.procedureDivision.paragraphs) {
      for (const paragraph of program.procedureDivision.paragraphs) {
        if ((paragraph as any).statements) {
          for (const statement of (paragraph as any).statements) {
            if (statement instanceof StatementNode) {
              this.analyzeStatementForCalls(paragraph.name, statement);
            }
          }
        }
      }
    }
    
    // Analyze sections
    if (program.procedureDivision.sections) {
      for (const section of program.procedureDivision.sections) {
        if (section.paragraphs) {
          for (const paragraph of section.paragraphs) {
            if ((paragraph as any).statements) {
              for (const statement of (paragraph as any).statements) {
                if (statement instanceof StatementNode) {
                  this.analyzeStatementForCalls(paragraph.name, statement);
                }
              }
            }
          }
        }
      }
    }
  }
  
  /**
   * Analyze statement for PERFORM, GO TO, and CALL statements
   */
  private analyzeStatementForCalls(callerName: string, statement: StatementNode): void {
    const sourceText = statement.sourceText || '';
    const statementType = statement.statementType;
    
    // Extract targets based on statement type
    let targets: string[] = [];
    
    if (statementType === 'PERFORM') {
      // PERFORM PARA-NAME or PERFORM PARA-NAME UNTIL condition
      const performMatch = sourceText.match(/PERFORM\s+([A-Z0-9\-]+)/i);
      if (performMatch) {
        targets.push(performMatch[1]);
      }
    } else if (statementType === 'GO') {
      // GO TO target or GO TO target1 target2 ... DEPENDING ON
      const dependingOnMatch = sourceText.match(/GO\s+TO\s+((?:[A-Z0-9\-]+\s*)+)\s+DEPENDING\s+ON/i);
      const simpleGoToMatch = sourceText.match(/GO\s+TO\s+([A-Z0-9\-]+)(?!\s+\w+\s+DEPENDING)/i);
      
      if (dependingOnMatch) {
        const targetsText = dependingOnMatch[1];
        targets = targetsText.trim().split(/\s+/);
      } else if (simpleGoToMatch) {
        targets.push(simpleGoToMatch[1]);
      }
    } else if (statementType === 'CALL') {
      // CALL "program-name" or CALL variable-name
      const callMatch = sourceText.match(/CALL\s+["']([^"']+)["']|CALL\s+([A-Z0-9\-]+)/i);
      if (callMatch) {
        targets.push(callMatch[1] || callMatch[2]);
      }
    }
    
    // Add call relationships
    for (const target of targets) {
      if (target && this.callGraph.has(target)) {
        const caller = this.callGraph.get(callerName);
        const callee = this.callGraph.get(target);
        
        if (caller && callee) {
          caller.callees.push(target);
          callee.callers.push(callerName);
        }
      }
    }
  }
  
  /**
   * Mark reachable nodes starting from entry points
   */
  private markReachableNodes(): void {
    // Start from the first paragraph or section as entry point
    let entryPoint: string | undefined;
    
    if (this.currentProgram?.procedureDivision?.paragraphs?.length) {
      entryPoint = this.currentProgram.procedureDivision.paragraphs[0].name;
    } else if (this.currentProgram?.procedureDivision?.sections?.length) {
      entryPoint = this.currentProgram.procedureDivision.sections[0].name;
    }
    
    if (entryPoint && this.callGraph.has(entryPoint)) {
      this.markReachableFromNode(entryPoint, new Set());
    }
  }
  
  /**
   * Mark nodes as reachable using DFS
   */
  private markReachableFromNode(nodeName: string, visited: Set<string>): void {
    if (visited.has(nodeName)) {
      return;
    }
    
    visited.add(nodeName);
    const node = this.callGraph.get(nodeName);
    
    if (node) {
      node.reachable = true;
      
      // Mark all callees as reachable
      for (const callee of node.callees) {
        this.markReachableFromNode(callee, visited);
      }
    }
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
    // MOVE source TO destination - handle array subscripts and literals
    const moveMatch = sourceText.match(/MOVE\s+([A-Z0-9\-"'()]+)\s+TO\s+([A-Z0-9\-()]+)/i);
    if (moveMatch) {
      const source = moveMatch[1];
      const destination = moveMatch[2];
      
      // Extract variable name from array notation (e.g., VAR(1) -> VAR)
      const sourceVar = this.extractVariableName(source);
      const destVar = this.extractVariableName(destination);
      
      if (sourceVar && this.variableUsage.has(sourceVar)) {
        this.markVariableAsRead(sourceVar, statement.location);
      }
      if (destVar && this.variableUsage.has(destVar)) {
        this.markVariableAsWritten(destVar, statement.location);
      }
    }
  }

  private analyzeStringStatement(statement: StatementNode, sourceText: string): void {
    // STRING var1 DELIMITED BY ... var2 DELIMITED BY ... INTO result
    const stringMatch = sourceText.match(/STRING\s+(.*?)\s+INTO\s+([A-Z0-9\-()]+)/i);
    if (stringMatch) {
      const sourcesPart = stringMatch[1];
      const destination = this.extractVariableName(stringMatch[2]);
      
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
      if (destination && this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
    }
  }

  private analyzeDisplayStatement(statement: StatementNode, sourceText: string): void {
    // DISPLAY variable - handle array subscripts
    const displayMatch = sourceText.match(/DISPLAY\s+([A-Z0-9\-()]+)/i);
    if (displayMatch) {
      const variable = this.extractVariableName(displayMatch[1]);
      if (variable && this.variableUsage.has(variable)) {
        this.markVariableAsRead(variable, statement.location);
      }
    }
  }

  private analyzeComputeStatement(statement: StatementNode, sourceText: string): void {
    // COMPUTE result = expression - handle array subscripts
    const computeMatch = sourceText.match(/COMPUTE\s+([A-Z0-9\-()]+)\s*=\s*(.*)/i);
    if (computeMatch) {
      const destination = this.extractVariableName(computeMatch[1]);
      const expression = computeMatch[2];
      
      // Mark destination as written
      if (destination && this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
      
      // Extract variables from expression (including array subscripts)
      const variableMatches = expression.match(/([A-Z0-9\-]+(?:\([^)]+\))?)/g);
      if (variableMatches) {
        for (const variable of variableMatches) {
          const varName = this.extractVariableName(variable);
          if (varName && this.variableUsage.has(varName)) {
            this.markVariableAsRead(varName, statement.location);
          }
        }
      }
    }
  }

  private analyzeArithmeticStatement(statement: StatementNode, sourceText: string): void {
    // Handle GIVING clause differently - destination is written, sources are read
    const givingMatch = sourceText.match(/^(ADD|SUBTRACT|MULTIPLY|DIVIDE)\s+(.*?)\s+GIVING\s+([A-Z0-9\-]+)/i);
    if (givingMatch) {
      const operation = givingMatch[1];
      const sourcePart = givingMatch[2];
      const destination = givingMatch[3];
      
      // Mark destination as written
      if (this.variableUsage.has(destination)) {
        this.markVariableAsWritten(destination, statement.location);
      }
      
      // Mark source variables as read
      const sourceVariables = sourcePart.match(/([A-Z0-9\-]+)/g);
      if (sourceVariables) {
        for (const variable of sourceVariables) {
          if (this.variableUsage.has(variable) && !['TO', 'BY', 'FROM'].includes(variable)) {
            this.markVariableAsRead(variable, statement.location);
          }
        }
      }
    } else {
      // Handle TO clause for ADD/SUBTRACT (ADD A TO B - B is both read and written)
      const toMatch = sourceText.match(/^(ADD|SUBTRACT)\s+([A-Z0-9\-]+)\s+TO\s+([A-Z0-9\-]+)/i);
      if (toMatch) {
        const source = toMatch[2];
        const destination = toMatch[3];
        
        if (this.variableUsage.has(source)) {
          this.markVariableAsRead(source, statement.location);
        }
        if (this.variableUsage.has(destination)) {
          this.markVariableAsRead(destination, statement.location);
          this.markVariableAsWritten(destination, statement.location);
        }
      } else {
        // Generic handling for other arithmetic operations
        const variableMatches = sourceText.match(/([A-Z0-9\-]+)/g);
        if (variableMatches) {
          for (const variable of variableMatches) {
            if (this.variableUsage.has(variable)) {
              this.markVariableAsRead(variable, statement.location);
            }
          }
        }
      }
    }
  }

  private analyzeGenericStatement(statement: StatementNode, sourceText: string): void {
    // Generic variable extraction for other statements
    const variableMatches = sourceText.match(/([A-Z0-9\-]+(?:\([^)]+\))?)/g);
    if (variableMatches) {
      for (const variable of variableMatches) {
        const varName = this.extractVariableName(variable);
        if (varName && this.variableUsage.has(varName)) {
          this.markVariableAsRead(varName, statement.location);
        }
      }
    }
  }
  
  /**
   * Extract variable name from array notation (e.g., VAR(1) -> VAR)
   */
  private extractVariableName(text: string): string | null {
    if (!text || typeof text !== 'string') {
      return null;
    }
    
    // Remove quotes for string literals
    if (text.startsWith('"') || text.startsWith("'")) {
      return null;
    }
    
    // Extract variable name from array notation
    const match = text.match(/^([A-Z0-9\-]+)(?:\([^)]+\))?$/i);
    return match ? match[1] : null;
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
    // Dead code detection is primarily handled by unreachable paragraph detection
    // which is called separately. This method can be used for additional dead code patterns
    // like code after unconditional returns, but for now we avoid duplicate calls
  }

  /**
   * Detect unreachable paragraphs
   */
  private detectUnreachableParagraphs(): void {
    for (const [name, node] of this.callGraph) {
      if (!node.reachable) {
        this.errorHandler.addError(
          new AnalysisError(
            `Unreachable code detected: ${name}`,
            'static-analysis',
            'UNREACHABLE_CODE',
            node.location
          )
        );
      }
    }
  }

  /**
   * Detect unused variables
   */
  private detectUnusedVariables(): void {
    for (const [name, usage] of this.variableUsage) {
      if (!usage.isRead && !usage.isWritten) {
        this.errorHandler.addError(
          new AnalysisError(
            `Unused variable: ${name}`,
            'static-analysis',
            'UNUSED_VARIABLE',
            usage.definition.location
          )
        );
      }
    }
  }

  /**
   * Detect circular dependencies
   */
  private detectCircularDependencies(): void {
    const visiting = new Set<string>();
    const visited = new Set<string>();
    
    for (const [name, node] of this.callGraph) {
      if (!visited.has(name)) {
        this.detectCircularDependencyDFS(name, visiting, visited, []);
      }
    }
  }
  
  /**
   * DFS to detect circular dependencies
   */
  private detectCircularDependencyDFS(nodeName: string, visiting: Set<string>, visited: Set<string>, path: string[]): void {
    if (visiting.has(nodeName)) {
      // Found a cycle
      const cycleStart = path.indexOf(nodeName);
      const cycle = path.slice(cycleStart).concat(nodeName);
      const node = this.callGraph.get(nodeName);
      
      if (node) {
        this.errorHandler.addError(
          new AnalysisError(
            `Circular dependency detected: ${cycle.join(' -> ')}`,
            'static-analysis',
            'CIRCULAR_DEPENDENCY',
            node.location
          )
        );
      }
      return;
    }
    
    if (visited.has(nodeName)) {
      return;
    }
    
    const node = this.callGraph.get(nodeName);
    if (!node) {
      return;
    }
    
    visiting.add(nodeName);
    path.push(nodeName);
    
    for (const callee of node.callees) {
      this.detectCircularDependencyDFS(callee, visiting, visited, path);
    }
    
    visiting.delete(nodeName);
    visited.add(nodeName);
    path.pop();
  }

  /**
   * Detect infinite loops
   */
  private detectInfiniteLoops(): void {
    // Infinite loops are often detected as circular dependencies
    // but we can also detect self-referencing paragraphs
    for (const [name, node] of this.callGraph) {
      if (node.callees.includes(name)) {
        this.errorHandler.addError(
          new AnalysisError(
            `Potential infinite loop detected: ${name} calls itself`,
            'static-analysis',
            'INFINITE_LOOP',
            node.location
          )
        );
      }
    }
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
    for (const [name, usage] of this.variableUsage) {
      // Detect write-only variables
      if (usage.isWritten && !usage.isRead) {
        this.errorHandler.addError(
          new AnalysisError(
            `Write-only variable: ${name} (written but never read)`,
            'static-analysis',
            'WRITE_ONLY_VARIABLE',
            usage.definition.location
          )
        );
      }
      
      // Detect uninitialized variables that are read
      if (usage.isRead && !usage.isInitialized && !usage.isWritten) {
        this.errorHandler.addError(
          new AnalysisError(
            `Uninitialized variable: ${name} is read before being initialized`,
            'static-analysis',
            'UNINITIALIZED_VARIABLE',
            usage.definition.location
          )
        );
      }
      
      // Detect read-only variables (might be constants)
      if (usage.isRead && !usage.isWritten && usage.isInitialized) {
        this.errorHandler.addError(
          new AnalysisError(
            `Read-only variable: ${name} (read but never written)`,
            'static-analysis',
            'READ_ONLY_VARIABLE',
            usage.definition.location
          )
        );
      }
    }
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
      errorCount: this.getErrors().length,
      errorsCount: this.getErrors().length,
      warningsCount: this.getWarnings().length,
      hasWarnings: this.getWarnings().length > 0,
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