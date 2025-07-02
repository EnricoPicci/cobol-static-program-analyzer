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
import { SourceLocation, VariableDefinition, CallReference } from '../core/types';
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
}

export const DEFAULT_STATIC_ANALYSIS_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: true
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
        this.analyzeVariableUsagePatterns();
      }

    } catch (error) {
      this.errorHandler.addAnalysisWarning(
        `Static analysis failed: ${error instanceof Error ? error.message : String(error)}`,
        'static-analysis',
        'STATIC_ANALYSIS_FAILED',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
    }
  }

  /**
   * Build call graph from procedure division
   */
  private buildCallGraph(program: CobolProgram): void {
    if (!program.procedureDivision) return;

    const procDiv = program.procedureDivision;

    // Add all sections and paragraphs to call graph
    procDiv.sections.forEach(section => {
      this.callGraph.set(section.name, {
        name: section.name,
        type: 'section',
        location: section.location,
        callers: [],
        callees: [],
        visited: false,
        reachable: false
      });

      section.paragraphs.forEach(paragraph => {
        this.callGraph.set(paragraph.name, {
          name: paragraph.name,
          type: 'paragraph',
          location: paragraph.location,
          callers: [],
          callees: [],
          visited: false,
          reachable: false
        });
      });
    });

    procDiv.paragraphs.forEach(paragraph => {
      this.callGraph.set(paragraph.name, {
        name: paragraph.name,
        type: 'paragraph',
        location: paragraph.location,
        callers: [],
        callees: [],
        visited: false,
        reachable: false
      });
    });

    // Build call relationships
    this.analyzeCallRelationships(procDiv);
  }

  /**
   * Analyze call relationships in procedure division
   */
  private analyzeCallRelationships(procDiv: ProcedureDivision): void {
    // Process sections
    procDiv.sections.forEach(section => {
      this.analyzeNodeCalls(section);
    });

    // Process standalone paragraphs
    procDiv.paragraphs.forEach(paragraph => {
      this.analyzeNodeCalls(paragraph);
    });
  }

  /**
   * Analyze calls from a section or paragraph
   */
  private analyzeNodeCalls(node: SectionNode | ParagraphNode): void {
    const callerName = node.name;
    const callerNode = this.callGraph.get(callerName);
    if (!callerNode) return;

    // Extract calls from statements
    const calls = this.extractCallsFromNode(node);
    
    calls.forEach(call => {
      const calleeNode = this.callGraph.get(call.name);
      if (calleeNode) {
        callerNode.callees.push(call.name);
        calleeNode.callers.push(callerName);
      }
    });
  }

  /**
   * Extract calls from statements in a node
   */
  private extractCallsFromNode(node: SectionNode | ParagraphNode): CallReference[] {
    const calls: CallReference[] = [];
    
    // For now, use the calledSectionsParagraphs property
    // In a real implementation, we'd parse statements to find PERFORM, GO TO, etc.
    calls.push(...node.calledSectionsParagraphs);
    
    return calls;
  }

  /**
   * Build variable usage map
   */
  private buildVariableUsageMap(program: CobolProgram): void {
    if (!program.dataDivision) return;

    const allVariables = [
      ...program.dataDivision.workingStorageVariables,
      ...program.dataDivision.fileVariables,
      ...program.dataDivision.linkageVariables
    ];

    allVariables.forEach(variable => {
      this.variableUsage.set(variable.name, {
        name: variable.name,
        definition: variable,
        references: [],
        isRead: false,
        isWritten: false,
        isInitialized: !!variable.initialValue
      });
    });

    // Analyze variable usage in procedure division
    if (program.procedureDivision) {
      this.analyzeVariableReferences(program.procedureDivision);
    }
  }

  /**
   * Analyze variable references in procedure division
   */
  private analyzeVariableReferences(procDiv: ProcedureDivision): void {
    // Process sections
    procDiv.sections.forEach(section => {
      this.analyzeVariableReferencesInNode(section);
    });

    // Process standalone paragraphs
    procDiv.paragraphs.forEach(paragraph => {
      this.analyzeVariableReferencesInNode(paragraph);
    });
  }

  /**
   * Analyze variable references in a node
   */
  private analyzeVariableReferencesInNode(node: SectionNode | ParagraphNode): void {
    // In a real implementation, we'd parse statements to find variable references
    // For now, this is a placeholder that could be extended with actual statement analysis
    
    // Example: look for MOVE, ADD, SUBTRACT, etc. statements
    // This would require more detailed statement parsing
  }

  /**
   * Detect dead code (unreachable sections/paragraphs)
   */
  private detectDeadCode(): void {
    // Mark entry points as reachable
    this.markEntryPointsReachable();
    
    // Propagate reachability through call graph
    this.propagateReachability();
    
    // Report unreachable code
    this.callGraph.forEach(node => {
      if (!node.reachable) {
        this.errorHandler.addAnalysisWarning(
          `Unreachable ${node.type} '${node.name}' - no path from program entry point`,
          'dead-code-detection',
          'UNREACHABLE_CODE',
          node.location,
          [`Consider removing unused ${node.type} or add a reference to it`]
        );
      }
    });
  }

  /**
   * Mark entry points as reachable
   */
  private markEntryPointsReachable(): void {
    // In COBOL, execution starts from the first executable statement
    // Mark the first section/paragraph as reachable
    if (this.currentProgram?.procedureDivision) {
      const procDiv = this.currentProgram.procedureDivision;
      
      // First section or paragraph is the entry point
      let entryPoint: string | undefined;
      
      if (procDiv.sections.length > 0) {
        entryPoint = procDiv.sections[0].name;
      } else if (procDiv.paragraphs.length > 0) {
        entryPoint = procDiv.paragraphs[0].name;
      }
      
      if (entryPoint) {
        const entryNode = this.callGraph.get(entryPoint);
        if (entryNode) {
          entryNode.reachable = true;
        }
      }
    }
  }

  /**
   * Propagate reachability through call graph
   */
  private propagateReachability(): void {
    let changed = true;
    
    while (changed) {
      changed = false;
      
      this.callGraph.forEach(node => {
        if (node.reachable) {
          node.callees.forEach(calleeName => {
            const calleeNode = this.callGraph.get(calleeName);
            if (calleeNode && !calleeNode.reachable) {
              calleeNode.reachable = true;
              changed = true;
            }
          });
        }
      });
    }
  }

  /**
   * Detect unreachable paragraphs
   */
  private detectUnreachableParagraphs(): void {
    // This is handled by detectDeadCode, but we can add specific paragraph analysis
    const unreachableParagraphs = Array.from(this.callGraph.values())
      .filter(node => node.type === 'paragraph' && !node.reachable);
    
    unreachableParagraphs.forEach(paragraph => {
      this.errorHandler.addAnalysisWarning(
        `Paragraph '${paragraph.name}' is never executed`,
        'unreachable-paragraph',
        'UNREACHABLE_PARAGRAPH',
        paragraph.location,
        ['Add a PERFORM statement to call this paragraph', 'Remove the paragraph if it\'s no longer needed']
      );
    });
  }

  /**
   * Detect unused variables
   */
  private detectUnusedVariables(): void {
    this.variableUsage.forEach(usage => {
      if (usage.references.length === 0) {
        this.errorHandler.addAnalysisWarning(
          `Variable '${usage.name}' is declared but never used`,
          'unused-variable',
          'UNUSED_VARIABLE',
          usage.definition.location,
          ['Remove the unused variable declaration', 'Add code that uses this variable']
        );
      } else if (!usage.isRead && usage.isWritten) {
        this.errorHandler.addAnalysisWarning(
          `Variable '${usage.name}' is written to but never read`,
          'unused-variable',
          'WRITE_ONLY_VARIABLE',
          usage.definition.location,
          ['Add code that reads this variable', 'Remove the variable if it\'s not needed']
        );
      } else if (usage.isRead && !usage.isWritten && !usage.isInitialized) {
        this.errorHandler.addAnalysisWarning(
          `Variable '${usage.name}' is read but never initialized or written to`,
          'uninitialized-variable',
          'UNINITIALIZED_VARIABLE',
          usage.definition.location,
          ['Initialize the variable with a VALUE clause', 'Add code that sets the variable before use']
        );
      }
    });
  }

  /**
   * Detect circular dependencies
   */
  private detectCircularDependencies(): void {
    const visited = new Set<string>();
    const recursionStack = new Set<string>();
    
    this.callGraph.forEach((node, name) => {
      if (!visited.has(name)) {
        this.findCircularDependency(name, visited, recursionStack, []);
      }
    });
  }

  /**
   * Find circular dependency using DFS
   */
  private findCircularDependency(
    nodeName: string,
    visited: Set<string>,
    recursionStack: Set<string>,
    path: string[]
  ): void {
    visited.add(nodeName);
    recursionStack.add(nodeName);
    path.push(nodeName);
    
    const node = this.callGraph.get(nodeName);
    if (!node) return;
    
    for (const callee of node.callees) {
      if (!visited.has(callee)) {
        this.findCircularDependency(callee, visited, recursionStack, [...path]);
      } else if (recursionStack.has(callee)) {
        // Found circular dependency
        const cycleStart = path.indexOf(callee);
        const cycle = path.slice(cycleStart).concat(callee);
        
        this.errorHandler.addAnalysisWarning(
          `Circular dependency detected: ${cycle.join(' -> ')}`,
          'circular-dependency',
          'CIRCULAR_DEPENDENCY',
          node.location,
          ['Restructure the code to eliminate the circular call pattern']
        );
      }
    }
    
    recursionStack.delete(nodeName);
  }

  /**
   * Detect infinite loops
   */
  private detectInfiniteLoops(): void {
    // Look for self-referencing paragraphs/sections
    this.callGraph.forEach(node => {
      if (node.callees.includes(node.name)) {
        this.errorHandler.addAnalysisWarning(
          `Potential infinite loop: ${node.type} '${node.name}' calls itself`,
          'infinite-loop',
          'INFINITE_LOOP',
          node.location,
          ['Add a condition to break the loop', 'Restructure the logic to avoid self-reference']
        );
      }
    });
  }

  /**
   * Detect unreferenced sections
   */
  private detectUnreferencedSections(): void {
    const unreferencedSections = Array.from(this.callGraph.values())
      .filter(node => node.type === 'section' && node.callers.length === 0);
    
    unreferencedSections.forEach(section => {
      // Skip the first section as it's the entry point
      if (this.currentProgram?.procedureDivision?.sections[0]?.name !== section.name) {
        this.errorHandler.addAnalysisWarning(
          `Section '${section.name}' is never referenced`,
          'unreferenced-section',
          'UNREFERENCED_SECTION',
          section.location,
          ['Add a PERFORM statement to call this section', 'Remove the section if it\'s no longer needed']
        );
      }
    });
  }

  /**
   * Check for missing GO TO targets
   */
  private checkMissingGoToTargets(): void {
    // This would require parsing GO TO statements and checking targets
    // Placeholder for now - would be implemented with detailed statement analysis
  }

  /**
   * Analyze variable usage patterns
   */
  private analyzeVariableUsagePatterns(): void {
    this.variableUsage.forEach(usage => {
      // Check for variables that are only used in their own definition
      if (usage.references.length === 1 && 
          usage.references[0].line === usage.definition.location.line) {
        this.errorHandler.addAnalysisWarning(
          `Variable '${usage.name}' is only referenced in its own definition`,
          'variable-usage',
          'SELF_REFERENTIAL_VARIABLE',
          usage.definition.location
        );
      }
    });
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
   * Clear all analysis results
   */
  clear(): void {
    this.errorHandler.clear();
    this.callGraph.clear();
    this.variableUsage.clear();
  }

  /**
   * Check if analysis found errors
   */
  hasErrors(): boolean {
    return this.errorHandler.hasErrors();
  }

  /**
   * Get analysis summary
   */
  getSummary(): {
    errorCount: number;
    warningCount: number;
    hasErrors: boolean;
    hasWarnings: boolean;
    callGraphNodes: number;
    variablesAnalyzed: number;
  } {
    const baseSummary = this.errorHandler.getSummary();
    return {
      ...baseSummary,
      callGraphNodes: this.callGraph.size,
      variablesAnalyzed: this.variableUsage.size
    };
  }

  /**
   * Get call graph for external use
   */
  getCallGraph(): Map<string, CallGraphNode> {
    return new Map(this.callGraph);
  }

  /**
   * Get variable usage information
   */
  getVariableUsage(): Map<string, VariableUsage> {
    return new Map(this.variableUsage);
  }
}