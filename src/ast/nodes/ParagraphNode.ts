/**
 * AST Node representing a COBOL paragraph
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, CallReference, ComplexityMetrics, CommentInfo } from '../../core/types';
import { StatementNode } from './StatementNode';

/**
 * Represents a COBOL paragraph
 */
export class ParagraphNode extends BaseASTNode {
  public readonly type = 'ParagraphNode';
  
  /** Paragraph name */
  public name: string;
  
  /** Original source code of the paragraph */
  public sourceCode: string;
  
  /** Statements contained in this paragraph */
  public statements: StatementNode[] = [];
  
  /** Sections/paragraphs called from this paragraph */
  public calledSectionsParagraphs: CallReference[] = [];
  
  /** Paragraph start location */
  public startLocation: SourceLocation;
  
  /** Paragraph end location */
  public endLocation?: SourceLocation;
  
  /** Associated comments */
  public comments?: CommentInfo[];
  
  /** Is this paragraph reachable (dead code analysis) */
  public reachable?: boolean;
  
  /** Who calls this paragraph */
  public callers?: CallReference[];
  
  /** Complexity metrics */
  public complexity?: ComplexityMetrics;
  
  /** Fall-through behavior (executes into next paragraph) */
  public fallsThrough?: boolean;
  
  /** Exit points from this paragraph */
  public exitPoints?: SourceLocation[];
  
  /** Entry points to this paragraph */
  public entryPoints?: SourceLocation[];

  constructor(
    name: string,
    sourceCode: string,
    location: SourceLocation,
    startLocation: SourceLocation
  ) {
    super('ParagraphNode', location);
    this.name = name;
    this.sourceCode = sourceCode;
    this.startLocation = startLocation;
  }

  /**
   * Add a statement to this paragraph
   */
  addStatement(statement: StatementNode): void {
    statement.parent = this;
    this.statements.push(statement);
    this.addChild(statement);
  }

  /**
   * Remove a statement from this paragraph
   */
  removeStatement(statement: StatementNode): boolean {
    const index = this.statements.indexOf(statement);
    if (index >= 0) {
      this.statements.splice(index, 1);
      statement.parent = undefined;
      this.removeChild(statement);
      return true;
    }
    return false;
  }

  /**
   * Add a call reference
   */
  addCallReference(reference: CallReference): void {
    this.calledSectionsParagraphs.push(reference);
  }

  /**
   * Get call references by type
   */
  getCallReferences(type?: 'section' | 'paragraph'): CallReference[] {
    if (type) {
      return this.calledSectionsParagraphs.filter(ref => ref.type === type);
    }
    return [...this.calledSectionsParagraphs];
  }

  /**
   * Check if this paragraph calls another section/paragraph
   */
  calls(name: string): boolean {
    return this.calledSectionsParagraphs.some(ref => ref.name === name);
  }

  /**
   * Get all PERFORM statements in this paragraph
   */
  getPerformStatements(): StatementNode[] {
    return this.statements.filter(stmt => stmt.statementType === 'PERFORM');
  }

  /**
   * Get all conditional statements
   */
  getConditionalStatements(): StatementNode[] {
    return this.statements.filter(stmt => 
      stmt.statementType === 'IF' || 
      stmt.statementType === 'EVALUATE' ||
      stmt.statementType === 'SEARCH'
    );
  }

  /**
   * Get paragraph metrics
   */
  getMetrics(): {
    statementCount: number;
    callCount: number;
    lineCount: number;
    complexity?: ComplexityMetrics;
    performCount: number;
    conditionalCount: number;
  } {
    const lineCount = this.endLocation 
      ? this.endLocation.line - this.startLocation.line + 1
      : this.sourceCode.split('\n').length;

    return {
      statementCount: this.statements.length,
      callCount: this.calledSectionsParagraphs.length,
      lineCount,
      complexity: this.complexity,
      performCount: this.getPerformStatements().length,
      conditionalCount: this.getConditionalStatements().length
    };
  }

  /**
   * Check if paragraph is entry point
   */
  isEntryPoint(): boolean {
    return this.callers?.length === 0 || this.fallsThrough === false;
  }

  /**
   * Check if paragraph is terminal (ends execution)
   */
  isTerminal(): boolean {
    const lastStatement = this.statements[this.statements.length - 1];
    return lastStatement?.statementType === 'STOP' || 
           lastStatement?.statementType === 'EXIT' ||
           lastStatement?.statementType === 'GOBACK';
  }

  /**
   * Get all called targets
   */
  getCalledTargets(): string[] {
    return this.calledSectionsParagraphs.map(ref => ref.name);
  }

  /**
   * Calculate cyclomatic complexity
   */
  calculateComplexity(): number {
    let complexity = 1; // Base complexity
    
    for (const statement of this.statements) {
      // Add complexity for decision statements
      if (statement.statementType === 'IF' || 
          statement.statementType === 'EVALUATE' ||
          statement.statementType === 'PERFORM' ||
          statement.statementType === 'SEARCH') {
        complexity++;
      }
    }
    
    return complexity;
  }

  /**
   * Find statements by type
   */
  findStatements(type: string): StatementNode[] {
    return this.statements.filter(stmt => stmt.statementType === type);
  }

  /**
   * Get maximum nesting depth
   */
  getMaxNestingDepth(): number {
    let maxDepth = 0;
    
    const calculateDepth = (statements: StatementNode[], currentDepth: number): void => {
      for (const stmt of statements) {
        maxDepth = Math.max(maxDepth, currentDepth);
        
        // If statement has nested statements, recurse
        if (stmt.children && stmt.children.length > 0) {
          const nestedStatements = stmt.children.filter(child => child.type === 'StatementNode') as StatementNode[];
          if (nestedStatements.length > 0) {
            calculateDepth(nestedStatements, currentDepth + 1);
          }
        }
      }
    };
    
    calculateDepth(this.statements, 1);
    return maxDepth;
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitParagraphNode) {
      return visitor.visitParagraphNode(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Convert to JSON representation matching README format
   */
  toJSON(): any {
    return {
      name: this.name,
      sourceCode: this.sourceCode,
      calledSectionsParagraphs: this.calledSectionsParagraphs.map(ref => ({
        name: ref.name
      }))
    };
  }

  /**
   * Get debugging information
   */
  getDebugInfo(): string {
    const metrics = this.getMetrics();
    return `Paragraph[${this.name}]: ${metrics.statementCount} statements, ` +
           `${metrics.callCount} calls, complexity: ${this.calculateComplexity()}, ` +
           `reachable: ${this.reachable !== false}`;
  }

  /**
   * Clone this paragraph (for code analysis)
   */
  clone(): ParagraphNode {
    const cloned = new ParagraphNode(
      this.name,
      this.sourceCode,
      { ...this.location },
      { ...this.startLocation }
    );
    
    cloned.endLocation = this.endLocation ? { ...this.endLocation } : undefined;
    cloned.comments = this.comments ? [...this.comments] : undefined;
    cloned.reachable = this.reachable;
    cloned.fallsThrough = this.fallsThrough;
    cloned.calledSectionsParagraphs = [...this.calledSectionsParagraphs];
    
    // Clone statements
    for (const statement of this.statements) {
      // Note: Would need to implement statement.clone() method
      cloned.addStatement(statement);
    }
    
    return cloned;
  }
}