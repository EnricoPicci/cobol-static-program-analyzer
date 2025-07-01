/**
 * AST Node representing a COBOL statement
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, StatementType } from '../../core/types';

/**
 * Represents a COBOL statement
 */
export class StatementNode extends BaseASTNode {
  public readonly type = 'StatementNode';
  
  /** Type of statement (MOVE, PERFORM, etc.) */
  public statementType: StatementType;
  
  /** Original source text of the statement */
  public sourceText: string;
  
  /** Statement operands/parameters */
  public operands?: string[];
  
  /** Target of the statement (for MOVE, PERFORM, etc.) */
  public target?: string;
  
  /** Condition associated with statement (IF, WHEN, etc.) */
  public condition?: string;
  
  /** Nested statements (for IF-THEN-ELSE, EVALUATE, etc.) */
  public nestedStatements?: StatementNode[];

  constructor(
    statementType: StatementType,
    sourceText: string,
    location: SourceLocation
  ) {
    super('StatementNode', location);
    this.statementType = statementType;
    this.sourceText = sourceText;
  }

  /**
   * Add nested statement
   */
  addNestedStatement(statement: StatementNode): void {
    if (!this.nestedStatements) {
      this.nestedStatements = [];
    }
    statement.parent = this;
    this.nestedStatements.push(statement);
    this.addChild(statement);
  }

  /**
   * Check if this is a control flow statement
   */
  isControlFlow(): boolean {
    return ['IF', 'ELSE', 'EVALUATE', 'WHEN', 'PERFORM', 'GO', 'CALL'].includes(this.statementType);
  }

  /**
   * Check if this is a data movement statement
   */
  isDataMovement(): boolean {
    return ['MOVE', 'SET', 'INITIALIZE'].includes(this.statementType);
  }

  /**
   * Check if this is an arithmetic statement
   */
  isArithmetic(): boolean {
    return ['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE'].includes(this.statementType);
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitStatementNode) {
      return visitor.visitStatementNode(this);
    }
    return visitor.visitNode(this);
  }
}