/**
 * AST Node representing COBOL Environment Division
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation } from '../../core/types';

/**
 * Represents the ENVIRONMENT DIVISION
 */
export class EnvironmentDivision extends BaseASTNode {
  public readonly type = 'EnvironmentDivision';
  
  /** Source computer specification */
  public sourceComputer?: string;
  
  /** Object computer specification */
  public objectComputer?: string;
  
  /** Special names assignments */
  public specialNames?: Map<string, string>;
  
  /** File control specifications */
  public fileControl?: Map<string, any>;
  
  /** I-O control specifications */
  public ioControl?: any;

  constructor(location: SourceLocation) {
    super('EnvironmentDivision', location);
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitEnvironmentDivision) {
      return visitor.visitEnvironmentDivision(this);
    }
    return visitor.visitNode(this);
  }
}