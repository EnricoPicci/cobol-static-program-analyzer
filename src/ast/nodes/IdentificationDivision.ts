/**
 * AST Node representing COBOL Identification Division
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation } from '../../core/types';

/**
 * Represents the IDENTIFICATION DIVISION
 */
export class IdentificationDivision extends BaseASTNode {
  public readonly type = 'IdentificationDivision';
  
  /** Program identifier (required) */
  public programId: string;
  
  /** Program author */
  public author?: string;
  
  /** Date written */
  public dateWritten?: string;
  
  /** Date compiled */
  public dateCompiled?: string;
  
  /** Installation information */
  public installation?: string;
  
  /** Security classification */
  public security?: string;
  
  /** Program remarks/documentation */
  public remarks?: string[];

  constructor(programId: string, location: SourceLocation) {
    super('IdentificationDivision', location);
    this.programId = programId;
  }

  /**
   * Set author information
   */
  setAuthor(author: string): void {
    this.author = author;
  }

  /**
   * Set date written
   */
  setDateWritten(date: string): void {
    this.dateWritten = date;
  }

  /**
   * Set date compiled
   */
  setDateCompiled(date: string): void {
    this.dateCompiled = date;
  }

  /**
   * Set installation information
   */
  setInstallation(installation: string): void {
    this.installation = installation;
  }

  /**
   * Set security classification
   */
  setSecurity(security: string): void {
    this.security = security;
  }

  /**
   * Add a remark
   */
  addRemark(remark: string): void {
    if (!this.remarks) {
      this.remarks = [];
    }
    this.remarks.push(remark);
  }

  /**
   * Validate program ID format
   */
  isValidProgramId(): boolean {
    if (!this.programId) return false;
    
    // COBOL program ID rules:
    // - 1-30 characters
    // - Start with letter
    // - Contains only letters, digits, and hyphens
    const programIdPattern = /^[A-Za-z][A-Za-z0-9-]{0,29}$/;
    return programIdPattern.test(this.programId);
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitIdentificationDivision) {
      return visitor.visitIdentificationDivision(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Convert to JSON representation
   */
  toJSON(): any {
    return {
      type: this.type,
      programId: this.programId,
      author: this.author,
      dateWritten: this.dateWritten,
      dateCompiled: this.dateCompiled,
      installation: this.installation,
      security: this.security,
      remarks: this.remarks
    };
  }
}