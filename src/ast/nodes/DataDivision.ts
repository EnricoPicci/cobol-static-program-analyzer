/**
 * AST Node representing COBOL Data Division
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, VariableDefinition } from '../../core/types';

/**
 * Represents the DATA DIVISION
 */
export class DataDivision extends BaseASTNode {
  public readonly type = 'DataDivision';
  
  /** File Section variables */
  public fileSection?: VariableDefinition[];
  
  /** Working Storage Section variables */
  public workingStorage?: VariableDefinition[];
  
  /** Working Storage Section (full structure) */
  public workingStorageSection?: any;
  
  /** Linkage Section variables */
  public linkageSection?: VariableDefinition[];
  
  /** Local Storage Section variables */
  public localStorageSection?: VariableDefinition[];

  constructor(location: SourceLocation) {
    super('DataDivision', location);
  }

  /**
   * Add working storage variable
   */
  addWorkingStorageVariable(variable: VariableDefinition): void {
    if (!this.workingStorage) {
      this.workingStorage = [];
    }
    this.workingStorage.push(variable);
  }

  /**
   * Add file section variable
   */
  addFileVariable(variable: VariableDefinition): void {
    if (!this.fileSection) {
      this.fileSection = [];
    }
    this.fileSection.push(variable);
  }

  /**
   * Add linkage section variable
   */
  addLinkageVariable(variable: VariableDefinition): void {
    if (!this.linkageSection) {
      this.linkageSection = [];
    }
    this.linkageSection.push(variable);
  }

  /**
   * Find variable by name across all sections
   */
  findVariable(name: string): VariableDefinition | undefined {
    const allVariables = [
      ...(this.workingStorage || []),
      ...(this.fileSection || []),
      ...(this.linkageSection || []),
      ...(this.localStorageSection || [])
    ];
    
    return allVariables.find(variable => variable.name === name);
  }

  /**
   * Get all variables
   */
  getAllVariables(): VariableDefinition[] {
    return [
      ...(this.workingStorage || []),
      ...(this.fileSection || []),
      ...(this.linkageSection || []),
      ...(this.localStorageSection || [])
    ];
  }

  /**
   * Backward compatibility getters
   */
  get workingStorageVariables(): VariableDefinition[] {
    return this.workingStorage || [];
  }

  get fileVariables(): VariableDefinition[] {
    return this.fileSection || [];
  }

  get linkageVariables(): VariableDefinition[] {
    return this.linkageSection || [];
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitDataDivision) {
      return visitor.visitDataDivision(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Convert to JSON representation matching README format
   */
  toJSON(): any {
    return {
      workingStorage: this.workingStorage?.map(v => ({
        name: v.name,
        type: v.dataType?.picture || 'UNKNOWN',
        initialValue: v.initialValue
      })),
      fileSection: this.fileSection?.map(v => ({
        name: v.name,
        structure: v.dataType?.picture || 'UNKNOWN'
      })),
      linkageSection: this.linkageSection?.map(v => ({
        name: v.name,
        type: v.dataType?.picture || 'UNKNOWN'
      }))
    };
  }
}