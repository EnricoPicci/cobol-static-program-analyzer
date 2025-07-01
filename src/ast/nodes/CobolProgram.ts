/**
 * Root AST node representing a complete COBOL program
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, PerformanceMetrics } from '../../core/types';
import { IdentificationDivision } from './IdentificationDivision';
import { EnvironmentDivision } from './EnvironmentDivision';
import { DataDivision } from './DataDivision';
import { ProcedureDivision } from './ProcedureDivision';

/**
 * Represents a complete COBOL program with all divisions
 */
export class CobolProgram extends BaseASTNode {
  public readonly type = 'CobolProgram';
  
  /** Program name from PROGRAM-ID */
  public name: string;
  
  /** Required identification division */
  public identificationDivision: IdentificationDivision;
  
  /** Optional environment division */
  public environmentDivision?: EnvironmentDivision;
  
  /** Optional data division */
  public dataDivision?: DataDivision;
  
  /** Optional procedure division */
  public procedureDivision?: ProcedureDivision;
  
  /** Performance metrics for this program */
  public performanceMetrics?: PerformanceMetrics;
  
  /** Program compilation timestamp */
  public compiledAt?: Date;
  
  /** Source file path */
  public sourceFile?: string;
  
  /** Program size metrics */
  public metrics?: {
    lineCount: number;
    statementCount: number;
    complexityScore: number;
    copybookCount: number;
  };

  constructor(
    name: string,
    identificationDivision: IdentificationDivision,
    location: SourceLocation
  ) {
    super('CobolProgram', location);
    this.name = name;
    this.identificationDivision = identificationDivision;
    this.addChild(identificationDivision);
  }

  /**
   * Set the environment division
   */
  setEnvironmentDivision(division: EnvironmentDivision): void {
    if (this.environmentDivision) {
      this.removeChild(this.environmentDivision);
    }
    this.environmentDivision = division;
    this.addChild(division);
  }

  /**
   * Set the data division
   */
  setDataDivision(division: DataDivision): void {
    if (this.dataDivision) {
      this.removeChild(this.dataDivision);
    }
    this.dataDivision = division;
    this.addChild(division);
  }

  /**
   * Set the procedure division
   */
  setProcedureDivision(division: ProcedureDivision): void {
    if (this.procedureDivision) {
      this.removeChild(this.procedureDivision);
    }
    this.procedureDivision = division;
    this.addChild(division);
  }

  /**
   * Check if program has required divisions
   */
  isValid(): boolean {
    return !!this.identificationDivision && !!this.identificationDivision.programId;
  }

  /**
   * Get all sections across all divisions
   */
  getAllSections(): any[] {
    const sections: any[] = [];
    
    if (this.dataDivision) {
      // Add data division sections
      if (this.dataDivision.fileSection) sections.push(this.dataDivision.fileSection);
      if (this.dataDivision.workingStorageSection) sections.push(this.dataDivision.workingStorageSection);
      if (this.dataDivision.linkageSection) sections.push(this.dataDivision.linkageSection);
      if (this.dataDivision.localStorageSection) sections.push(this.dataDivision.localStorageSection);
    }
    
    if (this.procedureDivision) {
      sections.push(...this.procedureDivision.sections);
    }
    
    return sections;
  }

  /**
   * Get all paragraphs in the program
   */
  getAllParagraphs(): any[] {
    const paragraphs: any[] = [];
    
    if (this.procedureDivision) {
      // Add standalone paragraphs
      paragraphs.push(...this.procedureDivision.paragraphs);
      
      // Add paragraphs from sections
      for (const section of this.procedureDivision.sections) {
        paragraphs.push(...section.paragraphs);
      }
    }
    
    return paragraphs;
  }

  /**
   * Get all variable declarations
   */
  getAllVariables(): any[] {
    if (!this.dataDivision) return [];
    
    const variables: any[] = [];
    
    if (this.dataDivision.workingStorage) {
      variables.push(...this.dataDivision.workingStorage);
    }
    
    if (this.dataDivision.linkageSection) {
      variables.push(...this.dataDivision.linkageSection);
    }
    
    if (this.dataDivision.localStorageSection) {
      variables.push(...this.dataDivision.localStorageSection);
    }
    
    return variables;
  }

  /**
   * Find a paragraph by name
   */
  findParagraph(name: string): any | undefined {
    return this.getAllParagraphs().find(para => para.name === name);
  }

  /**
   * Find a section by name
   */
  findSection(name: string): any | undefined {
    return this.getAllSections().find(section => section.name === name);
  }

  /**
   * Find a variable by name
   */
  findVariable(name: string): any | undefined {
    return this.getAllVariables().find(variable => variable.name === name);
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitCobolProgram) {
      return visitor.visitCobolProgram(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Get program summary
   */
  getSummary(): {
    name: string;
    hasProcedureDivision: boolean;
    hasDataDivision: boolean;
    hasEnvironmentDivision: boolean;
    sectionCount: number;
    paragraphCount: number;
    variableCount: number;
  } {
    return {
      name: this.name,
      hasProcedureDivision: !!this.procedureDivision,
      hasDataDivision: !!this.dataDivision,
      hasEnvironmentDivision: !!this.environmentDivision,
      sectionCount: this.getAllSections().length,
      paragraphCount: this.getAllParagraphs().length,
      variableCount: this.getAllVariables().length
    };
  }

  /**
   * Convert to JSON representation
   */
  toJSON(): any {
    return {
      type: this.type,
      name: this.name,
      location: this.location,
      identificationDivision: this.identificationDivision,
      environmentDivision: this.environmentDivision,
      dataDivision: this.dataDivision,
      procedureDivision: this.procedureDivision,
      metrics: this.metrics,
      summary: this.getSummary()
    };
  }
}