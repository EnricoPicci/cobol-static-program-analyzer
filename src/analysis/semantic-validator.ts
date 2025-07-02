/**
 * Semantic Validator - Tier 2 Error Detection
 * Validates COBOL language rules and semantic constraints
 */

import { CobolProgram } from '../ast/nodes/CobolProgram';
import { ASTNode } from '../ast/nodes/ASTNode';
import { SectionNode } from '../ast/nodes/SectionNode';
import { ParagraphNode } from '../ast/nodes/ParagraphNode';
import { StatementNode } from '../ast/nodes/StatementNode';
import { DataDivision } from '../ast/nodes/DataDivision';
import { ProcedureDivision } from '../ast/nodes/ProcedureDivision';
import { SourceLocation, VariableDefinition } from '../core/types';
import { SemanticError, CobolErrorHandler } from '../parser/error-handler';

/**
 * Semantic validation configuration
 */
export interface SemanticValidationConfig {
  /** Enforce strict division order */
  enforceStrictDivisionOrder: boolean;
  
  /** Check for required COBOL elements */
  checkRequiredElements: boolean;
  
  /** Validate naming conventions */
  validateNamingConventions: boolean;
  
  /** Check data type compatibility */
  checkDataTypeCompatibility: boolean;
  
  /** Validate paragraph/section references */
  validateReferences: boolean;
  
  /** Check for duplicate names */
  checkDuplicateNames: boolean;
}

export const DEFAULT_SEMANTIC_CONFIG: SemanticValidationConfig = {
  enforceStrictDivisionOrder: true,
  checkRequiredElements: true,
  validateNamingConventions: true,
  checkDataTypeCompatibility: true,
  validateReferences: true,
  checkDuplicateNames: true
};

/**
 * Semantic validator for COBOL programs
 */
export class SemanticValidator {
  private config: SemanticValidationConfig;
  private errorHandler: CobolErrorHandler;
  private currentProgram?: CobolProgram;

  constructor(config: SemanticValidationConfig = DEFAULT_SEMANTIC_CONFIG) {
    this.config = config;
    this.errorHandler = new CobolErrorHandler();
  }

  /**
   * Validate entire COBOL program
   */
  validate(program: CobolProgram): void {
    this.currentProgram = program;
    this.errorHandler.clear();

    try {
      // Tier 2 Semantic Validations
      this.validateDivisionOrder(program);
      this.validateRequiredElements(program);
      this.validateNamingConventions(program);
      this.validateDataTypes(program);
      this.validateReferences(program);
      this.validateDuplicateNames(program);
      
    } catch (error) {
      this.errorHandler.addSemanticError(
        `Semantic validation failed: ${error instanceof Error ? error.message : String(error)}`,
        'SEMANTIC_VALIDATION_FAILED',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
    }
  }

  /**
   * Validate division order (IDENTIFICATION -> ENVIRONMENT -> DATA -> PROCEDURE)
   */
  private validateDivisionOrder(program: CobolProgram): void {
    if (!this.config.enforceStrictDivisionOrder) return;

    const divisions: { name: string; present: boolean; location?: SourceLocation }[] = [
      { name: 'IDENTIFICATION', present: !!program.identificationDivision, location: program.identificationDivision?.location },
      { name: 'ENVIRONMENT', present: !!program.environmentDivision, location: program.environmentDivision?.location },
      { name: 'DATA', present: !!program.dataDivision, location: program.dataDivision?.location },
      { name: 'PROCEDURE', present: !!program.procedureDivision, location: program.procedureDivision?.location }
    ];

    let lastDivisionIndex = -1;
    
    for (let i = 0; i < divisions.length; i++) {
      const division = divisions[i];
      
      if (division.present) {
        if (i < lastDivisionIndex) {
          this.errorHandler.addSemanticError(
            `${division.name} DIVISION appears out of order. Expected order: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE`,
            'DIVISION_ORDER_ERROR',
            division.location || { line: 1, column: 1, endLine: 1, endColumn: 1 }
          );
        }
        lastDivisionIndex = i;
      }
    }
  }

  /**
   * Validate required COBOL elements
   */
  private validateRequiredElements(program: CobolProgram): void {
    if (!this.config.checkRequiredElements) return;

    // IDENTIFICATION DIVISION is required
    if (!program.identificationDivision) {
      this.errorHandler.addSemanticError(
        'IDENTIFICATION DIVISION is required',
        'MISSING_IDENTIFICATION_DIVISION',
        { line: 1, column: 1, endLine: 1, endColumn: 1 }
      );
    }

    // PROGRAM-ID is required in IDENTIFICATION DIVISION
    if (program.identificationDivision && !program.identificationDivision.programId) {
      this.errorHandler.addSemanticError(
        'PROGRAM-ID is required in IDENTIFICATION DIVISION',
        'MISSING_PROGRAM_ID',
        program.identificationDivision.location
      );
    }

    // If DATA DIVISION exists, at least one section should be present
    if (program.dataDivision) {
      const hasAnySection = program.dataDivision.workingStorageVariables.length > 0 ||
                           program.dataDivision.fileVariables.length > 0 ||
                           program.dataDivision.linkageVariables.length > 0;
      
      if (!hasAnySection) {
        this.errorHandler.addSemanticError(
          'DATA DIVISION should contain at least one section (FILE, WORKING-STORAGE, or LINKAGE)',
          'EMPTY_DATA_DIVISION',
          program.dataDivision.location
        );
      }
    }
  }

  /**
   * Validate COBOL naming conventions
   */
  private validateNamingConventions(program: CobolProgram): void {
    if (!this.config.validateNamingConventions) return;

    // Program name validation
    if (program.identificationDivision?.programId) {
      this.validateName(
        program.identificationDivision.programId,
        'PROGRAM-ID',
        program.identificationDivision.location,
        'INVALID_PROGRAM_NAME'
      );
    }

    // Section name validation
    if (program.procedureDivision) {
      program.procedureDivision.sections.forEach(section => {
        this.validateName(section.name, 'SECTION', section.location, 'INVALID_SECTION_NAME');
      });

      // Paragraph name validation
      program.procedureDivision.paragraphs.forEach(paragraph => {
        this.validateName(paragraph.name, 'PARAGRAPH', paragraph.location, 'INVALID_PARAGRAPH_NAME');
      });
    }

    // Variable name validation
    if (program.dataDivision) {
      [...program.dataDivision.workingStorageVariables,
       ...program.dataDivision.fileVariables,
       ...program.dataDivision.linkageVariables].forEach(variable => {
        this.validateName(variable.name, 'VARIABLE', variable.location, 'INVALID_VARIABLE_NAME');
      });
    }
  }

  /**
   * Validate individual name according to COBOL rules
   */
  private validateName(name: string, type: string, location: SourceLocation, errorCode: string): void {
    // COBOL naming rules:
    // - 1-30 characters
    // - Must start with letter
    // - Can contain letters, digits, hyphens
    // - Cannot end with hyphen
    // - Cannot contain consecutive hyphens

    if (!name || name.length === 0) {
      this.errorHandler.addSemanticError(
        `${type} name cannot be empty`,
        errorCode,
        location
      );
      return;
    }

    if (name.length > 30) {
      this.errorHandler.addSemanticError(
        `${type} name '${name}' exceeds maximum length of 30 characters (${name.length})`,
        errorCode,
        location
      );
    }

    if (!/^[A-Za-z]/.test(name)) {
      this.errorHandler.addSemanticError(
        `${type} name '${name}' must start with a letter`,
        errorCode,
        location
      );
    }

    if (!/^[A-Za-z0-9-]+$/.test(name)) {
      this.errorHandler.addSemanticError(
        `${type} name '${name}' contains invalid characters. Only letters, digits, and hyphens are allowed`,
        errorCode,
        location
      );
    }

    if (name.endsWith('-')) {
      this.errorHandler.addSemanticError(
        `${type} name '${name}' cannot end with a hyphen`,
        errorCode,
        location
      );
    }

    if (name.includes('--')) {
      this.errorHandler.addSemanticError(
        `${type} name '${name}' cannot contain consecutive hyphens`,
        errorCode,
        location
      );
    }
  }

  /**
   * Validate data type compatibility
   */
  private validateDataTypes(program: CobolProgram): void {
    if (!this.config.checkDataTypeCompatibility) return;

    if (!program.dataDivision) return;

    const allVariables = [
      ...program.dataDivision.workingStorageVariables,
      ...program.dataDivision.fileVariables,
      ...program.dataDivision.linkageVariables
    ];

    allVariables.forEach(variable => {
      this.validateVariableDataType(variable);
    });
  }

  /**
   * Validate individual variable data type
   */
  private validateVariableDataType(variable: VariableDefinition): void {
    if (!variable.dataType) return;

    const { picture, usage } = variable.dataType;

    // Validate PICTURE clause
    if (picture) {
      if (!this.isValidPictureClause(picture)) {
        this.errorHandler.addSemanticError(
          `Invalid PICTURE clause '${picture}' for variable '${variable.name}'`,
          'INVALID_PICTURE_CLAUSE',
          variable.location
        );
      }
    }

    // Validate USAGE clause compatibility
    if (usage && picture) {
      if (!this.isCompatibleUsageAndPicture(usage, picture)) {
        this.errorHandler.addSemanticError(
          `USAGE '${usage}' is not compatible with PICTURE '${picture}' for variable '${variable.name}'`,
          'INCOMPATIBLE_USAGE_PICTURE',
          variable.location
        );
      }
    }
  }

  /**
   * Validate PICTURE clause format
   */
  private isValidPictureClause(picture: string): boolean {
    // Basic PICTURE validation - can be extended
    const picturePattern = /^[X9AVS\(\)\.,$\-\+*Z]+$/i;
    return picturePattern.test(picture);
  }

  /**
   * Check USAGE and PICTURE compatibility
   */
  private isCompatibleUsageAndPicture(usage: string, picture: string): boolean {
    // Basic compatibility rules - can be extended
    switch (usage) {
      case 'DISPLAY':
        return true; // Most flexible
      case 'COMPUTATIONAL':
      case 'COMP':
        return /^[9SV\(\)]+$/.test(picture); // Numeric only
      case 'COMP-1':
      case 'COMP-2':
        return picture === '' || picture === 'S9(4)' || picture === 'S9(8)';
      default:
        return true;
    }
  }

  /**
   * Validate paragraph and section references
   */
  private validateReferences(program: CobolProgram): void {
    if (!this.config.validateReferences) return;

    if (!program.procedureDivision) return;

    // Collect all available section and paragraph names
    const availableNames = new Set<string>();
    
    program.procedureDivision.sections.forEach(section => {
      availableNames.add(section.name);
      section.paragraphs.forEach(paragraph => {
        availableNames.add(paragraph.name);
      });
    });

    program.procedureDivision.paragraphs.forEach(paragraph => {
      availableNames.add(paragraph.name);
    });

    // Check references in procedure division
    program.procedureDivision.calledSectionsParagraphs.forEach(reference => {
      if (!availableNames.has(reference.name)) {
        this.errorHandler.addSemanticError(
          `Reference to undefined ${reference.type} '${reference.name}'`,
          'UNDEFINED_REFERENCE',
          reference.location
        );
      }
    });
  }

  /**
   * Check for duplicate names
   */
  private validateDuplicateNames(program: CobolProgram): void {
    if (!this.config.checkDuplicateNames) return;

    // Check duplicate section names
    if (program.procedureDivision) {
      this.checkDuplicatesInList(
        program.procedureDivision.sections.map(s => ({ name: s.name, location: s.location })),
        'SECTION',
        'DUPLICATE_SECTION_NAME'
      );

      // Check duplicate paragraph names
      const allParagraphs = [
        ...program.procedureDivision.paragraphs,
        ...program.procedureDivision.sections.flatMap(s => s.paragraphs)
      ];
      
      this.checkDuplicatesInList(
        allParagraphs.map(p => ({ name: p.name, location: p.location })),
        'PARAGRAPH',
        'DUPLICATE_PARAGRAPH_NAME'
      );
    }

    // Check duplicate variable names
    if (program.dataDivision) {
      const allVariables = [
        ...program.dataDivision.workingStorageVariables,
        ...program.dataDivision.fileVariables,
        ...program.dataDivision.linkageVariables
      ];

      this.checkDuplicatesInList(
        allVariables.map(v => ({ name: v.name, location: v.location })),
        'VARIABLE',
        'DUPLICATE_VARIABLE_NAME'
      );
    }
  }

  /**
   * Check for duplicates in a list of named items
   */
  private checkDuplicatesInList(
    items: { name: string; location: SourceLocation }[],
    type: string,
    errorCode: string
  ): void {
    const nameMap = new Map<string, SourceLocation[]>();

    items.forEach(item => {
      const normalizedName = item.name.toUpperCase();
      if (!nameMap.has(normalizedName)) {
        nameMap.set(normalizedName, []);
      }
      nameMap.get(normalizedName)!.push(item.location);
    });

    nameMap.forEach((locations, name) => {
      if (locations.length > 1) {
        locations.forEach((location, index) => {
          if (index > 0) { // Skip first occurrence
            this.errorHandler.addSemanticError(
              `Duplicate ${type} name '${name}'. Previous declaration at line ${locations[0].line}`,
              errorCode,
              location,
              [locations[0]] // Related location
            );
          }
        });
      }
    });
  }

  /**
   * Get validation errors
   */
  getErrors(): SemanticError[] {
    return this.errorHandler.getErrors().filter(e => e instanceof SemanticError) as SemanticError[];
  }

  /**
   * Get validation warnings
   */
  getWarnings(): SemanticError[] {
    return this.errorHandler.getWarnings().filter(w => w instanceof SemanticError) as SemanticError[];
  }

  /**
   * Get error handler for integration
   */
  getErrorHandler(): CobolErrorHandler {
    return this.errorHandler;
  }

  /**
   * Clear all validation results
   */
  clear(): void {
    this.errorHandler.clear();
  }

  /**
   * Check if validation passed
   */
  hasErrors(): boolean {
    return this.errorHandler.hasErrors();
  }

  /**
   * Get validation summary
   */
  getSummary(): {
    errorCount: number;
    warningCount: number;
    hasErrors: boolean;
    hasWarnings: boolean;
  } {
    return this.errorHandler.getSummary();
  }
}