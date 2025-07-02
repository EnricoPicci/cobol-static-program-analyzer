/**
 * Integration tests for AST Builder with realistic COBOL program structures
 */

import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { ParseTree, ParserRuleContext, TerminalNode } from 'antlr4ng';

import { CobolASTBuilder, DEFAULT_AST_BUILDER_CONFIG } from '../../../src/ast/builder';
import { CobolProgram } from '../../../src/ast/nodes/CobolProgram';
import { IdentificationDivision } from '../../../src/ast/nodes/IdentificationDivision';

// Enhanced mock classes for integration testing
class IntegrationMockContext implements ParseTree {
  public mockRuleName: string;
  public mockText: string;
  public mockChildren: ParseTree[] = [];
  private mockStart: any;
  private mockStop: any;
  public parent!: ParseTree;
  public sourceInterval: any = { a: 0, b: 0 };
  public payload: any;

  constructor(ruleName: string, text: string = '', line: number = 1, column: number = 0) {
    this.mockRuleName = ruleName;
    this.mockText = text;
    this.mockStart = {
      line,
      charPositionInLine: column,
      startIndex: 0,
      text: text.split(' ')[0] || ''
    };
    this.mockStop = {
      line,
      charPositionInLine: column + text.length,
      stopIndex: text.length - 1,
      text: text.split(' ').pop() || ''
    };
    this.sourceInterval = { a: 0, b: text.length - 1 };
    this.payload = { ruleName, text, start: this.mockStart, stop: this.mockStop };

    // Mock constructor name for rule identification
    Object.defineProperty(this.constructor, 'name', {
      value: `${ruleName.charAt(0).toUpperCase() + ruleName.slice(1)}Context`,
      configurable: true
    });
  }

  get text(): string {
    return this.mockText;
  }

  get start(): any {
    return this.mockStart;
  }

  get stop(): any {
    return this.mockStop;
  }

  getChildCount(): number {
    return this.mockChildren.length;
  }

  getChild(index: number): ParseTree {
    return this.mockChildren[index];
  }

  setParent(parent: ParseTree): void {
    this.parent = parent;
  }

  getParent(): ParseTree | undefined {
    return this.parent;
  }

  getPayload(): any {
    return this.payload;
  }

  getSourceInterval(): any {
    return this.sourceInterval;
  }

  getText(): string {
    return this.mockText;
  }

  toStringTree(): string {
    return this.mockText;
  }

  toString(): string {
    return this.mockText;
  }

  accept<T>(visitor: any): T {
    return visitor.visitChildren ? visitor.visitChildren(this) : visitor.visit(this);
  }

  addChild(child: ParseTree): void {
    this.mockChildren.push(child);
    if ('setParent' in child) {
      (child as any).setParent(this);
    }
  }
}

class IntegrationMockTerminal implements TerminalNode {
  public mockText: string;
  private mockSymbol: any;
  public parent!: ParseTree;
  public sourceInterval: any;
  public payload: any;

  constructor(text: string, line: number = 1, column: number = 0) {
    this.mockText = text;
    this.mockSymbol = {
      line,
      charPositionInLine: column,
      startIndex: 0,
      text: text
    };
    this.sourceInterval = { a: 0, b: text.length - 1 };
    this.payload = this.mockSymbol;
  }

  get text(): string {
    return this.mockText;
  }

  get symbol(): any {
    return this.mockSymbol;
  }

  getSymbol(): any {
    return this.mockSymbol;
  }

  get childCount(): number {
    return 0;
  }

  getChildCount(): number {
    return 0;
  }

  getChild(i: number): any {
    return null; // Terminal nodes have no children
  }

  setParent(parent: ParseTree): void {
    this.parent = parent;
  }

  getParent(): ParseTree | undefined {
    return this.parent;
  }

  getPayload(): any {
    return this.payload;
  }

  getSourceInterval(): any {
    return this.sourceInterval;
  }

  getText(): string {
    return this.mockText;
  }

  toStringTree(): string {
    return this.mockText;
  }

  toString(): string {
    return this.mockText;
  }

  accept<T>(visitor: any): T {
    return visitor.visitTerminal ? visitor.visitTerminal(this) : visitor.visit(this);
  }
}

describe('CobolASTBuilder Integration Tests', () => {
  let builder: CobolASTBuilder;

  beforeEach(() => {
    builder = new CobolASTBuilder(DEFAULT_AST_BUILDER_CONFIG);
  });

  afterEach(() => {
    builder.cleanup();
  });

  describe('Complete Program Structure', () => {
    it('should build AST for simple COBOL program', () => {
      // Create a mock parse tree for a simple COBOL program
      const compilationUnit = new IntegrationMockContext('compilationUnit', '', 1, 0);
      const programUnit = new IntegrationMockContext('programUnit', '', 1, 0);
      
      // Identification Division
      const identDiv = new IntegrationMockContext('identificationDivision', 
        'IDENTIFICATION DIVISION. PROGRAM-ID. HELLO-WORLD.', 1, 0);
      const programIdPara = new IntegrationMockContext('programIdParagraph', 
        'PROGRAM-ID. HELLO-WORLD.', 1, 0);
      programIdPara.addChild(new IntegrationMockTerminal('PROGRAM-ID', 1, 0));
      programIdPara.addChild(new IntegrationMockTerminal('.', 1, 10));
      programIdPara.addChild(new IntegrationMockTerminal('HELLO-WORLD', 1, 12));
      programIdPara.addChild(new IntegrationMockTerminal('.', 1, 23));
      identDiv.addChild(programIdPara);

      // Data Division
      const dataDiv = new IntegrationMockContext('dataDivision', 
        'DATA DIVISION. WORKING-STORAGE SECTION. 01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".', 2, 0);
      const wsSection = new IntegrationMockContext('workingStorageSection', 
        'WORKING-STORAGE SECTION. 01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".', 2, 0);
      const dataEntry = new IntegrationMockContext('dataDescriptionEntry', 
        '01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".', 3, 0);
      wsSection.addChild(dataEntry);
      dataDiv.addChild(wsSection);

      // Procedure Division
      const procDiv = new IntegrationMockContext('procedureDivision', 
        'PROCEDURE DIVISION. DISPLAY WS-MESSAGE. STOP RUN.', 4, 0);
      const procBody = new IntegrationMockContext('procedureDivisionBody', 
        'DISPLAY WS-MESSAGE. STOP RUN.', 4, 0);
      const paragraph = new IntegrationMockContext('paragraph', 
        'MAIN-PARA. DISPLAY WS-MESSAGE. STOP RUN.', 4, 0);
      const stmt1 = new IntegrationMockContext('statement', 'DISPLAY WS-MESSAGE', 4, 0);
      const stmt2 = new IntegrationMockContext('statement', 'STOP RUN', 4, 20);
      paragraph.addChild(stmt1);
      paragraph.addChild(stmt2);
      procBody.addChild(paragraph);
      procDiv.addChild(procBody);

      // Assemble the program
      programUnit.addChild(identDiv);
      programUnit.addChild(dataDiv);
      programUnit.addChild(procDiv);
      compilationUnit.addChild(programUnit);

      // Build the AST
      const ast = builder.build(compilationUnit);

      // Verify AST structure
      expect(ast).toBeInstanceOf(CobolProgram);
      expect(ast.name).toBe('HELLO-WORLD');
      expect(ast.identificationDivision).toBeDefined();
      expect(ast.identificationDivision.programId).toBe('HELLO-WORLD');
      expect(ast.dataDivision).toBeDefined();
      expect(ast.procedureDivision).toBeDefined();
    });

    it('should handle program with sections and paragraphs', () => {
      const compilationUnit = new IntegrationMockContext('compilationUnit', '', 1, 0);
      const programUnit = new IntegrationMockContext('programUnit', '', 1, 0);
      
      // Identification Division
      const identDiv = new IntegrationMockContext('identificationDivision', 
        'IDENTIFICATION DIVISION. PROGRAM-ID. PAYROLL-SYSTEM.', 1, 0);
      const programIdPara = new IntegrationMockContext('programIdParagraph', 
        'PROGRAM-ID. PAYROLL-SYSTEM.', 1, 0);
      programIdPara.addChild(new IntegrationMockTerminal('PAYROLL-SYSTEM', 1, 12));
      identDiv.addChild(programIdPara);

      // Procedure Division with sections
      const procDiv = new IntegrationMockContext('procedureDivision', '', 4, 0);
      const procBody = new IntegrationMockContext('procedureDivisionBody', '', 4, 0);
      
      // Main section
      const mainSection = new IntegrationMockContext('procedureSection', 
        'MAIN-PROCESSING SECTION. INITIALIZATION. PROCESS-RECORDS. FINALIZATION.', 4, 0);
      const sectionHeader = new IntegrationMockContext('procedureSectionHeader', 
        'MAIN-PROCESSING SECTION', 4, 0);
      mainSection.addChild(sectionHeader);
      
      // Paragraphs within section
      const para1 = new IntegrationMockContext('paragraph', 
        'INITIALIZATION. DISPLAY "Starting payroll processing".', 5, 0);
      const para2 = new IntegrationMockContext('paragraph', 
        'PROCESS-RECORDS. PERFORM READ-EMPLOYEE-FILE.', 6, 0);
      const para3 = new IntegrationMockContext('paragraph', 
        'FINALIZATION. DISPLAY "Payroll processing complete".', 7, 0);
      
      mainSection.addChild(para1);
      mainSection.addChild(para2);
      mainSection.addChild(para3);
      procBody.addChild(mainSection);
      procDiv.addChild(procBody);

      programUnit.addChild(identDiv);
      programUnit.addChild(procDiv);
      compilationUnit.addChild(programUnit);

      const ast = builder.build(compilationUnit);

      expect(ast.name).toBe('PAYROLL-SYSTEM');
      expect(ast.procedureDivision).toBeDefined();
      expect(ast.procedureDivision!.sections).toHaveLength(1);
      
      const section = ast.procedureDivision!.sections[0];
      expect(section.name).toBe('MAIN-PROCESSING');
      expect(section.paragraphs).toHaveLength(3);
      expect(section.paragraphs[0].name).toBe('INITIALIZATION');
      expect(section.paragraphs[1].name).toBe('PROCESS-RECORDS');
      expect(section.paragraphs[2].name).toBe('FINALIZATION');
    });

    it('should handle complete identification division with all paragraphs', () => {
      const identDiv = new IntegrationMockContext('identificationDivision', '', 1, 0);
      
      // Program ID
      const programIdPara = new IntegrationMockContext('programIdParagraph', 
        'PROGRAM-ID. INVENTORY-SYSTEM.', 1, 0);
      programIdPara.addChild(new IntegrationMockTerminal('INVENTORY-SYSTEM', 1, 12));
      identDiv.addChild(programIdPara);

      // Author
      const authorPara = new IntegrationMockContext('authorParagraph', 
        'AUTHOR. Jane Developer.', 2, 0);
      identDiv.addChild(authorPara);

      // Date Written
      const dateWrittenPara = new IntegrationMockContext('dateWrittenParagraph', 
        'DATE-WRITTEN. 2024-01-15.', 3, 0);
      identDiv.addChild(dateWrittenPara);

      // Installation
      const installationPara = new IntegrationMockContext('installationParagraph', 
        'INSTALLATION. Corporate Data Center.', 4, 0);
      identDiv.addChild(installationPara);

      // Security
      const securityPara = new IntegrationMockContext('securityParagraph', 
        'SECURITY. Confidential.', 5, 0);
      identDiv.addChild(securityPara);

      // Remarks
      const remarksPara = new IntegrationMockContext('remarksParagraph', 
        'REMARKS. This program manages inventory transactions.', 6, 0);
      identDiv.addChild(remarksPara);

      const identDivision = (builder as any).visitIdentificationDivision(identDiv);

      expect(identDivision).toBeInstanceOf(IdentificationDivision);
      expect(identDivision.programId).toBe('INVENTORY-SYSTEM');
      expect(identDivision.author).toBe('Jane Developer.');
      expect(identDivision.dateWritten).toBe('2024-01-15.');
      expect(identDivision.installation).toBe('Corporate Data Center.');
      expect(identDivision.security).toBe('Confidential.');
      expect(identDivision.remarks).toContain('This program manages inventory transactions.');
    });
  });

  describe('Data Division Complex Structures', () => {
    it('should parse complex working storage variables', () => {
      const wsSection = new IntegrationMockContext('workingStorageSection', '', 1, 0);
      
      // Group item
      const groupItem = new IntegrationMockContext('dataDescriptionEntry', 
        '01 WS-EMPLOYEE-RECORD.', 1, 0);
      wsSection.addChild(groupItem);

      // Elementary items
      const nameItem = new IntegrationMockContext('dataDescriptionEntry', 
        '   05 WS-EMP-NAME PIC X(30).', 2, 0);
      wsSection.addChild(nameItem);

      const idItem = new IntegrationMockContext('dataDescriptionEntry', 
        '   05 WS-EMP-ID PIC 9(6).', 3, 0);
      wsSection.addChild(idItem);

      const salaryItem = new IntegrationMockContext('dataDescriptionEntry', 
        '   05 WS-EMP-SALARY PIC 9(7)V99 COMP-3.', 4, 0);
      wsSection.addChild(salaryItem);

      // Counter with initial value
      const counterItem = new IntegrationMockContext('dataDescriptionEntry', 
        '01 WS-RECORD-COUNT PIC 9(5) VALUE ZERO.', 5, 0);
      wsSection.addChild(counterItem);

      const variables = (builder as any).extractVariableDeclarations(wsSection);

      expect(variables).toHaveLength(5);
      
      // Group item
      expect(variables[0].name).toBe('WS-EMPLOYEE-RECORD');
      expect(variables[0].level).toBe(1);
      
      // Elementary items
      expect(variables[1].name).toBe('WS-EMP-NAME');
      expect(variables[1].level).toBe(5);
      expect(variables[1].dataType.picture).toBe('X(30)');
      
      expect(variables[2].name).toBe('WS-EMP-ID');
      expect(variables[2].dataType.picture).toBe('9(6)');
      
      expect(variables[3].name).toBe('WS-EMP-SALARY');
      expect(variables[3].dataType.picture).toBe('9(7)V99');
      expect(variables[3].dataType.usage).toBe('COMP-3');
      
      expect(variables[4].name).toBe('WS-RECORD-COUNT');
      expect(variables[4].initialValue).toBe('ZERO');
    });
  });

  describe('Statement Analysis Integration', () => {
    it('should analyze various statement types in context', () => {
      const statements = [
        {
          ctx: new IntegrationMockContext('statement', 'MOVE WS-INPUT-NAME TO WS-OUTPUT-NAME'),
          expectedTarget: 'WS-OUTPUT-NAME',
          expectedOperands: ['WS-INPUT-NAME']
        },
        {
          ctx: new IntegrationMockContext('statement', 'ADD WS-DAILY-SALES TO WS-TOTAL-SALES'),
          expectedTarget: undefined,
          expectedOperands: ['WS-DAILY-SALES', 'WS-TOTAL-SALES']
        },
        {
          ctx: new IntegrationMockContext('statement', 'COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-DEDUCTIONS'),
          expectedTarget: 'WS-NET-PAY',
          expectedOperands: []
        },
        {
          ctx: new IntegrationMockContext('statement', 'DISPLAY "Employee: " WS-EMP-NAME " Salary: " WS-EMP-SALARY'),
          expectedTarget: undefined,
          expectedOperands: ['EMPLOYEE:', 'WS-EMP-NAME', 'SALARY:', 'WS-EMP-SALARY']
        },
        {
          ctx: new IntegrationMockContext('statement', 'PERFORM PROCESS-EMPLOYEE-RECORD'),
          expectedTarget: 'PROCESS-EMPLOYEE-RECORD',
          expectedOperands: []
        }
      ];

      statements.forEach(({ ctx, expectedTarget, expectedOperands }) => {
        const target = (builder as any).extractStatementTarget(ctx);
        const operands = (builder as any).extractStatementOperands(ctx);

        if (expectedTarget) {
          expect(target).toBe(expectedTarget);
        } else {
          expect(target).toBeUndefined();
        }

        expectedOperands.forEach(expectedOperand => {
          expect(operands).toContain(expectedOperand);
        });
      });
    });
  });

  describe('Performance Integration', () => {
    it('should handle large program structures efficiently', () => {
      const startTime = performance.now();
      
      // Create a program with many sections and paragraphs
      const compilationUnit = new IntegrationMockContext('compilationUnit', '', 1, 0);
      const programUnit = new IntegrationMockContext('programUnit', '', 1, 0);
      
      const identDiv = new IntegrationMockContext('identificationDivision', '', 1, 0);
      const programIdPara = new IntegrationMockContext('programIdParagraph', 'PROGRAM-ID. LARGE-PROGRAM.', 1, 0);
      programIdPara.addChild(new IntegrationMockTerminal('LARGE-PROGRAM', 1, 12));
      identDiv.addChild(programIdPara);

      const procDiv = new IntegrationMockContext('procedureDivision', '', 2, 0);
      const procBody = new IntegrationMockContext('procedureDivisionBody', '', 2, 0);

      // Create multiple sections with multiple paragraphs each
      for (let s = 1; s <= 10; s++) {
        const section = new IntegrationMockContext('procedureSection', '', 2, 0);
        const sectionHeader = new IntegrationMockContext('procedureSectionHeader', `SECTION-${s} SECTION`, 2, 0);
        section.addChild(sectionHeader);

        for (let p = 1; p <= 10; p++) {
          const paragraph = new IntegrationMockContext('paragraph', `PARA-${s}-${p}. DISPLAY "Processing".`, 2, 0);
          const statement = new IntegrationMockContext('statement', 'DISPLAY "Processing"', 2, 0);
          paragraph.addChild(statement);
          section.addChild(paragraph);
        }
        procBody.addChild(section);
      }

      procDiv.addChild(procBody);
      programUnit.addChild(identDiv);
      programUnit.addChild(procDiv);
      compilationUnit.addChild(programUnit);

      const ast = builder.build(compilationUnit);
      const endTime = performance.now();

      expect(ast).toBeInstanceOf(CobolProgram);
      expect(ast.procedureDivision!.sections).toHaveLength(10);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second

      const stats = builder.getPerformanceStats();
      expect(stats.cacheSize).toBeGreaterThan(0);
    });
  });
});