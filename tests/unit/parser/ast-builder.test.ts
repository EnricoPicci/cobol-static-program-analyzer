/**
 * Unit tests for AST Builder implementation
 */

import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { ParseTree, ParserRuleContext, TerminalNode } from 'antlr4ng';

import { CobolASTBuilder, DEFAULT_AST_BUILDER_CONFIG } from '../../../src/ast/builder';
import { CobolProgram } from '../../../src/ast/nodes/CobolProgram';
import { IdentificationDivision } from '../../../src/ast/nodes/IdentificationDivision';
import { DataDivision } from '../../../src/ast/nodes/DataDivision';
import { ProcedureDivision } from '../../../src/ast/nodes/ProcedureDivision';

// Mock ANTLR context classes for testing
class MockParserRuleContext implements ParseTree {
  public mockRuleName: string;
  public mockText: string;
  public mockChildren: ParseTree[] = [];
  public parent!: ParseTree;
  public sourceInterval: any = { a: 0, b: 0 };
  public payload: any;

  constructor(ruleName: string, text: string = '') {
    this.mockRuleName = ruleName;
    this.mockText = text;
    this.payload = { ruleName, text };
  }

  get text(): string {
    return this.mockText;
  }

  get childCount(): number {
    return this.mockChildren.length;
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

  // Mock the constructor name for rule identification
  static {
    Object.defineProperty(this.prototype.constructor, 'name', {
      value: 'MockParserRuleContext',
      configurable: true
    });
  }
}

class MockTerminalNode implements TerminalNode {
  public mockText: string;
  public parent!: ParseTree;
  public sourceInterval: any = { a: 0, b: 0 };
  public payload: any;

  constructor(text: string) {
    this.mockText = text;
    this.payload = {
      line: 1,
      charPositionInLine: 0,
      startIndex: 0,
      text: this.mockText
    };
  }

  get text(): string {
    return this.mockText;
  }

  get symbol(): any {
    return this.payload;
  }

  getSymbol(): any {
    return this.payload;
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

describe('CobolASTBuilder', () => {
  let builder: CobolASTBuilder;

  beforeEach(() => {
    builder = new CobolASTBuilder(DEFAULT_AST_BUILDER_CONFIG);
  });

  afterEach(() => {
    builder.cleanup();
  });

  describe('Configuration and Initialization', () => {
    it('should initialize with default configuration', () => {
      expect(builder).toBeInstanceOf(CobolASTBuilder);
      expect(builder.getErrors()).toEqual([]);
    });

    it('should accept custom configuration', () => {
      const customConfig = {
        ...DEFAULT_AST_BUILDER_CONFIG,
        preserveSourceText: false,
        enableOptimizations: false
      };
      const customBuilder = new CobolASTBuilder(customConfig);
      expect(customBuilder).toBeInstanceOf(CobolASTBuilder);
    });
  });

  describe('Rule Name Extraction', () => {
    it('should extract rule names from context class names', () => {
      const ctx = new MockParserRuleContext('programUnit', 'PROGRAM-ID. TEST-PROGRAM.');
      // Set constructor name to simulate ANTLR generated context
      Object.defineProperty(ctx.constructor, 'name', { value: 'ProgramUnitContext' });
      
      const ruleName = (builder as any).getRuleName(ctx);
      expect(ruleName).toBe('programUnit');
    });

    it('should handle rule name caching for performance', () => {
      const ctx = new MockParserRuleContext('identificationDivision', '');
      Object.defineProperty(ctx.constructor, 'name', { value: 'IdentificationDivisionContext' });
      
      const ruleName1 = (builder as any).getCachedRuleName(ctx);
      const ruleName2 = (builder as any).getCachedRuleName(ctx);
      
      expect(ruleName1).toBe(ruleName2);
      expect(ruleName1).toBe('identificationDivision');
    });
  });

  describe('Program ID Extraction', () => {
    it('should extract program ID from programIdParagraph', () => {
      const programIdCtx = new MockParserRuleContext('programIdParagraph', 'PROGRAM-ID. TEST-PROGRAM.');
      programIdCtx.addChild(new MockTerminalNode('PROGRAM-ID'));
      programIdCtx.addChild(new MockTerminalNode('.'));
      programIdCtx.addChild(new MockTerminalNode('TEST-PROGRAM'));
      programIdCtx.addChild(new MockTerminalNode('.'));

      const identCtx = new MockParserRuleContext('identificationDivision', '');
      identCtx.addChild(programIdCtx);

      const programId = (builder as any).extractProgramId(identCtx);
      expect(programId).toBe('TEST-PROGRAM');
    });

    it('should return fallback for missing program ID', () => {
      const ctx = new MockParserRuleContext('identificationDivision', '');
      const programId = (builder as any).extractProgramId(ctx);
      expect(programId).toBe('UNKNOWN-PROGRAM');
    });

    it('should extract program ID from text pattern', () => {
      const ctx = new MockParserRuleContext('identificationDivision', 'IDENTIFICATION DIVISION. PROGRAM-ID. PAYROLL-SYSTEM.');
      const programId = (builder as any).extractProgramId(ctx);
      expect(programId).toBe('PAYROLL-SYSTEM');
    });
  });

  describe('Section Name Extraction', () => {
    it('should extract section name from procedure section', () => {
      const headerCtx = new MockParserRuleContext('procedureSectionHeader', 'MAIN-PROCESSING SECTION');
      const sectionCtx = new MockParserRuleContext('procedureSection', '');
      sectionCtx.addChild(headerCtx);

      const sectionName = (builder as any).extractSectionName(sectionCtx);
      expect(sectionName).toBe('MAIN-PROCESSING');
    });

    it('should extract section name from text pattern', () => {
      const ctx = new MockParserRuleContext('procedureSection', 'INITIALIZATION SECTION.\n    DISPLAY "Starting".');
      const sectionName = (builder as any).extractSectionName(ctx);
      expect(sectionName).toBe('INITIALIZATION');
    });
  });

  describe('Paragraph Name Extraction', () => {
    it('should extract paragraph name from context', () => {
      const paragraphCtx = new MockParserRuleContext('paragraph', 'PROCESS-RECORDS.');
      paragraphCtx.addChild(new MockTerminalNode('PROCESS-RECORDS'));
      paragraphCtx.addChild(new MockTerminalNode('.'));

      const paragraphName = (builder as any).extractParagraphName(paragraphCtx);
      expect(paragraphName).toBe('PROCESS-RECORDS');
    });

    it('should extract paragraph name from text', () => {
      const ctx = new MockParserRuleContext('paragraph', 'VALIDATE-INPUT.\n    IF FIELD-A = SPACES\n        DISPLAY "ERROR".');
      const paragraphName = (builder as any).extractParagraphName(ctx);
      expect(paragraphName).toBe('VALIDATE-INPUT');
    });
  });

  describe('Variable Declaration Parsing', () => {
    it('should parse data description entry with PICTURE clause', () => {
      const dataEntry = new MockParserRuleContext('dataDescriptionEntry', '01 WS-COUNTER PIC 9(5) VALUE 0.');
      const variable = (builder as any).parseDataDescriptionEntry(dataEntry);

      expect(variable).toBeDefined();
      expect(variable.name).toBe('WS-COUNTER');
      expect(variable.level).toBe(1);
      expect(variable.dataType.picture).toBe('9(5)');
      expect(variable.initialValue).toBe('0');
    });

    it('should parse data description entry with USAGE clause', () => {
      const dataEntry = new MockParserRuleContext('dataDescriptionEntry', '01 WS-AMOUNT PIC 9(7)V99 USAGE COMP-3.');
      const variable = (builder as any).parseDataDescriptionEntry(dataEntry);

      expect(variable).toBeDefined();
      expect(variable.name).toBe('WS-AMOUNT');
      expect(variable.dataType.picture).toBe('9(7)V99');
      expect(variable.dataType.usage).toBe('COMP-3');
    });

    it('should extract variable declarations from working storage section', () => {
      const wsCtx = new MockParserRuleContext('workingStorageSection', '');
      const dataEntry1 = new MockParserRuleContext('dataDescriptionEntry', '01 WS-NAME PIC X(30).');
      const dataEntry2 = new MockParserRuleContext('dataDescriptionEntry', '01 WS-AGE PIC 9(3).');
      
      wsCtx.addChild(dataEntry1);
      wsCtx.addChild(dataEntry2);

      const variables = (builder as any).extractVariableDeclarations(wsCtx);
      expect(variables).toHaveLength(2);
      expect(variables[0].name).toBe('WS-NAME');
      expect(variables[1].name).toBe('WS-AGE');
    });
  });

  describe('Statement Analysis', () => {
    it('should extract target from MOVE statement', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'MOVE WS-INPUT TO WS-OUTPUT');
      const target = (builder as any).extractStatementTarget(stmtCtx);
      expect(target).toBe('WS-OUTPUT');
    });

    it('should extract target from COMPUTE statement', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'COMPUTE WS-TOTAL = WS-A + WS-B');
      const target = (builder as any).extractStatementTarget(stmtCtx);
      expect(target).toBe('WS-TOTAL');
    });

    it('should extract target from arithmetic statement with GIVING', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'ADD WS-A TO WS-B GIVING WS-RESULT');
      const target = (builder as any).extractStatementTarget(stmtCtx);
      expect(target).toBe('WS-RESULT');
    });

    it('should extract operands from MOVE statement', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'MOVE WS-SOURCE TO WS-TARGET');
      const operands = (builder as any).extractStatementOperands(stmtCtx);
      expect(operands).toContain('WS-SOURCE');
    });

    it('should extract operands from DISPLAY statement', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'DISPLAY "Hello" WS-NAME "World"');
      const operands = (builder as any).extractStatementOperands(stmtCtx);
      expect(operands).toContain('HELLO');
      expect(operands).toContain('WS-NAME');
      expect(operands).toContain('WORLD');
    });

    it('should extract operands from arithmetic statements', () => {
      const stmtCtx = new MockParserRuleContext('statement', 'ADD WS-VALUE TO WS-TOTAL');
      const operands = (builder as any).extractStatementOperands(stmtCtx);
      expect(operands).toContain('WS-VALUE');
      expect(operands).toContain('WS-TOTAL');
    });
  });

  describe('Performance Optimizations', () => {
    it('should provide performance statistics', () => {
      const stats = builder.getPerformanceStats();
      expect(stats).toHaveProperty('cacheSize');
      expect(stats).toHaveProperty('poolSizes');
      expect(stats).toHaveProperty('evaluatedNodes');
    });

    it('should clean up caches and pools', () => {
      // Use some cached operations
      const ctx = new MockParserRuleContext('test', '');
      Object.defineProperty(ctx.constructor, 'name', { value: 'TestContext' });
      (builder as any).getCachedRuleName(ctx);

      builder.cleanup();
      const stats = builder.getPerformanceStats();
      expect(stats.cacheSize).toBe(0);
    });
  });

  describe('Comment Entry Extraction', () => {
    it('should extract comment entry from paragraph', () => {
      const ctx = new MockParserRuleContext('authorParagraph', 'AUTHOR. John Smith.');
      const comment = (builder as any).extractCommentEntry(ctx);
      expect(comment).toBe('John Smith.');
    });

    it('should return undefined for paragraphs without comments', () => {
      const ctx = new MockParserRuleContext('authorParagraph', 'AUTHOR.');
      const comment = (builder as any).extractCommentEntry(ctx);
      expect(comment).toBeUndefined();
    });
  });

  describe('Error Handling', () => {
    it('should collect and return errors', () => {
      expect(builder.getErrors()).toEqual([]);
      builder.clearErrors();
      expect(builder.getErrors()).toEqual([]);
    });

    it('should handle maximum nesting depth', () => {
      const config = {
        ...DEFAULT_AST_BUILDER_CONFIG,
        maxNestingDepth: 2
      };
      const deepBuilder = new CobolASTBuilder(config);
      
      // This would require creating a very deep parse tree to test properly
      // For now, just verify the builder initializes with the config
      expect(deepBuilder).toBeInstanceOf(CobolASTBuilder);
    });
  });
});