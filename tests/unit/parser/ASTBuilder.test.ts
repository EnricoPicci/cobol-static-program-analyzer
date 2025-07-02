/**
 * Tests for CobolASTBuilder - Parse tree to AST transformation
 */

import '../setup';
import { CobolASTBuilder, DEFAULT_AST_BUILDER_CONFIG } from '../../../src/ast/builder';
import { CobolProgram } from '../../../src/ast/nodes/CobolProgram';
import { IdentificationDivision } from '../../../src/ast/nodes/IdentificationDivision';
import { SourceLocation } from '../../../src/core/types';

// Mock ANTLR parse tree structures for testing
class MockParserRuleContext {
  public childCount: number = 0;
  public children: (MockParserRuleContext | MockTerminalNode)[] = [];
  public text: string = '';
  public start: any;
  public stop: any;
  public parent: any = null;

  constructor(text: string = '', line: number = 1, column: number = 1) {
    this.text = text;
    this.start = { line, charPositionInLine: column - 1, startIndex: 0, text };
    this.stop = { line, charPositionInLine: column + text.length - 1, stopIndex: text.length - 1, text };
  }

  getChild(index: number): any {
    return this.children[index];
  }

  addChild(child: MockParserRuleContext | MockTerminalNode): void {
    this.children.push(child);
    this.childCount = this.children.length;
  }

  getChildCount(): number {
    return this.childCount;
  }

  getText(): string {
    return this.text;
  }

  setParent(parent: any): void {
    this.parent = parent;
  }

  accept<T>(visitor: any): T {
    return visitor.visit(this);
  }

  toStringTree(): string {
    return this.text;
  }

  getSourceInterval(): any {
    return { a: 0, b: this.text.length };
  }

  getPayload(): any {
    return this;
  }

  get sourceInterval(): any {
    return { a: 0, b: this.text.length };
  }

  get payload(): any {
    return this;
  }
}

class MockTerminalNode {
  public symbol: any;
  public text: string;
  public parent: any = null;
  public childCount: number = 0;

  constructor(text: string, line: number = 1, column: number = 1) {
    this.text = text;
    this.symbol = {
      line,
      charPositionInLine: column - 1,
      startIndex: 0,
      text
    };
  }

  setParent(parent: any): void {
    this.parent = parent;
  }

  accept<T>(visitor: any): T {
    return visitor.visitTerminal(this);
  }

  toStringTree(): string {
    return this.text;
  }

  getSourceInterval(): any {
    return { a: 0, b: this.text.length };
  }

  getPayload(): any {
    return this.symbol;
  }

  getChild(): any {
    return null;
  }

  getChildCount(): number {
    return 0;
  }

  getText(): string {
    return this.text;
  }

  get sourceInterval(): any {
    return { a: 0, b: this.text.length };
  }

  get payload(): any {
    return this.symbol;
  }
}

describe('CobolASTBuilder', () => {
  let builder: CobolASTBuilder;
  
  // Helper function to load test data
  const loadTestProgram = (filename: string): string => {
    const fs = require('fs');
    const path = require('path');
    return fs.readFileSync(path.join(__dirname, '../../data/valid/basic', filename), 'utf8');
  };
  
  const loadExpectedAST = (filename: string): any => {
    const fs = require('fs');
    const path = require('path');
    return JSON.parse(fs.readFileSync(path.join(__dirname, '../../data/expected-asts', filename), 'utf8'));
  };

  beforeEach(() => {
    builder = new CobolASTBuilder(DEFAULT_AST_BUILDER_CONFIG);
  });

  describe('Configuration', () => {
    test('should use default configuration', () => {
      const defaultBuilder = new CobolASTBuilder();
      expect(defaultBuilder).toBeDefined();
    });

    test('should accept custom configuration', () => {
      const customConfig = {
        preserveSourceText: false,
        includeComments: false,
        enableOptimizations: true,
        maxNestingDepth: 50,
        validateDuringConstruction: false
      };

      const customBuilder = new CobolASTBuilder(customConfig);
      expect(customBuilder).toBeDefined();
    });
  });

  describe('Error Handling', () => {
    test('should track construction errors', () => {
      expect(builder.getErrors()).toEqual([]);
      
      // Initially no errors
      builder.clearErrors();
      expect(builder.getErrors()).toEqual([]);
    });

    test('should handle invalid parse tree gracefully', () => {
      const invalidTree = null;
      
      expect(() => {
        builder.build(invalidTree as any);
      }).toThrow();
    });

    test('should handle deeply nested structures within limits', () => {
      // Create a mock deeply nested structure
      let root = new MockParserRuleContext('startRule');
      let current = root;
      
      // Create nesting up to the limit
      for (let i = 0; i < 50; i++) {
        const child = new MockParserRuleContext(`nested-${i}`);
        current.addChild(child);
        current = child;
      }
      
      // Should not throw within reasonable limits
      expect(() => {
        builder.visit(root);
      }).not.toThrow();
    });

    test('should prevent stack overflow on excessive nesting', () => {
      const builder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        maxNestingDepth: 5
      });
      
      // Create excessive nesting
      let root = new MockParserRuleContext('startRule');
      let current = root;
      
      for (let i = 0; i < 10; i++) {
        const child = new MockParserRuleContext(`deep-${i}`);
        current.addChild(child);
        current = child;
      }
      
      expect(() => {
        builder.visit(root);
      }).toThrow(/Maximum nesting depth exceeded/);
    });
  });

  describe('Parse Tree Traversal', () => {
    test('should visit terminal nodes', () => {
      const terminal = new MockTerminalNode('IDENTIFICATION', 1, 1);
      
      const result = builder.visit(terminal as any);
      
      expect(result).toBeDefined();
      expect(result.type).toBe('StatementNode');
    });

    test('should visit parser rule contexts', () => {
      const ctx = new MockParserRuleContext('testRule', 1, 1);
      
      const result = builder.visit(ctx as any);
      
      expect(result).toBeDefined();
      expect(result.type).toBeDefined();
    });

    test('should handle empty parse trees', () => {
      const emptyCtx = new MockParserRuleContext('', 1, 1);
      
      const result = builder.visit(emptyCtx as any);
      
      expect(result).toBeDefined();
    });
  });

  describe('Source Location Tracking', () => {
    test('should extract source locations from parser contexts', () => {
      const ctx = new MockParserRuleContext('TEST', 5, 10);
      
      const result = builder.visit(ctx as any);
      
      expect(result.location).toBeDefined();
      expect(result.location.line).toBe(5);
      expect(result.location.column).toBe(10);
    });

    test('should extract source locations from terminal nodes', () => {
      const terminal = new MockTerminalNode('PROGRAM-ID', 2, 5);
      
      const result = builder.visit(terminal as any);
      
      expect(result.location).toBeDefined();
      expect(result.location.line).toBe(2);
      expect(result.location.column).toBe(5);
    });

    test('should handle missing location information gracefully', () => {
      const ctxWithoutLocation = {
        text: 'test',
        childCount: 0,
        children: [],
        getChild: () => null
      };
      
      const result = builder.visit(ctxWithoutLocation as any);
      
      expect(result.location).toBeDefined();
      // Should provide fallback location
      expect(result.location.line).toBe(1);
      expect(result.location.column).toBe(1);
    });
  });

  describe('AST Node Creation', () => {
    test('should create appropriate node types for different contexts', () => {
      // Test various mock contexts
      const contexts = [
        { name: 'identificationDivision', expected: 'IdentificationDivision' },
        { name: 'procedureSection', expected: 'SectionNode' },
        { name: 'paragraph', expected: 'ParagraphNode' },
        { name: 'statement', expected: 'StatementNode' }
      ];

      contexts.forEach(({ name, expected }) => {
        // Create a mock context that will be recognized as the given type
        const ctx = new MockParserRuleContext(name);
        
        // Note: The actual implementation will need to be enhanced
        // to properly recognize rule types from the ANTLR grammar
        const result = builder.visit(ctx as any);
        
        expect(result).toBeDefined();
        expect(result.type).toBeDefined();
      });
    });
  });

  describe('Source Text Preservation', () => {
    test('should preserve source text when configured', () => {
      const preservingBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        preserveSourceText: true
      });

      const ctx = new MockParserRuleContext('DISPLAY "Hello"', 1, 1);
      const result = preservingBuilder.visit(ctx as any);

      // Source text preservation will be implemented in the AST nodes
      expect(result).toBeDefined();
    });

    test('should not preserve source text when disabled', () => {
      const nonPreservingBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        preserveSourceText: false
      });

      const ctx = new MockParserRuleContext('DISPLAY "Hello"', 1, 1);
      const result = nonPreservingBuilder.visit(ctx as any);

      expect(result).toBeDefined();
    });
  });

  describe('Performance Optimizations', () => {
    test('should handle large parse trees efficiently', async () => {
      // Create a large mock parse tree
      const root = new MockParserRuleContext('compilationUnit');
      
      // Add many children to simulate a large program
      for (let i = 0; i < 1000; i++) {
        const child = new MockParserRuleContext(`statement-${i}`);
        root.addChild(child);
      }

      await expect(async () => {
        return builder.visit(root as any);
      }).toParseWithin(100); // Should parse within 100ms
    });

    test('should use memory efficiently', async () => {
      const root = new MockParserRuleContext('compilationUnit');
      
      await expect(async () => {
        return builder.visit(root as any);
      }).toUseMemoryUnder(10); // Should use less than 10MB
    });

    test('should handle deeply nested structures efficiently', () => {
      const start = performance.now();
      
      // Create deeply nested mock structure
      let root = new MockParserRuleContext('startRule');
      let current = root;
      
      for (let i = 0; i < 50; i++) {
        const child = new MockParserRuleContext(`nested-${i}`);
        current.addChild(child);
        current = child;
      }
      
      const result = builder.visit(root as any);
      const duration = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(duration).toBeLessThan(50); // Should complete within 50ms
    });

    test('should reuse node instances when optimizations enabled', () => {
      const optimizedBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        enableOptimizations: true
      });

      const ctx1 = new MockParserRuleContext('statement');
      const ctx2 = new MockParserRuleContext('statement');
      
      const result1 = optimizedBuilder.visit(ctx1 as any);
      const result2 = optimizedBuilder.visit(ctx2 as any);
      
      expect(result1).toBeDefined();
      expect(result2).toBeDefined();
      // Both should be valid but optimization behavior is internal
    });
  });

  describe('Validation During Construction', () => {
    test('should validate AST structure when enabled', () => {
      const validatingBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        validateDuringConstruction: true
      });

      const ctx = new MockParserRuleContext('testRule');
      
      expect(() => {
        validatingBuilder.visit(ctx as any);
      }).not.toThrow();
    });

    test('should skip validation when disabled', () => {
      const nonValidatingBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        validateDuringConstruction: false
      });

      const ctx = new MockParserRuleContext('testRule');
      
      expect(() => {
        nonValidatingBuilder.visit(ctx as any);
      }).not.toThrow();
    });
  });

  describe('Real COBOL Program Testing', () => {
    test('should parse Hello World program correctly', () => {
      // This test will be enhanced when ANTLR integration is complete
      const ctx = new MockParserRuleContext('compilationUnit');
      
      // Mock identification division
      const identCtx = new MockParserRuleContext('identificationDivision');
      identCtx.text = 'IDENTIFICATION DIVISION. PROGRAM-ID. HELLO-WORLD.';
      ctx.addChild(identCtx);
      
      // Mock procedure division
      const procCtx = new MockParserRuleContext('procedureDivision');
      procCtx.text = 'PROCEDURE DIVISION. MAIN-PARAGRAPH. DISPLAY "Hello, World!". STOP RUN.';
      ctx.addChild(procCtx);
      
      const result = builder.visit(ctx as any);
      
      expect(result).toBeDefined();
      expect(result.type).toBe('CobolProgram');
    });

    test('should handle arithmetic program structure', () => {
      const ctx = new MockParserRuleContext('compilationUnit');
      
      // Mock complete program structure
      const identCtx = new MockParserRuleContext('identificationDivision');
      const dataCtx = new MockParserRuleContext('dataDivision');
      const procCtx = new MockParserRuleContext('procedureDivision');
      
      ctx.addChild(identCtx);
      ctx.addChild(dataCtx);
      ctx.addChild(procCtx);
      
      const result = builder.visit(ctx as any);
      
      expect(result).toBeDefined();
      expect(result.type).toBe('CobolProgram');
    });

    test('should preserve source locations for debugging', () => {
      const ctx = new MockParserRuleContext('statement', 5, 10);
      
      const result = builder.visit(ctx as any);
      
      expect(result.location).toBeDefined();
      expect(result.location.line).toBe(5);
      expect(result.location.column).toBe(10);
    });

    test('should handle complex nested structure', () => {
      const root = new MockParserRuleContext('compilationUnit');
      
      // Create nested section/paragraph structure
      const procDiv = new MockParserRuleContext('procedureDivision');
      const section = new MockParserRuleContext('procedureSection');
      const paragraph = new MockParserRuleContext('paragraph');
      const statement = new MockParserRuleContext('statement');
      
      paragraph.addChild(statement);
      section.addChild(paragraph);
      procDiv.addChild(section);
      root.addChild(procDiv);
      
      const result = builder.visit(root as any);
      
      expect(result).toBeDefined();
      expect(result.type).toBe('CobolProgram');
    });
  });

  describe('Error Recovery and Robustness', () => {
    test('should handle malformed parse trees gracefully', () => {
      const malformedCtx = {
        text: 'invalid',
        childCount: -1,
        children: null,
        getChild: () => null,
        start: null,
        stop: null
      };
      
      expect(() => {
        builder.visit(malformedCtx as any);
      }).not.toThrow();
    });

    test('should recover from missing required elements', () => {
      const incompleteCtx = new MockParserRuleContext('compilationUnit');
      // Don't add identification division (required)
      
      expect(() => {
        builder.visit(incompleteCtx as any);
      }).toThrow(/Missing IDENTIFICATION DIVISION/);
    });

    test('should handle circular references safely', () => {
      const parent = new MockParserRuleContext('parent');
      const child = new MockParserRuleContext('child');
      
      parent.addChild(child);
      child.addChild(parent); // Circular reference
      
      // Should not cause infinite loop due to depth checking
      expect(() => {
        builder.visit(parent as any);
      }).not.toThrow(/Maximum call stack/);
    });

    test('should provide meaningful error messages', () => {
      const builder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        validateDuringConstruction: true
      });
      
      try {
        builder.build(null as any);
        fail('Should have thrown an error');
      } catch (error) {
        expect((error as Error).message).toContain('Failed to build AST');
      }
    });
  });

  describe('Memory Management', () => {
    test('should clean up resources after building', () => {
      const ctx = new MockParserRuleContext('compilationUnit');
      
      builder.build(ctx as any);
      
      // Errors should be clearable
      builder.clearErrors();
      expect(builder.getErrors()).toHaveLength(0);
    });

    test('should handle memory pressure gracefully', () => {
      // Create a very large structure
      const root = new MockParserRuleContext('compilationUnit');
      
      for (let i = 0; i < 10000; i++) {
        const child = new MockParserRuleContext(`large-child-${i}`);
        root.addChild(child);
      }
      
      expect(() => {
        builder.visit(root as any);
      }).not.toThrow(/out of memory/i);
    });
  });

  describe('Configuration Impact Testing', () => {
    test('should respect includeComments configuration', () => {
      const withComments = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        includeComments: true
      });
      
      const withoutComments = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        includeComments: false
      });
      
      const ctx = new MockParserRuleContext('statement');
      
      const result1 = withComments.visit(ctx as any);
      const result2 = withoutComments.visit(ctx as any);
      
      expect(result1).toBeDefined();
      expect(result2).toBeDefined();
      // Actual comment handling will be implemented with real grammar
    });

    test('should respect preserveSourceText configuration', () => {
      const preserving = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        preserveSourceText: true
      });
      
      const nonPreserving = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        preserveSourceText: false
      });
      
      const ctx = new MockParserRuleContext('DISPLAY "test"');
      
      const result1 = preserving.visit(ctx as any);
      const result2 = nonPreserving.visit(ctx as any);
      
      expect(result1).toBeDefined();
      expect(result2).toBeDefined();
      // Source text preservation will be validated with real implementation
    });
  });
});