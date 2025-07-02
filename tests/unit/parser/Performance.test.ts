/**
 * Performance tests for parser components
 * Tests memory usage, parsing speed, and stress scenarios
 */

import '../setup';
import { CobolParser, DEFAULT_COBOL_PARSER_CONFIG } from '../../../src/parser/cobol-parser';
import { CobolASTBuilder, DEFAULT_AST_BUILDER_CONFIG } from '../../../src/ast/builder';
import { CobolErrorHandler, ErrorRecoveryStrategy } from '../../../src/parser/error-handler';

describe('Parser Performance Tests', () => {
  const loadTestProgram = (category: string, filename: string): string => {
    const fs = require('fs');
    const path = require('path');
    return fs.readFileSync(path.join(__dirname, `../../data/valid/${category}`, filename), 'utf8');
  };

  // Helper to create large test programs
  const createLargeProgram = (size: 'small' | 'medium' | 'large' | 'huge'): string => {
    const sizeMap = {
      small: 100,
      medium: 1000,
      large: 5000,
      huge: 10000
    };
    
    const lineCount = sizeMap[size];
    const lines = [
      '       IDENTIFICATION DIVISION.',
      '       PROGRAM-ID. LARGE-PROGRAM.',
      '       DATA DIVISION.',
      '       WORKING-STORAGE SECTION.'
    ];
    
    // Add many variable declarations
    for (let i = 1; i <= lineCount; i++) {
      lines.push(`       01 VAR-${i.toString().padStart(5, '0')} PIC X(10) VALUE "TEST${i}".`);
    }
    
    lines.push('       PROCEDURE DIVISION.');
    lines.push('       MAIN-PARA.');
    
    // Add many statements
    for (let i = 1; i <= Math.min(lineCount / 10, 500); i++) {
      lines.push(`           DISPLAY VAR-${i.toString().padStart(5, '0')}.`);
    }
    
    lines.push('           STOP RUN.');
    
    return lines.join('\n');
  };

  describe('Parsing Speed Benchmarks', () => {
    let parser: CobolParser;

    beforeEach(() => {
      parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enableProfiling: true
      });
    });

    test('should parse small programs quickly', async () => {
      const source = loadTestProgram('basic', 'hello-world.cbl');
      
      await expect(async () => {
        const result = await parser.parse(source);
        return result;
      }).toParseWithin(50); // Should parse within 50ms
    });

    test('should parse medium programs efficiently', async () => {
      const source = loadTestProgram('intermediate', 'nested-performs.cbl');
      
      await expect(async () => {
        const result = await parser.parse(source);
        return result;
      }).toParseWithin(200); // Should parse within 200ms
    });

    test('should parse complex programs within limits', async () => {
      const source = loadTestProgram('advanced', 'inventory-system.cbl');
      
      await expect(async () => {
        const result = await parser.parse(source);
        return result;
      }).toParseWithin(1000); // Should parse within 1 second
    });

    test('should handle artificially large programs', async () => {
      const source = createLargeProgram('medium');
      
      await expect(async () => {
        const result = await parser.parse(source);
        return result;
      }).toParseWithin(2000); // Should parse within 2 seconds
    });

    test('should provide performance metrics when enabled', async () => {
      const source = loadTestProgram('basic', 'arithmetic.cbl');
      const result = await parser.parse(source);
      
      if (result.performance) {
        expect(result.performance.parseTime).toBeGreaterThan(0);
        expect(result.performance.memoryUsage).toBeGreaterThanOrEqual(0);
        expect(result.performance.nodeCount).toBeGreaterThanOrEqual(0);
        expect(result.performance.copybookCount).toBeGreaterThanOrEqual(0);
      }
    });
  });

  describe('Memory Usage Tests', () => {
    let parser: CobolParser;

    beforeEach(() => {
      parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
    });

    test('should use minimal memory for small programs', async () => {
      const source = loadTestProgram('basic', 'hello-world.cbl');
      
      await expect(async () => {
        const result = await parser.parse(source);
        return result;
      }).toUseMemoryUnder(5); // Should use less than 5MB
    });

    test('should scale memory usage reasonably', async () => {
      const smallSource = createLargeProgram('small');
      const mediumSource = createLargeProgram('medium');
      
      const measureMemory = async (source: string): Promise<number> => {
        const before = process.memoryUsage().heapUsed;
        await parser.parse(source);
        global.gc && global.gc();
        const after = process.memoryUsage().heapUsed;
        return (after - before) / (1024 * 1024); // MB
      };
      
      const smallMemory = await measureMemory(smallSource);
      const mediumMemory = await measureMemory(mediumSource);
      
      expect(smallMemory).toBeLessThan(50);
      expect(mediumMemory).toBeLessThan(200);
      // Medium should use more than small, but not disproportionately
      expect(mediumMemory / smallMemory).toBeLessThan(20);
    });

    test('should not leak memory across multiple parses', async () => {
      const source = loadTestProgram('basic', 'data-movement.cbl');
      
      const initialMemory = process.memoryUsage().heapUsed;
      
      // Parse the same program multiple times
      for (let i = 0; i < 10; i++) {
        await parser.parse(source);
      }
      
      global.gc && global.gc();
      const finalMemory = process.memoryUsage().heapUsed;
      const memoryGrowth = (finalMemory - initialMemory) / (1024 * 1024);
      
      // Should not grow by more than 10MB
      expect(memoryGrowth).toBeLessThan(10);
    });
  });

  describe('AST Builder Performance', () => {
    let builder: CobolASTBuilder;

    beforeEach(() => {
      builder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        enableOptimizations: true
      });
    });

    test('should build AST quickly for nested structures', () => {
      // Create mock deeply nested structure
      const createNestedMock = (depth: number): any => {
        const root = {
          text: 'root',
          childCount: 1,
          children: [],
          getChild: (index: number) => root.children[index],
          start: { line: 1, charPositionInLine: 0, startIndex: 0, text: 'root' },
          stop: { line: 1, charPositionInLine: 4, stopIndex: 4, text: 'root' }
        };
        
        let current = root;
        for (let i = 0; i < depth; i++) {
          const child = {
            text: `child-${i}`,
            childCount: i < depth - 1 ? 1 : 0,
            children: [],
            getChild: (index: number) => child.children[index],
            start: { line: i + 2, charPositionInLine: 0, startIndex: 0, text: `child-${i}` },
            stop: { line: i + 2, charPositionInLine: 7, stopIndex: 7, text: `child-${i}` }
          };
          
          (current.children as any[]).push(child);
          current = child;
        }
        
        return root;
      };
      
      const start = performance.now();
      const mockTree = createNestedMock(50);
      const result = builder.visit(mockTree as any);
      const duration = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(duration).toBeLessThan(100); // Should complete within 100ms
    });

    test('should handle wide trees efficiently', () => {
      // Create mock tree with many siblings
      const createWideMock = (width: number): any => {
        const root = {
          text: 'root',
          childCount: width,
          children: [],
          getChild: (index: number) => root.children[index],
          start: { line: 1, charPositionInLine: 0, startIndex: 0, text: 'root' },
          stop: { line: 1, charPositionInLine: 4, stopIndex: 4, text: 'root' }
        };
        
        for (let i = 0; i < width; i++) {
          const child = {
            text: `sibling-${i}`,
            childCount: 0,
            children: [],
            getChild: () => null,
            start: { line: 2, charPositionInLine: i * 10, startIndex: i * 10, text: `sibling-${i}` },
            stop: { line: 2, charPositionInLine: i * 10 + 9, stopIndex: i * 10 + 9, text: `sibling-${i}` }
          };
          
          (root.children as any[]).push(child);
        }
        
        return root;
      };
      
      const start = performance.now();
      const mockTree = createWideMock(1000);
      const result = builder.visit(mockTree as any);
      const duration = performance.now() - start;
      
      expect(result).toBeDefined();
      expect(duration).toBeLessThan(200); // Should complete within 200ms
    });

    test('should use object pooling for optimizations', () => {
      const optimizedBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        enableOptimizations: true
      });
      
      const standardBuilder = new CobolASTBuilder({
        ...DEFAULT_AST_BUILDER_CONFIG,
        enableOptimizations: false
      });
      
      const mockTree = {
        text: 'test',
        childCount: 0,
        children: [],
        getChild: () => null,
        start: { line: 1, charPositionInLine: 0, startIndex: 0, text: 'test' },
        stop: { line: 1, charPositionInLine: 4, stopIndex: 4, text: 'test' }
      };
      
      // Measure time for multiple builds
      const measureTime = (builder: CobolASTBuilder, iterations: number): number => {
        const start = performance.now();
        for (let i = 0; i < iterations; i++) {
          builder.visit(mockTree as any);
        }
        return performance.now() - start;
      };
      
      const optimizedTime = measureTime(optimizedBuilder, 100);
      const standardTime = measureTime(standardBuilder, 100);
      
      expect(optimizedTime).toBeLessThan(100);
      expect(standardTime).toBeLessThan(200);
      // Optimized should be at least as fast as standard
      expect(optimizedTime).toBeLessThanOrEqual(standardTime * 1.1);
    });
  });

  describe('Error Handler Performance', () => {
    test('should handle many errors efficiently', () => {
      const errorHandler = new CobolErrorHandler(10000, ErrorRecoveryStrategy.COLLECT_ALL);
      
      const start = performance.now();
      
      // Add many errors
      for (let i = 0; i < 1000; i++) {
        errorHandler.addSyntaxError(`Error ${i}`, undefined, i, i);
      }
      
      const duration = performance.now() - start;
      
      expect(errorHandler.getErrors()).toHaveLength(1000);
      expect(duration).toBeLessThan(100); // Should handle 1000 errors within 100ms
    });

    test('should generate reports quickly', () => {
      const errorHandler = new CobolErrorHandler(1000, ErrorRecoveryStrategy.COLLECT_ALL);
      
      // Add various types of errors
      for (let i = 0; i < 100; i++) {
        const location = { line: i, column: 1, endLine: i, endColumn: 10 };
        errorHandler.addSyntaxError(`Syntax error ${i}`, undefined, i, 1);
        errorHandler.addSemanticError(`Semantic error ${i}`, `SEM_${i}`, location);
        errorHandler.addAnalysisWarning(`Analysis warning ${i}`, 'analysis', `WARN_${i}`, location);
      }
      
      const start = performance.now();
      const report = errorHandler.generateReport();
      const diagnostics = errorHandler.getAllDiagnostics();
      const duration = performance.now() - start;
      
      expect(report).toContain('Errors');
      expect(report).toContain('Warnings');
      expect(diagnostics).toHaveLength(300);
      expect(duration).toBeLessThan(50); // Should generate report within 50ms
    });
  });

  describe('Stress Testing', () => {
    test('should handle concurrent parsing requests', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = loadTestProgram('basic', 'arithmetic.cbl');
      
      const promises = [];
      for (let i = 0; i < 20; i++) {
        promises.push(parser.parse(source, `concurrent-${i}.cbl`));
      }
      
      const start = performance.now();
      const results = await Promise.all(promises);
      const duration = performance.now() - start;
      
      expect(results).toHaveLength(20);
      expect(duration).toBeLessThan(5000); // All 20 should complete within 5 seconds
      
      results.forEach((result, index) => {
        expect(result).toBeDefined();
        expect(result.sourceInfo.originalLength).toBe(source.length);
      });
    });

    test('should handle repeated parsing without degradation', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = loadTestProgram('intermediate', 'conditional-logic.cbl');
      
      const times: number[] = [];
      
      for (let i = 0; i < 10; i++) {
        const start = performance.now();
        await parser.parse(source);
        const duration = performance.now() - start;
        times.push(duration);
      }
      
      // Performance should not degrade significantly
      const firstHalf = times.slice(0, 5);
      const secondHalf = times.slice(5, 10);
      
      const firstAvg = firstHalf.reduce((a, b) => a + b) / firstHalf.length;
      const secondAvg = secondHalf.reduce((a, b) => a + b) / secondHalf.length;
      
      // Second half should not be more than 50% slower than first half
      expect(secondAvg).toBeLessThan(firstAvg * 1.5);
    });

    test('should handle edge cases without performance issues', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      
      const edgeCases = [
        '', // Empty
        '      *> Just a comment',
        'A'.repeat(10000), // Very long line
        'IDENTIFICATION DIVISION.\n'.repeat(1000), // Many repeated lines
        Array(1000).fill('01 VAR PIC X.').join('\n') // Many variable declarations
      ];
      
      for (const source of edgeCases) {
        const start = performance.now();
        const result = await parser.parse(source);
        const duration = performance.now() - start;
        
        expect(result).toBeDefined();
        expect(duration).toBeLessThan(1000); // Each edge case within 1 second
      }
    });
  });

  describe('Memory Stress Testing', () => {
    test('should handle very large programs without excessive memory use', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const hugeSource = createLargeProgram('huge');
      
      const initialMemory = process.memoryUsage().heapUsed;
      
      await expect(async () => {
        const result = await parser.parse(hugeSource);
        return result;
      }).toUseMemoryUnder(500); // Should use less than 500MB
      
      global.gc && global.gc();
      const finalMemory = process.memoryUsage().heapUsed;
      const memoryGrowth = (finalMemory - initialMemory) / (1024 * 1024);
      
      // Should release most memory after parsing
      expect(memoryGrowth).toBeLessThan(100);
    });

    test('should not accumulate memory across many small parses', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = loadTestProgram('basic', 'hello-world.cbl');
      
      const initialMemory = process.memoryUsage().heapUsed;
      
      // Parse many times
      for (let i = 0; i < 100; i++) {
        await parser.parse(source);
        
        // Occasionally force garbage collection
        if (i % 10 === 0) {
          global.gc && global.gc();
        }
      }
      
      global.gc && global.gc();
      const finalMemory = process.memoryUsage().heapUsed;
      const memoryGrowth = (finalMemory - initialMemory) / (1024 * 1024);
      
      // Should not accumulate significant memory
      expect(memoryGrowth).toBeLessThan(50);
    });
  });
});