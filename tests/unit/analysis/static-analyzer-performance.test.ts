/**
 * Performance tests for Static Analyzer - Phase 4
 * Tests performance characteristics and scalability of static analysis
 */

import '../setup';
import { StaticAnalyzer, DEFAULT_STATIC_ANALYSIS_CONFIG } from '../../../src/analysis/static-analyzer';
import { CobolProgram } from '../../../src/ast/nodes/CobolProgram';
import { IdentificationDivision } from '../../../src/ast/nodes/IdentificationDivision';
import { ProcedureDivision } from '../../../src/ast/nodes/ProcedureDivision';
import { DataDivision } from '../../../src/ast/nodes/DataDivision';
import { ParagraphNode } from '../../../src/ast/nodes/ParagraphNode';
import { SectionNode } from '../../../src/ast/nodes/SectionNode';
import { StatementNode } from '../../../src/ast/nodes/StatementNode';
import { SourceLocation, VariableDefinition } from '../../../src/core/types';

/**
 * Performance test utilities
 */
class PerformanceTestUtils {
  static measureExecutionTime<T>(fn: () => T): { result: T; executionTime: number } {
    const startTime = performance.now();
    const result = fn();
    const endTime = performance.now();
    return { result, executionTime: endTime - startTime };
  }

  static createLargeProgram(numParagraphs: number, numVariables: number): CobolProgram {
    const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
    const program = new CobolProgram(
      `LARGE-PROGRAM-${numParagraphs}-${numVariables}`,
      new IdentificationDivision(`LARGE-PROGRAM-${numParagraphs}-${numVariables}`, location),
      location
    );
    
    program.procedureDivision = new ProcedureDivision(location);
    program.dataDivision = new DataDivision(location);

    // Create variables
    const variables: VariableDefinition[] = [];
    for (let i = 0; i < numVariables; i++) {
      variables.push({
        name: `VAR-${i}`,
        level: 1,
        location: location,
        children: [],
        dataType: { picture: 'X(10)' }
      });
    }
    program.dataDivision.workingStorage = variables;

    // Create paragraphs with statements
    const paragraphs: ParagraphNode[] = [];
    for (let i = 0; i < numParagraphs; i++) {
      const paraName = `PARA-${i}`;
      const paragraph = new ParagraphNode(paraName, `${paraName}.`, location, location);
      
      // Add statements to paragraph
      const statements: StatementNode[] = [];
      
      // Add PERFORM statement to next paragraph (creates call chain)
      if (i < numParagraphs - 1) {
        const performStmt = new StatementNode('PERFORM', `PERFORM PARA-${i + 1}`, location);
        performStmt.target = `PARA-${i + 1}`;
        statements.push(performStmt);
      }
      
      // Add variable usage statements
      const varIndex = i % numVariables;
      const moveStmt = new StatementNode('MOVE', `MOVE "VALUE${i}" TO VAR-${varIndex}`, location);
      moveStmt.operands = [`"VALUE${i}"`, `VAR-${varIndex}`];
      statements.push(moveStmt);
      
      const displayStmt = new StatementNode('DISPLAY', `DISPLAY VAR-${varIndex}`, location);
      displayStmt.operands = [`VAR-${varIndex}`];
      statements.push(displayStmt);
      
      (paragraph as any).statements = statements;
      paragraphs.push(paragraph);
    }
    
    program.procedureDivision.paragraphs = paragraphs;
    return program;
  }

  static createComplexCallGraph(depth: number, breadth: number): CobolProgram {
    const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
    const program = new CobolProgram(
      `COMPLEX-CALL-GRAPH-${depth}-${breadth}`,
      new IdentificationDivision(`COMPLEX-CALL-GRAPH-${depth}-${breadth}`, location),
      location
    );
    
    program.procedureDivision = new ProcedureDivision(location);
    program.dataDivision = new DataDivision(location);

    const paragraphs: ParagraphNode[] = [];
    
    // Create complex call graph with depth and breadth
    for (let level = 0; level < depth; level++) {
      for (let node = 0; node < breadth; node++) {
        const paraName = `LEVEL-${level}-NODE-${node}`;
        const paragraph = new ParagraphNode(paraName, `${paraName}.`, location, location);
        const statements: StatementNode[] = [];
        
        // Each node calls multiple nodes at the next level
        if (level < depth - 1) {
          for (let nextNode = 0; nextNode < breadth; nextNode++) {
            const targetName = `LEVEL-${level + 1}-NODE-${nextNode}`;
            const performStmt = new StatementNode('PERFORM', `PERFORM ${targetName}`, location);
            performStmt.target = targetName;
            statements.push(performStmt);
          }
        }
        
        (paragraph as any).statements = statements;
        paragraphs.push(paragraph);
      }
    }
    
    program.procedureDivision.paragraphs = paragraphs;
    return program;
  }

  static createCircularProgram(chainLength: number): CobolProgram {
    const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
    const program = new CobolProgram(
      `CIRCULAR-PROGRAM-${chainLength}`,
      new IdentificationDivision(`CIRCULAR-PROGRAM-${chainLength}`, location),
      location
    );
    
    program.procedureDivision = new ProcedureDivision(location);
    program.dataDivision = new DataDivision(location);

    const paragraphs: ParagraphNode[] = [];
    
    // Create circular chain
    for (let i = 0; i < chainLength; i++) {
      const paraName = `CHAIN-${i}`;
      const paragraph = new ParagraphNode(paraName, `${paraName}.`, location, location);
      
      // Each paragraph calls the next one, last one calls first
      const nextIndex = (i + 1) % chainLength;
      const targetName = `CHAIN-${nextIndex}`;
      const performStmt = new StatementNode('PERFORM', `PERFORM ${targetName}`, location);
      performStmt.target = targetName;
      
      (paragraph as any).statements = [performStmt];
      paragraphs.push(paragraph);
    }
    
    program.procedureDivision.paragraphs = paragraphs;
    return program;
  }
}

describe('StaticAnalyzer - Performance Tests', () => {
  let analyzer: StaticAnalyzer;

  beforeEach(() => {
    analyzer = new StaticAnalyzer();
  });

  describe('Scalability Tests', () => {
    test('should handle small programs efficiently', () => {
      const program = PerformanceTestUtils.createLargeProgram(10, 10);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(10);
      expect(result.variablesAnalyzed).toBe(10);
      expect(executionTime).toBeLessThan(100); // Should complete in < 100ms
    });

    test('should handle medium programs efficiently', () => {
      const program = PerformanceTestUtils.createLargeProgram(50, 50);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(50);
      expect(result.variablesAnalyzed).toBe(50);
      expect(executionTime).toBeLessThan(500); // Should complete in < 500ms
    });

    test('should handle large programs within reasonable time', () => {
      const program = PerformanceTestUtils.createLargeProgram(100, 100);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(100);
      expect(result.variablesAnalyzed).toBe(100);
      expect(executionTime).toBeLessThan(2000); // Should complete in < 2 seconds
    });

    test('should handle very large programs', () => {
      const program = PerformanceTestUtils.createLargeProgram(200, 200);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(200);
      expect(result.variablesAnalyzed).toBe(200);
      expect(executionTime).toBeLessThan(5000); // Should complete in < 5 seconds
    });
  });

  describe('Complex Call Graph Performance', () => {
    test('should handle shallow wide call graphs', () => {
      const program = PerformanceTestUtils.createComplexCallGraph(3, 20);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(60); // 3 levels * 20 nodes
      expect(executionTime).toBeLessThan(1000); // Should complete in < 1 second
    });

    test('should handle deep narrow call graphs', () => {
      const program = PerformanceTestUtils.createComplexCallGraph(20, 3);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(60); // 20 levels * 3 nodes
      expect(executionTime).toBeLessThan(1000); // Should complete in < 1 second
    });

    test('should handle balanced call graphs', () => {
      const program = PerformanceTestUtils.createComplexCallGraph(10, 10);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getSummary();
      });

      expect(result.callGraphNodes).toBe(100); // 10 levels * 10 nodes
      expect(executionTime).toBeLessThan(2000); // Should complete in < 2 seconds
    });
  });

  describe('Circular Dependency Detection Performance', () => {
    test('should detect small circular dependencies quickly', () => {
      const program = PerformanceTestUtils.createCircularProgram(5);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getWarnings();
      });

      const circularWarnings = result.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
      expect(executionTime).toBeLessThan(100); // Should complete in < 100ms
    });

    test('should detect medium circular dependencies efficiently', () => {
      const program = PerformanceTestUtils.createCircularProgram(20);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getWarnings();
      });

      const circularWarnings = result.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
      expect(executionTime).toBeLessThan(500); // Should complete in < 500ms
    });

    test('should handle large circular dependencies', () => {
      const program = PerformanceTestUtils.createCircularProgram(50);
      
      const { result, executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
        return analyzer.getWarnings();
      });

      const circularWarnings = result.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
      expect(executionTime).toBeLessThan(2000); // Should complete in < 2 seconds
    });
  });

  describe('Memory Usage Tests', () => {
    test('should manage memory efficiently for large programs', () => {
      const program = PerformanceTestUtils.createLargeProgram(100, 100);
      
      // Measure memory usage before analysis
      const initialMemory = process.memoryUsage();
      
      analyzer.analyze(program);
      
      // Measure memory usage after analysis
      const finalMemory = process.memoryUsage();
      
      // Memory increase should be reasonable (less than 50MB)
      const memoryIncrease = finalMemory.heapUsed - initialMemory.heapUsed;
      expect(memoryIncrease).toBeLessThan(50 * 1024 * 1024); // 50MB
    });

    test('should clean up memory after analysis', () => {
      const program = PerformanceTestUtils.createLargeProgram(50, 50);
      
      // Measure memory before analysis
      const initialMemory = process.memoryUsage();
      
      analyzer.analyze(program);
      analyzer.clear();
      
      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }
      
      // Measure memory after cleanup
      const finalMemory = process.memoryUsage();
      
      // Memory should not increase significantly after cleanup
      const memoryIncrease = finalMemory.heapUsed - initialMemory.heapUsed;
      expect(memoryIncrease).toBeLessThan(10 * 1024 * 1024); // 10MB tolerance
    });
  });

  describe('Repeated Analysis Performance', () => {
    test('should handle repeated analysis without performance degradation', () => {
      const program = PerformanceTestUtils.createLargeProgram(30, 30);
      const executionTimes: number[] = [];
      
      // Run analysis multiple times
      for (let i = 0; i < 5; i++) {
        const { executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
          analyzer.analyze(program);
        });
        executionTimes.push(executionTime);
        analyzer.clear();
      }
      
      // Performance should not degrade significantly
      const firstTime = executionTimes[0];
      const lastTime = executionTimes[executionTimes.length - 1];
      
      expect(lastTime).toBeLessThan(firstTime * 2); // Should not be more than 2x slower
    });

    test('should handle multiple different programs efficiently', () => {
      const programs = [
        PerformanceTestUtils.createLargeProgram(20, 20),
        PerformanceTestUtils.createComplexCallGraph(5, 8),
        PerformanceTestUtils.createCircularProgram(10)
      ];
      
      const totalStartTime = performance.now();
      
      for (const program of programs) {
        analyzer.analyze(program);
        analyzer.clear();
      }
      
      const totalEndTime = performance.now();
      const totalTime = totalEndTime - totalStartTime;
      
      expect(totalTime).toBeLessThan(3000); // Should complete all in < 3 seconds
    });
  });

  describe('Configuration Impact on Performance', () => {
    test('should run faster with fewer analysis features enabled', () => {
      const program = PerformanceTestUtils.createLargeProgram(50, 50);
      
      // Full analysis configuration
      const fullAnalyzer = new StaticAnalyzer(DEFAULT_STATIC_ANALYSIS_CONFIG);
      
      // Minimal analysis configuration
      const minimalConfig = {
        ...DEFAULT_STATIC_ANALYSIS_CONFIG,
        detectDeadCode: false,
        detectCircularDependencies: false,
        detectInfiniteLoops: false,
        analyzeVariableUsage: false,
        calculateCodeQualityMetrics: false,
        analyzePerformanceBottlenecks: false,
        detectSecurityVulnerabilities: false,
        performControlFlowAnalysis: false,
        performDataFlowAnalysis: false,
        analyzeMaintainability: false,
        detectCodeSmells: false,
        analyzeComplexityMetrics: false
      };
      const minimalAnalyzer = new StaticAnalyzer(minimalConfig);
      
      // Run multiple iterations to get more stable measurements
      const iterations = 3;
      let fullTotalTime = 0;
      let minimalTotalTime = 0;
      
      for (let i = 0; i < iterations; i++) {
        const { executionTime: fullTime } = PerformanceTestUtils.measureExecutionTime(() => {
          fullAnalyzer.analyze(program);
        });
        fullTotalTime += fullTime;
        
        const { executionTime: minimalTime } = PerformanceTestUtils.measureExecutionTime(() => {
          minimalAnalyzer.analyze(program);
        });
        minimalTotalTime += minimalTime;
      }
      
      const avgFullTime = fullTotalTime / iterations;
      const avgMinimalTime = minimalTotalTime / iterations;
      
      // Minimal should be faster, but allow for 10% tolerance due to measurement variability
      expect(avgMinimalTime).toBeLessThan(avgFullTime * 1.1);
    });
  });

  describe('Error Handling Performance', () => {
    test('should handle malformed programs without performance issues', () => {
      const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
      const program = new CobolProgram(
        'MALFORMED-PROGRAM',
        new IdentificationDivision('MALFORMED-PROGRAM', location),
        location
      );
      
      // Create program with potential issues
      program.procedureDivision = new ProcedureDivision(location);
      program.dataDivision = new DataDivision(location);
      
      // Add paragraphs with malformed statements
      const paragraphs: ParagraphNode[] = [];
      for (let i = 0; i < 20; i++) {
        const paragraph = new ParagraphNode(`PARA-${i}`, `PARA-${i}.`, location, location);
        const statements: StatementNode[] = [
          new StatementNode('DISPLAY', 'DISPLAY "MALFORMED"', location),
          new StatementNode('DISPLAY', 'DISPLAY "Test"', location),
          new StatementNode('PERFORM', 'PERFORM NONEXISTENT', location)
        ];
        (paragraph as any).statements = statements;
        paragraphs.push(paragraph);
      }
      
      program.procedureDivision.paragraphs = paragraphs;
      
      const { executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
        analyzer.analyze(program);
      });
      
      expect(executionTime).toBeLessThan(1000); // Should handle errors efficiently
    });
  });

  describe('Benchmark Tests', () => {
    test('should meet performance benchmarks for typical program sizes', () => {
      const benchmarks = [
        { paragraphs: 10, variables: 10, maxTime: 50 },
        { paragraphs: 25, variables: 25, maxTime: 150 },
        { paragraphs: 50, variables: 50, maxTime: 300 },
        { paragraphs: 100, variables: 100, maxTime: 1000 }
      ];
      
      for (const benchmark of benchmarks) {
        const program = PerformanceTestUtils.createLargeProgram(
          benchmark.paragraphs,
          benchmark.variables
        );
        
        const { executionTime } = PerformanceTestUtils.measureExecutionTime(() => {
          analyzer.analyze(program);
        });
        
        expect(executionTime).toBeLessThan(benchmark.maxTime);
      }
    });
  });
});