/**
 * Comprehensive edge case tests for Static Analyzer - Phase 4
 * Tests complex scenarios, boundary conditions, and error handling
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
 * Helper function to create a test program
 */
function createTestProgram(name: string): CobolProgram {
  const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
  const identificationDivision = new IdentificationDivision(name, location);
  const program = new CobolProgram(name, identificationDivision, location);
  program.procedureDivision = new ProcedureDivision(location);
  program.dataDivision = new DataDivision(location);
  return program;
}

/**
 * Helper function to create a paragraph with statements
 */
function createParagraphWithStatements(name: string, statements: StatementNode[]): ParagraphNode {
  const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
  const paragraph = new ParagraphNode(name, `${name}.`, location, location);
  (paragraph as any).statements = statements;
  return paragraph;
}

/**
 * Helper function to create a section with paragraphs
 */
function createSectionWithParagraphs(name: string, paragraphs: ParagraphNode[]): SectionNode {
  const location: SourceLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
  const section = new SectionNode(name, `${name} SECTION.`, location, location);
  section.paragraphs = paragraphs;
  return section;
}

describe('StaticAnalyzer - Edge Cases', () => {
  let analyzer: StaticAnalyzer;
  let mockLocation: SourceLocation;

  beforeEach(() => {
    analyzer = new StaticAnalyzer();
    mockLocation = { line: 1, column: 1, endLine: 1, endColumn: 10 };
  });

  describe('Complex Dead Code Detection', () => {
    test('should detect dead code in complex call hierarchies', () => {
      const program = createTestProgram('COMPLEX-DEAD-CODE');

      // Create complex hierarchy: A -> B -> C, D is unreachable
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-B';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('PERFORM', 'PERFORM PARA-C', mockLocation)
      ]);
      paraB.statements[0].target = 'PARA-C';

      const paraC = createParagraphWithStatements('PARA-C', [
        new StatementNode('DISPLAY', 'DISPLAY "Test"', mockLocation)
      ]);

      const paraD = createParagraphWithStatements('PARA-D', [
        new StatementNode('DISPLAY', 'DISPLAY "Unreachable"', mockLocation)
      ]);

      program.procedureDivision!.paragraphs = [paraA, paraB, paraC, paraD];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const unreachableWarnings = warnings.filter(w => w.code === 'UNREACHABLE_CODE');
      expect(unreachableWarnings).toHaveLength(1);
      expect(unreachableWarnings[0].message).toContain('PARA-D');
    });

    test('should handle conditional reachability', () => {
      const program = createTestProgram('CONDITIONAL-REACHABILITY');

      // Create conditional paths: A -> B (if condition) -> C, D (else) -> E
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('IF', 'IF CONDITION-VAR = "Y"', mockLocation),
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation),
        new StatementNode('ELSE', 'ELSE', mockLocation),
        new StatementNode('PERFORM', 'PERFORM PARA-D', mockLocation)
      ]);
      paraA.statements[1].target = 'PARA-B';
      paraA.statements[3].target = 'PARA-D';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('PERFORM', 'PERFORM PARA-C', mockLocation)
      ]);
      paraB.statements[0].target = 'PARA-C';

      const paraC = createParagraphWithStatements('PARA-C', [
        new StatementNode('DISPLAY', 'DISPLAY "Path C"', mockLocation)
      ]);

      const paraD = createParagraphWithStatements('PARA-D', [
        new StatementNode('PERFORM', 'PERFORM PARA-E', mockLocation)
      ]);
      paraD.statements[0].target = 'PARA-E';

      const paraE = createParagraphWithStatements('PARA-E', [
        new StatementNode('DISPLAY', 'DISPLAY "Path E"', mockLocation)
      ]);

      program.procedureDivision!.paragraphs = [paraA, paraB, paraC, paraD, paraE];

      analyzer.analyze(program);
      const callGraph = analyzer.getCallGraph();

      // All paragraphs should be reachable in this case
      expect(callGraph.get('PARA-A')?.reachable).toBe(true);
      expect(callGraph.get('PARA-B')?.reachable).toBe(true);
      expect(callGraph.get('PARA-C')?.reachable).toBe(true);
      expect(callGraph.get('PARA-D')?.reachable).toBe(true);
      expect(callGraph.get('PARA-E')?.reachable).toBe(true);
    });

    test('should handle sections with mixed reachability', () => {
      const program = createTestProgram('MIXED-SECTION-REACHABILITY');

      // Create sections with mixed reachability
      const reachablePara = createParagraphWithStatements('REACHABLE-PARA', [
        new StatementNode('DISPLAY', 'DISPLAY "Reachable"', mockLocation)
      ]);

      const unreachablePara = createParagraphWithStatements('UNREACHABLE-PARA', [
        new StatementNode('DISPLAY', 'DISPLAY "Unreachable"', mockLocation)
      ]);

      const mainSection = createSectionWithParagraphs('MAIN-SECTION', [reachablePara]);
      const deadSection = createSectionWithParagraphs('DEAD-SECTION', [unreachablePara]);

      program.procedureDivision!.sections = [mainSection, deadSection];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const unreachableWarnings = warnings.filter(w => w.code === 'UNREACHABLE_CODE');
      expect(unreachableWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('Complex Variable Usage Patterns', () => {
    test('should detect complex variable usage in nested structures', () => {
      const program = createTestProgram('COMPLEX-VARIABLE-USAGE');

      // Create complex variable hierarchy
      const parentVar: VariableDefinition = {
        name: 'PARENT-STRUCT',
        level: 1,
        location: mockLocation,
        children: ['CHILD-VAR1', 'CHILD-VAR2']
      };

      const unusedVar: VariableDefinition = {
        name: 'UNUSED-STRUCT',
        level: 1,
        location: mockLocation,
        children: ['UNUSED-CHILD']
      };

      program.dataDivision!.workingStorage = [parentVar, unusedVar];

      // Use only one child variable
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('MOVE', 'MOVE "VALUE" TO CHILD-VAR1', mockLocation)
      ]);
      mainPara.statements[0].operands = ['"VALUE"', 'CHILD-VAR1'];

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const unusedWarnings = warnings.filter(w => w.code === 'UNUSED_VARIABLE');
      expect(unusedWarnings.length).toBeGreaterThan(0);
    });

    test('should handle variable aliases and redefinitions', () => {
      const program = createTestProgram('VARIABLE-ALIASES');

      // Create variables with REDEFINES
      const baseVar: VariableDefinition = {
        name: 'BASE-VAR',
        level: 1,
        location: mockLocation,
        children: [],
        dataType: { picture: 'X(10)' }
      };

      const redefinedVar: VariableDefinition = {
        name: 'REDEFINED-VAR',
        level: 1,
        location: mockLocation,
        children: [],
        dataType: { picture: '9(10)' }
      };

      program.dataDivision!.workingStorage = [baseVar, redefinedVar];

      // Use both variables
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('MOVE', 'MOVE "HELLO" TO BASE-VAR', mockLocation),
        new StatementNode('DISPLAY', 'DISPLAY REDEFINED-VAR', mockLocation)
      ]);
      mainPara.statements[0].operands = ['"HELLO"', 'BASE-VAR'];
      mainPara.statements[1].operands = ['REDEFINED-VAR'];

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('BASE-VAR')?.isWritten).toBe(true);
      expect(variableUsage.get('REDEFINED-VAR')?.isRead).toBe(true);
    });

    test('should detect write-only variables with multiple writes', () => {
      const program = createTestProgram('WRITE-ONLY-VARS');

      const writeOnlyVar: VariableDefinition = {
        name: 'WRITE-ONLY-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      program.dataDivision!.workingStorage = [writeOnlyVar];

      // Write to variable multiple times but never read
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('MOVE', 'MOVE "VALUE1" TO WRITE-ONLY-VAR', mockLocation),
        new StatementNode('MOVE', 'MOVE "VALUE2" TO WRITE-ONLY-VAR', mockLocation),
        new StatementNode('COMPUTE', 'COMPUTE WRITE-ONLY-VAR = 100', mockLocation)
      ]);
      mainPara.statements[0].operands = ['"VALUE1"', 'WRITE-ONLY-VAR'];
      mainPara.statements[1].operands = ['"VALUE2"', 'WRITE-ONLY-VAR'];
      mainPara.statements[2].sourceText = 'COMPUTE WRITE-ONLY-VAR = 100';

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const writeOnlyWarnings = warnings.filter(w => w.code === 'WRITE_ONLY_VARIABLE');
      expect(writeOnlyWarnings).toHaveLength(1);
      expect(writeOnlyWarnings[0].message).toContain('WRITE-ONLY-VAR');
    });

    test('should handle array variable usage', () => {
      const program = createTestProgram('ARRAY-VARIABLES');

      const arrayVar: VariableDefinition = {
        name: 'ARRAY-VAR',
        level: 1,
        location: mockLocation,
        children: [],
        dataType: { picture: 'X(10)' }
      };

      program.dataDivision!.workingStorage = [arrayVar];

      // Use array with subscripts
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('MOVE', 'MOVE "VALUE" TO ARRAY-VAR(1)', mockLocation),
        new StatementNode('DISPLAY', 'DISPLAY ARRAY-VAR(2)', mockLocation)
      ]);
      mainPara.statements[0].operands = ['"VALUE"', 'ARRAY-VAR(1)'];
      mainPara.statements[1].operands = ['ARRAY-VAR(2)'];

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('ARRAY-VAR')?.isWritten).toBe(true);
      expect(variableUsage.get('ARRAY-VAR')?.isRead).toBe(true);
    });
  });

  describe('Advanced Circular Dependency Detection', () => {
    test('should detect deep circular dependencies', () => {
      const program = createTestProgram('DEEP-CIRCULAR');

      // Create deep circular dependency: A -> B -> C -> D -> A
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-B';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('PERFORM', 'PERFORM PARA-C', mockLocation)
      ]);
      paraB.statements[0].target = 'PARA-C';

      const paraC = createParagraphWithStatements('PARA-C', [
        new StatementNode('PERFORM', 'PERFORM PARA-D', mockLocation)
      ]);
      paraC.statements[0].target = 'PARA-D';

      const paraD = createParagraphWithStatements('PARA-D', [
        new StatementNode('PERFORM', 'PERFORM PARA-A', mockLocation)
      ]);
      paraD.statements[0].target = 'PARA-A';

      program.procedureDivision!.paragraphs = [paraA, paraB, paraC, paraD];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const circularWarnings = warnings.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
    });

    test('should detect multiple independent circular dependencies', () => {
      const program = createTestProgram('MULTIPLE-CIRCULAR');

      // Create two independent circular dependencies
      // Circle 1: A -> B -> A
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-B';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('PERFORM', 'PERFORM PARA-A', mockLocation)
      ]);
      paraB.statements[0].target = 'PARA-A';

      // Circle 2: C -> D -> C
      const paraC = createParagraphWithStatements('PARA-C', [
        new StatementNode('PERFORM', 'PERFORM PARA-D', mockLocation)
      ]);
      paraC.statements[0].target = 'PARA-D';

      const paraD = createParagraphWithStatements('PARA-D', [
        new StatementNode('PERFORM', 'PERFORM PARA-C', mockLocation)
      ]);
      paraD.statements[0].target = 'PARA-C';

      program.procedureDivision!.paragraphs = [paraA, paraB, paraC, paraD];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const circularWarnings = warnings.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThanOrEqual(2);
    });

    test('should handle complex dependency graphs with mixed calls', () => {
      const program = createTestProgram('MIXED-CALLS');

      // Create complex graph with PERFORM, GO TO, and CALL
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation),
        new StatementNode('GO', 'GO TO PARA-C', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-B';
      paraA.statements[1].target = 'PARA-C';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('CALL', 'CALL "EXTERNAL-PROG"', mockLocation),
        new StatementNode('GO', 'GO TO PARA-A', mockLocation)
      ]);
      paraB.statements[0].target = 'EXTERNAL-PROG';
      paraB.statements[1].target = 'PARA-A';

      const paraC = createParagraphWithStatements('PARA-C', [
        new StatementNode('PERFORM', 'PERFORM PARA-D', mockLocation)
      ]);
      paraC.statements[0].target = 'PARA-D';

      const paraD = createParagraphWithStatements('PARA-D', [
        new StatementNode('DISPLAY', 'DISPLAY "End"', mockLocation)
      ]);

      program.procedureDivision!.paragraphs = [paraA, paraB, paraC, paraD];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const circularWarnings = warnings.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('Complex GO TO Target Validation', () => {
    test('should handle GO TO with computed targets', () => {
      const program = createTestProgram('COMPUTED-GOTO');

      // Create GO TO DEPENDING ON scenario
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('GO', 'GO TO PARA-A PARA-B PARA-C DEPENDING ON CHOICE-VAR', mockLocation)
      ]);
      mainPara.statements[0].sourceText = 'GO TO PARA-A PARA-B PARA-C DEPENDING ON CHOICE-VAR';

      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('DISPLAY', 'DISPLAY "Choice A"', mockLocation)
      ]);

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('DISPLAY', 'DISPLAY "Choice B"', mockLocation)
      ]);

      // Missing PARA-C to trigger missing target warning
      program.procedureDivision!.paragraphs = [mainPara, paraA, paraB];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const missingTargetWarnings = warnings.filter(w => w.code === 'MISSING_GOTO_TARGET');
      expect(missingTargetWarnings.length).toBeGreaterThan(0);
    });

    test('should handle GO TO with section and paragraph targets', () => {
      const program = createTestProgram('SECTION-GOTO');

      // Create GO TO that targets sections
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('GO', 'GO TO PROCESS-SECTION', mockLocation)
      ]);
      mainPara.statements[0].target = 'PROCESS-SECTION';

      const processPara = createParagraphWithStatements('PROCESS-PARA', [
        new StatementNode('DISPLAY', 'DISPLAY "Processing"', mockLocation)
      ]);

      const processSection = createSectionWithParagraphs('PROCESS-SECTION', [processPara]);

      program.procedureDivision!.paragraphs = [mainPara];
      program.procedureDivision!.sections = [processSection];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const missingTargetWarnings = warnings.filter(w => w.code === 'MISSING_GOTO_TARGET');
      expect(missingTargetWarnings).toHaveLength(0);
    });

    test('should handle malformed GO TO statements', () => {
      const program = createTestProgram('MALFORMED-GOTO');

      // Create malformed GO TO statements
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('GO', 'GO TO', mockLocation), // Missing target
        new StatementNode('GO', 'GO TO 123-INVALID', mockLocation) // Invalid target name
      ]);
      mainPara.statements[0].sourceText = 'GO TO';
      mainPara.statements[1].sourceText = 'GO TO 123-INVALID';

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      const missingTargetWarnings = warnings.filter(w => w.code === 'MISSING_GOTO_TARGET');
      expect(missingTargetWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('Infinite Loop Detection Edge Cases', () => {
    test('should detect indirect infinite loops', () => {
      const program = createTestProgram('INDIRECT-INFINITE');

      // Create indirect infinite loop: A -> B -> A
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-B', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-B';

      const paraB = createParagraphWithStatements('PARA-B', [
        new StatementNode('PERFORM', 'PERFORM PARA-A', mockLocation)
      ]);
      paraB.statements[0].target = 'PARA-A';

      program.procedureDivision!.paragraphs = [paraA, paraB];

      analyzer.analyze(program);
      const warnings = analyzer.getWarnings();

      // Should detect circular dependency which can lead to infinite loop
      const circularWarnings = warnings.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
    });

    test('should handle PERFORM loops with conditions', () => {
      const program = createTestProgram('PERFORM-LOOPS');

      // Create PERFORM loops that might be infinite
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('PERFORM', 'PERFORM LOOP-PARA UNTIL CONDITION = "Y"', mockLocation)
      ]);
      mainPara.statements[0].sourceText = 'PERFORM LOOP-PARA UNTIL CONDITION = "Y"';

      const loopPara = createParagraphWithStatements('LOOP-PARA', [
        new StatementNode('DISPLAY', 'DISPLAY "Looping"', mockLocation)
        // Note: No statement that changes CONDITION - potential infinite loop
      ]);

      program.procedureDivision!.paragraphs = [mainPara, loopPara];

      analyzer.analyze(program);
      const callGraph = analyzer.getCallGraph();

      // Should still build call graph correctly
      expect(callGraph.get('MAIN-PARA')?.callees).toContain('LOOP-PARA');
    });
  });

  describe('Complex Statement Parsing', () => {
    test('should handle complex COMPUTE statements', () => {
      const program = createTestProgram('COMPLEX-COMPUTE');

      const vars: VariableDefinition[] = [
        { name: 'VAR1', level: 1, location: mockLocation, children: [] },
        { name: 'VAR2', level: 1, location: mockLocation, children: [] },
        { name: 'VAR3', level: 1, location: mockLocation, children: [] },
        { name: 'RESULT', level: 1, location: mockLocation, children: [] }
      ];

      program.dataDivision!.workingStorage = vars;

      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('COMPUTE', 'COMPUTE RESULT = (VAR1 + VAR2) * VAR3 / 100', mockLocation)
      ]);
      mainPara.statements[0].sourceText = 'COMPUTE RESULT = (VAR1 + VAR2) * VAR3 / 100';

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('VAR1')?.isRead).toBe(true);
      expect(variableUsage.get('VAR2')?.isRead).toBe(true);
      expect(variableUsage.get('VAR3')?.isRead).toBe(true);
      expect(variableUsage.get('RESULT')?.isWritten).toBe(true);
    });

    test('should handle complex IF statements with nested conditions', () => {
      const program = createTestProgram('COMPLEX-IF');

      const vars: VariableDefinition[] = [
        { name: 'CONDITION1', level: 1, location: mockLocation, children: [] },
        { name: 'CONDITION2', level: 1, location: mockLocation, children: [] },
        { name: 'RESULT', level: 1, location: mockLocation, children: [] }
      ];

      program.dataDivision!.workingStorage = vars;

      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('IF', 'IF CONDITION1 = "Y" AND CONDITION2 = "N"', mockLocation)
      ]);
      mainPara.statements[0].condition = 'CONDITION1 = "Y" AND CONDITION2 = "N"';
      mainPara.statements[0].nestedStatements = [
        new StatementNode('MOVE', 'MOVE "TRUE" TO RESULT', mockLocation)
      ];

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('CONDITION1')?.isRead).toBe(true);
      expect(variableUsage.get('CONDITION2')?.isRead).toBe(true);
    });

    test('should handle complex string operations', () => {
      const program = createTestProgram('STRING-OPERATIONS');

      const vars: VariableDefinition[] = [
        { name: 'STRING1', level: 1, location: mockLocation, children: [] },
        { name: 'STRING2', level: 1, location: mockLocation, children: [] },
        { name: 'RESULT-STRING', level: 1, location: mockLocation, children: [] }
      ];

      program.dataDivision!.workingStorage = vars;

      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('STRING', 'STRING STRING1 DELIMITED BY SIZE STRING2 DELIMITED BY SPACE INTO RESULT-STRING', mockLocation)
      ]);
      mainPara.statements[0].sourceText = 'STRING STRING1 DELIMITED BY SIZE STRING2 DELIMITED BY SPACE INTO RESULT-STRING';

      program.procedureDivision!.paragraphs = [mainPara];

      analyzer.analyze(program);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('STRING1')?.isRead).toBe(true);
      expect(variableUsage.get('STRING2')?.isRead).toBe(true);
      expect(variableUsage.get('RESULT-STRING')?.isWritten).toBe(true);
    });
  });

  describe('Error Boundary Testing', () => {
    test('should handle null or undefined program gracefully', () => {
      expect(() => {
        analyzer.analyze(null as any);
      }).not.toThrow();
    });

    test('should handle program with circular structure references', () => {
      const program = createTestProgram('CIRCULAR-REFS');

      // Create potentially problematic circular references
      const paraA = createParagraphWithStatements('PARA-A', [
        new StatementNode('PERFORM', 'PERFORM PARA-A', mockLocation)
      ]);
      paraA.statements[0].target = 'PARA-A';

      program.procedureDivision!.paragraphs = [paraA];

      expect(() => {
        analyzer.analyze(program);
      }).not.toThrow();

      const warnings = analyzer.getWarnings();
      const infiniteLoopWarnings = warnings.filter(w => w.code === 'INFINITE_LOOP');
      expect(infiniteLoopWarnings.length).toBeGreaterThan(0);
    });

    test('should handle very large call graphs', () => {
      const program = createTestProgram('LARGE-CALL-GRAPH');

      // Create a large number of paragraphs
      const paragraphs: ParagraphNode[] = [];
      for (let i = 0; i < 100; i++) {
        const paraName = `PARA-${i}`;
        const nextParaName = `PARA-${i + 1}`;
        
        const statements: StatementNode[] = [];
        if (i < 99) {
          const performStmt = new StatementNode('PERFORM', `PERFORM ${nextParaName}`, mockLocation);
          performStmt.target = nextParaName;
          statements.push(performStmt);
        }
        
        const paragraph = createParagraphWithStatements(paraName, statements);
        paragraphs.push(paragraph);
      }

      program.procedureDivision!.paragraphs = paragraphs;

      const startTime = Date.now();
      analyzer.analyze(program);
      const endTime = Date.now();

      // Should complete in reasonable time (less than 1 second)
      expect(endTime - startTime).toBeLessThan(1000);

      const callGraph = analyzer.getCallGraph();
      expect(callGraph.size).toBe(100);
    });

    test('should handle malformed statement structures', () => {
      const program = createTestProgram('MALFORMED-STATEMENTS');

      // Create statements with missing or invalid properties
      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('DISPLAY', undefined as any, mockLocation),
        new StatementNode('DISPLAY', 'DISPLAY "Test"', mockLocation)
      ]);

      program.procedureDivision!.paragraphs = [mainPara];

      expect(() => {
        analyzer.analyze(program);
      }).not.toThrow();
    });
  });

  describe('Configuration Edge Cases', () => {
    test('should handle configuration with all features disabled', () => {
      const config = {
        ...DEFAULT_STATIC_ANALYSIS_CONFIG,
        detectDeadCode: false,
        detectUnreachableParagraphs: false,
        detectUnusedVariables: false,
        detectCircularDependencies: false,
        detectInfiniteLoops: false,
        detectUnreferencedSections: false,
        checkMissingGoToTargets: false,
        analyzeVariableUsage: false
      };

      const configuredAnalyzer = new StaticAnalyzer(config);
      const program = createTestProgram('DISABLED-CONFIG');

      const mainPara = createParagraphWithStatements('MAIN-PARA', [
        new StatementNode('DISPLAY', 'DISPLAY "Test"', mockLocation)
      ]);
      program.procedureDivision!.paragraphs = [mainPara];

      configuredAnalyzer.analyze(program);
      const warnings = configuredAnalyzer.getWarnings();

      // Should have no warnings since all features are disabled
      expect(warnings).toHaveLength(0);
    });

    test('should handle invalid configuration gracefully', () => {
      const invalidConfig = {
        detectDeadCode: null,
        detectUnusedVariables: undefined,
        invalidProperty: 'invalid'
      } as any;

      expect(() => {
        new StaticAnalyzer(invalidConfig);
      }).not.toThrow();
    });
  });
});