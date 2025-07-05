/**
 * Unit tests for Static Analyzer - Phase 4
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
 * Helper function to create a CobolProgram with proper constructor arguments
 */
function createTestCobolProgram(name: string, location: SourceLocation): CobolProgram {
  const identificationDivision = new IdentificationDivision(name, location);
  return new CobolProgram(name, identificationDivision, location);
}

describe('StaticAnalyzer - Phase 4', () => {
  let analyzer: StaticAnalyzer;
  let mockProgram: CobolProgram;
  let mockLocation: SourceLocation;

  beforeEach(() => {
    analyzer = new StaticAnalyzer();
    mockLocation = {
      line: 1,
      column: 1,
      endLine: 1,
      endColumn: 10
    };

    // Create a mock COBOL program
    const mockIdentificationDivision = new IdentificationDivision('TEST-PROGRAM', mockLocation);
    mockProgram = new CobolProgram('TEST-PROGRAM', mockIdentificationDivision, mockLocation);
    mockProgram.procedureDivision = new ProcedureDivision(mockLocation);
    mockProgram.dataDivision = new DataDivision(mockLocation);
  });

  describe('Configuration', () => {
    test('should use default configuration', () => {
      const defaultAnalyzer = new StaticAnalyzer();
      expect(defaultAnalyzer.getConfiguration()).toBeDefined();
    });

    test('should accept custom configuration', () => {
      const customConfig = {
        ...DEFAULT_STATIC_ANALYSIS_CONFIG,
        detectDeadCode: false
      };
      const customAnalyzer = new StaticAnalyzer(customConfig);
      expect(customAnalyzer).toBeDefined();
    });
  });

  describe('Call Graph Generation', () => {
    test('should build call graph from PERFORM statements', () => {
      // Create paragraphs
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      const subParagraph = new ParagraphNode('SUB-PROCESS', 'SUB-PROCESS.', mockLocation, mockLocation);

      // Create PERFORM statement
      const performStatement = new StatementNode('PERFORM', 'PERFORM SUB-PROCESS', mockLocation);
      performStatement.target = 'SUB-PROCESS';
      (mainParagraph as any).statements = [performStatement];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph, subParagraph];

      analyzer.analyze(mockProgram);
      const callGraph = analyzer.getCallGraph();

      expect(callGraph.size).toBeGreaterThan(0);
      expect(callGraph.has('MAIN-PROCESS')).toBe(true);
      expect(callGraph.has('SUB-PROCESS')).toBe(true);
    });

    test('should extract calls from GO TO statements', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      const targetParagraph = new ParagraphNode('TARGET-PARA', 'TARGET-PARA.', mockLocation, mockLocation);

      // Create GO TO statement
      const goToStatement = new StatementNode('GO', 'GO TO TARGET-PARA', mockLocation);
      goToStatement.target = 'TARGET-PARA';
      (mainParagraph as any).statements = [goToStatement];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph, targetParagraph];

      analyzer.analyze(mockProgram);
      const callGraph = analyzer.getCallGraph();

      expect(callGraph.has('MAIN-PROCESS')).toBe(true);
      expect(callGraph.has('TARGET-PARA')).toBe(true);
    });

    test('should handle CALL statements', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create CALL statement
      const callStatement = new StatementNode('CALL', 'CALL "SUBPROGRAM"', mockLocation);
      callStatement.target = 'SUBPROGRAM';
      (mainParagraph as any).statements = [callStatement];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const callGraph = analyzer.getCallGraph();

      expect(callGraph.has('MAIN-PROCESS')).toBe(true);
    });
  });

  describe('Dead Code Detection', () => {
    test('should detect unreachable paragraphs', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      const unreachableParagraph = new ParagraphNode('UNREACHABLE', 'UNREACHABLE.', mockLocation, mockLocation);

      (mainParagraph as any).statements = [];
      (unreachableParagraph as any).statements = [];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph, unreachableParagraph];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const unreachableWarnings = warnings.filter(w => w.code === 'UNREACHABLE_CODE');
      expect(unreachableWarnings.length).toBeGreaterThan(0);
    });

    test('should not report entry point as unreachable', () => {
      const entryParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      (entryParagraph as any).statements = [];

      mockProgram.procedureDivision!.paragraphs = [entryParagraph];

      analyzer.analyze(mockProgram);
      const callGraph = analyzer.getCallGraph();
      const entryNode = callGraph.get('MAIN-PROCESS');

      expect(entryNode?.reachable).toBe(true);
    });
  });

  describe('Variable Usage Analysis', () => {
    test('should track MOVE statement variable references', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create MOVE statement
      const moveStatement = new StatementNode('MOVE', 'MOVE VAR1 TO VAR2', mockLocation);
      moveStatement.operands = ['VAR1', 'VAR2'];
      (mainParagraph as any).statements = [moveStatement];

      // Add variables to data division
      const var1: VariableDefinition = {
        name: 'VAR1',
        level: 1,
        location: mockLocation,
        children: []
      };
      const var2: VariableDefinition = {
        name: 'VAR2',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [var1, var2];
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.has('VAR1')).toBe(true);
      expect(variableUsage.has('VAR2')).toBe(true);
      expect(variableUsage.get('VAR1')?.isRead).toBe(true);
      expect(variableUsage.get('VAR2')?.isWritten).toBe(true);
    });

    test('should track arithmetic statement variable references', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create ADD statement
      const addStatement = new StatementNode('ADD', 'ADD VAR1 TO VAR2 GIVING VAR3', mockLocation);
      addStatement.sourceText = 'ADD VAR1 TO VAR2 GIVING VAR3';
      (mainParagraph as any).statements = [addStatement];

      // Add variables to data division
      const variables: VariableDefinition[] = [
        { name: 'VAR1', level: 1, location: mockLocation, children: [] },
        { name: 'VAR2', level: 1, location: mockLocation, children: [] },
        { name: 'VAR3', level: 1, location: mockLocation, children: [] }
      ];

      mockProgram.dataDivision!.workingStorage = variables;
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('VAR1')?.isRead).toBe(true);
      expect(variableUsage.get('VAR2')?.isRead).toBe(true);
      expect(variableUsage.get('VAR3')?.isWritten).toBe(true);
    });

    test('should track COMPUTE statement variable references', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create COMPUTE statement
      const computeStatement = new StatementNode('COMPUTE', 'COMPUTE RESULT = VAR1 + VAR2', mockLocation);
      computeStatement.sourceText = 'COMPUTE RESULT = VAR1 + VAR2';
      (mainParagraph as any).statements = [computeStatement];

      // Add variables to data division
      const variables: VariableDefinition[] = [
        { name: 'VAR1', level: 1, location: mockLocation, children: [] },
        { name: 'VAR2', level: 1, location: mockLocation, children: [] },
        { name: 'RESULT', level: 1, location: mockLocation, children: [] }
      ];

      mockProgram.dataDivision!.workingStorage = variables;
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage.get('VAR1')?.isRead).toBe(true);
      expect(variableUsage.get('VAR2')?.isRead).toBe(true);
      expect(variableUsage.get('RESULT')?.isWritten).toBe(true);
    });

    test('should detect unused variables', () => {
      // Add unused variable to data division
      const unusedVar: VariableDefinition = {
        name: 'UNUSED-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [unusedVar];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const unusedWarnings = warnings.filter(w => w.code === 'UNUSED_VARIABLE');
      expect(unusedWarnings.length).toBeGreaterThan(0);
      expect(unusedWarnings[0].message).toContain('UNUSED-VAR');
    });

    test('should detect uninitialized variables', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create DISPLAY statement that reads uninitialized variable
      const displayStatement = new StatementNode('DISPLAY', 'DISPLAY UNINIT-VAR', mockLocation);
      displayStatement.operands = ['UNINIT-VAR'];
      (mainParagraph as any).statements = [displayStatement];

      // Add uninitialized variable to data division
      const uninitVar: VariableDefinition = {
        name: 'UNINIT-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [uninitVar];
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const uninitWarnings = warnings.filter(w => w.code === 'UNINITIALIZED_VARIABLE');
      expect(uninitWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('Circular Dependency Detection', () => {
    test('should detect circular dependencies', () => {
      const para1 = new ParagraphNode('PARA1', 'PARA1.', mockLocation, mockLocation);
      const para2 = new ParagraphNode('PARA2', 'PARA2.', mockLocation, mockLocation);

      // PARA1 calls PARA2
      const perform1 = new StatementNode('PERFORM', 'PERFORM PARA2', mockLocation);
      perform1.target = 'PARA2';
      (para1 as any).statements = [perform1];

      // PARA2 calls PARA1 (circular)
      const perform2 = new StatementNode('PERFORM', 'PERFORM PARA1', mockLocation);
      perform2.target = 'PARA1';
      (para2 as any).statements = [perform2];

      mockProgram.procedureDivision!.paragraphs = [para1, para2];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const circularWarnings = warnings.filter(w => w.code === 'CIRCULAR_DEPENDENCY');
      expect(circularWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('GO TO Target Checking', () => {
    test('should detect missing GO TO targets', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);

      // Create GO TO statement with missing target
      const goToStatement = new StatementNode('GO', 'GO TO NONEXISTENT', mockLocation);
      goToStatement.target = 'NONEXISTENT';
      (mainParagraph as any).statements = [goToStatement];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const missingTargetWarnings = warnings.filter(w => w.code === 'MISSING_GOTO_TARGET');
      expect(missingTargetWarnings.length).toBeGreaterThan(0);
      expect(missingTargetWarnings[0].message).toContain('NONEXISTENT');
    });

    test('should not report warning for valid GO TO targets', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      const targetParagraph = new ParagraphNode('TARGET-PARA', 'TARGET-PARA.', mockLocation, mockLocation);

      // Create GO TO statement with valid target
      const goToStatement = new StatementNode('GO', 'GO TO TARGET-PARA', mockLocation);
      goToStatement.target = 'TARGET-PARA';
      (mainParagraph as any).statements = [goToStatement];

      mockProgram.procedureDivision!.paragraphs = [mainParagraph, targetParagraph];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const missingTargetWarnings = warnings.filter(w => w.code === 'MISSING_GOTO_TARGET');
      expect(missingTargetWarnings.length).toBe(0);
    });
  });

  describe('Infinite Loop Detection', () => {
    test('should detect self-referencing paragraphs', () => {
      const selfRefParagraph = new ParagraphNode('SELF-REF', 'SELF-REF.', mockLocation, mockLocation);

      // Create self-referencing PERFORM
      const performStatement = new StatementNode('PERFORM', 'PERFORM SELF-REF', mockLocation);
      performStatement.target = 'SELF-REF';
      (selfRefParagraph as any).statements = [performStatement];

      mockProgram.procedureDivision!.paragraphs = [selfRefParagraph];

      analyzer.analyze(mockProgram);
      const warnings = analyzer.getWarnings();

      const infiniteLoopWarnings = warnings.filter(w => w.code === 'INFINITE_LOOP');
      expect(infiniteLoopWarnings.length).toBeGreaterThan(0);
    });
  });

  describe('Error Handling', () => {
    test('should handle empty programs gracefully', () => {
      const emptyIdDiv = new IdentificationDivision('EMPTY-PROGRAM', mockLocation);
      const emptyProgram = new CobolProgram('EMPTY-PROGRAM', emptyIdDiv, mockLocation);

      analyzer.analyze(emptyProgram);
      const summary = analyzer.getSummary();

      expect(summary.errorCount).toBe(0);
      expect(summary.callGraphNodes).toBe(0);
      expect(summary.variablesAnalyzed).toBe(0);
    });

    test('should handle programs without procedure division', () => {
      const noProcIdDiv = new IdentificationDivision('NO-PROC-PROGRAM', mockLocation);
      const programWithoutProc = new CobolProgram('NO-PROC-PROGRAM', noProcIdDiv, mockLocation);
      programWithoutProc.dataDivision = new DataDivision(mockLocation);

      analyzer.analyze(programWithoutProc);
      const summary = analyzer.getSummary();

      expect(summary.errorCount).toBe(0);
    });

    test('should handle programs without data division', () => {
      const noDataIdDiv = new IdentificationDivision('NO-DATA-PROGRAM', mockLocation);
      const programWithoutData = new CobolProgram('NO-DATA-PROGRAM', noDataIdDiv, mockLocation);
      programWithoutData.procedureDivision = new ProcedureDivision(mockLocation);

      analyzer.analyze(programWithoutData);
      const summary = analyzer.getSummary();

      expect(summary.variablesAnalyzed).toBe(0);
    });
  });

  describe('Summary and Reporting', () => {
    test('should provide comprehensive analysis summary', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      (mainParagraph as any).statements = [];

      const testVar: VariableDefinition = {
        name: 'TEST-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [testVar];
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const summary = analyzer.getSummary();

      expect(summary.callGraphNodes).toBe(1);
      expect(summary.variablesAnalyzed).toBe(1);
      expect(summary.hasWarnings).toBe(true); // Should have unused variable warning
    });

    test('should provide call graph access', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      const callGraph = analyzer.getCallGraph();

      expect(callGraph).toBeDefined();
      expect(callGraph.size).toBeGreaterThan(0);
    });

    test('should provide variable usage information', () => {
      const testVar: VariableDefinition = {
        name: 'TEST-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [testVar];

      analyzer.analyze(mockProgram);
      const variableUsage = analyzer.getVariableUsage();

      expect(variableUsage).toBeDefined();
      expect(variableUsage.has('TEST-VAR')).toBe(true);
    });
  });

  describe('Configuration Options', () => {
    test('should respect detectDeadCode configuration', () => {
      const config = {
        ...DEFAULT_STATIC_ANALYSIS_CONFIG,
        detectDeadCode: false
      };

      const configuredAnalyzer = new StaticAnalyzer(config);
      
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      const unreachableParagraph = new ParagraphNode('UNREACHABLE', 'UNREACHABLE.', mockLocation, mockLocation);

      mockProgram.procedureDivision!.paragraphs = [mainParagraph, unreachableParagraph];

      configuredAnalyzer.analyze(mockProgram);
      const warnings = configuredAnalyzer.getWarnings();

      const deadCodeWarnings = warnings.filter(w => w.code === 'UNREACHABLE_CODE');
      expect(deadCodeWarnings.length).toBe(0);
    });

    test('should respect detectUnusedVariables configuration', () => {
      const config = {
        ...DEFAULT_STATIC_ANALYSIS_CONFIG,
        detectUnusedVariables: false
      };

      const configuredAnalyzer = new StaticAnalyzer(config);

      const unusedVar: VariableDefinition = {
        name: 'UNUSED-VAR',
        level: 1,
        location: mockLocation,
        children: []
      };

      mockProgram.dataDivision!.workingStorage = [unusedVar];

      configuredAnalyzer.analyze(mockProgram);
      const warnings = configuredAnalyzer.getWarnings();

      const unusedWarnings = warnings.filter(w => w.code === 'UNUSED_VARIABLE');
      expect(unusedWarnings.length).toBe(0);
    });
  });

  describe('Cleanup and State Management', () => {
    test('should clear analysis state', () => {
      const mainParagraph = new ParagraphNode('MAIN-PROCESS', 'MAIN-PROCESS.', mockLocation, mockLocation);
      mockProgram.procedureDivision!.paragraphs = [mainParagraph];

      analyzer.analyze(mockProgram);
      expect(analyzer.getSummary().callGraphNodes).toBeGreaterThan(0);

      analyzer.clear();
      expect(analyzer.getSummary().callGraphNodes).toBe(0);
    });

    test('should handle multiple analysis runs', () => {
      const program1IdDiv = new IdentificationDivision('PROGRAM1', mockLocation);
      const program1 = new CobolProgram('PROGRAM1', program1IdDiv, mockLocation);
      program1.procedureDivision = new ProcedureDivision(mockLocation);
      const para1 = new ParagraphNode('PARA1', 'PARA1.', mockLocation, mockLocation);
      program1.procedureDivision.paragraphs = [para1];

      const program2IdDiv = new IdentificationDivision('PROGRAM2', mockLocation);
      const program2 = new CobolProgram('PROGRAM2', program2IdDiv, mockLocation);
      program2.procedureDivision = new ProcedureDivision(mockLocation);
      const para2 = new ParagraphNode('PARA2', 'PARA2.', mockLocation, mockLocation);
      program2.procedureDivision.paragraphs = [para2];

      // First analysis
      analyzer.analyze(program1);
      const summary1 = analyzer.getSummary();
      expect(summary1.callGraphNodes).toBe(1);

      // Second analysis should replace first
      analyzer.analyze(program2);
      const summary2 = analyzer.getSummary();
      expect(summary2.callGraphNodes).toBe(1);
    });
  });
});