/**
 * Tests for CobolProgram AST node
 */

import '../setup';
import { CobolProgram } from '../../../src/ast/nodes/CobolProgram';
import { IdentificationDivision } from '../../../src/ast/nodes/IdentificationDivision';
import { SourceLocation } from '../../../src/core/types';

describe('CobolProgram', () => {
  const mockLocation: SourceLocation = {
    line: 1,
    column: 1,
    endLine: 1,
    endColumn: 20,
    file: 'test.cbl'
  };

  describe('Construction', () => {
    test('should create program with identification division', () => {
      const identDiv = new IdentificationDivision('TEST-PROGRAM', mockLocation);
      const program = new CobolProgram('TEST-PROGRAM', identDiv, mockLocation);

      expect(program.type).toBe('CobolProgram');
      expect(program.name).toBe('TEST-PROGRAM');
      expect(program.identificationDivision).toBe(identDiv);
      expect(program.children).toContain(identDiv);
    });

    test('should have valid COBOL structure with required division', () => {
      const identDiv = new IdentificationDivision('VALID-PROG', mockLocation);
      const program = new CobolProgram('VALID-PROG', identDiv, mockLocation);

      expect(program).toHaveValidCOBOLStructure();
      expect(program).toContainDivision('IDENTIFICATION');
    });

    test('should validate program ID format', () => {
      const identDiv = new IdentificationDivision('VALID-NAME', mockLocation);
      const program = new CobolProgram('VALID-NAME', identDiv, mockLocation);

      expect(program.isValid()).toBe(true);
      expect(identDiv.isValidProgramId()).toBe(true);
    });
  });

  describe('Division Management', () => {
    let program: CobolProgram;
    let identDiv: IdentificationDivision;

    beforeEach(() => {
      identDiv = new IdentificationDivision('TEST-PROG', mockLocation);
      program = new CobolProgram('TEST-PROG', identDiv, mockLocation);
    });

    test('should handle all division types', () => {
      expect(program).toContainDivision('IDENTIFICATION');
      
      // Initially no other divisions
      expect(program.environmentDivision).toBeUndefined();
      expect(program.dataDivision).toBeUndefined();
      expect(program.procedureDivision).toBeUndefined();
    });
  });

  describe('Program Analysis', () => {
    test('should provide program summary', () => {
      const identDiv = new IdentificationDivision('SUMMARY-TEST', mockLocation);
      const program = new CobolProgram('SUMMARY-TEST', identDiv, mockLocation);

      const summary = program.getSummary();

      expect(summary.name).toBe('SUMMARY-TEST');
      expect(summary.hasProcedureDivision).toBe(false);
      expect(summary.hasDataDivision).toBe(false);
      expect(summary.hasEnvironmentDivision).toBe(false);
      expect(summary.sectionCount).toBe(0);
      expect(summary.paragraphCount).toBe(0);
      expect(summary.variableCount).toBe(0);
    });

    test('should convert to JSON format matching README specification', () => {
      const identDiv = new IdentificationDivision('JSON-TEST', mockLocation);
      const program = new CobolProgram('JSON-TEST', identDiv, mockLocation);

      const json = program.toJSON();

      expect(json).toHaveProperty('type', 'CobolProgram');
      expect(json).toHaveProperty('name', 'JSON-TEST');
      expect(json).toHaveProperty('identificationDivision');
      expect(json).toHaveProperty('summary');
    });
  });

  describe('Node Hierarchy', () => {
    test('should maintain parent-child relationships', () => {
      const identDiv = new IdentificationDivision('HIERARCHY-TEST', mockLocation);
      const program = new CobolProgram('HIERARCHY-TEST', identDiv, mockLocation);

      expect(identDiv.parent).toBe(program);
      expect(program.children).toContain(identDiv);
      expect(program.getRoot()).toBe(program);
      expect(identDiv.getDepth()).toBe(1);
    });
  });

  describe('Visitor Pattern', () => {
    test('should accept visitors', () => {
      const identDiv = new IdentificationDivision('VISITOR-TEST', mockLocation);
      const program = new CobolProgram('VISITOR-TEST', identDiv, mockLocation);

      const mockVisitor = {
        visitNode: jest.fn(() => 'visited'),
        visitCobolProgram: jest.fn(() => 'program-visited')
      };

      const result = program.accept(mockVisitor);

      expect(mockVisitor.visitCobolProgram).toHaveBeenCalledWith(program);
      expect(result).toBe('program-visited');
    });
  });
});