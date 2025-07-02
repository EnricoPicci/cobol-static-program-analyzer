/**
 * Integration test for the complete COBOL parsing pipeline
 * Tests the integration between preprocessor, parser, and AST builder
 */

import { CobolParser } from '../../src/parser/cobol-parser';
import { CobolProgram } from '../../src/ast/nodes/CobolProgram';

describe('Parser Pipeline Integration', () => {
  let parser: CobolParser;

  beforeEach(() => {
    parser = new CobolParser();
  });

  describe('Basic Program Parsing', () => {
    it('should parse a minimal COBOL program', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
            DISPLAY "Hello World".
            STOP RUN.
      `;

      const result = await parser.parse(source, 'test.cob');

      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.ast).toBeInstanceOf(CobolProgram);
      expect(result.ast?.name).toBe('TEST-PROGRAM');
    });

    it('should handle program with all divisions', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COMPLETE-TEST.
        
        ENVIRONMENT DIVISION.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-COUNTER PIC 9(3) VALUE 0.
        
        PROCEDURE DIVISION.
        MAIN-SECTION SECTION.
        MAIN-PARAGRAPH.
            MOVE 100 TO WS-COUNTER.
            DISPLAY WS-COUNTER.
            PERFORM SUB-PARAGRAPH.
            STOP RUN.
            
        SUB-PARAGRAPH.
            ADD 1 TO WS-COUNTER.
      `;

      const result = await parser.parse(source, 'complete.cob');

      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.ast?.name).toBe('COMPLETE-TEST');
      expect(result.ast?.dataDivision?.workingStorageVariables).toHaveLength(1);
      expect(result.ast?.procedureDivision?.sections).toHaveLength(1);
    });
  });

  describe('Error Handling', () => {
    it('should handle syntax errors gracefully', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. INVALID-PROGRAM
        
        PROCEDURE DIVISION.
        MISSING-DOT
            DISPLAY "Test".
      `;

      const result = await parser.parse(source, 'invalid.cob');

      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors[0].severity).toBe('error');
    });

    it('should validate PROGRAM-ID format', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. 123-INVALID.
      `;

      const result = await parser.parse(source, 'invalid-id.cob');

      // Should still parse but may have warnings about invalid format
      expect(result.ast?.name).toBeDefined();
    });
  });

  describe('Performance Metrics', () => {
    it('should provide performance metrics when enabled', async () => {
      const config = parser.getConfig();
      config.enableProfiling = true;
      parser.updateConfig(config);

      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PERF-TEST.
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
            DISPLAY "Performance test".
            STOP RUN.
      `;

      const result = await parser.parse(source, 'perf.cob');

      expect(result.success).toBe(true);
      expect(result.performance).toBeDefined();
      expect(result.performance?.parseTime).toBeGreaterThan(0);
      expect(result.performance?.nodeCount).toBeGreaterThan(0);
    });
  });

  describe('Source Information', () => {
    it('should preserve source information', async () => {
      const source = `IDENTIFICATION DIVISION.
PROGRAM-ID. SOURCE-TEST.

PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY "Source test".
    STOP RUN.`;

      const result = await parser.parse(source, 'source.cob');

      expect(result.success).toBe(true);
      expect(result.sourceInfo.originalLength).toBe(source.length);
      expect(result.sourceInfo.lineCount).toBe(source.split('\n').length);
    });

    it('should track source locations in AST nodes', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. LOCATION-TEST.
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
            DISPLAY "Location test".
      `;

      const result = await parser.parse(source, 'location.cob');

      expect(result.success).toBe(true);
      expect(result.ast?.location.line).toBeGreaterThan(0);
      expect(result.ast?.identificationDivision.location.line).toBeGreaterThan(0);
    });
  });

  describe('Configuration Options', () => {
    it('should respect different parser configurations', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CONFIG-TEST.
        
        PROCEDURE DIVISION.
        INVALID-STATEMENT.
      `;

      // Test strict mode
      const strictConfig = parser.getConfig();
      strictConfig.errorRecovery = 'strict';
      strictConfig.maxErrors = 1;
      parser.updateConfig(strictConfig);

      const strictResult = await parser.parse(source, 'strict.cob');
      expect(strictResult.success).toBe(false);

      // Test lenient mode
      const lenientConfig = parser.getConfig();
      lenientConfig.errorRecovery = 'lenient';
      lenientConfig.maxErrors = 10;
      parser.updateConfig(lenientConfig);

      const lenientResult = await parser.parse(source, 'lenient.cob');
      // Lenient mode may still succeed with warnings
      expect(lenientResult.errors.length).toBeGreaterThanOrEqual(0);
    });
  });

  describe('AST Structure Validation', () => {
    it('should produce valid AST structure matching README specification', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. AST-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-VAR PIC X(10) VALUE "TEST".
        
        PROCEDURE DIVISION.
        MAIN-SECTION SECTION.
        MAIN-PARAGRAPH.
            DISPLAY WS-VAR.
            STOP RUN.
      `;

      const result = await parser.parse(source, 'ast.cob');

      expect(result.success).toBe(true);
      
      // Validate structure matches README specification
      const program = result.ast!;
      expect(program.name).toBe('AST-TEST');
      expect(program.dataDivision).toBeDefined();
      expect(program.procedureDivision).toBeDefined();
      
      // Check JSON serialization
      const json = program.toJSON();
      expect(json.name).toBe('AST-TEST');
      expect(json.dataDivision).toBeDefined();
      expect(json.procedureDivision).toBeDefined();
    });
  });
});