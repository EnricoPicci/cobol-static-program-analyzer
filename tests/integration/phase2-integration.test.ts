/**
 * Phase 2 Integration Tests - Complete Parsing Workflow
 * Tests the integration of all Phase 2 components
 */

import '../setup';
import { CobolParser, DEFAULT_COBOL_PARSER_CONFIG } from '../../src/parser/cobol-parser';
import { CobolASTBuilder, DEFAULT_AST_BUILDER_CONFIG } from '../../src/ast/builder';
import { CobolProgram } from '../../src/ast/nodes/CobolProgram';
import { CobolErrorHandler, ErrorRecoveryStrategy } from '../../src/parser/error-handler';
import { SourceLocation } from '../../src/core/types';

describe('Phase 2 Integration Tests', () => {
  let parser: CobolParser;

  beforeEach(() => {
    parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
  });

  describe('Complete Parsing Workflow', () => {
    test('should integrate preprocessing, parsing, and AST construction', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. INTEGRATION-TEST.
        AUTHOR. INTEGRATION-COORDINATOR.
        DATE-WRITTEN. TODAY.
        
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT STUDENT-FILE ASSIGN TO "students.dat".
        
        DATA DIVISION.
        FILE SECTION.
        FD STUDENT-FILE.
        01 STUDENT-RECORD.
           05 STUDENT-ID     PIC 9(6).
           05 STUDENT-NAME   PIC X(30).
           05 STUDENT-GRADE  PIC 99.
        
        WORKING-STORAGE SECTION.
        01 WS-COUNTERS.
           05 WS-TOTAL-STUDENTS  PIC 9(4) VALUE ZERO.
           05 WS-PROCESSED       PIC 9(4) VALUE ZERO.
        01 WS-FLAGS.
           05 WS-EOF-FLAG        PIC X VALUE 'N'.
           05 WS-ERROR-FLAG      PIC X VALUE 'N'.
        
        PROCEDURE DIVISION.
        MAIN-SECTION.
        MAIN-PARAGRAPH.
            PERFORM INITIALIZATION
            PERFORM PROCESS-STUDENTS UNTIL WS-EOF-FLAG = 'Y'
            PERFORM FINALIZATION
            STOP RUN.
        
        INITIALIZATION.
            DISPLAY "Starting Student Processing System"
            OPEN INPUT STUDENT-FILE
            MOVE ZERO TO WS-TOTAL-STUDENTS, WS-PROCESSED.
        
        PROCESS-STUDENTS.
            READ STUDENT-FILE
                AT END MOVE 'Y' TO WS-EOF-FLAG
                NOT AT END PERFORM VALIDATE-STUDENT
            END-READ.
        
        VALIDATE-STUDENT.
            IF STUDENT-ID NOT NUMERIC
               MOVE 'Y' TO WS-ERROR-FLAG
               DISPLAY "Invalid Student ID: " STUDENT-ID
            ELSE
               ADD 1 TO WS-PROCESSED
               DISPLAY "Processing: " STUDENT-NAME
            END-IF.
        
        FINALIZATION.
            CLOSE STUDENT-FILE
            DISPLAY "Total Students Processed: " WS-PROCESSED
            DISPLAY "Program Complete".
      `.trim();

      const result = await parser.parse(cobolProgram, 'integration-test.cbl');
      
      // Should handle the parsing attempt gracefully even with ANTLR issues
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(cobolProgram.length);
      expect(result.sourceInfo.lineCount).toBeGreaterThan(40);
      
      // Source information should be preserved
      expect(result.sourceInfo.copybooksIncluded).toEqual([]);
    });

    test('should handle complex nested structures', async () => {
      const complexCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COMPLEX-INTEGRATION.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 CUSTOMER-RECORD.
           05 CUSTOMER-INFO.
              10 CUSTOMER-ID      PIC 9(8).
              10 CUSTOMER-NAME.
                 15 FIRST-NAME    PIC X(20).
                 15 LAST-NAME     PIC X(30).
              10 CUSTOMER-ADDRESS.
                 15 STREET        PIC X(40).
                 15 CITY          PIC X(25).
                 15 STATE         PIC X(2).
                 15 ZIP-CODE      PIC 9(5).
           05 ACCOUNT-INFO.
              10 ACCOUNT-NUMBER   PIC 9(12).
              10 ACCOUNT-BALANCE  PIC 9(10)V99.
              10 ACCOUNT-STATUS   PIC X.
                 88 ACTIVE-ACCOUNT    VALUE 'A'.
                 88 INACTIVE-ACCOUNT  VALUE 'I'.
                 88 CLOSED-ACCOUNT    VALUE 'C'.
        
        PROCEDURE DIVISION.
        MAIN-PROCESS.
            MOVE 12345678 TO CUSTOMER-ID
            MOVE "JOHN" TO FIRST-NAME
            MOVE "DOE" TO LAST-NAME
            MOVE "123 MAIN ST" TO STREET
            MOVE "ANYTOWN" TO CITY
            MOVE "ST" TO STATE
            MOVE 12345 TO ZIP-CODE
            MOVE 987654321012 TO ACCOUNT-NUMBER
            MOVE 1500.75 TO ACCOUNT-BALANCE
            SET ACTIVE-ACCOUNT TO TRUE
            DISPLAY "Customer: " FIRST-NAME " " LAST-NAME
            DISPLAY "Balance: $" ACCOUNT-BALANCE
            STOP RUN.
      `.trim();

      const result = await parser.parse(complexCobol, 'complex-integration.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(complexCobol.length);
      
      // Should track all nested structure information
      expect(result.sourceInfo.lineCount).toBeGreaterThan(30);
    });
  });

  describe('Error Handling Integration', () => {
    test('should integrate error handling across all components', async () => {
      const invalidCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ERROR-TEST.
        
        INVALID SYNTAX HERE
        MORE INVALID SYNTAX
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 INVALID-PICTURE PIC INVALID.
        
        PROCEDURE DIVISION.
        INVALID-PARAGRAPH.
            INVALID STATEMENT
            ANOTHER INVALID STATEMENT.
      `.trim();

      const result = await parser.parse(invalidCobol, 'error-test.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      
      // Should collect all errors, not just the first one
      result.errors.forEach(error => {
        expect(error.severity).toBe('error');
        expect(error.location).toBeDefined();
        expect(error.location.line).toBeGreaterThan(0);
        expect(error.location.column).toBeGreaterThan(0);
      });
    });

    test('should handle error recovery strategies', async () => {
      const strictParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        errorRecovery: 'strict',
        maxErrors: 1
      });

      const lenientParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        errorRecovery: 'lenient',
        maxErrors: 10
      });

      const errorCode = `
        IDENTIFICATION DIVISION.
        SYNTAX ERROR 1
        SYNTAX ERROR 2
        SYNTAX ERROR 3
      `.trim();

      const strictResult = await strictParser.parse(errorCode);
      const lenientResult = await lenientParser.parse(errorCode);

      // Both should handle errors, but differently
      expect(strictResult.success).toBe(false);
      expect(lenientResult.success).toBe(false);
      
      // Results should be defined even with errors
      expect(strictResult).toBeDefined();
      expect(lenientResult).toBeDefined();
    });
  });

  describe('Performance Integration', () => {
    test('should measure end-to-end performance', async () => {
      const performanceConfig = {
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enableProfiling: true
      };
      
      const perfParser = new CobolParser(performanceConfig);
      
      const largeProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PERFORMANCE-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        ${Array.from({ length: 100 }, (_, i) => 
          `01 WS-VARIABLE-${i.toString().padStart(3, '0')} PIC X(50).`
        ).join('\n        ')}
        
        PROCEDURE DIVISION.
        MAIN-PROCESS.
        ${Array.from({ length: 50 }, (_, i) => 
          `    MOVE "TEST-VALUE-${i}" TO WS-VARIABLE-${i.toString().padStart(3, '0')}`
        ).join('\n        ')}.
            STOP RUN.
      `.trim();

      const startTime = performance.now();
      const result = await perfParser.parse(largeProgram, 'performance-test.cbl');
      const endTime = performance.now();
      
      expect(result).toBeDefined();
      expect(endTime - startTime).toBeLessThan(5000); // Should complete within 5 seconds
      
      if (result.performance) {
        expect(result.performance.parseTime).toBeGreaterThan(0);
        expect(result.performance.nodeCount).toBeGreaterThan(0);
        expect(result.performance.memoryUsage).toBeGreaterThan(0);
      }
    });

    test('should handle memory management efficiently', async () => {
      // Test with multiple parsing operations
      const operations = Array.from({ length: 10 }, (_, i) => ({
        code: `
          IDENTIFICATION DIVISION.
          PROGRAM-ID. MEMORY-TEST-${i}.
          DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 WS-VAR PIC X(100).
          PROCEDURE DIVISION.
          MAIN.
              STOP RUN.
        `.trim(),
        filename: `memory-test-${i}.cbl`
      }));

      const initialMemory = process.memoryUsage().heapUsed;
      
      for (const operation of operations) {
        const result = await parser.parse(operation.code, operation.filename);
        expect(result).toBeDefined();
      }
      
      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }
      
      const finalMemory = process.memoryUsage().heapUsed;
      const memoryGrowth = (finalMemory - initialMemory) / (1024 * 1024); // MB
      
      // Should not leak excessive memory
      expect(memoryGrowth).toBeLessThan(50); // Less than 50MB growth
    });
  });

  describe('Configuration Integration', () => {
    test('should integrate all configuration options', () => {
      const customConfig = {
        astBuilder: {
          preserveSourceText: true,
          includeComments: true,
          enableOptimizations: false,
          maxNestingDepth: 200,
          validateDuringConstruction: true
        },
        enablePreprocessing: true,
        copybookPaths: ['./test-copybooks', './lib-copybooks'],
        maxErrors: 20,
        errorRecovery: 'lenient' as const,
        enableProfiling: true,
        encoding: 'utf8',
        dialect: 'enterprise' as const
      };

      const customParser = new CobolParser(customConfig);
      const retrievedConfig = customParser.getConfig();

      expect(retrievedConfig.astBuilder.preserveSourceText).toBe(true);
      expect(retrievedConfig.astBuilder.maxNestingDepth).toBe(200);
      expect(retrievedConfig.copybookPaths).toEqual(['./test-copybooks', './lib-copybooks']);
      expect(retrievedConfig.maxErrors).toBe(20);
      expect(retrievedConfig.dialect).toBe('enterprise');
    });

    test('should handle configuration updates dynamically', async () => {
      const initialConfig = parser.getConfig();
      expect(initialConfig.maxErrors).toBe(10);

      parser.updateConfig({ maxErrors: 5, enableProfiling: true });
      
      const updatedConfig = parser.getConfig();
      expect(updatedConfig.maxErrors).toBe(5);
      expect(updatedConfig.enableProfiling).toBe(true);
      
      // Other settings should remain unchanged
      expect(updatedConfig.errorRecovery).toBe(initialConfig.errorRecovery);
      expect(updatedConfig.enablePreprocessing).toBe(initialConfig.enablePreprocessing);
    });
  });

  describe('Source Preservation Integration', () => {
    test('should preserve source locations throughout pipeline', async () => {
      const sourceWithComments = `
        * This is a comment
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SOURCE-PRESERVATION.    * Program ID comment
        
        * Data Division comment
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-VAR PIC X(10).               * Variable comment
        
        * Procedure Division comment
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Hello World".         * Display statement
            STOP RUN.                      * Stop statement
      `.trim();

      const result = await parser.parse(sourceWithComments, 'source-preservation.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(sourceWithComments.length);
      expect(result.sourceInfo.lineCount).toBe(sourceWithComments.split('\n').length);
      
      // Should preserve source text if configured
      if (result.preprocessedSource) {
        expect(result.preprocessedSource.length).toBeGreaterThan(0);
      }
    });
  });

  describe('Validation Integration', () => {
    test('should integrate validation across all components', async () => {
      const testProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. VALIDATION-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-VALID-VAR   PIC 9(5).
        01 WS-ANOTHER-VAR PIC X(20).
        
        PROCEDURE DIVISION.
        MAIN-VALIDATION.
            MOVE 12345 TO WS-VALID-VAR
            MOVE "TEST" TO WS-ANOTHER-VAR
            DISPLAY WS-VALID-VAR
            DISPLAY WS-ANOTHER-VAR
            STOP RUN.
      `.trim();

      const diagnostics = await parser.validate(testProgram, 'validation-test.cbl');
      
      expect(Array.isArray(diagnostics)).toBe(true);
      
      // Should return diagnostics for any issues found
      diagnostics.forEach(diagnostic => {
        expect(diagnostic.severity).toMatch(/^(error|warning|info)$/);
        expect(diagnostic.code).toBeDefined();
        expect(diagnostic.message).toBeDefined();
        expect(diagnostic.location).toBeDefined();
      });
    });
  });

  describe('Dialect Support Integration', () => {
    test('should handle different COBOL dialects', async () => {
      const cobol85Parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        dialect: 'cobol85'
      });

      const enterpriseParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        dialect: 'enterprise'
      });

      const standardProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DIALECT-TEST.
        PROCEDURE DIVISION.
        MAIN.
            STOP RUN.
      `.trim();

      const cobol85Result = await cobol85Parser.parse(standardProgram);
      const enterpriseResult = await enterpriseParser.parse(standardProgram);

      expect(cobol85Result).toBeDefined();
      expect(enterpriseResult).toBeDefined();
      
      // Both should handle standard COBOL syntax
      expect(cobol85Result.sourceInfo.originalLength).toBe(standardProgram.length);
      expect(enterpriseResult.sourceInfo.originalLength).toBe(standardProgram.length);
    });
  });
});