/**
 * Integration tests for COPY statement processing with parser pipeline
 */

import '../setup';
import { CobolParser, DEFAULT_COBOL_PARSER_CONFIG } from '../../src/parser/cobol-parser';
import { createCopyProcessor, DEFAULT_COPYBOOK_CONFIG } from '../../src/copy';
import * as fs from 'fs/promises';
import * as path from 'path';

// Setup test environment
const TEST_COPYBOOK_DIR = path.join(__dirname, 'copy-test-copybooks');

const TEST_COPYBOOKS = {
  'CUSTOMER-REC': `
       01 CUSTOMER-RECORD.
          05 CUST-ID       PIC 9(8).
          05 CUST-NAME     PIC X(30).
          05 CUST-ADDRESS.
             10 STREET     PIC X(25).
             10 CITY       PIC X(20).
             10 STATE      PIC X(2).
             10 ZIP        PIC 9(5).
          05 CUST-BALANCE  PIC 9(7)V99.
  `,
  'PRODUCT-REC': `
       01 PRODUCT-RECORD.
          05 PROD-ID       PIC 9(6).
          05 PROD-NAME     PIC X(40).
          05 PROD-PRICE    PIC 9(5)V99.
          05 PROD-QTY      PIC 9(4).
  `,
  'COMMON-VARS': `
       01 COMMON-VARIABLES.
          05 WS-COUNTER    PIC 9(4) VALUE ZERO.
          05 WS-FLAG       PIC X VALUE 'N'.
          05 WS-TOTAL      PIC 9(8)V99 VALUE ZERO.
  `,
  'NESTED-MAIN': `
       01 MAIN-STRUCTURE.
          05 HEADER-INFO   PIC X(20).
          COPY CUSTOMER-REC.
          COPY PRODUCT-REC.
          05 FOOTER-INFO   PIC X(20).
  `,
  'TEMPLATE-REC': `
       01 PREFIX-RECORD.
          05 PREFIX-ID     PIC 9(8).
          05 PREFIX-NAME   PIC X(30).
          05 PREFIX-STATUS PIC X.
  `
};

beforeAll(async () => {
  try {
    await fs.mkdir(TEST_COPYBOOK_DIR, { recursive: true });
    
    for (const [name, content] of Object.entries(TEST_COPYBOOKS)) {
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, `${name}.cpy`), content);
    }
  } catch (error) {
    console.warn('Could not setup test copybooks:', error);
  }
});

afterAll(async () => {
  try {
    for (const name of Object.keys(TEST_COPYBOOKS)) {
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, `${name}.cpy`));
    }
    await fs.rmdir(TEST_COPYBOOK_DIR);
  } catch (error) {
    console.warn('Could not cleanup test copybooks:', error);
  }
});

describe('COPY Processing Integration Tests', () => {
  let parser: CobolParser;

  beforeEach(() => {
    parser = new CobolParser({
      ...DEFAULT_COBOL_PARSER_CONFIG,
      copybookPaths: [TEST_COPYBOOK_DIR],
      enablePreprocessing: true
    });
  });

  describe('Basic COPY Integration', () => {
    test('should integrate COPY processing with parser pipeline', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COPY-INTEGRATION-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-REC.
        COPY COMMON-VARS.
        
        PROCEDURE DIVISION.
        MAIN-PROCESS.
            MOVE 12345678 TO CUST-ID
            MOVE "JOHN DOE" TO CUST-NAME
            MOVE "123 MAIN ST" TO STREET
            MOVE "ANYTOWN" TO CITY
            MOVE "CA" TO STATE
            MOVE 90210 TO ZIP
            MOVE 1500.75 TO CUST-BALANCE
            
            ADD 1 TO WS-COUNTER
            MOVE 'Y' TO WS-FLAG
            ADD CUST-BALANCE TO WS-TOTAL
            
            DISPLAY "Customer: " CUST-NAME
            DISPLAY "Balance: $" CUST-BALANCE
            DISPLAY "Total: $" WS-TOTAL
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'copy-integration.cbl');
      
      
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.sourceInfo.copybooksIncluded).toContain('CUSTOMER-REC');
      expect(result.sourceInfo.copybooksIncluded).toContain('COMMON-VARS');
      
      // Verify that copybook content was included
      if (result.preprocessedSource) {
        expect(result.preprocessedSource).toContain('CUST-ID');
        expect(result.preprocessedSource).toContain('WS-COUNTER');
        expect(result.preprocessedSource).not.toContain('COPY CUSTOMER-REC');
      }
    });

    test('should handle multiple copybooks with complex structure', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MULTI-COPY-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-REC.
        01 SEPARATOR PIC X(50) VALUE ALL '-'.
        COPY PRODUCT-REC.
        01 ANOTHER-SEPARATOR PIC X(50) VALUE ALL '='.
        COPY COMMON-VARS.
        
        PROCEDURE DIVISION.
        PROCESS-RECORDS.
            MOVE 1001 TO CUST-ID
            MOVE 2001 TO PROD-ID
            MOVE 0 TO WS-COUNTER
            
            PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 10
                ADD PROD-PRICE TO WS-TOTAL
            END-PERFORM
            
            DISPLAY "Processing complete"
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'multi-copy.cbl');
      
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.sourceInfo.copybooksIncluded).toHaveLength(3);
      
      if (result.preprocessedSource) {
        expect(result.preprocessedSource).toContain('CUST-ID');
        expect(result.preprocessedSource).toContain('PROD-ID');
        expect(result.preprocessedSource).toContain('WS-COUNTER');
      }
    });
  });

  describe('Nested COPY Integration', () => {
    test('should handle nested COPY statements', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NESTED-COPY-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY NESTED-MAIN.
        COPY COMMON-VARS.
        
        PROCEDURE DIVISION.
        NESTED-PROCESS.
            MOVE "HEADER DATA" TO HEADER-INFO
            MOVE 5001 TO CUST-ID
            MOVE 6001 TO PROD-ID
            MOVE "FOOTER DATA" TO FOOTER-INFO
            
            DISPLAY "Header: " HEADER-INFO
            DISPLAY "Customer ID: " CUST-ID
            DISPLAY "Product ID: " PROD-ID
            DISPLAY "Footer: " FOOTER-INFO
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'nested-copy.cbl');
      
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      
      // Should include the main copybook and its nested dependencies
      expect(result.sourceInfo.copybooksIncluded.length).toBeGreaterThanOrEqual(3);
      expect(result.sourceInfo.copybooksIncluded).toContain('NESTED-MAIN');
      
      if (result.preprocessedSource) {
        expect(result.preprocessedSource).toContain('MAIN-STRUCTURE');
        expect(result.preprocessedSource).toContain('CUST-ID'); // From nested COPY
        expect(result.preprocessedSource).toContain('PROD-ID'); // From nested COPY
      }
    });
  });

  describe('REPLACING Clause Integration', () => {
    test('should handle REPLACING clauses in parser integration', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. REPLACING-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY TEMPLATE-REC REPLACING ==PREFIX== BY ==CUSTOMER==.
        COPY TEMPLATE-REC REPLACING ==PREFIX== BY ==VENDOR==.
        
        PROCEDURE DIVISION.
        REPLACING-PROCESS.
            MOVE 1001 TO CUSTOMER-ID
            MOVE 2001 TO VENDOR-ID
            MOVE "ACME CORP" TO CUSTOMER-NAME
            MOVE "SUPPLIER INC" TO VENDOR-NAME
            MOVE 'A' TO CUSTOMER-STATUS
            MOVE 'B' TO VENDOR-STATUS
            
            DISPLAY "Customer: " CUSTOMER-NAME " (" CUSTOMER-STATUS ")"
            DISPLAY "Vendor: " VENDOR-NAME " (" VENDOR-STATUS ")"
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'replacing.cbl');
      
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      
      if (result.preprocessedSource) {
        expect(result.preprocessedSource).toContain('CUSTOMER-RECORD');
        expect(result.preprocessedSource).toContain('VENDOR-RECORD');
        expect(result.preprocessedSource).toContain('CUSTOMER-ID');
        expect(result.preprocessedSource).toContain('VENDOR-ID');
        expect(result.preprocessedSource).not.toContain('PREFIX-RECORD');
        expect(result.preprocessedSource).not.toContain('PREFIX-ID');
      }
    });
  });

  describe('Error Handling Integration', () => {
    test('should handle copybook not found errors', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ERROR-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY NONEXISTENT-COPYBOOK.
        COPY CUSTOMER-REC.
        
        PROCEDURE DIVISION.
        ERROR-PROCESS.
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'error.cbl');
      
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      
      const copyErrors = result.errors.filter(e => e.code === 'COPYBOOK_NOT_FOUND');
      expect(copyErrors).toHaveLength(1);
      expect(copyErrors[0].message).toContain('NONEXISTENT-COPYBOOK');
    });

    test('should handle circular dependency errors', async () => {
      // Create circular dependency copybooks for this test
      const circularA = 'COPY CIRCULAR-B.';
      const circularB = 'COPY CIRCULAR-A.';
      
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-A.cpy'), circularA);
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-B.cpy'), circularB);

      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CIRCULAR-ERROR-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CIRCULAR-A.
        
        PROCEDURE DIVISION.
        CIRCULAR-ERROR-PROCESS.
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'circular-error.cbl');
      
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      
      const circularErrors = result.errors.filter(e => e.code === 'CIRCULAR_DEPENDENCY');
      expect(circularErrors.length).toBeGreaterThan(0);
      
      // Clean up
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-A.cpy'));
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-B.cpy'));
    });
  });

  describe('Performance Integration', () => {
    test('should process multiple copybooks efficiently', async () => {
      const multipleCopies = Array.from({ length: 5 }, () => 
        'COPY CUSTOMER-REC.\nCOPY PRODUCT-REC.\nCOPY COMMON-VARS.'
      ).join('\n        ');
      
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PERFORMANCE-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        ${multipleCopies}
        
        PROCEDURE DIVISION.
        PERFORMANCE-PROCESS.
            STOP RUN.
      `.trim();

      const startTime = performance.now();
      const result = await parser.parse(cobolProgram, 'performance.cbl');
      const endTime = performance.now();
      
      expect(result.success).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(endTime - startTime).toBeLessThan(2000); // Should complete within 2 seconds
    });
  });

  describe('Configuration Integration', () => {
    test('should respect parser copybook configuration', async () => {
      const customParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        copybookPaths: ['/nonexistent/path', TEST_COPYBOOK_DIR],
        enablePreprocessing: true,
        maxErrors: 5
      });

      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CONFIG-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-REC.
        
        PROCEDURE DIVISION.
        CONFIG-PROCESS.
            STOP RUN.
      `.trim();

      const result = await customParser.parse(cobolProgram, 'config.cbl');
      
      expect(result.success).toBe(true);
      expect(result.sourceInfo.copybooksIncluded).toContain('CUSTOMER-REC');
    });

    test('should work with preprocessing disabled', async () => {
      const noPreprocessParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enablePreprocessing: false
      });

      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NO-PREPROCESS-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-REC.
        
        PROCEDURE DIVISION.
        NO-PREPROCESS-PROCESS.
            STOP RUN.
      `.trim();

      const result = await noPreprocessParser.parse(cobolProgram, 'no-preprocess.cbl');
      
      // Should succeed but COPY statements won't be processed
      expect(result.sourceInfo.copybooksIncluded).toHaveLength(0);
      if (result.preprocessedSource) {
        expect(result.preprocessedSource).toContain('COPY CUSTOMER-REC');
      }
    });
  });

  describe('AST Integration', () => {
    test('should build AST correctly with processed copybooks', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. AST-INTEGRATION-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-REC.
        
        PROCEDURE DIVISION.
        AST-PROCESS.
            MOVE 1001 TO CUST-ID
            DISPLAY CUST-NAME
            STOP RUN.
      `.trim();

      const result = await parser.parse(cobolProgram, 'ast-integration.cbl');
      
      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();
      
      if (result.ast) {
        expect(result.ast.name).toBe('AST-INTEGRATION-TEST');
        expect(result.ast.dataDivision).toBeDefined();
        expect(result.ast.procedureDivision).toBeDefined();
      }
    });
  });
});
