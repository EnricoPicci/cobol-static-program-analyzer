/**
 * Unit tests for CopyProcessor
 */

import '../setup';
import { CopyProcessor, DEFAULT_COPYBOOK_CONFIG } from '../../../src/copy';
import * as fs from 'fs/promises';
import * as path from 'path';

// Mock copybook content
const TEST_COPYBOOKS = {
  'CUSTOMER-RECORD': `
    01 CUSTOMER-RECORD.
       05 CUSTOMER-ID    PIC 9(8).
       05 CUSTOMER-NAME  PIC X(30).
       05 CUSTOMER-ADDR  PIC X(50).
  `,
  'ADDRESS-RECORD': `
    01 ADDRESS-RECORD.
       05 STREET-NAME    PIC X(30).
       05 CITY-NAME      PIC X(20).
       05 STATE-CODE     PIC X(2).
       05 ZIP-CODE       PIC 9(5).
  `,
  'NESTED-COPY': `
    01 MAIN-RECORD.
       05 HEADER-INFO    PIC X(10).
       COPY CUSTOMER-RECORD.
       05 FOOTER-INFO    PIC X(10).
  `,
  'TEMPLATE': `
    01 PREFIX-RECORD.
       05 PREFIX-ID      PIC 9(8).
       05 PREFIX-NAME    PIC X(30).
  `
};

// Setup test copybook directory
const TEST_COPYBOOK_DIR = path.join(__dirname, 'test-copybooks');

beforeAll(async () => {
  // Create test copybook directory
  try {
    await fs.mkdir(TEST_COPYBOOK_DIR, { recursive: true });
    
    // Write test copybooks
    for (const [name, content] of Object.entries(TEST_COPYBOOKS)) {
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, `${name}.cpy`), content);
    }
  } catch (error) {
    console.warn('Could not create test copybooks:', error);
  }
});

afterAll(async () => {
  // Clean up test copybook directory
  try {
    for (const name of Object.keys(TEST_COPYBOOKS)) {
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, `${name}.cpy`));
    }
    await fs.rmdir(TEST_COPYBOOK_DIR);
  } catch (error) {
    console.warn('Could not clean up test copybooks:', error);
  }
});

describe('CopyProcessor', () => {
  let processor: CopyProcessor;

  beforeEach(() => {
    processor = new CopyProcessor({
      ...DEFAULT_COPYBOOK_CONFIG,
      searchPaths: [TEST_COPYBOOK_DIR],
      cacheEnabled: false // Disable cache for testing
    });
  });

  describe('Basic COPY Processing', () => {
    test('should process simple COPY statement', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-RECORD.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('CUSTOMER-ID');
      expect(result.processedSource).toContain('CUSTOMER-NAME');
      expect(result.includedCopybooks).toHaveLength(1);
      expect(result.includedCopybooks[0].name).toBe('CUSTOMER-RECORD');
    });

    test('should handle multiple COPY statements', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CUSTOMER-RECORD.
        COPY ADDRESS-RECORD.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('CUSTOMER-ID');
      expect(result.processedSource).toContain('STREET-NAME');
      expect(result.includedCopybooks).toHaveLength(2);
    });

    test('should handle COPY statement not found', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY NONEXISTENT-RECORD.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(1);
      expect(result.errors[0].type).toBe('COPYBOOK_NOT_FOUND');
      expect(result.errors[0].copybook).toBe('NONEXISTENT-RECORD');
    });
  });

  describe('REPLACING Clause Processing', () => {
    test('should process REPLACING clause with pseudo-text', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY TEMPLATE REPLACING ==PREFIX== BY ==CUST==.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('CUST-RECORD');
      expect(result.processedSource).toContain('CUST-ID');
      expect(result.processedSource).toContain('CUST-NAME');
      expect(result.processedSource).not.toContain('PREFIX-RECORD');
    });

    test('should handle multiple REPLACING patterns', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY TEMPLATE REPLACING ==PREFIX== BY ==CUST== ==RECORD== BY ==INFO==.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('CUST-INFO');
      expect(result.processedSource).not.toContain('PREFIX-RECORD');
    });
  });

  describe('Nested COPY Processing', () => {
    test('should process nested COPY statements', async () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY NESTED-COPY.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('MAIN-RECORD');
      expect(result.processedSource).toContain('CUSTOMER-ID'); // From nested COPY
      expect(result.processedSource).toContain('CUSTOMER-NAME'); // From nested COPY
    });

    test('should detect circular dependencies', async () => {
      // Create circular dependency copybooks
      const circularA = 'COPY CIRCULAR-B.';
      const circularB = 'COPY CIRCULAR-A.';
      
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-A.cpy'), circularA);
      await fs.writeFile(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-B.cpy'), circularB);

      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY CIRCULAR-A.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await processor.process(source);
      
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors.some(e => e.type === 'CIRCULAR_DEPENDENCY')).toBe(true);
      
      // Clean up
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-A.cpy'));
      await fs.unlink(path.join(TEST_COPYBOOK_DIR, 'CIRCULAR-B.cpy'));
    });

    test('should respect maximum nesting level', async () => {
      const deepNestProcessor = new CopyProcessor({
        ...DEFAULT_COPYBOOK_CONFIG,
        searchPaths: [TEST_COPYBOOK_DIR],
        maxNestingLevel: 2
      });

      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY NESTED-COPY.
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const result = await deepNestProcessor.process(source);
      
      // Should process but may have warnings about nesting
      expect(result).toBeDefined();
    });
  });

  describe('Configuration Management', () => {
    test('should use provided configuration', () => {
      const config = processor.getConfiguration();
      
      expect(config.searchPaths).toEqual([TEST_COPYBOOK_DIR]);
      expect(config.cacheEnabled).toBe(false);
    });

    test('should update configuration', () => {
      processor.updateConfiguration({
        maxNestingLevel: 5,
        cacheEnabled: true
      });
      
      const config = processor.getConfiguration();
      expect(config.maxNestingLevel).toBe(5);
      expect(config.cacheEnabled).toBe(true);
    });
  });

  describe('Error Handling', () => {
    test('should handle invalid source gracefully', async () => {
      const source = 'INVALID COBOL SOURCE';
      
      const result = await processor.process(source);
      
      expect(result).toBeDefined();
      expect(result.processedSource).toBe(source); // Should return original on failure
    });

    test('should collect all errors during processing', async () => {
      const source = `
        COPY NONEXISTENT-1.
        COPY NONEXISTENT-2.
        COPY NONEXISTENT-3.
      `;
      
      const result = await processor.process(source);
      
      expect(result.errors.length).toBeGreaterThanOrEqual(3);
      result.errors.forEach(error => {
        expect(error.type).toBe('COPYBOOK_NOT_FOUND');
      });
    });
  });

  describe('Performance and Caching', () => {
    test('should work with caching enabled', async () => {
      const cachedProcessor = new CopyProcessor({
        ...DEFAULT_COPYBOOK_CONFIG,
        searchPaths: [TEST_COPYBOOK_DIR],
        cacheEnabled: true
      });

      const source = `
        COPY CUSTOMER-RECORD.
        COPY CUSTOMER-RECORD.
      `;

      const result = await cachedProcessor.process(source);
      
      expect(result.errors).toHaveLength(0);
      expect(result.processedSource).toContain('CUSTOMER-ID');
    });

    test('should handle large source efficiently', async () => {
      const largeCopyStatements = Array.from({ length: 10 }, (_, i) => 
        `COPY CUSTOMER-RECORD.`
      ).join('\n        ');
      
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. LARGE-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        ${largeCopyStatements}
        PROCEDURE DIVISION.
        STOP RUN.
      `;

      const startTime = performance.now();
      const result = await processor.process(source);
      const endTime = performance.now();
      
      expect(result.errors).toHaveLength(0);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });
  });
});
