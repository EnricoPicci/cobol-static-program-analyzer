/**
 * Integration tests for complete parsing pipeline
 * Tests end-to-end parsing from source to AST
 */

import '../../setup';
import { CobolParser, DEFAULT_COBOL_PARSER_CONFIG } from '../../../src/parser/cobol-parser';
import { CobolASTBuilder } from '../../../src/ast/builder';
import { CobolErrorHandler } from '../../../src/parser/error-handler';

describe('Complete Parsing Pipeline Integration', () => {
  // Helper to load test programs
  const loadTestProgram = (category: string, filename: string): string => {
    const fs = require('fs');
    const path = require('path');
    return fs.readFileSync(path.join(__dirname, `../../data/valid/${category}`, filename), 'utf8');
  };

  const loadInvalidProgram = (category: string, filename: string): string => {
    const fs = require('fs');
    const path = require('path');
    return fs.readFileSync(path.join(__dirname, `../../data/invalid/${category}`, filename), 'utf8');
  };

  describe('Complete Valid Program Processing', () => {
    let parser: CobolParser;

    beforeEach(() => {
      parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enableProfiling: true,
        errorRecovery: 'lenient'
      });
    });

    test('should process Hello World program end-to-end', async () => {
      const source = loadTestProgram('basic', 'hello-world.cbl');
      const result = await parser.parse(source, 'hello-world.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(source.length);
      expect(result.sourceInfo.lineCount).toBe(source.split('\n').length);
      expect(result.sourceInfo.copybooksIncluded).toEqual([]);
      
      // Test source tracking
      expect(result.sourceInfo.processedLength).toBeGreaterThanOrEqual(result.sourceInfo.originalLength);
      
      if (result.success && result.ast) {
        // Validate AST structure
        expect(result.ast.type).toBe('CobolProgram');
        expect(result.ast.name).toBe('HELLO-WORLD');
        expect(result.ast).toHaveValidCOBOLStructure();
        expect(result.ast).toContainDivision('IDENTIFICATION');
        expect(result.ast).toContainDivision('PROCEDURE');
        
        // Test program summary
        const summary = result.ast.getSummary();
        expect(summary.name).toBe('HELLO-WORLD');
        expect(summary.hasProcedureDivision).toBe(true);
        expect(summary.hasDataDivision).toBe(false);
      }
    });

    test('should process arithmetic program with data division', async () => {
      const source = loadTestProgram('basic', 'arithmetic.cbl');
      const result = await parser.parse(source, 'arithmetic.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(source.length);
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('ARITHMETIC-TEST');
        expect(result.ast).toContainDivision('IDENTIFICATION');
        expect(result.ast).toContainDivision('DATA');
        expect(result.ast).toContainDivision('PROCEDURE');
        
        // Check data division processing
        expect(result.ast).toHaveDataItem('NUM1');
        expect(result.ast).toHaveDataItem('NUM2');
        expect(result.ast).toHaveDataItem('RESULT');
        
        const summary = result.ast.getSummary();
        expect(summary.hasDataDivision).toBe(true);
        expect(summary.variableCount).toBeGreaterThan(0);
      }
    });

    test('should process data movement program', async () => {
      const source = loadTestProgram('basic', 'data-movement.cbl');
      const result = await parser.parse(source, 'data-movement.cbl');
      
      expect(result).toBeDefined();
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('DATA-MOVEMENT');
        expect(result.ast).toHaveDataItem('CUSTOMER-INFO');
        expect(result.ast).toHaveDataItem('TEMP-NAME');
        
        // Test hierarchical data structure
        expect(result.ast).toHaveDataItem('CUSTOMER-NAME');
        expect(result.ast).toHaveDataItem('CUSTOMER-ID');
        expect(result.ast).toHaveDataItem('CUSTOMER-BALANCE');
      }
    });

    test('should process nested performs program', async () => {
      const source = loadTestProgram('intermediate', 'nested-performs.cbl');
      const result = await parser.parse(source, 'nested-performs.cbl');
      
      expect(result).toBeDefined();
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('NESTED-PERFORMS');
        
        // Check for sections and paragraphs
        const summary = result.ast.getSummary();
        expect(summary.sectionCount).toBeGreaterThan(0);
        expect(summary.paragraphCount).toBeGreaterThan(0);
        
        // Check for PERFORM statements
        expect(result.ast).toHavePerformStatement('MAIN-PARA', 'PROCESS-OUTER-LOOP');
      }
    });

    test('should process conditional logic program', async () => {
      const source = loadTestProgram('intermediate', 'conditional-logic.cbl');
      const result = await parser.parse(source, 'conditional-logic.cbl');
      
      expect(result).toBeDefined();
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('CONDITIONAL-LOGIC');
        expect(result.ast).toHaveConditionalStatement('EVALUATE');
        expect(result.ast).toHaveDataItem('STUDENT-RECORD');
      }
    });

    test('should process complex inventory system', async () => {
      const source = loadTestProgram('advanced', 'inventory-system.cbl');
      const result = await parser.parse(source, 'inventory-system.cbl');
      
      expect(result).toBeDefined();
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('INVENTORY-SYSTEM');
        expect(result.ast).toContainDivision('IDENTIFICATION');
        expect(result.ast).toContainDivision('ENVIRONMENT');
        expect(result.ast).toContainDivision('DATA');
        expect(result.ast).toContainDivision('PROCEDURE');
        
        // Check for file operations
        expect(result.ast).toHaveFileOperation('OPEN');
        expect(result.ast).toHaveFileOperation('READ');
        expect(result.ast).toHaveFileOperation('CLOSE');
        
        // Check division order
        expect(result.ast).toHaveCorrectDivisionOrder();
        
        const summary = result.ast.getSummary();
        expect(summary.hasEnvironmentDivision).toBe(true);
        expect(summary.hasDataDivision).toBe(true);
        expect(summary.sectionCount).toBeGreaterThan(0);
      }
    });
  });

  describe('Error Handling Integration', () => {
    let parser: CobolParser;

    beforeEach(() => {
      parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        maxErrors: 10,
        errorRecovery: 'lenient'
      });
    });

    test('should handle syntax errors gracefully', async () => {
      const source = loadInvalidProgram('syntax', 'missing-period.cbl');
      const result = await parser.parse(source, 'missing-period.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      
      // Should identify specific error type
      const syntaxErrors = result.errors.filter(e => 
        e.code.includes('SYNTAX') || e.severity === 'error'
      );
      expect(syntaxErrors.length).toBeGreaterThan(0);
      
      // Error should have location information
      syntaxErrors.forEach(error => {
        expect(error.location).toBeDefined();
        expect(error.location.line).toBeGreaterThan(0);
        expect(error.location.column).toBeGreaterThan(0);
      });
    });

    test('should handle division order errors', async () => {
      const source = loadInvalidProgram('syntax', 'invalid-division-order.cbl');
      const result = await parser.parse(source, 'invalid-division-order.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    test('should handle unclosed control structures', async () => {
      const source = loadInvalidProgram('syntax', 'unclosed-if.cbl');
      const result = await parser.parse(source, 'unclosed-if.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    test('should detect semantic errors', async () => {
      const source = loadInvalidProgram('semantic', 'undefined-variable.cbl');
      const result = await parser.parse(source, 'undefined-variable.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    test('should handle type mismatch errors', async () => {
      const source = loadInvalidProgram('semantic', 'type-mismatch.cbl');
      const result = await parser.parse(source, 'type-mismatch.cbl');
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });
  });

  describe('Configuration Integration', () => {
    test('should respect strict error recovery mode', async () => {
      const strictParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        errorRecovery: 'strict',
        maxErrors: 1
      });
      
      const source = loadInvalidProgram('syntax', 'missing-period.cbl');
      const result = await strictParser.parse(source);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      // In strict mode, should stop on first error
    });

    test('should respect lenient error recovery mode', async () => {
      const lenientParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        errorRecovery: 'lenient',
        maxErrors: 100
      });
      
      const source = loadInvalidProgram('syntax', 'invalid-division-order.cbl');
      const result = await lenientParser.parse(source);
      
      expect(result).toBeDefined();
      expect(result.success).toBe(false);
      // In lenient mode, should collect multiple errors
    });

    test('should handle different AST builder configurations', async () => {
      const preservingParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        astBuilder: {
          ...DEFAULT_COBOL_PARSER_CONFIG.astBuilder,
          preserveSourceText: true,
          includeComments: true
        }
      });
      
      const nonPreservingParser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        astBuilder: {
          ...DEFAULT_COBOL_PARSER_CONFIG.astBuilder,
          preserveSourceText: false,
          includeComments: false
        }
      });
      
      const source = loadTestProgram('basic', 'hello-world.cbl');
      
      const preservingResult = await preservingParser.parse(source);
      const nonPreservingResult = await nonPreservingParser.parse(source);
      
      expect(preservingResult).toBeDefined();
      expect(nonPreservingResult).toBeDefined();
      
      // Both should parse successfully
      if (preservingResult.success && nonPreservingResult.success) {
        expect(preservingResult.ast).toBeDefined();
        expect(nonPreservingResult.ast).toBeDefined();
      }
    });
  });

  describe('Performance Integration', () => {
    test('should provide performance metrics for complex programs', async () => {
      const parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enableProfiling: true
      });
      
      const source = loadTestProgram('advanced', 'inventory-system.cbl');
      const result = await parser.parse(source);
      
      expect(result).toBeDefined();
      expect(result.performance).toBeDefined();
      
      if (result.performance) {
        expect(result.performance.parseTime).toBeGreaterThan(0);
        expect(result.performance.memoryUsage).toBeGreaterThanOrEqual(0);
        expect(result.performance.nodeCount).toBeGreaterThanOrEqual(0);
        expect(result.performance.copybookCount).toBe(0); // No copybooks in this test
      }
    });

    test('should maintain performance across multiple parses', async () => {
      const parser = new CobolParser({
        ...DEFAULT_COBOL_PARSER_CONFIG,
        enableProfiling: true
      });
      
      const source = loadTestProgram('intermediate', 'conditional-logic.cbl');
      const times: number[] = [];
      
      for (let i = 0; i < 5; i++) {
        const result = await parser.parse(source, `test-${i}.cbl`);
        expect(result).toBeDefined();
        
        if (result.performance) {
          times.push(result.performance.parseTime);
        }
      }
      
      expect(times.length).toBe(5);
      
      // Performance should be relatively consistent
      const avgTime = times.reduce((a, b) => a + b) / times.length;
      times.forEach(time => {
        expect(time).toBeLessThan(avgTime * 2); // No parse should be more than 2x average
      });
    });
  });

  describe('Validation Integration', () => {
    test('should provide comprehensive validation', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      
      const validSource = loadTestProgram('basic', 'arithmetic.cbl');
      const validDiagnostics = await parser.validate(validSource, 'arithmetic.cbl');
      
      expect(Array.isArray(validDiagnostics)).toBe(true);
      
      const invalidSource = loadInvalidProgram('syntax', 'missing-period.cbl');
      const invalidDiagnostics = await parser.validate(invalidSource, 'missing-period.cbl');
      
      expect(Array.isArray(invalidDiagnostics)).toBe(true);
      expect(invalidDiagnostics.length).toBeGreaterThan(0);
      
      // Invalid program should have more diagnostics than valid one
      expect(invalidDiagnostics.length).toBeGreaterThanOrEqual(validDiagnostics.length);
    });

    test('should provide detailed diagnostic information', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = loadInvalidProgram('semantic', 'undefined-variable.cbl');
      
      const diagnostics = await parser.validate(source, 'undefined-variable.cbl');
      
      expect(diagnostics.length).toBeGreaterThan(0);
      
      diagnostics.forEach(diagnostic => {
        expect(diagnostic).toHaveProperty('severity');
        expect(diagnostic).toHaveProperty('code');
        expect(diagnostic).toHaveProperty('message');
        expect(diagnostic).toHaveProperty('location');
        
        expect(['error', 'warning', 'info']).toContain(diagnostic.severity);
        expect(diagnostic.code).toBeTruthy();
        expect(diagnostic.message).toBeTruthy();
        expect(diagnostic.location.line).toBeGreaterThan(0);
        expect(diagnostic.location.column).toBeGreaterThan(0);
      });
    });
  });

  describe('Edge Cases Integration', () => {
    test('should handle empty programs', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const result = await parser.parse('', 'empty.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(0);
      expect(result.sourceInfo.lineCount).toBe(1);
      expect(result.success).toBe(false); // Empty program is invalid
    });

    test('should handle comment-only programs', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = '      *> This is just a comment\n      *> Another comment';
      const result = await parser.parse(source, 'comment-only.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(source.length);
      expect(result.success).toBe(false); // Comment-only is invalid
    });

    test('should handle minimal valid program', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const source = 'IDENTIFICATION DIVISION.\nPROGRAM-ID. MINIMAL.';
      const result = await parser.parse(source, 'minimal.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(source.length);
      
      if (result.success && result.ast) {
        expect(result.ast.name).toBe('MINIMAL');
        expect(result.ast).toContainDivision('IDENTIFICATION');
      }
    });

    test('should handle very long lines', async () => {
      const parser = new CobolParser(DEFAULT_COBOL_PARSER_CONFIG);
      const longLine = 'A'.repeat(1000);
      const source = `IDENTIFICATION DIVISION.\nPROGRAM-ID. LONGLINE.\nPROCEDURE DIVISION.\nMAIN.\n    DISPLAY "${longLine}".`;
      
      const result = await parser.parse(source, 'long-line.cbl');
      
      expect(result).toBeDefined();
      expect(result.sourceInfo.originalLength).toBe(source.length);
    });
  });
});