/**
 * Tests for three-tier error handling framework
 */

import '../setup';
import {
  CobolErrorHandler,
  SyntaxError,
  SemanticError,
  AnalysisError,
  PreprocessingError,
  ASTConstructionError,
  ParsingError,
  ErrorRecoveryStrategy
} from '../../../src/parser/error-handler';
import { SourceLocation } from '../../../src/core/types';

describe('Error Handling Framework', () => {
  const mockLocation: SourceLocation = {
    line: 10,
    column: 5,
    endLine: 10,
    endColumn: 15,
    file: 'test.cbl'
  };

  describe('Error Classes', () => {
    describe('SyntaxError', () => {
      test('should create syntax error with location', () => {
        const error = new SyntaxError('Unexpected token', undefined, ['IDENTIFICATION'], mockLocation);
        
        expect(error.message).toBe('Unexpected token');
        expect(error.code).toBe('SYNTAX_ERROR');
        expect(error.severity).toBe('error');
        expect(error.location).toBe(mockLocation);
        expect(error.expectedTokens).toEqual(['IDENTIFICATION']);
      });

      test('should convert to diagnostic message', () => {
        const error = new SyntaxError('Missing semicolon', undefined, undefined, mockLocation);
        const diagnostic = error.toDiagnostic();
        
        expect(diagnostic.severity).toBe('error');
        expect(diagnostic.code).toBe('SYNTAX_ERROR');
        expect(diagnostic.message).toBe('Missing semicolon');
        expect(diagnostic.location).toBe(mockLocation);
      });
    });

    describe('SemanticError', () => {
      test('should create semantic error with related locations', () => {
        const relatedLocation: SourceLocation = {
          line: 15,
          column: 10,
          endLine: 15,
          endColumn: 20
        };

        const error = new SemanticError(
          'Undefined variable',
          'UNDEFINED_VARIABLE',
          mockLocation,
          [relatedLocation]
        );
        
        expect(error.message).toBe('Undefined variable');
        expect(error.code).toBe('UNDEFINED_VARIABLE');
        expect(error.relatedLocations).toEqual([relatedLocation]);
      });

      test('should include related locations in diagnostic', () => {
        const relatedLocation: SourceLocation = {
          line: 20,
          column: 1,
          endLine: 20,
          endColumn: 10
        };

        const error = new SemanticError(
          'Duplicate declaration',
          'DUPLICATE_DECLARATION',
          mockLocation,
          [relatedLocation]
        );
        
        const diagnostic = error.toDiagnostic();
        expect(diagnostic.relatedLocations).toEqual([relatedLocation]);
      });
    });

    describe('AnalysisError', () => {
      test('should create analysis error with suggestions', () => {
        const suggestions = ['Use PERFORM instead', 'Consider refactoring'];
        const error = new AnalysisError(
          'Dead code detected',
          'dead-code-analysis',
          'DEAD_CODE',
          mockLocation,
          suggestions
        );
        
        expect(error.message).toBe('Dead code detected');
        expect(error.analysisType).toBe('dead-code-analysis');
        expect(error.suggestions).toEqual(suggestions);
        expect(error.severity).toBe('warning');
      });

      test('should include suggestions in diagnostic', () => {
        const suggestions = ['Use MOVE statement', 'Initialize variable'];
        const error = new AnalysisError(
          'Uninitialized variable',
          'data-flow-analysis',
          'UNINITIALIZED_VARIABLE',
          mockLocation,
          suggestions
        );
        
        const diagnostic = error.toDiagnostic();
        expect(diagnostic.suggestions).toEqual(suggestions);
      });
    });

    describe('PreprocessingError', () => {
      test('should create preprocessing error with copybook info', () => {
        const includeChain = ['MAIN.COB', 'COPYBOOK1.CPY', 'COPYBOOK2.CPY'];
        const error = new PreprocessingError(
          'Circular dependency detected',
          'COPYBOOK2.CPY',
          includeChain,
          mockLocation
        );
        
        expect(error.message).toBe('Circular dependency detected');
        expect(error.copybookName).toBe('COPYBOOK2.CPY');
        expect(error.includeChain).toEqual(includeChain);
      });
    });

    describe('ASTConstructionError', () => {
      test('should create AST construction error with context', () => {
        const error = new ASTConstructionError(
          'Failed to create node',
          'ParagraphNode',
          'procedure division',
          mockLocation
        );
        
        expect(error.message).toBe('Failed to create node');
        expect(error.nodeType).toBe('ParagraphNode');
        expect(error.context).toBe('procedure division');
      });
    });
  });

  describe('CobolErrorHandler', () => {
    let errorHandler: CobolErrorHandler;

    beforeEach(() => {
      errorHandler = new CobolErrorHandler(5, ErrorRecoveryStrategy.COLLECT_ALL);
    });

    describe('Error Collection', () => {
      test('should collect multiple errors', () => {
        const error1 = new SyntaxError('Error 1', undefined, undefined, mockLocation);
        const error2 = new SemanticError('Error 2', 'CODE2', mockLocation);
        
        errorHandler.addError(error1);
        errorHandler.addError(error2);
        
        expect(errorHandler.getErrors()).toHaveLength(2);
        expect(errorHandler.hasErrors()).toBe(true);
      });

      test('should separate errors and warnings', () => {
        const error = new SyntaxError('Syntax error', undefined, undefined, mockLocation);
        const warning = new AnalysisError('Warning', 'analysis', 'WARNING_CODE', mockLocation);
        
        errorHandler.addError(error);
        errorHandler.addError(warning);
        
        expect(errorHandler.getErrors()).toHaveLength(1);
        expect(errorHandler.getWarnings()).toHaveLength(1);
        expect(errorHandler.hasErrors()).toBe(true);
        expect(errorHandler.hasWarnings()).toBe(true);
      });

      test('should provide summary information', () => {
        const error = new SyntaxError('Error', undefined, undefined, mockLocation);
        const warning = new AnalysisError('Warning', 'analysis', 'WARNING', mockLocation);
        
        errorHandler.addError(error);
        errorHandler.addError(warning);
        
        const summary = errorHandler.getSummary();
        expect(summary.errorCount).toBe(1);
        expect(summary.warningCount).toBe(1);
        expect(summary.hasErrors).toBe(true);
        expect(summary.hasWarnings).toBe(true);
      });
    });

    describe('Error Recovery Strategies', () => {
      test('should fail fast when configured', () => {
        const failFastHandler = new CobolErrorHandler(10, ErrorRecoveryStrategy.FAIL_FAST);
        const error = new SyntaxError('Fatal error', undefined, undefined, mockLocation);
        
        expect(() => {
          failFastHandler.addError(error);
        }).toThrow('Fatal error');
      });

      test('should collect all errors when configured', () => {
        const collectAllHandler = new CobolErrorHandler(10, ErrorRecoveryStrategy.COLLECT_ALL);
        
        for (let i = 0; i < 5; i++) {
          const error = new SyntaxError(`Error ${i}`, undefined, undefined, mockLocation);
          collectAllHandler.addError(error);
        }
        
        expect(collectAllHandler.getErrors()).toHaveLength(5);
      });

      test('should stop when max errors reached', () => {
        const limitedHandler = new CobolErrorHandler(3, ErrorRecoveryStrategy.COLLECT_ALL);
        
        // Add 3 errors (should be fine)
        for (let i = 0; i < 3; i++) {
          const error = new SyntaxError(`Error ${i}`, undefined, undefined, mockLocation);
          limitedHandler.addError(error);
        }
        
        // 4th error should trigger the limit
        const finalError = new SyntaxError('Final error', undefined, undefined, mockLocation);
        expect(() => {
          limitedHandler.addError(finalError);
        }).toThrow(/Too many errors/);
      });
    });

    describe('Convenient Error Addition Methods', () => {
      test('should add syntax errors from ANTLR data', () => {
        const mockToken = {
          line: 5,
          charPositionInLine: 10,
          text: 'INVALID'
        };
        
        errorHandler.addSyntaxError('Unexpected token', mockToken as any, 5, 10);
        
        const errors = errorHandler.getErrors();
        expect(errors).toHaveLength(1);
        expect(errors[0]).toBeInstanceOf(SyntaxError);
        expect(errors[0].location?.line).toBe(5);
        expect(errors[0].location?.column).toBe(11); // Should be +1 for 1-based indexing
      });

      test('should add semantic errors with codes', () => {
        errorHandler.addSemanticError(
          'Variable not declared',
          'UNDECLARED_VARIABLE',
          mockLocation
        );
        
        const errors = errorHandler.getErrors();
        expect(errors).toHaveLength(1);
        expect(errors[0]).toBeInstanceOf(SemanticError);
        expect(errors[0].code).toBe('UNDECLARED_VARIABLE');
      });

      test('should add analysis warnings with suggestions', () => {
        errorHandler.addAnalysisWarning(
          'Unreachable code',
          'control-flow',
          'UNREACHABLE_CODE',
          mockLocation,
          ['Remove this code', 'Add condition']
        );
        
        const warnings = errorHandler.getWarnings();
        expect(warnings).toHaveLength(1);
        expect(warnings[0]).toBeInstanceOf(AnalysisError);
        expect(warnings[0].code).toBe('UNREACHABLE_CODE');
      });
    });

    describe('Diagnostic Generation', () => {
      test('should generate all diagnostics', () => {
        const error = new SyntaxError('Syntax error', undefined, undefined, mockLocation);
        const warning = new AnalysisError('Warning', 'analysis', 'WARNING', mockLocation);
        
        errorHandler.addError(error);
        errorHandler.addError(warning);
        
        const diagnostics = errorHandler.getAllDiagnostics();
        expect(diagnostics).toHaveLength(2);
        expect(diagnostics[0].severity).toBe('error');
        expect(diagnostics[1].severity).toBe('warning');
      });

      test('should generate error report', () => {
        const error = new SyntaxError('Missing period', undefined, undefined, mockLocation);
        const warning = new AnalysisError('Unused variable', 'analysis', 'UNUSED_VAR', mockLocation);
        
        errorHandler.addError(error);
        errorHandler.addError(warning);
        
        const report = errorHandler.generateReport();
        expect(report).toContain('Errors (1)');
        expect(report).toContain('Warnings (1)');
        expect(report).toContain('Missing period');
        expect(report).toContain('Unused variable');
        expect(report).toContain('10:5'); // Location info
      });

      test('should generate clean report when no errors', () => {
        const report = errorHandler.generateReport();
        expect(report).toBe('No errors or warnings');
      });
    });

    describe('State Management', () => {
      test('should clear all errors and warnings', () => {
        const error = new SyntaxError('Error', undefined, undefined, mockLocation);
        const warning = new AnalysisError('Warning', 'analysis', 'WARNING', mockLocation);
        
        errorHandler.addError(error);
        errorHandler.addError(warning);
        
        expect(errorHandler.hasErrors()).toBe(true);
        expect(errorHandler.hasWarnings()).toBe(true);
        
        errorHandler.clear();
        
        expect(errorHandler.hasErrors()).toBe(false);
        expect(errorHandler.hasWarnings()).toBe(false);
        expect(errorHandler.getErrors()).toHaveLength(0);
        expect(errorHandler.getWarnings()).toHaveLength(0);
      });
    });
  });
});