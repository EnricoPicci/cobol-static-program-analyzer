/**
 * Unit tests for ReplacingProcessor
 */

import '../setup';
import { ReplacingProcessor, ReplacingClause, ReplacingPattern } from '../../../src/copy';

describe('ReplacingProcessor', () => {
  let processor: ReplacingProcessor;

  beforeEach(() => {
    processor = new ReplacingProcessor();
  });

  describe('Basic Replacement', () => {
    test('should replace pseudo-text patterns', () => {
      const content = 'PREFIX-RECORD PIC X(10). PREFIX-NAME PIC X(20).';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'PREFIX', type: 'pseudotext' },
          newText: { content: 'CUSTOMER', type: 'pseudotext' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('CUSTOMER-RECORD');
      expect(result).toContain('CUSTOMER-NAME');
      expect(result).not.toContain('PREFIX-RECORD');
    });

    test('should replace literal patterns', () => {
      const content = 'MOVE "OLD-VALUE" TO WS-FIELD.';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'OLD-VALUE', type: 'literal' },
          newText: { content: 'NEW-VALUE', type: 'literal' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('NEW-VALUE');
      expect(result).not.toContain('OLD-VALUE');
    });

    test('should replace identifier patterns', () => {
      const content = 'MOVE OLD-VAR TO NEW-VAR. DISPLAY OLD-VAR.';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'OLD-VAR', type: 'identifier' },
          newText: { content: 'UPDATED-VAR', type: 'identifier' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('UPDATED-VAR');
      expect(result).not.toContain('OLD-VAR');
    });
  });

  describe('Multiple Patterns', () => {
    test('should apply multiple replacement patterns in order', () => {
      const content = 'PREFIX-RECORD. PREFIX-NAME. RECORD-TYPE.';
      const replacing: ReplacingClause = {
        patterns: [
          {
            oldText: { content: 'PREFIX', type: 'pseudotext' },
            newText: { content: 'CUSTOMER', type: 'pseudotext' }
          },
          {
            oldText: { content: 'RECORD', type: 'pseudotext' },
            newText: { content: 'INFO', type: 'pseudotext' }
          }
        ]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('CUSTOMER-INFO');
      expect(result).toContain('CUSTOMER-NAME');
      expect(result).toContain('INFO-TYPE');
    });

    test('should handle overlapping replacements correctly', () => {
      const content = 'TEST-RECORD-DATA TEST-FIELD.';
      const replacing: ReplacingClause = {
        patterns: [
          {
            oldText: { content: 'TEST', type: 'pseudotext' },
            newText: { content: 'PROD', type: 'pseudotext' }
          },
          {
            oldText: { content: 'RECORD', type: 'pseudotext' },
            newText: { content: 'TABLE', type: 'pseudotext' }
          }
        ]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('PROD-TABLE-DATA');
      expect(result).toContain('PROD-FIELD');
    });
  });

  describe('Edge Cases', () => {
    test('should handle empty replacement text', () => {
      const content = 'PREFIX-RECORD PREFIX-NAME';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'PREFIX-', type: 'pseudotext' },
          newText: { content: '', type: 'pseudotext' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('RECORD');
      expect(result).toContain('NAME');
      expect(result).not.toContain('PREFIX-');
    });

    test('should handle special regex characters in patterns', () => {
      const content = 'PATTERN+FIELD PATTERN*RECORD';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'PATTERN+', type: 'pseudotext' },
          newText: { content: 'DATA-', type: 'pseudotext' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      expect(result).toContain('DATA-FIELD');
    });

    test('should handle case sensitivity', () => {
      const content = 'PREFIX-Record prefix-name PREFIX-DATA';
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'PREFIX', type: 'pseudotext' },
          newText: { content: 'CUSTOMER', type: 'pseudotext' }
        }]
      };

      const result = processor.applyReplacing(content, replacing);
      
      // Should replace case-insensitively
      expect(result).toContain('CUSTOMER-Record');
      expect(result).toContain('CUSTOMER-name');
      expect(result).toContain('CUSTOMER-DATA');
    });
  });

  describe('Validation', () => {
    test('should validate valid replacing clause', () => {
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'OLD', type: 'pseudotext' },
          newText: { content: 'NEW', type: 'pseudotext' }
        }]
      };

      const errors = processor.validateReplacingClause(replacing);
      
      expect(errors).toHaveLength(0);
    });

    test('should detect empty old text', () => {
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: '', type: 'pseudotext' },
          newText: { content: 'NEW', type: 'pseudotext' }
        }]
      };

      const errors = processor.validateReplacingClause(replacing);
      
      expect(errors).toHaveLength(1);
      expect(errors[0].type).toBe('REPLACING_ERROR');
      expect(errors[0].message).toContain('Old text cannot be empty');
    });

    test('should detect missing new text', () => {
      const replacing: ReplacingClause = {
        patterns: [{
          oldText: { content: 'OLD', type: 'pseudotext' },
          newText: null as any
        }]
      };

      const errors = processor.validateReplacingClause(replacing);
      
      expect(errors).toHaveLength(1);
      expect(errors[0].type).toBe('REPLACING_ERROR');
      expect(errors[0].message).toContain('New text is required');
    });

    test('should detect empty pattern list', () => {
      const replacing: ReplacingClause = {
        patterns: []
      };

      const errors = processor.validateReplacingClause(replacing);
      
      expect(errors).toHaveLength(1);
      expect(errors[0].type).toBe('REPLACING_ERROR');
      expect(errors[0].message).toContain('must contain at least one pattern');
    });
  });

  describe('Utility Functions', () => {
    test('should parse replacing clause from text', () => {
      const text = 'REPLACING ==OLD== BY ==NEW== ==PREFIX== BY ==CUSTOMER==';
      
      const clause = ReplacingProcessor.parseReplacingClause(text);
      
      expect(clause).not.toBeNull();
      expect(clause!.patterns).toHaveLength(2);
      expect(clause!.patterns[0].oldText.content).toBe('OLD');
      expect(clause!.patterns[0].newText.content).toBe('NEW');
      expect(clause!.patterns[1].oldText.content).toBe('PREFIX');
      expect(clause!.patterns[1].newText.content).toBe('CUSTOMER');
    });

    test('should detect replacing clause in content', () => {
      const withReplacing = 'COPY TEMPLATE REPLACING ==OLD== BY ==NEW==.';
      const withoutReplacing = 'COPY TEMPLATE.';
      
      expect(ReplacingProcessor.hasReplacingClause(withReplacing)).toBe(true);
      expect(ReplacingProcessor.hasReplacingClause(withoutReplacing)).toBe(false);
    });

    test('should return null for invalid replacing text', () => {
      const invalidText = 'INVALID REPLACING TEXT';
      
      const clause = ReplacingProcessor.parseReplacingClause(invalidText);
      
      expect(clause).toBeNull();
    });
  });
});
