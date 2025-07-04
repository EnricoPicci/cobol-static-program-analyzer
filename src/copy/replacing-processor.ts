/**
 * REPLACING clause processing implementation
 */

import {
  ReplacingProcessor as IReplacingProcessor,
  ReplacingClause,
  ReplacingPattern,
  PseudoText,
  CopyProcessingError,
  SourceLocation
} from './types';

export class ReplacingProcessor implements IReplacingProcessor {
  applyReplacing(content: string, replacing: ReplacingClause): string {
    let result = content;
    
    // Apply each replacing pattern in order
    for (const pattern of replacing.patterns) {
      result = this.replacePattern(result, pattern);
    }
    
    return result;
  }

  validateReplacingClause(replacing: ReplacingClause): CopyProcessingError[] {
    const errors: CopyProcessingError[] = [];
    
    if (!replacing.patterns || replacing.patterns.length === 0) {
      errors.push({
        type: 'REPLACING_ERROR',
        message: 'REPLACING clause must contain at least one pattern',
        location: { line: 1, column: 1 }
      });
      return errors;
    }
    
    for (let i = 0; i < replacing.patterns.length; i++) {
      const pattern = replacing.patterns[i];
      const patternErrors = this.validatePattern(pattern, i + 1);
      errors.push(...patternErrors);
    }
    
    return errors;
  }

  private replacePattern(content: string, pattern: ReplacingPattern): string {
    const oldPattern = this.expandPseudoText(pattern.oldText);
    const newPattern = this.expandPseudoText(pattern.newText);
    
    // Handle different types of replacement
    if (pattern.oldText.type === 'pseudotext') {
      return this.replacePseudoText(content, oldPattern, newPattern);
    } else if (pattern.oldText.type === 'literal') {
      return this.replaceLiteral(content, oldPattern, newPattern);
    } else {
      return this.replaceIdentifier(content, oldPattern, newPattern);
    }
  }

  private expandPseudoText(pseudoText: PseudoText): string {
    let content = pseudoText.content;
    
    // Remove pseudo-text delimiters if present
    if (pseudoText.type === 'pseudotext') {
      content = content.replace(/^==|==$/g, '').trim();
    }
    
    // Remove quotes if it's a literal
    if (pseudoText.type === 'literal') {
      content = content.replace(/^["']|["']$/g, '');
    }
    
    return content;
  }

  private replacePseudoText(content: string, oldText: string, newText: string): string {
    // Pseudo-text replacement is more complex as it can span multiple tokens
    // For now, implement simple text replacement
    // TODO: Implement proper COBOL tokenization and pseudo-text replacement
    
    const escapedOldText = this.escapeRegex(oldText);
    const regex = new RegExp(escapedOldText, 'gi');
    
    return content.replace(regex, newText);
  }

  private replaceLiteral(content: string, oldText: string, newText: string): string {
    // Replace literal strings, considering both quoted and unquoted forms
    const variations = [
      `"${oldText}"`,
      `'${oldText}'`,
      oldText
    ];
    
    let result = content;
    for (const variation of variations) {
      const escapedVariation = this.escapeRegex(variation);
      const regex = new RegExp(`\\b${escapedVariation}\\b`, 'gi');
      result = result.replace(regex, newText);
    }
    
    return result;
  }

  private replaceIdentifier(content: string, oldText: string, newText: string): string {
    // Replace identifiers (word boundaries)
    const escapedOldText = this.escapeRegex(oldText);
    const regex = new RegExp(`\\b${escapedOldText}\\b`, 'gi');
    
    return content.replace(regex, newText);
  }

  private escapeRegex(text: string): string {
    return text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }

  private validatePattern(pattern: ReplacingPattern, index: number): CopyProcessingError[] {
    const errors: CopyProcessingError[] = [];
    const location: SourceLocation = { line: 1, column: 1 };
    
    if (!pattern.oldText || !pattern.oldText.content.trim()) {
      errors.push({
        type: 'REPLACING_ERROR',
        message: `Pattern ${index}: Old text cannot be empty`,
        location
      });
      // Don't validate pseudo-text format if content is empty
      return errors;
    }
    
    if (!pattern.newText) {
      errors.push({
        type: 'REPLACING_ERROR',
        message: `Pattern ${index}: New text is required`,
        location
      });
    }
    
    // Validate pseudo-text format only if content is not empty
    if (pattern.oldText.type === 'pseudotext') {
      if (!this.isValidPseudoText(pattern.oldText.content)) {
        errors.push({
          type: 'REPLACING_ERROR',
          message: `Pattern ${index}: Invalid pseudo-text format`,
          location
        });
      }
    }
    
    if (pattern.newText && pattern.newText.type === 'pseudotext') {
      if (!this.isValidPseudoText(pattern.newText.content)) {
        errors.push({
          type: 'REPLACING_ERROR',
          message: `Pattern ${index}: Invalid pseudo-text format in replacement`,
          location
        });
      }
    }
    
    return errors;
  }

  private isValidPseudoText(content: string): boolean {
    // Basic validation for pseudo-text format
    // Should start and end with == or be properly formatted
    return content.trim().length > 0;
  }

  // Utility method to create a replacing clause from text
  static parseReplacingClause(text: string): ReplacingClause | null {
    // Simple parser for REPLACING clause
    // Format: REPLACING ==old== BY ==new== ==old2== BY ==new2==
    
    const patterns: ReplacingPattern[] = [];
    const regex = /==([^=]*)==\s+BY\s+==([^=]*)==/gi;
    let match;
    
    while ((match = regex.exec(text)) !== null) {
      patterns.push({
        oldText: {
          content: match[1],
          type: 'pseudotext'
        },
        newText: {
          content: match[2],
          type: 'pseudotext'
        }
      });
    }
    
    return patterns.length > 0 ? { patterns } : null;
  }

  // Check if content contains replacing patterns
  static hasReplacingClause(content: string): boolean {
    return /REPLACING\s+/i.test(content);
  }
}
