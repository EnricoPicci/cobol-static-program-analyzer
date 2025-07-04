/**
 * ANTLR visitor for extracting COPY statements from preprocessor parse tree
 */

import { Cobol85PreprocessorVisitor } from '../generated/preprocessor/Cobol85PreprocessorVisitor';
import { CopyStatementContext } from '../generated/preprocessor/Cobol85PreprocessorParser';
import {
  CopyStatementInfo,
  ReplacingClause,
  ReplacingPattern,
  PseudoText,
  SourceLocation
} from './types';
import { ReplacingProcessor } from './replacing-processor';

export class CopyStatementVisitor extends Cobol85PreprocessorVisitor<CopyStatementInfo[]> {
  private copyStatements: CopyStatementInfo[] = [];

  constructor() {
    super();
    this.visitStartRule = this.visitStartRuleImpl;
    this.visitCopyStatement = this.visitCopyStatementImpl;
  }

  private visitStartRuleImpl = (ctx: any): CopyStatementInfo[] => {
    this.copyStatements = [];
    this.traverseTree(ctx);
    return this.copyStatements;
  };

  private visitCopyStatementImpl = (ctx: CopyStatementContext): CopyStatementInfo[] => {
    try {
      const copyInfo = this.extractCopyInfo(ctx);
      if (copyInfo) {
        this.copyStatements.push(copyInfo);
      }
    } catch (error) {
      console.warn('Error processing COPY statement:', error);
    }
    return this.copyStatements;
  };

  private traverseTree(ctx: any): void {
    if (!ctx) return;
    
    // Check if this is a copy statement
    if (this.isCopyStatement(ctx)) {
      try {
        const copyInfo = this.extractCopyInfo(ctx);
        if (copyInfo) {
          this.copyStatements.push(copyInfo);
        }
      } catch (error) {
        console.warn('Error processing COPY statement:', error);
      }
    }
    
    // Traverse children
    if (ctx.children) {
      for (const child of ctx.children) {
        this.traverseTree(child);
      }
    }
  }

  private isCopyStatement(ctx: any): boolean {
    if (!ctx || !ctx.constructor) return false;
    const className = ctx.constructor.name;
    return className.toLowerCase().includes('copystatement');
  }

  private extractCopyInfo(ctx: any): CopyStatementInfo | null {
    try {
      const copySourceCtx = this.findChildByRuleName(ctx, 'copySource');
      if (!copySourceCtx) {
        return null;
      }

      const name = this.extractCopybookName(copySourceCtx);
      if (!name) {
        return null;
      }

      const library = this.extractLibraryName(copySourceCtx);
      const replacingClause = this.extractReplacingClause(ctx);
      const sourceLocation = this.extractSourceLocation(ctx);

      return {
        name,
        library: library || undefined,
        replacingClause: replacingClause || undefined,
        sourceLocation
      };
    } catch (error) {
      console.warn('Error extracting copy info:', error);
      return null;
    }
  }

  private extractCopybookName(copySourceCtx: any): string | null {
    try {
      // Look for literal, cobolWord, or filename
      const literal = this.findChildByRuleName(copySourceCtx, 'literal');
      if (literal) {
        return this.extractTextFromNode(literal);
      }

      const cobolWord = this.findChildByRuleName(copySourceCtx, 'cobolWord');
      if (cobolWord) {
        return this.extractTextFromNode(cobolWord);
      }

      const filename = this.findChildByRuleName(copySourceCtx, 'filename');
      if (filename) {
        return this.extractTextFromNode(filename);
      }

      return null;
    } catch (error) {
      console.warn('Error extracting copybook name:', error);
      return null;
    }
  }

  private extractLibraryName(copySourceCtx: any): string | null {
    try {
      // Look for (OF | IN) copyLibrary pattern
      const children = this.getChildren(copySourceCtx);
      
      for (let i = 0; i < children.length - 1; i++) {
        const current = children[i];
        const next = children[i + 1];
        
        if (this.isTokenType(current, ['OF', 'IN'])) {
          const copyLibrary = this.findChildByRuleName(next, 'copyLibrary');
          if (copyLibrary) {
            return this.extractTextFromNode(copyLibrary);
          }
        }
      }
      
      return null;
    } catch (error) {
      console.warn('Error extracting library name:', error);
      return null;
    }
  }

  private extractReplacingClause(ctx: any): ReplacingClause | null {
    try {
      const replacingPhraseCtx = this.findChildByRuleName(ctx, 'replacingPhrase');
      if (!replacingPhraseCtx) {
        return null;
      }

      const patterns: ReplacingPattern[] = [];
      const replaceClauses = this.findChildrenByRuleName(replacingPhraseCtx, 'replaceClause');
      
      for (const replaceClause of replaceClauses) {
        const pattern = this.extractReplacingPattern(replaceClause);
        if (pattern) {
          patterns.push(pattern);
        }
      }

      return patterns.length > 0 ? { patterns } : null;
    } catch (error) {
      console.warn('Error extracting replacing clause:', error);
      return null;
    }
  }

  private extractReplacingPattern(replaceClauseCtx: any): ReplacingPattern | null {
    try {
      const children = this.getChildren(replaceClauseCtx);
      
      // Find replaceable (old text) and replacement (new text)
      let replaceableCtx = null;
      let replacementCtx = null;
      let byFound = false;
      
      for (const child of children) {
        if (this.isTokenType(child, ['BY'])) {
          byFound = true;
        } else if (!byFound && this.isRuleName(child, 'replaceable')) {
          replaceableCtx = child;
        } else if (byFound && this.isRuleName(child, 'replacement')) {
          replacementCtx = child;
          break;
        }
      }
      
      if (!replaceableCtx || !replacementCtx) {
        return null;
      }
      
      const oldText = this.extractPseudoText(replaceableCtx);
      const newText = this.extractPseudoText(replacementCtx);
      
      if (!oldText || !newText) {
        return null;
      }
      
      return { oldText, newText };
    } catch (error) {
      console.warn('Error extracting replacing pattern:', error);
      return null;
    }
  }

  private extractPseudoText(ctx: any): PseudoText | null {
    try {
      // Check for pseudo-text
      const pseudoTextCtx = this.findChildByRuleName(ctx, 'pseudoText');
      if (pseudoTextCtx) {
        return {
          content: this.extractTextFromNode(pseudoTextCtx),
          type: 'pseudotext'
        };
      }
      
      // Check for literal
      const literalCtx = this.findChildByRuleName(ctx, 'literal');
      if (literalCtx) {
        return {
          content: this.extractTextFromNode(literalCtx),
          type: 'literal'
        };
      }
      
      // Check for cobolWord (identifier)
      const cobolWordCtx = this.findChildByRuleName(ctx, 'cobolWord');
      if (cobolWordCtx) {
        return {
          content: this.extractTextFromNode(cobolWordCtx),
          type: 'identifier'
        };
      }
      
      return null;
    } catch (error) {
      console.warn('Error extracting pseudo text:', error);
      return null;
    }
  }

  private extractSourceLocation(ctx: any): SourceLocation {
    try {
      // Try to get source location from context
      if (ctx && typeof ctx.start === 'object' && ctx.start) {
        return {
          line: ctx.start.line || 1,
          column: ctx.start.column || 1
        };
      }
      
      return { line: 1, column: 1 };
    } catch (error) {
      return { line: 1, column: 1 };
    }
  }

  // Utility methods for ANTLR tree navigation
  private findChildByRuleName(ctx: any, ruleName: string): any | null {
    if (!ctx || !ctx.children) {
      return null;
    }
    
    for (const child of ctx.children) {
      if (this.isRuleName(child, ruleName)) {
        return child;
      }
    }
    
    return null;
  }

  private findChildrenByRuleName(ctx: any, ruleName: string): any[] {
    const result: any[] = [];
    
    if (!ctx || !ctx.children) {
      return result;
    }
    
    for (const child of ctx.children) {
      if (this.isRuleName(child, ruleName)) {
        result.push(child);
      }
    }
    
    return result;
  }

  private isRuleName(ctx: any, ruleName: string): boolean {
    if (!ctx || !ctx.constructor) {
      return false;
    }
    
    const className = ctx.constructor.name;
    return className.toLowerCase().includes(ruleName.toLowerCase());
  }

  private isTokenType(ctx: any, tokenTypes: string[]): boolean {
    if (!ctx || typeof ctx.getText !== 'function') {
      return false;
    }
    
    const text = ctx.getText().toUpperCase();
    return tokenTypes.some(type => text === type.toUpperCase());
  }

  private getChildren(ctx: any): any[] {
    return ctx && ctx.children ? ctx.children : [];
  }

  private extractTextFromNode(ctx: any): string {
    if (!ctx) {
      return '';
    }
    
    if (typeof ctx.getText === 'function') {
      let text = ctx.getText();
      
      // Remove quotes from literals
      if (text.startsWith('"') && text.endsWith('"')) {
        text = text.slice(1, -1);
      } else if (text.startsWith("'") && text.endsWith("'")) {
        text = text.slice(1, -1);
      }
      
      // Remove pseudo-text delimiters
      if (text.startsWith('==') && text.endsWith('==')) {
        text = text.slice(2, -2);
      }
      
      return text.trim();
    }
    
    return '';
  }
}
