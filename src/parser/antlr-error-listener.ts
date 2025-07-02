/**
 * ANTLR Error Listener - Tier 1 Syntax Error Handling
 * Custom error listener for comprehensive syntax error recovery
 */

import { ANTLRErrorListener, RecognitionException, Recognizer, Token, ATNConfigSet, DFA, BitSet, Parser, ParserRuleContext } from 'antlr4ng';

import { SyntaxError, CobolErrorHandler, ErrorRecoveryStrategy } from './error-handler';
import { SourceLocation } from '../core/types';

/**
 * COBOL-specific syntax error recovery strategies
 */
export enum CobolRecoveryStrategy {
  /** Skip to end of current statement */
  SKIP_TO_STATEMENT_END = 'skip_to_statement_end',
  
  /** Skip to next division */
  SKIP_TO_NEXT_DIVISION = 'skip_to_next_division',
  
  /** Skip to next section */
  SKIP_TO_NEXT_SECTION = 'skip_to_next_section',
  
  /** Skip to next paragraph */
  SKIP_TO_NEXT_PARAGRAPH = 'skip_to_next_paragraph',
  
  /** Insert missing period */
  INSERT_MISSING_PERIOD = 'insert_missing_period',
  
  /** Replace with generic identifier */
  REPLACE_WITH_GENERIC = 'replace_with_generic'
}

/**
 * Enhanced ANTLR error listener for COBOL parsing
 */
export class CobolErrorListener implements ANTLRErrorListener {
  private errorHandler: CobolErrorHandler;
  private recoveryStrategy: ErrorRecoveryStrategy;
  private maxRecoveryAttempts: number;
  private recoveryAttempts: number = 0;
  
  // Token patterns for recovery
  private readonly STATEMENT_TERMINATORS = new Set(['.', 'END-IF', 'END-PERFORM', 'END-EVALUATE', 'END-READ', 'END-WRITE']);
  private readonly DIVISION_KEYWORDS = new Set(['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']);
  private readonly SECTION_KEYWORDS = new Set(['SECTION']);
  
  constructor(
    errorHandler: CobolErrorHandler,
    recoveryStrategy: ErrorRecoveryStrategy = ErrorRecoveryStrategy.RECOVER_AND_CONTINUE,
    maxRecoveryAttempts: number = 50
  ) {
    this.errorHandler = errorHandler;
    this.recoveryStrategy = recoveryStrategy;
    this.maxRecoveryAttempts = maxRecoveryAttempts;
  }

  /**
   * Handle syntax errors
   */
  syntaxError<T extends Token>(
    recognizer: Recognizer,
    offendingSymbol: T | undefined,
    line: number,
    charPositionInLine: number,
    msg: string,
    e: RecognitionException | undefined
  ): void {
    const location: SourceLocation = {
      line,
      column: charPositionInLine + 1,
      endLine: line,
      endColumn: charPositionInLine + (offendingSymbol?.text?.length || 1) + 1,
      offset: offendingSymbol?.start,
      length: offendingSymbol?.text?.length || 1
    };

    // Create enhanced error message
    const enhancedMessage = this.createEnhancedErrorMessage(msg, offendingSymbol, recognizer);
    
    // Create syntax error with recovery information
    const syntaxError = new SyntaxError(
      enhancedMessage,
      offendingSymbol,
      this.extractExpectedTokens(recognizer, e),
      location
    );

    // Add to error handler
    this.errorHandler.addError(syntaxError);

    // Attempt recovery if enabled
    if (this.recoveryStrategy !== ErrorRecoveryStrategy.FAIL_FAST && 
        this.recoveryAttempts < this.maxRecoveryAttempts) {
      this.attemptRecovery(recognizer, offendingSymbol, e);
    }
  }

  /**
   * Handle ambiguity warnings
   */
  reportAmbiguity(
    recognizer: Parser,
    dfa: DFA,
    startIndex: number,
    stopIndex: number,
    exact: boolean,
    ambigAlts: BitSet | undefined,
    configs: ATNConfigSet
  ): void {
    // Report ambiguity as warning
    const location: SourceLocation = {
      line: 1, // Would need to extract from recognizer context
      column: 1,
      endLine: 1,
      endColumn: 1
    };

    this.errorHandler.addAnalysisWarning(
      `Grammar ambiguity detected between indices ${startIndex} and ${stopIndex}`,
      'grammar-ambiguity',
      'GRAMMAR_AMBIGUITY',
      location,
      ['Review grammar rules for potential conflicts']
    );
  }

  /**
   * Handle attempted full context parsing
   */
  reportAttemptingFullContext(
    recognizer: Parser,
    dfa: DFA,
    startIndex: number,
    stopIndex: number,
    conflictingAlts: BitSet | undefined,
    configs: ATNConfigSet
  ): void {
    // This is typically not an error, just a performance notice
    // Could be logged for debugging purposes
  }

  /**
   * Handle context sensitivity
   */
  reportContextSensitivity(
    recognizer: Parser,
    dfa: DFA,
    startIndex: number,
    stopIndex: number,
    prediction: number,
    configs: ATNConfigSet
  ): void {
    // Context sensitivity is usually handled automatically by ANTLR
    // Could be logged for advanced debugging
  }

  /**
   * Create enhanced error message with context
   */
  private createEnhancedErrorMessage(
    originalMessage: string,
    offendingSymbol: Token | undefined,
    recognizer: Recognizer
  ): string {
    let enhancedMessage = originalMessage;

    if (offendingSymbol) {
      const tokenText = offendingSymbol.text;
      const tokenType = this.getTokenTypeName(recognizer, offendingSymbol.type);
      
      // Add context-specific suggestions
      const suggestions = this.generateErrorSuggestions(tokenText, tokenType, originalMessage);
      
      if (suggestions.length > 0) {
        enhancedMessage += ` Suggestions: ${suggestions.join(', ')}`;
      }

      // Add COBOL-specific context
      const cobolContext = this.getCobolContext(tokenText, originalMessage);
      if (cobolContext) {
        enhancedMessage += ` Context: ${cobolContext}`;
      }
    }

    return enhancedMessage;
  }

  /**
   * Generate COBOL-specific error suggestions
   */
  private generateErrorSuggestions(tokenText: string | undefined, tokenType: string, originalMessage: string): string[] {
    const suggestions: string[] = [];

    if (!tokenText) return suggestions;

    const upperTokenText = tokenText.toUpperCase();
    const lowerMessage = originalMessage.toLowerCase();

    // Missing period suggestions
    if (lowerMessage.includes('expecting') && lowerMessage.includes("'.'")) {
      suggestions.push('Add a period (.) to end the statement');
    }

    // Division keyword suggestions
    if (this.DIVISION_KEYWORDS.has(upperTokenText) && lowerMessage.includes('expecting')) {
      suggestions.push('Ensure DIVISION keyword is followed by DIVISION');
    }

    // Section suggestions
    if (upperTokenText.endsWith('-SECTION') || lowerMessage.includes('section')) {
      suggestions.push('Check section name spelling and format');
    }

    // Identifier suggestions
    if (lowerMessage.includes('identifier') || lowerMessage.includes('name')) {
      if (upperTokenText.includes('-')) {
        suggestions.push('COBOL identifiers can contain hyphens');
      }
      if (upperTokenText.length > 30) {
        suggestions.push('COBOL identifiers cannot exceed 30 characters');
      }
      if (!/^[A-Z]/.test(upperTokenText)) {
        suggestions.push('COBOL identifiers must start with a letter');
      }
    }

    // Statement-specific suggestions
    if (upperTokenText === 'MOVE' && lowerMessage.includes('expecting')) {
      suggestions.push('MOVE statement syntax: MOVE source TO destination');
    }

    if (upperTokenText === 'PERFORM' && lowerMessage.includes('expecting')) {
      suggestions.push('PERFORM statement syntax: PERFORM paragraph-name');
    }

    // Picture clause suggestions
    if (upperTokenText.startsWith('PIC') || lowerMessage.includes('picture')) {
      suggestions.push('Check PICTURE clause format (e.g., PIC X(10), PIC 9(5)V99)');
    }

    return suggestions;
  }

  /**
   * Get COBOL-specific context information
   */
  private getCobolContext(tokenText: string | undefined, originalMessage: string): string | undefined {
    if (!tokenText) return undefined;

    const upperTokenText = tokenText.toUpperCase();

    // Division context
    if (this.DIVISION_KEYWORDS.has(upperTokenText)) {
      return 'Within COBOL division declaration';
    }

    // Data division context
    if (['01', '02', '03', '04', '05', '06', '07', '08', '09'].some(level => upperTokenText.startsWith(level))) {
      return 'Within data item declaration';
    }

    // Procedure division context
    if (upperTokenText.includes('SECTION') || upperTokenText.includes('PARAGRAPH')) {
      return 'Within procedure division structure';
    }

    return undefined;
  }

  /**
   * Extract expected tokens from recognition exception
   */
  private extractExpectedTokens(recognizer: Recognizer, e: RecognitionException | undefined): string[] {
    const expectedTokens: string[] = [];

    if (e && 'getExpectedTokens' in e && e.getExpectedTokens) {
      // Extract expected token names
      const expectedTokensSet = e.getExpectedTokens();
      if (expectedTokensSet && expectedTokensSet.toArray) {
        const expectedTokensArray = expectedTokensSet.toArray();
        for (let i = 0; i < expectedTokensArray.length; i++) {
          const tokenType = expectedTokensArray[i];
        const tokenName = this.getTokenTypeName(recognizer, tokenType);
        if (tokenName) {
          expectedTokens.push(tokenName);
          }
        }
      }
    }

    return expectedTokens;
  }

  /**
   * Get token type name from recognizer
   */
  private getTokenTypeName(recognizer: Recognizer, tokenType: number): string {
    if (recognizer.vocabulary) {
      const symbolicName = recognizer.vocabulary.getSymbolicName(tokenType);
      const literalName = recognizer.vocabulary.getLiteralName(tokenType);
      return symbolicName || literalName || `TOKEN_${tokenType}`;
    }
    return `TOKEN_${tokenType}`;
  }

  /**
   * Attempt error recovery
   */
  private attemptRecovery(
    recognizer: Recognizer,
    offendingSymbol: Token | undefined,
    e: RecognitionException | undefined
  ): void {
    this.recoveryAttempts++;

    if (!(recognizer instanceof Parser)) {
      return; // Can only recover from parser errors
    }

    const parser = recognizer as Parser;
    const strategy = this.determineRecoveryStrategy(offendingSymbol, e);

    switch (strategy) {
      case CobolRecoveryStrategy.INSERT_MISSING_PERIOD:
        this.insertMissingPeriod(parser);
        break;
        
      case CobolRecoveryStrategy.SKIP_TO_STATEMENT_END:
        this.skipToStatementEnd(parser);
        break;
        
      case CobolRecoveryStrategy.SKIP_TO_NEXT_DIVISION:
        this.skipToNextDivision(parser);
        break;
        
      case CobolRecoveryStrategy.SKIP_TO_NEXT_SECTION:
        this.skipToNextSection(parser);
        break;
        
      case CobolRecoveryStrategy.SKIP_TO_NEXT_PARAGRAPH:
        this.skipToNextParagraph(parser);
        break;
        
      default:
        // Default recovery - consume the token and continue
        parser.consume();
        break;
    }
  }

  /**
   * Determine appropriate recovery strategy
   */
  private determineRecoveryStrategy(
    offendingSymbol: Token | undefined,
    e: RecognitionException | undefined
  ): CobolRecoveryStrategy {
    if (!offendingSymbol) {
      return CobolRecoveryStrategy.SKIP_TO_STATEMENT_END;
    }

    const tokenText = offendingSymbol.text?.toUpperCase() || '';

    // If we're expecting a period, try to insert one
    if (e && e.message && e.message.includes("expecting '.'")) {
      return CobolRecoveryStrategy.INSERT_MISSING_PERIOD;
    }

    // If we hit a division keyword, skip to next division
    if (this.DIVISION_KEYWORDS.has(tokenText)) {
      return CobolRecoveryStrategy.SKIP_TO_NEXT_DIVISION;
    }

    // If we hit section keyword, skip to next section
    if (tokenText.includes('SECTION')) {
      return CobolRecoveryStrategy.SKIP_TO_NEXT_SECTION;
    }

    // Default to skipping to statement end
    return CobolRecoveryStrategy.SKIP_TO_STATEMENT_END;
  }

  /**
   * Insert missing period recovery
   */
  private insertMissingPeriod(parser: Parser): void {
    // This would create a virtual period token
    // Implementation depends on specific parser structure
    // For now, just consume and continue
    parser.consume();
  }

  /**
   * Skip to statement end recovery
   */
  private skipToStatementEnd(parser: Parser): void {
    while (parser.getCurrentToken() && parser.getCurrentToken().type !== -1) {
      const token = parser.getCurrentToken();
      if (token && this.STATEMENT_TERMINATORS.has(token.text?.toUpperCase() || '')) {
        break;
      }
      parser.consume();
    }
  }

  /**
   * Skip to next division recovery
   */
  private skipToNextDivision(parser: Parser): void {
    while (parser.getCurrentToken() && parser.getCurrentToken().type !== -1) {
      const token = parser.getCurrentToken();
      if (token && this.DIVISION_KEYWORDS.has(token.text?.toUpperCase() || '')) {
        break;
      }
      parser.consume();
    }
  }

  /**
   * Skip to next section recovery
   */
  private skipToNextSection(parser: Parser): void {
    while (parser.getCurrentToken() && parser.getCurrentToken().type !== -1) {
      const token = parser.getCurrentToken();
      if (token && token.text?.toUpperCase().includes('SECTION')) {
        break;
      }
      parser.consume();
    }
  }

  /**
   * Skip to next paragraph recovery
   */
  private skipToNextParagraph(parser: Parser): void {
    let foundPeriod = false;
    while (parser.getCurrentToken() && parser.getCurrentToken().type !== -1) {
      const token = parser.getCurrentToken();
      if (token?.text === '.') {
        foundPeriod = true;
      } else if (foundPeriod && token && /^[A-Z][A-Z0-9-]*$/.test(token.text?.toUpperCase() || '')) {
        // Found potential paragraph name after period
        break;
      }
      parser.consume();
    }
  }

  /**
   * Get recovery statistics
   */
  getRecoveryStats(): {
    attempts: number;
    maxAttempts: number;
    strategy: ErrorRecoveryStrategy;
  } {
    return {
      attempts: this.recoveryAttempts,
      maxAttempts: this.maxRecoveryAttempts,
      strategy: this.recoveryStrategy
    };
  }

  /**
   * Reset recovery state
   */
  reset(): void {
    this.recoveryAttempts = 0;
  }
}