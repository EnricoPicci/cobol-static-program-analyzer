/**
 * Simple COBOL AST Parser
 * 
 * This parser creates a tree representation of a COBOL program using the AST node interfaces.
 * It parses COBOL source code and returns a ProgramNode as the root of the AST.
 */

import {
  ProgramNode,
  DivisionNode,
  SectionNode,
  ParagraphNode,
  StatementNode,
  IdentificationDivisionNode,
  EnvironmentDivisionNode,
  DataDivisionNode,
  ProcedureDivisionNode,
  Position,
  SourceRange,
  SourceCode,
  DataFlowInfo,
  BaseNode
} from './nodes';

/**
 * Token types for lexical analysis
 */
export enum TokenType {
  KEYWORD = 'KEYWORD',
  IDENTIFIER = 'IDENTIFIER',
  LITERAL = 'LITERAL',
  OPERATOR = 'OPERATOR',
  PUNCTUATION = 'PUNCTUATION',
  COMMENT = 'COMMENT',
  NEWLINE = 'NEWLINE',
  EOF = 'EOF',
  DOT = 'DOT',
  DIVISION = 'DIVISION',
  SECTION = 'SECTION'
}

/**
 * Represents a token in the source code
 */
export interface Token {
  type: TokenType;
  value: string;
  position: Position;
  length: number;
}

/**
 * COBOL keywords for parsing
 */
const COBOL_KEYWORDS = new Set([
  'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
  'DIVISION', 'SECTION', 'PARAGRAPH',
  'PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'DATE-COMPILED',
  'CONFIGURATION', 'INPUT-OUTPUT', 'FILE-CONTROL', 'I-O-CONTROL',
  'FILE', 'WORKING-STORAGE', 'LINKAGE', 'LOCAL-STORAGE',
  'MOVE', 'PERFORM', 'CALL', 'IF', 'ELSE', 'END-IF',
  'EVALUATE', 'WHEN', 'OTHER', 'GO', 'TO', 'STOP', 'RUN',
  'DISPLAY', 'ACCEPT', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
  'COMPUTE', 'OPEN', 'CLOSE', 'READ', 'WRITE', 'PIC', 'VALUE',
  'MAIN-PARAGRAPH', 'CALCULATION-PARA'
]);

/**
 * Simple lexer for tokenizing COBOL source code
 */
export class CobolLexer {
  private source: string;
  private position: number = 0;
  private line: number = 1;
  private column: number = 1;

  constructor(source: string) {
    this.source = source;
  }

  /**
   * Tokenize the entire source code
   */
  tokenize(): Token[] {
    const tokens: Token[] = [];
    
    while (this.position < this.source.length) {
      const token = this.nextToken();
      if (token) {
        tokens.push(token);
      }
    }

    // Add EOF token
    tokens.push({
      type: TokenType.EOF,
      value: '',
      position: this.getCurrentPosition(),
      length: 0
    });

    return tokens;
  }

  private nextToken(): Token | null {
    this.skipWhitespace();

    if (this.position >= this.source.length) {
      return null;
    }

    const startPosition = this.getCurrentPosition();
    const char = this.source[this.position];

    // Handle comments (lines starting with *)
    if (char === '*' && this.column === 1) {
      return this.readComment(startPosition);
    }

    // Handle newlines
    if (char === '\n' || char === '\r') {
      return this.readNewline(startPosition);
    }

    // Handle dots (period)
    if (char === '.') {
      this.advance();
      return {
        type: TokenType.DOT,
        value: '.',
        position: startPosition,
        length: 1
      };
    }

    // Handle operators and punctuation
    if (this.isOperatorOrPunctuation(char)) {
      return this.readOperatorOrPunctuation(startPosition);
    }

    // Handle string literals
    if (char === '"' || char === "'") {
      return this.readStringLiteral(startPosition);
    }

    // Handle numbers
    if (this.isDigit(char)) {
      return this.readNumber(startPosition);
    }

    // Handle identifiers and keywords
    if (this.isAlpha(char) || char === '-') {
      return this.readIdentifierOrKeyword(startPosition);
    }

    // Skip unknown characters
    this.advance();
    return null;
  }

  private readComment(startPosition: Position): Token {
    let value = '';
    while (this.position < this.source.length && this.source[this.position] !== '\n') {
      value += this.source[this.position];
      this.advance();
    }
    
    return {
      type: TokenType.COMMENT,
      value,
      position: startPosition,
      length: value.length
    };
  }

  private readNewline(startPosition: Position): Token {
    const char = this.source[this.position];
    this.advance();
    
    // Handle CRLF
    if (char === '\r' && this.position < this.source.length && this.source[this.position] === '\n') {
      this.advance();
      return {
        type: TokenType.NEWLINE,
        value: '\r\n',
        position: startPosition,
        length: 2
      };
    }
    
    return {
      type: TokenType.NEWLINE,
      value: char,
      position: startPosition,
      length: 1
    };
  }

  private readStringLiteral(startPosition: Position): Token {
    const quote = this.source[this.position];
    let value = quote;
    this.advance();

    while (this.position < this.source.length) {
      const char = this.source[this.position];
      value += char;
      this.advance();

      if (char === quote) {
        break;
      }
    }

    return {
      type: TokenType.LITERAL,
      value,
      position: startPosition,
      length: value.length
    };
  }

  private readNumber(startPosition: Position): Token {
    let value = '';
    
    while (this.position < this.source.length && (this.isDigit(this.source[this.position]) || this.source[this.position] === '.')) {
      value += this.source[this.position];
      this.advance();
    }

    return {
      type: TokenType.LITERAL,
      value,
      position: startPosition,
      length: value.length
    };
  }

  private readIdentifierOrKeyword(startPosition: Position): Token {
    let value = '';
    
    while (this.position < this.source.length && 
           (this.isAlphaNumeric(this.source[this.position]) || this.source[this.position] === '-')) {
      value += this.source[this.position];
      this.advance();
    }

    const upperValue = value.toUpperCase();
    const type = COBOL_KEYWORDS.has(upperValue) ? TokenType.KEYWORD : TokenType.IDENTIFIER;

    return {
      type,
      value: upperValue,
      position: startPosition,
      length: value.length
    };
  }

  private readOperatorOrPunctuation(startPosition: Position): Token {
    const char = this.source[this.position];
    this.advance();

    return {
      type: TokenType.PUNCTUATION,
      value: char,
      position: startPosition,
      length: 1
    };
  }

  private skipWhitespace(): void {
    while (this.position < this.source.length) {
      const char = this.source[this.position];
      if (char === ' ' || char === '\t') {
        this.advance();
      } else {
        break;
      }
    }
  }

  private advance(): void {
    if (this.position < this.source.length) {
      if (this.source[this.position] === '\n') {
        this.line++;
        this.column = 1;
      } else {
        this.column++;
      }
      this.position++;
    }
  }

  private getCurrentPosition(): Position {
    return {
      line: this.line,
      column: this.column,
      offset: this.position
    };
  }

  private isAlpha(char: string): boolean {
    return /[a-zA-Z]/.test(char);
  }

  private isDigit(char: string): boolean {
    return /[0-9]/.test(char);
  }

  private isAlphaNumeric(char: string): boolean {
    return /[a-zA-Z0-9]/.test(char);
  }

  private isOperatorOrPunctuation(char: string): boolean {
    return /[+\-*/=<>(),:;]/.test(char);
  }
}

/**
 * Simple COBOL AST Parser
 */
export class CobolParser {
  private tokens: Token[];
  private position: number = 0;
  private source: string;

  constructor(source: string) {
    this.source = source;
    const lexer = new CobolLexer(source);
    this.tokens = lexer.tokenize().filter(token => 
      token.type !== TokenType.COMMENT && token.type !== TokenType.NEWLINE
    );
  }

  /**
   * Parse the COBOL source code and return the AST
   */
  parse(): ProgramNode {
    const programNode = this.parseProgram();
    return programNode;
  }

  private parseProgram(): ProgramNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    const divisions: DivisionNode[] = [];
    let programName = 'UNKNOWN';

    // Parse divisions
    while (!this.isAtEnd()) {
      const currentToken = this.getCurrentToken();
      
      if (this.checkKeyword('IDENTIFICATION')) {
        const identDivision = this.parseIdentificationDivision();
        divisions.push(identDivision);
        programName = identDivision.programId;
      } else if (this.checkKeyword('ENVIRONMENT')) {
        divisions.push(this.parseEnvironmentDivision());
      } else if (this.checkKeyword('DATA')) {
        divisions.push(this.parseDataDivision());
      } else if (this.checkKeyword('PROCEDURE')) {
        divisions.push(this.parseProcedureDivision());
      } else {
        this.advance(); // Skip unknown tokens
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;
    
    return {
      type: 'Program',
      programName,
      divisions,
      identificationDivision: divisions.find(d => d.type === 'IdentificationDivision') as IdentificationDivisionNode,
      environmentDivision: divisions.find(d => d.type === 'EnvironmentDivision') as EnvironmentDivisionNode,
      dataDivision: divisions.find(d => d.type === 'DataDivision') as DataDivisionNode,
      procedureDivision: divisions.find(d => d.type === 'ProcedureDivision') as ProcedureDivisionNode,
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: divisions
    };
  }

  private parseIdentificationDivision(): IdentificationDivisionNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    this.consume('IDENTIFICATION', 'Expected IDENTIFICATION');
    this.consume('DIVISION', 'Expected DIVISION');
    this.consume('.', 'Expected period after DIVISION');

    let programId = 'UNKNOWN';
    let author: string | undefined;
    let dateWritten: string | undefined;

    // Parse PROGRAM-ID
    if (this.checkKeyword('PROGRAM-ID')) {
      this.advance(); // consume PROGRAM-ID
      this.consume('.', 'Expected period after PROGRAM-ID');
      if (this.check(TokenType.IDENTIFIER)) {
        programId = this.advance().value;
        this.consume('.', 'Expected period after program name');
      }
    }

    // Parse optional paragraphs
    while (!this.isAtEnd() && !this.checkKeyword('ENVIRONMENT') && !this.checkKeyword('DATA') && !this.checkKeyword('PROCEDURE')) {
      if (this.checkKeyword('AUTHOR')) {
        this.advance(); // consume AUTHOR
        this.consume('.', 'Expected period after AUTHOR');
        if (this.check(TokenType.IDENTIFIER)) {
          author = this.advance().value;
        }
      } else if (this.checkKeyword('DATE-WRITTEN')) {
        this.advance(); // consume DATE-WRITTEN
        this.consume('.', 'Expected period after DATE-WRITTEN');
        if (this.check(TokenType.IDENTIFIER) || this.check(TokenType.LITERAL)) {
          dateWritten = this.advance().value;
        }
      } else {
        this.advance(); // Skip unknown tokens
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'IdentificationDivision',
      divisionType: 'Identification',
      programId,
      author,
      dateWritten,
      sections: [],
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: []
    };
  }

  private parseEnvironmentDivision(): EnvironmentDivisionNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    this.consume('ENVIRONMENT', 'Expected ENVIRONMENT');
    this.consume('DIVISION', 'Expected DIVISION');
    this.consume('.', 'Expected period after DIVISION');

    const sections: SectionNode[] = [];

    // Parse sections
    while (!this.isAtEnd() && !this.checkKeyword('DATA') && !this.checkKeyword('PROCEDURE')) {
      if (this.checkKeyword('CONFIGURATION')) {
        this.advance(); // consume CONFIGURATION
        sections.push(this.parseSection('CONFIGURATION'));
      } else if (this.checkKeyword('INPUT-OUTPUT')) {
        this.advance(); // consume INPUT-OUTPUT
        sections.push(this.parseSection('INPUT-OUTPUT'));
      } else {
        this.advance();
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'EnvironmentDivision',
      divisionType: 'Environment',
      sections,
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: sections
    };
  }

  private parseDataDivision(): DataDivisionNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    this.consume('DATA', 'Expected DATA');
    this.consume('DIVISION', 'Expected DIVISION');
    this.consume('.', 'Expected period after DIVISION');

    const sections: SectionNode[] = [];

    // Parse sections
    while (!this.isAtEnd() && !this.checkKeyword('PROCEDURE')) {
      if (this.checkKeyword('FILE')) {
        this.advance(); // consume FILE
        sections.push(this.parseSection('FILE'));
      } else if (this.checkKeyword('WORKING-STORAGE')) {
        this.advance(); // consume WORKING-STORAGE
        sections.push(this.parseSection('WORKING-STORAGE'));
      } else if (this.checkKeyword('LINKAGE')) {
        this.advance(); // consume LINKAGE
        sections.push(this.parseSection('LINKAGE'));
      } else if (this.checkKeyword('LOCAL-STORAGE')) {
        this.advance(); // consume LOCAL-STORAGE
        sections.push(this.parseSection('LOCAL-STORAGE'));
      } else {
        this.advance();
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'DataDivision',
      divisionType: 'Data',
      sections,
      dataItems: [],
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: sections
    };
  }

  private parseProcedureDivision(): ProcedureDivisionNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    this.consume('PROCEDURE', 'Expected PROCEDURE');
    this.consume('DIVISION', 'Expected DIVISION');
    this.consume('.', 'Expected period after DIVISION');

    const sections: SectionNode[] = [];
    const paragraphs: ParagraphNode[] = [];
    const statements: StatementNode[] = [];

    // Parse sections and paragraphs
    while (!this.isAtEnd()) {
      if (this.check(TokenType.IDENTIFIER) && this.peekNext()?.value === 'SECTION') {
        const sectionName = this.advance().value; // consume section name
        sections.push(this.parseSection(sectionName));
      } else if ((this.check(TokenType.IDENTIFIER) || this.check(TokenType.KEYWORD)) && this.peekNext()?.value === '.') {
        paragraphs.push(this.parseParagraph());
      } else if (this.checkKeyword('MOVE') || this.checkKeyword('PERFORM') || 
                 this.checkKeyword('CALL') || this.checkKeyword('IF') ||
                 this.checkKeyword('DISPLAY') || this.checkKeyword('STOP')) {
        statements.push(this.parseStatement());
      } else {
        this.advance();
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'ProcedureDivision',
      divisionType: 'Procedure',
      sections,
      paragraphs,
      statements,
      dataFlowInfo: this.createEmptyDataFlowInfo(),
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: [...sections, ...paragraphs, ...statements]
    };
  }

  private parseSection(sectionName: string): SectionNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    this.consume('SECTION', 'Expected SECTION');
    this.consume('.', 'Expected period after SECTION');

    const paragraphs: ParagraphNode[] = [];

    // Parse paragraphs within the section
    while (!this.isAtEnd() && 
           !this.checkKeyword('SECTION') && 
           !this.checkKeyword('DIVISION') &&
           !this.checkKeyword('PROCEDURE') &&
           !this.checkKeyword('DATA') &&
           !this.checkKeyword('ENVIRONMENT') &&
           !this.checkKeyword('IDENTIFICATION')) {
      
      if (this.check(TokenType.IDENTIFIER) && this.peekNext()?.value === '.') {
        paragraphs.push(this.parseParagraph());
      } else {
        this.advance();
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'Section',
      sectionName,
      paragraphs,
      dataFlowInfo: this.createEmptyDataFlowInfo(),
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: paragraphs
    };
  }

  private parseParagraph(): ParagraphNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    
    let paragraphName: string;
    if (this.check(TokenType.IDENTIFIER)) {
      paragraphName = this.consume(TokenType.IDENTIFIER, 'Expected paragraph name').value;
    } else if (this.check(TokenType.KEYWORD)) {
      paragraphName = this.advance().value; // consume the keyword token
    } else {
      throw new Error('Expected paragraph name (identifier or keyword)');
    }
    this.consume('.', 'Expected period after paragraph name');

    const statements: StatementNode[] = [];

    // Parse statements within the paragraph
    while (!this.isAtEnd() && 
           !((this.check(TokenType.IDENTIFIER) || this.check(TokenType.KEYWORD)) && this.peekNext()?.value === '.') &&
           !this.checkKeyword('SECTION') && 
           !this.checkKeyword('DIVISION') &&
           !this.checkKeyword('PROCEDURE') &&
           !this.checkKeyword('DATA') &&
           !this.checkKeyword('ENVIRONMENT') &&
           !this.checkKeyword('IDENTIFICATION')) {
      
      if (this.checkKeyword('MOVE') || this.checkKeyword('PERFORM') || 
          this.checkKeyword('CALL') || this.checkKeyword('IF') ||
          this.checkKeyword('DISPLAY') || this.checkKeyword('STOP')) {
        statements.push(this.parseStatement());
      } else {
        this.advance();
      }
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'Paragraph',
      paragraphName,
      statements,
      isEntryPoint: false,
      dataFlowInfo: this.createEmptyDataFlowInfo(),
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: statements
    };
  }

  private parseStatement(): StatementNode {
    const startPosition = this.getCurrentToken()?.position || { line: 1, column: 1, offset: 0 };
    let statementType = 'Unknown';

    if (this.checkKeyword('MOVE')) {
      statementType = 'Move';
      this.advance(); // consume MOVE
    } else if (this.checkKeyword('PERFORM')) {
      statementType = 'Perform';
      this.advance(); // consume PERFORM
    } else if (this.checkKeyword('CALL')) {
      statementType = 'Call';
      this.advance(); // consume CALL
    } else if (this.checkKeyword('IF')) {
      statementType = 'If';
      this.advance(); // consume IF
    } else if (this.checkKeyword('DISPLAY')) {
      statementType = 'Display';
      this.advance(); // consume DISPLAY
    } else if (this.checkKeyword('STOP')) {
      statementType = 'Stop';
      this.advance(); // consume STOP
    }

    // Skip tokens until we find a period or end of statement
    while (!this.isAtEnd() && !this.check(TokenType.DOT)) {
      this.advance();
    }

    if (this.check(TokenType.DOT)) {
      this.advance(); // consume the period
    }

    const endPosition = this.getPreviousToken()?.position || startPosition;

    return {
      type: 'Statement',
      statementType,
      dataFlowInfo: this.createEmptyDataFlowInfo(),
      location: { start: startPosition, end: endPosition },
      sourceCode: this.createSourceCode(startPosition, endPosition),
      children: []
    };
  }

  // Utility methods
  private match(...keywords: string[]): boolean {
    for (const keyword of keywords) {
      if (this.check(TokenType.KEYWORD) && this.getCurrentToken()?.value === keyword) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.getCurrentToken()?.type === type;
  }

  private checkKeyword(keyword: string): boolean {
    if (this.isAtEnd()) return false;
    const token = this.getCurrentToken();
    return token?.type === TokenType.KEYWORD && token.value === keyword;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.position++;
    return this.getPreviousToken()!;
  }

  private isAtEnd(): boolean {
    return this.getCurrentToken()?.type === TokenType.EOF;
  }

  private getCurrentToken(): Token | undefined {
    return this.tokens[this.position];
  }

  private getPreviousToken(): Token | undefined {
    return this.tokens[this.position - 1];
  }

  private peekNext(): Token | undefined {
    return this.tokens[this.position + 1];
  }

  private consume(expected: string | TokenType, message: string): Token {
    const current = this.getCurrentToken();
    
    if (typeof expected === 'string') {
      if (current?.value === expected || (current?.type === TokenType.KEYWORD && current.value === expected)) {
        return this.advance();
      }
    } else {
      if (current?.type === expected) {
        return this.advance();
      }
    }

    throw new Error(`${message}. Got: ${current?.value || 'EOF'} at line ${current?.position.line || 0}`);
  }

  private createSourceCode(start: Position, end: Position): SourceCode {
    const startOffset = start.offset;
    const endOffset = end.offset;
    const text = this.source.substring(startOffset, endOffset);
    
    return {
      text,
      range: { start, end }
    };
  }

  private createEmptyDataFlowInfo(): DataFlowInfo {
    return {
      readVariables: [],
      writtenVariables: [],
      referencedVariables: [],
      modifiedVariables: [],
      calledParagraphs: [],
      calledPrograms: []
    };
  }
}

/**
 * Main parsing function - creates a COBOL AST from source code
 */
export function parseCobol(source: string): ProgramNode {
  const parser = new CobolParser(source);
  return parser.parse();
}
