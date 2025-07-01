/**
 * AST Node representing a COBOL section
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, CallReference, ComplexityMetrics, CommentInfo } from '../../core/types';
import { ParagraphNode } from './ParagraphNode';
import { StatementNode } from './StatementNode';

/**
 * Represents a COBOL section (e.g., MAIN-SECTION SECTION)
 */
export class SectionNode extends BaseASTNode {
  public readonly type = 'SectionNode';
  
  /** Section name */
  public name: string;
  
  /** Original source code of the section */
  public sourceCode: string;
  
  /** Paragraphs contained in this section */
  public paragraphs: ParagraphNode[] = [];
  
  /** Sections/paragraphs called from this section */
  public calledSectionsParagraphs: CallReference[] = [];
  
  /** Whether this is in DECLARATIVES section */
  public declaratives?: boolean;
  
  /** USE statements in declaratives */
  public useStatements?: StatementNode[];
  
  /** Section execution priority */
  public priority?: number;
  
  /** Section header location */
  public startLocation: SourceLocation;
  
  /** Section end location */
  public endLocation?: SourceLocation;
  
  /** Associated comments */
  public comments?: CommentInfo[];
  
  /** Is this the main entry point section */
  public isMainSection?: boolean;
  
  /** Error handling type for this section */
  public errorHandling?: 'AT END' | 'INVALID KEY' | 'OVERFLOW' | 'ON SIZE ERROR';
  
  /** Complexity metrics */
  public complexity?: ComplexityMetrics;
  
  /** Is this section reachable (dead code analysis) */
  public reachable?: boolean;
  
  /** Who calls this section */
  public callers?: CallReference[];

  constructor(
    name: string,
    sourceCode: string,
    location: SourceLocation,
    startLocation: SourceLocation
  ) {
    super('SectionNode', location);
    this.name = name;
    this.sourceCode = sourceCode;
    this.startLocation = startLocation;
  }

  /**
   * Add a paragraph to this section
   */
  addParagraph(paragraph: ParagraphNode): void {
    paragraph.parent = this;
    this.paragraphs.push(paragraph);
    this.addChild(paragraph);
  }

  /**
   * Remove a paragraph from this section
   */
  removeParagraph(paragraph: ParagraphNode): boolean {
    const index = this.paragraphs.indexOf(paragraph);
    if (index >= 0) {
      this.paragraphs.splice(index, 1);
      paragraph.parent = undefined;
      this.removeChild(paragraph);
      return true;
    }
    return false;
  }

  /**
   * Find a paragraph by name within this section
   */
  findParagraph(name: string): ParagraphNode | undefined {
    return this.paragraphs.find(para => para.name === name);
  }

  /**
   * Get all statements in this section
   */
  getAllStatements(): StatementNode[] {
    const statements: StatementNode[] = [];
    
    for (const paragraph of this.paragraphs) {
      statements.push(...paragraph.statements);
    }
    
    return statements;
  }

  /**
   * Add a call reference
   */
  addCallReference(reference: CallReference): void {
    this.calledSectionsParagraphs.push(reference);
  }

  /**
   * Get call references by type
   */
  getCallReferences(type?: 'section' | 'paragraph'): CallReference[] {
    if (type) {
      return this.calledSectionsParagraphs.filter(ref => ref.type === type);
    }
    return [...this.calledSectionsParagraphs];
  }

  /**
   * Check if this section calls another section/paragraph
   */
  calls(name: string): boolean {
    return this.calledSectionsParagraphs.some(ref => ref.name === name);
  }

  /**
   * Get section metrics
   */
  getMetrics(): {
    paragraphCount: number;
    statementCount: number;
    callCount: number;
    lineCount: number;
    complexity?: ComplexityMetrics;
  } {
    const statements = this.getAllStatements();
    const lineCount = this.endLocation 
      ? this.endLocation.line - this.startLocation.line + 1
      : this.sourceCode.split('\n').length;

    return {
      paragraphCount: this.paragraphs.length,
      statementCount: statements.length,
      callCount: this.calledSectionsParagraphs.length,
      lineCount,
      complexity: this.complexity
    };
  }

  /**
   * Check if section is entry point
   */
  isEntryPoint(): boolean {
    return this.isMainSection || this.callers?.length === 0;
  }

  /**
   * Get all called targets
   */
  getCalledTargets(): string[] {
    return this.calledSectionsParagraphs.map(ref => ref.name);
  }

  /**
   * Calculate cyclomatic complexity
   */
  calculateComplexity(): number {
    let complexity = 1; // Base complexity
    const statements = this.getAllStatements();
    
    for (const statement of statements) {
      // Add complexity for decision statements
      if (statement.statementType === 'IF' || 
          statement.statementType === 'EVALUATE' ||
          statement.statementType === 'PERFORM' ||
          statement.statementType === 'SEARCH') {
        complexity++;
      }
    }
    
    return complexity;
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitSectionNode) {
      return visitor.visitSectionNode(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Convert to JSON representation matching README format
   */
  toJSON(): any {
    return {
      name: this.name,
      sourceCode: this.sourceCode,
      paragraphs: this.paragraphs.map(p => p.toJSON()),
      calledSectionsParagraphs: this.calledSectionsParagraphs.map(ref => ({
        name: ref.name
      }))
    };
  }

  /**
   * Get debugging information
   */
  getDebugInfo(): string {
    const metrics = this.getMetrics();
    return `Section[${this.name}]: ${metrics.paragraphCount} paragraphs, ` +
           `${metrics.statementCount} statements, ${metrics.callCount} calls, ` +
           `complexity: ${this.calculateComplexity()}`;
  }
}