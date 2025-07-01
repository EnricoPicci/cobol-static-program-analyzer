/**
 * AST Node representing COBOL Procedure Division
 */

import { BaseASTNode, ASTVisitor } from './ASTNode';
import { SourceLocation, CallReference } from '../../core/types';
import { SectionNode } from './SectionNode';
import { ParagraphNode } from './ParagraphNode';

/**
 * Represents the PROCEDURE DIVISION
 */
export class ProcedureDivision extends BaseASTNode {
  public readonly type = 'ProcedureDivision';
  
  /** Sections in this division */
  public sections: SectionNode[] = [];
  
  /** Standalone paragraphs (not in sections) */
  public paragraphs: ParagraphNode[] = [];
  
  /** All called sections/paragraphs from this division */
  public calledSectionsParagraphs: CallReference[] = [];
  
  /** USING clause parameters */
  public usingParameters?: string[];
  
  /** RETURNING clause parameter */
  public returningParameter?: string;
  
  /** Declaratives section */
  public declarativesSection?: SectionNode[];

  constructor(location: SourceLocation) {
    super('ProcedureDivision', location);
  }

  /**
   * Add a section
   */
  addSection(section: SectionNode): void {
    section.parent = this;
    this.sections.push(section);
    this.addChild(section);
  }

  /**
   * Add a standalone paragraph
   */
  addParagraph(paragraph: ParagraphNode): void {
    paragraph.parent = this;
    this.paragraphs.push(paragraph);
    this.addChild(paragraph);
  }

  /**
   * Find section by name
   */
  findSection(name: string): SectionNode | undefined {
    return this.sections.find(section => section.name === name);
  }

  /**
   * Find paragraph by name (searches both standalone and section paragraphs)
   */
  findParagraph(name: string): ParagraphNode | undefined {
    // Check standalone paragraphs
    let paragraph = this.paragraphs.find(para => para.name === name);
    if (paragraph) return paragraph;
    
    // Check paragraphs in sections
    for (const section of this.sections) {
      paragraph = section.findParagraph(name);
      if (paragraph) return paragraph;
    }
    
    return undefined;
  }

  /**
   * Get all paragraphs (standalone + section paragraphs)
   */
  getAllParagraphs(): ParagraphNode[] {
    const allParagraphs = [...this.paragraphs];
    
    for (const section of this.sections) {
      allParagraphs.push(...section.paragraphs);
    }
    
    return allParagraphs;
  }

  /**
   * Accept visitor pattern
   */
  accept<T>(visitor: ASTVisitor<T>): T {
    if (visitor.visitProcedureDivision) {
      return visitor.visitProcedureDivision(this);
    }
    return visitor.visitNode(this);
  }

  /**
   * Convert to JSON representation matching README format
   */
  toJSON(): any {
    return {
      sections: this.sections.map(s => s.toJSON()),
      paragraphs: this.paragraphs.map(p => p.toJSON()),
      calledSectionsParagraphs: this.calledSectionsParagraphs.map(ref => ({
        name: ref.name
      }))
    };
  }
}