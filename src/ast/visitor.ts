/**
 * Visitor pattern infrastructure for AST traversal
 */

import { ASTNode, ASTVisitor } from './nodes/ASTNode';
import { CobolProgram } from './nodes/CobolProgram';
import { IdentificationDivision } from './nodes/IdentificationDivision';
import { EnvironmentDivision } from './nodes/EnvironmentDivision';
import { DataDivision } from './nodes/DataDivision';
import { ProcedureDivision } from './nodes/ProcedureDivision';
import { SectionNode } from './nodes/SectionNode';
import { ParagraphNode } from './nodes/ParagraphNode';
import { StatementNode } from './nodes/StatementNode';

/**
 * Base visitor implementation with default traversal behavior
 */
export abstract class BaseASTVisitor<T> implements ASTVisitor<T> {
  
  /**
   * Default node visitor - override for custom behavior
   */
  visitNode(node: ASTNode): T {
    // Default behavior: visit all children
    for (const child of node.children) {
      child.accept(this);
    }
    return this.defaultResult();
  }

  /**
   * Visit COBOL program
   */
  visitCobolProgram(node: CobolProgram): T {
    // Visit all divisions
    node.identificationDivision.accept(this);
    
    if (node.environmentDivision) {
      node.environmentDivision.accept(this);
    }
    
    if (node.dataDivision) {
      node.dataDivision.accept(this);
    }
    
    if (node.procedureDivision) {
      node.procedureDivision.accept(this);
    }
    
    return this.defaultResult();
  }

  /**
   * Visit identification division
   */
  visitIdentificationDivision(node: IdentificationDivision): T {
    return this.visitNode(node);
  }

  /**
   * Visit environment division
   */
  visitEnvironmentDivision(node: EnvironmentDivision): T {
    return this.visitNode(node);
  }

  /**
   * Visit data division
   */
  visitDataDivision(node: DataDivision): T {
    return this.visitNode(node);
  }

  /**
   * Visit procedure division
   */
  visitProcedureDivision(node: ProcedureDivision): T {
    // Visit sections
    for (const section of node.sections) {
      section.accept(this);
    }
    
    // Visit standalone paragraphs
    for (const paragraph of node.paragraphs) {
      paragraph.accept(this);
    }
    
    return this.defaultResult();
  }

  /**
   * Visit section node
   */
  visitSectionNode(node: SectionNode): T {
    // Visit all paragraphs in the section
    for (const paragraph of node.paragraphs) {
      paragraph.accept(this);
    }
    
    return this.defaultResult();
  }

  /**
   * Visit paragraph node
   */
  visitParagraphNode(node: ParagraphNode): T {
    // Visit all statements in the paragraph
    for (const statement of node.statements) {
      statement.accept(this);
    }
    
    return this.defaultResult();
  }

  /**
   * Visit statement node
   */
  visitStatementNode(node: StatementNode): T {
    // Visit nested statements if any
    if (node.nestedStatements) {
      for (const nestedStatement of node.nestedStatements) {
        nestedStatement.accept(this);
      }
    }
    
    return this.defaultResult();
  }

  /**
   * Default result to return when no specific result is needed
   */
  protected abstract defaultResult(): T;
}

/**
 * Simple visitor that collects all nodes of a specific type
 */
export class NodeCollectorVisitor<T extends ASTNode> extends BaseASTVisitor<T[]> {
  private nodes: T[] = [];
  
  constructor(private nodeType: string) {
    super();
  }

  visitNode(node: ASTNode): T[] {
    if (node.type === this.nodeType) {
      this.nodes.push(node as T);
    }
    
    // Continue traversal
    super.visitNode(node);
    return this.nodes;
  }

  protected defaultResult(): T[] {
    return this.nodes;
  }

  getCollectedNodes(): T[] {
    return [...this.nodes];
  }

  clear(): void {
    this.nodes = [];
  }
}

/**
 * Visitor that builds a list of all call references
 */
export class CallReferenceVisitor extends BaseASTVisitor<string[]> {
  private calls: string[] = [];

  visitSectionNode(node: SectionNode): string[] {
    this.calls.push(...node.getCalledTargets());
    return super.visitSectionNode(node);
  }

  visitParagraphNode(node: ParagraphNode): string[] {
    this.calls.push(...node.getCalledTargets());
    return super.visitParagraphNode(node);
  }

  protected defaultResult(): string[] {
    return this.calls;
  }

  getCallReferences(): string[] {
    return [...this.calls];
  }
}

/**
 * Visitor that counts nodes by type
 */
export class NodeCounterVisitor extends BaseASTVisitor<Map<string, number>> {
  private counts = new Map<string, number>();

  visitNode(node: ASTNode): Map<string, number> {
    const current = this.counts.get(node.type) || 0;
    this.counts.set(node.type, current + 1);
    
    return super.visitNode(node);
  }

  protected defaultResult(): Map<string, number> {
    return this.counts;
  }

  getCounts(): Map<string, number> {
    return new Map(this.counts);
  }

  getCount(nodeType: string): number {
    return this.counts.get(nodeType) || 0;
  }
}

/**
 * Visitor that validates AST structure
 */
export class ValidationVisitor extends BaseASTVisitor<boolean> {
  private errors: string[] = [];

  visitCobolProgram(node: CobolProgram): boolean {
    // Validate program structure
    if (!node.identificationDivision) {
      this.errors.push('Missing IDENTIFICATION DIVISION');
    }
    
    if (!node.identificationDivision?.programId) {
      this.errors.push('Missing PROGRAM-ID');
    }
    
    return super.visitCobolProgram(node);
  }

  visitSectionNode(node: SectionNode): boolean {
    // Validate section structure
    if (!node.name) {
      this.errors.push('Section missing name');
    }
    
    return super.visitSectionNode(node);
  }

  visitParagraphNode(node: ParagraphNode): boolean {
    // Validate paragraph structure
    if (!node.name) {
      this.errors.push('Paragraph missing name');
    }
    
    return super.visitParagraphNode(node);
  }

  protected defaultResult(): boolean {
    return this.errors.length === 0;
  }

  isValid(): boolean {
    return this.errors.length === 0;
  }

  getErrors(): string[] {
    return [...this.errors];
  }

  clearErrors(): void {
    this.errors = [];
  }
}

/**
 * Utility function to traverse AST and apply visitor
 */
export function traverseAST<T>(node: ASTNode, visitor: ASTVisitor<T>): T {
  return node.accept(visitor);
}

/**
 * Utility function to find all nodes of a specific type
 */
export function findNodesByType<T extends ASTNode>(root: ASTNode, nodeType: string): T[] {
  const collector = new NodeCollectorVisitor<T>(nodeType);
  root.accept(collector);
  return collector.getCollectedNodes();
}

/**
 * Utility function to count all nodes by type
 */
export function countNodesByType(root: ASTNode): Map<string, number> {
  const counter = new NodeCounterVisitor();
  root.accept(counter);
  return counter.getCounts();
}

/**
 * Utility function to validate AST structure
 */
export function validateAST(root: ASTNode): { valid: boolean; errors: string[] } {
  const validator = new ValidationVisitor();
  const valid = root.accept(validator);
  return {
    valid: validator.isValid(),
    errors: validator.getErrors()
  };
}