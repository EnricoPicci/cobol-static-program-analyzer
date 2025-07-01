/**
 * Base AST Node interface - foundation for all COBOL AST nodes
 */

import { SourceLocation, NodeMetadata, DiagnosticMessage } from '../../core/types';

/**
 * Base interface for all AST nodes in the COBOL parser
 */
export interface ASTNode {
  /** Node type identifier */
  readonly type: string;
  
  /** Source location information */
  location: SourceLocation;
  
  /** Parent node reference */
  parent?: ASTNode;
  
  /** Child nodes */
  children: ASTNode[];
  
  /** Additional metadata */
  metadata?: NodeMetadata;
  
  /** Validation errors for this node */
  errors?: DiagnosticMessage[];
  
  /** Validation warnings for this node */
  warnings?: DiagnosticMessage[];
  
  /** Whether this node has been validated */
  validated?: boolean;
  
  /** Accept a visitor */
  accept<T>(visitor: ASTVisitor<T>): T;
}

/**
 * Base class implementing common AST node functionality
 */
export abstract class BaseASTNode implements ASTNode {
  public readonly type: string;
  public location: SourceLocation;
  public parent?: ASTNode;
  public children: ASTNode[] = [];
  public metadata?: NodeMetadata;
  public errors?: DiagnosticMessage[];
  public warnings?: DiagnosticMessage[];
  public validated?: boolean;

  constructor(type: string, location: SourceLocation) {
    this.type = type;
    this.location = location;
  }

  /**
   * Add a child node
   */
  addChild(child: ASTNode): void {
    child.parent = this;
    this.children.push(child);
  }

  /**
   * Remove a child node
   */
  removeChild(child: ASTNode): boolean {
    const index = this.children.indexOf(child);
    if (index >= 0) {
      this.children.splice(index, 1);
      child.parent = undefined;
      return true;
    }
    return false;
  }

  /**
   * Find child nodes by type
   */
  findChildrenByType<T extends ASTNode>(type: string): T[] {
    return this.children.filter(child => child.type === type) as T[];
  }

  /**
   * Find first child node by type
   */
  findChildByType<T extends ASTNode>(type: string): T | undefined {
    return this.children.find(child => child.type === type) as T;
  }

  /**
   * Get all descendants of a specific type
   */
  findDescendantsByType<T extends ASTNode>(type: string): T[] {
    const results: T[] = [];
    
    for (const child of this.children) {
      if (child.type === type) {
        results.push(child as T);
      }
      
      if (child instanceof BaseASTNode) {
        results.push(...child.findDescendantsByType<T>(type));
      }
    }
    
    return results;
  }

  /**
   * Accept a visitor
   */
  abstract accept<T>(visitor: ASTVisitor<T>): T;

  /**
   * Get string representation for debugging
   */
  toString(): string {
    return `${this.type}@${this.location.line}:${this.location.column}`;
  }

  /**
   * Get the root node
   */
  getRoot(): ASTNode {
    let current: ASTNode = this;
    while (current.parent) {
      current = current.parent;
    }
    return current;
  }

  /**
   * Get the depth of this node in the tree
   */
  getDepth(): number {
    let depth = 0;
    let current: ASTNode | undefined = this.parent;
    while (current) {
      depth++;
      current = current.parent;
    }
    return depth;
  }

  /**
   * Check if this node is an ancestor of another node
   */
  isAncestorOf(node: ASTNode): boolean {
    let current: ASTNode | undefined = node.parent;
    while (current) {
      if (current === this) {
        return true;
      }
      current = current.parent;
    }
    return false;
  }

  /**
   * Get the path from root to this node
   */
  getPath(): string[] {
    const path: string[] = [];
    let current: ASTNode | undefined = this;
    
    while (current) {
      path.unshift(current.type);
      current = current.parent;
    }
    
    return path;
  }
}

/**
 * Visitor interface for traversing AST nodes
 */
export interface ASTVisitor<T> {
  visitNode(node: ASTNode): T;
  visitCobolProgram?(node: any): T;
  visitIdentificationDivision?(node: any): T;
  visitEnvironmentDivision?(node: any): T;
  visitDataDivision?(node: any): T;
  visitProcedureDivision?(node: any): T;
  visitSectionNode?(node: any): T;
  visitParagraphNode?(node: any): T;
  visitStatementNode?(node: any): T;
  visitVariableDeclaration?(node: any): T;
  visitFileDeclaration?(node: any): T;
}