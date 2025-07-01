/**
 * Core type definitions for the COBOL Static Program Analyzer
 */

/**
 * Source location information for AST nodes
 */
export interface SourceLocation {
  /** Starting line number (1-based) */
  line: number;
  /** Starting column number (1-based) */
  column: number;
  /** Ending line number (1-based) */
  endLine: number;
  /** Ending column number (1-based) */
  endColumn: number;
  /** Source file path */
  file?: string;
  /** Character offset from start of file */
  offset?: number;
  /** Length in characters */
  length?: number;
}

/**
 * Node metadata for additional AST information
 */
export interface NodeMetadata {
  /** Original source text */
  sourceText?: string;
  /** Comments associated with this node */
  comments?: CommentInfo[];
  /** Whether this node was generated during preprocessing */
  generated?: boolean;
  /** Copybook information if node came from COPY statement */
  copybook?: CopybookInfo;
  /** Parsing context information */
  parseContext?: string;
}

/**
 * Comment information
 */
export interface CommentInfo {
  /** Comment text */
  text: string;
  /** Comment location */
  location: SourceLocation;
  /** Comment type */
  type: 'line' | 'inline' | 'page-eject' | 'debug';
}

/**
 * Copybook reference information
 */
export interface CopybookInfo {
  /** Copybook name */
  name: string;
  /** Library name if specified */
  library?: string;
  /** Resolved file path */
  path?: string;
  /** REPLACING clause applied */
  replacing?: ReplacingClause[];
}

/**
 * REPLACING clause for COPY statements
 */
export interface ReplacingClause {
  /** Text to replace (pseudo-text) */
  oldText: string;
  /** Replacement text (pseudo-text) */
  newText: string;
}

/**
 * Call reference for PERFORM statements
 */
export interface CallReference {
  /** Name of called section or paragraph */
  name: string;
  /** Type of call */
  type: 'section' | 'paragraph';
  /** Location of the call */
  location: SourceLocation;
  /** Call context (PERFORM, GO TO, etc.) */
  context: string;
}

/**
 * Data type information for variables
 */
export interface DataType {
  /** PICTURE clause */
  picture?: string;
  /** USAGE clause */
  usage?: 'DISPLAY' | 'COMPUTATIONAL' | 'COMP-1' | 'COMP-2' | 'COMP-3' | 'COMP-4' | 'COMP-5' | 'POINTER' | 'FUNCTION-POINTER';
  /** Sign specification */
  sign?: 'LEADING' | 'TRAILING' | 'SEPARATE';
  /** Synchronization */
  sync?: boolean;
  /** Blank when zero */
  blankWhenZero?: boolean;
  /** Justified right */
  justified?: boolean;
}

/**
 * Variable definition information
 */
export interface VariableDefinition {
  /** Variable name */
  name: string;
  /** Level number (01-88) */
  level: number;
  /** Data type information */
  dataType?: DataType;
  /** Initial value */
  initialValue?: string;
  /** Parent variable (for group items) */
  parent?: string;
  /** Child variables */
  children: string[];
  /** Location in source */
  location: SourceLocation;
  /** Condition names (88 level) */
  conditionNames?: ConditionName[];
}

/**
 * Condition name (88 level) definition
 */
export interface ConditionName {
  /** Condition name */
  name: string;
  /** Values that make condition true */
  values: string[];
  /** Location in source */
  location: SourceLocation;
}

/**
 * Error and diagnostic information
 */
export interface DiagnosticMessage {
  /** Error severity */
  severity: 'error' | 'warning' | 'info';
  /** Error code */
  code: string;
  /** Human-readable message */
  message: string;
  /** Source location */
  location: SourceLocation;
  /** Suggested fixes */
  suggestions?: string[];
  /** Related locations */
  relatedLocations?: SourceLocation[];
}

/**
 * Performance metrics
 */
export interface PerformanceMetrics {
  /** Parse time in milliseconds */
  parseTime: number;
  /** Memory usage in bytes */
  memoryUsage: number;
  /** Number of AST nodes created */
  nodeCount: number;
  /** Number of copybooks processed */
  copybookCount: number;
}

/**
 * Complexity metrics for procedures
 */
export interface ComplexityMetrics {
  /** Cyclomatic complexity */
  cyclomaticComplexity: number;
  /** Number of statements */
  statementCount: number;
  /** Nesting depth */
  nestingDepth: number;
  /** Number of decision points */
  decisionPoints: number;
}

/**
 * File operation types
 */
export type FileOperation = 'READ' | 'WRITE' | 'OPEN' | 'CLOSE' | 'DELETE' | 'REWRITE' | 'START';

/**
 * COBOL statement types
 */
export type StatementType = 
  | 'MOVE' | 'ADD' | 'SUBTRACT' | 'MULTIPLY' | 'DIVIDE' | 'COMPUTE'
  | 'IF' | 'ELSE' | 'EVALUATE' | 'WHEN' | 'PERFORM' | 'GO' | 'CALL'
  | 'DISPLAY' | 'ACCEPT' | 'READ' | 'WRITE' | 'OPEN' | 'CLOSE'
  | 'STOP' | 'EXIT' | 'GOBACK' | 'CONTINUE' | 'NEXT' | 'SEARCH'
  | 'SORT' | 'MERGE' | 'RELEASE' | 'RETURN' | 'SET' | 'INITIALIZE'
  | 'INSPECT' | 'STRING' | 'UNSTRING' | 'ALTER' | 'CANCEL';