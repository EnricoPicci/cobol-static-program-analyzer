/**
 * TypeScript interfaces for COBOL Abstract Syntax Tree (AST) nodes
 * 
 * This file defines the structure of the COBOL AST, providing a comprehensive
 * set of interfaces for representing COBOL program constructs in a typed manner.
 */

// ============================================================================
// Base Types and Common Interfaces
// ============================================================================

/**
 * Represents a position in the source code
 */
export interface Position {
  line: number;
  column: number;
  offset: number;
}

/**
 * Represents a range in the source code
 */
export interface SourceRange {
  start: Position;
  end: Position;
}

/**
 * Represents the raw source code of a node
 */
export interface SourceCode {
  text: string;
  range: SourceRange;
}

/**
 * Base interface for all AST nodes
 */
export interface BaseNode {
  type: string;
  id?: string;
  location: SourceRange;
  sourceCode: SourceCode;
  comments?: CommentNode[];
  parent?: BaseNode;
  children?: BaseNode[];
}

/**
 * Represents a comment in the COBOL source code
 */
export interface CommentNode extends BaseNode {
  type: 'Comment';
  text: string;
  commentType: 'line' | 'block' | 'inline';
}

/**
 * Represents data flow information for analysis
 */
export interface DataFlowInfo {
  readVariables: string[];
  writtenVariables: string[];
  referencedVariables: string[];
  modifiedVariables: string[];
  calledParagraphs: string[];
  calledPrograms: string[];
}

// ============================================================================
// Program Structure Nodes
// ============================================================================

/**
 * Represents the entire COBOL program
 */
export interface ProgramNode extends BaseNode {
  type: 'Program';
  programName: string;
  divisions: DivisionNode[];
  identificationDivision?: IdentificationDivisionNode;
  environmentDivision?: EnvironmentDivisionNode;
  dataDivision?: DataDivisionNode;
  procedureDivision?: ProcedureDivisionNode;
  nestedPrograms?: ProgramNode[];
}

/**
 * Base interface for all division nodes
 */
export interface DivisionNode extends BaseNode {
  type: string;
  divisionType: 'Identification' | 'Environment' | 'Data' | 'Procedure';
  sections: SectionNode[];
}

/**
 * Represents the Identification Division
 */
export interface IdentificationDivisionNode extends DivisionNode {
  type: 'IdentificationDivision';
  divisionType: 'Identification';
  programId: string;
  author?: string;
  installation?: string;
  dateWritten?: string;
  dateCompiled?: string;
  security?: string;
  remarks?: string;
}

/**
 * Represents the Environment Division
 */
export interface EnvironmentDivisionNode extends DivisionNode {
  type: 'EnvironmentDivision';
  divisionType: 'Environment';
  configurationSection?: ConfigurationSectionNode;
  inputOutputSection?: InputOutputSectionNode;
}

/**
 * Represents the Data Division
 */
export interface DataDivisionNode extends DivisionNode {
  type: 'DataDivision';
  divisionType: 'Data';
  fileSection?: FileSectionNode;
  workingStorageSection?: WorkingStorageSectionNode;
  linkageSection?: LinkageSectionNode;
  localStorageSection?: LocalStorageSectionNode;
  screenSection?: ScreenSectionNode;
  reportSection?: ReportSectionNode;
  dataItems: DataItemNode[];
}

/**
 * Represents the Procedure Division
 */
export interface ProcedureDivisionNode extends DivisionNode {
  type: 'ProcedureDivision';
  divisionType: 'Procedure';
  usingClause?: UsingClauseNode;
  givingClause?: GivingClauseNode;
  declaratives?: DeclarativesNode;
  paragraphs: ParagraphNode[];
  statements: StatementNode[];
  dataFlowInfo: DataFlowInfo;
}

// ============================================================================
// Section Nodes
// ============================================================================

/**
 * Base interface for all section nodes
 */
export interface SectionNode extends BaseNode {
  type: string;
  sectionName: string;
  paragraphs: ParagraphNode[];
  dataFlowInfo: DataFlowInfo;
}

/**
 * Configuration Section (Environment Division)
 */
export interface ConfigurationSectionNode extends SectionNode {
  type: 'ConfigurationSection';
  sourceComputerParagraph?: SourceComputerParagraphNode;
  objectComputerParagraph?: ObjectComputerParagraphNode;
  specialNamesParagraph?: SpecialNamesParagraphNode;
}

/**
 * Input-Output Section (Environment Division)
 */
export interface InputOutputSectionNode extends SectionNode {
  type: 'InputOutputSection';
  fileControlParagraph?: FileControlParagraphNode;
  ioControlParagraph?: IoControlParagraphNode;
}

/**
 * File Section (Data Division)
 */
export interface FileSectionNode extends SectionNode {
  type: 'FileSection';
  fileDescriptions: FileDescriptionNode[];
}

/**
 * Working-Storage Section (Data Division)
 */
export interface WorkingStorageSectionNode extends SectionNode {
  type: 'WorkingStorageSection';
  dataItems: DataItemNode[];
}

/**
 * Linkage Section (Data Division)
 */
export interface LinkageSectionNode extends SectionNode {
  type: 'LinkageSection';
  dataItems: DataItemNode[];
}

/**
 * Local-Storage Section (Data Division)
 */
export interface LocalStorageSectionNode extends SectionNode {
  type: 'LocalStorageSection';
  dataItems: DataItemNode[];
}

/**
 * Screen Section (Data Division)
 */
export interface ScreenSectionNode extends SectionNode {
  type: 'ScreenSection';
  screenItems: ScreenItemNode[];
}

/**
 * Report Section (Data Division)
 */
export interface ReportSectionNode extends SectionNode {
  type: 'ReportSection';
  reportDescriptions: ReportDescriptionNode[];
}

// ============================================================================
// Paragraph and Statement Nodes
// ============================================================================

/**
 * Represents a paragraph within a section
 */
export interface ParagraphNode extends BaseNode {
  type: string;
  paragraphName: string;
  statements: StatementNode[];
  isEntryPoint: boolean;
  dataFlowInfo: DataFlowInfo;
}

/**
 * Base interface for all statement nodes
 */
export interface StatementNode extends BaseNode {
  type: string;
  statementType: string;
  dataFlowInfo: DataFlowInfo;
}

// ============================================================================
// Data Definition Nodes
// ============================================================================

/**
 * Represents a data item definition
 */
export interface DataItemNode extends BaseNode {
  type: 'DataItem';
  levelNumber: number;
  dataName: string;
  pictureClause?: PictureClauseNode;
  usageClause?: UsageClauseNode;
  valueClause?: ValueClauseNode;
  occursClause?: OccursClauseNode;
  redefinesClause?: RedefinesClauseNode;
  isGroup: boolean;
  children: DataItemNode[];
  byteLength?: number;
  dataType: DataType;
}

/**
 * Data type enumeration
 */
export type DataType = 
  | 'Alphabetic'
  | 'Alphanumeric'
  | 'AlphanumericEdited'
  | 'Numeric'
  | 'NumericEdited'
  | 'PackedDecimal'
  | 'Binary'
  | 'Index'
  | 'Pointer'
  | 'Group';

/**
 * Picture clause node
 */
export interface PictureClauseNode extends BaseNode {
  type: 'PictureClause';
  pictureString: string;
  characterType: 'A' | 'X' | '9' | 'S' | 'V' | 'P' | 'Z' | '*' | '+' | '-' | 'CR' | 'DB' | ',' | '.' | '/' | 'B' | '0';
  displayLength: number;
  storageLength: number;
}

/**
 * Usage clause node
 */
export interface UsageClauseNode extends BaseNode {
  type: 'UsageClause';
  usage: 'Display' | 'Computational' | 'Comp' | 'Comp-1' | 'Comp-2' | 'Comp-3' | 'Comp-4' | 'Comp-5' | 'PackedDecimal' | 'Binary' | 'Index' | 'Pointer';
}

/**
 * Value clause node
 */
export interface ValueClauseNode extends BaseNode {
  type: 'ValueClause';
  values: LiteralNode[];
}

/**
 * Occurs clause node
 */
export interface OccursClauseNode extends BaseNode {
  type: 'OccursClause';
  minOccurs: number;
  maxOccurs?: number;
  dependingOn?: string;
  indexedBy?: string[];
  keys?: KeyClauseNode[];
}

/**
 * Redefines clause node
 */
export interface RedefinesClauseNode extends BaseNode {
  type: 'RedefinesClause';
  redefinesName: string;
}

/**
 * Key clause for indexed tables
 */
export interface KeyClauseNode extends BaseNode {
  type: 'KeyClause';
  isAscending: boolean;
  keyNames: string[];
}

// ============================================================================
// Statement Specific Nodes
// ============================================================================

/**
 * Move statement
 */
export interface MoveStatementNode extends StatementNode {
  type: 'MoveStatement';
  statementType: 'Move';
  source: ExpressionNode;
  targets: IdentifierNode[];
}

/**
 * Perform statement
 */
export interface PerformStatementNode extends StatementNode {
  type: 'PerformStatement';
  statementType: 'Perform';
  targetParagraph: string;
  throughParagraph?: string;
  performType: 'Simple' | 'Times' | 'Until' | 'Varying';
  timesExpression?: ExpressionNode;
  untilCondition?: ConditionNode;
  varyingClause?: VaryingClauseNode;
  inlineStatements?: StatementNode[];
}

/**
 * Call statement
 */
export interface CallStatementNode extends StatementNode {
  type: 'CallStatement';
  statementType: 'Call';
  programName: string | IdentifierNode;
  usingParameters?: ParameterNode[];
  givingParameter?: IdentifierNode;
  onOverflowStatements?: StatementNode[];
  onExceptionStatements?: StatementNode[];
}

/**
 * If statement
 */
export interface IfStatementNode extends StatementNode {
  type: 'IfStatement';
  statementType: 'If';
  condition: ConditionNode;
  thenStatements: StatementNode[];
  elseStatements?: StatementNode[];
}

/**
 * Evaluate statement
 */
export interface EvaluateStatementNode extends StatementNode {
  type: 'EvaluateStatement';
  statementType: 'Evaluate';
  selectionSubjects: ExpressionNode[];
  whenBranches: WhenBranchNode[];
  whenOtherStatements?: StatementNode[];
}

/**
 * Go To statement
 */
export interface GoToStatementNode extends StatementNode {
  type: 'GoToStatement';
  statementType: 'GoTo';
  targetParagraphs: string[];
  dependingOn?: IdentifierNode;
}

/**
 * Stop statement
 */
export interface StopStatementNode extends StatementNode {
  type: 'StopStatement';
  statementType: 'Stop';
  stopType: 'Run' | 'Literal';
  literal?: LiteralNode;
}

// ============================================================================
// Expression and Condition Nodes
// ============================================================================

/**
 * Base interface for expressions
 */
export interface ExpressionNode extends BaseNode {
  type: string;
  expressionType: string;
  dataType?: DataType;
}

/**
 * Arithmetic expression
 */
export interface ArithmeticExpressionNode extends ExpressionNode {
  type: 'ArithmeticExpression';
  expressionType: 'Arithmetic';
  operator: '+' | '-' | '*' | '/' | '**';
  left: ExpressionNode;
  right: ExpressionNode;
}

/**
 * Identifier (variable reference)
 */
export interface IdentifierNode extends ExpressionNode {
  type: 'Identifier';
  expressionType: 'Identifier';
  name: string;
  qualifiers?: string[];
  subscripts?: ExpressionNode[];
  referenceModification?: ReferenceModificationNode;
}

/**
 * Literal value
 */
export interface LiteralNode extends ExpressionNode {
  type: 'Literal';
  expressionType: 'Literal';
  literalType: 'Numeric' | 'Alphanumeric' | 'Boolean' | 'Figurative';
  value: string | number | boolean;
}

/**
 * Function call
 */
export interface FunctionCallNode extends ExpressionNode {
  type: 'FunctionCall';
  expressionType: 'Function';
  functionName: string;
  arguments: ExpressionNode[];
}

/**
 * Condition expression
 */
export interface ConditionNode extends BaseNode {
  type: string;
  conditionType: string;
}

/**
 * Relational condition
 */
export interface RelationalConditionNode extends ConditionNode {
  type: 'RelationalCondition';
  conditionType: 'Relational';
  operator: '=' | '>' | '<' | '>=' | '<=' | '<>' | 'NOT =';
  left: ExpressionNode;
  right: ExpressionNode;
}

/**
 * Class condition
 */
export interface ClassConditionNode extends ConditionNode {
  type: 'ClassCondition';
  conditionType: 'Class';
  subject: ExpressionNode;
  className: 'Numeric' | 'Alphabetic' | 'AlphabeticLower' | 'AlphabeticUpper' | string;
  isNegated: boolean;
}

/**
 * Compound condition (AND/OR)
 */
export interface CompoundConditionNode extends ConditionNode {
  type: 'CompoundCondition';
  conditionType: 'Compound';
  operator: 'AND' | 'OR';
  left: ConditionNode;
  right: ConditionNode;
}

// ============================================================================
// Supporting Nodes
// ============================================================================

/**
 * Reference modification (substring)
 */
export interface ReferenceModificationNode extends BaseNode {
  type: 'ReferenceModification';
  startPosition: ExpressionNode;
  length?: ExpressionNode;
}

/**
 * When branch for Evaluate statement
 */
export interface WhenBranchNode extends BaseNode {
  type: 'WhenBranch';
  conditions: ExpressionNode[];
  statements: StatementNode[];
}

/**
 * Varying clause for Perform statement
 */
export interface VaryingClauseNode extends BaseNode {
  type: 'VaryingClause';
  identifier: IdentifierNode;
  fromValue: ExpressionNode;
  byValue: ExpressionNode;
  untilCondition: ConditionNode;
  afterClauses?: VaryingClauseNode[];
}

/**
 * Parameter node for Call statements
 */
export interface ParameterNode extends BaseNode {
  type: 'Parameter';
  parameterType: 'ByReference' | 'ByValue' | 'ByContent';
  identifier: IdentifierNode;
}

/**
 * Using clause for Procedure Division
 */
export interface UsingClauseNode extends BaseNode {
  type: 'UsingClause';
  parameters: ParameterNode[];
}

/**
 * Giving clause for Procedure Division
 */
export interface GivingClauseNode extends BaseNode {
  type: 'GivingClause';
  returnItem: IdentifierNode;
}

/**
 * Declaratives section
 */
export interface DeclarativesNode extends BaseNode {
  type: 'Declaratives';
  declarativeSections: DeclarativeSectionNode[];
}

/**
 * Declarative section
 */
export interface DeclarativeSectionNode extends BaseNode {
  type: 'DeclarativeSection';
  sectionName: string;
  useStatement: UseStatementNode;
  paragraphs: ParagraphNode[];
}

/**
 * Use statement
 */
export interface UseStatementNode extends StatementNode {
  type: 'UseStatement';
  statementType: 'Use';
  useType: 'After' | 'Before' | 'ForDebugging';
  procedures?: string[];
  files?: string[];
}

// ============================================================================
// File and I/O Related Nodes
// ============================================================================

/**
 * File description
 */
export interface FileDescriptionNode extends BaseNode {
  type: 'FileDescription';
  fileName: string;
  recordDescription: DataItemNode;
  blockContains?: number;
  recordContains?: number;
  labelRecords?: 'Standard' | 'Omitted';
}

/**
 * Screen item for Screen Section
 */
export interface ScreenItemNode extends BaseNode {
  type: 'ScreenItem';
  levelNumber: number;
  screenName?: string;
  lineClause?: number;
  columnClause?: number;
  pictureClause?: PictureClauseNode;
  valueClause?: ValueClauseNode;
  fromClause?: IdentifierNode;
  toClause?: IdentifierNode;
  usingClause?: IdentifierNode;
}

/**
 * Report description for Report Section
 */
export interface ReportDescriptionNode extends BaseNode {
  type: 'ReportDescription';
  reportName: string;
  pageLimit?: number;
  heading?: number;
  firstDetail?: number;
  lastDetail?: number;
  footing?: number;
}

// ============================================================================
// Paragraph Specific Nodes (Environment Division)
// ============================================================================

/**
 * Source Computer paragraph
 */
export interface SourceComputerParagraphNode extends ParagraphNode {
  type: 'SourceComputerParagraph';
  computerName?: string;
  withDebuggingMode?: boolean;
}

/**
 * Object Computer paragraph
 */
export interface ObjectComputerParagraphNode extends ParagraphNode {
  type: 'ObjectComputerParagraph';
  computerName?: string;
  memorySize?: number;
  programCollatingSequence?: string;
  segmentLimit?: number;
}

/**
 * Special Names paragraph
 */
export interface SpecialNamesParagraphNode extends ParagraphNode {
  type: 'SpecialNamesParagraph';
  specialNameClauses: SpecialNameClauseNode[];
}

/**
 * Special name clause
 */
export interface SpecialNameClauseNode extends BaseNode {
  type: 'SpecialNameClause';
  clauseType: 'CurrencySign' | 'DecimalPoint' | 'SymbolicCharacters' | 'Alphabet' | 'Class';
  // Additional properties would be defined based on clause type
}

/**
 * File Control paragraph
 */
export interface FileControlParagraphNode extends ParagraphNode {
  type: 'FileControlParagraph';
  selectStatements: SelectStatementNode[];
}

/**
 * Select statement for file control
 */
export interface SelectStatementNode extends StatementNode {
  type: 'SelectStatement';
  statementType: 'Select';
  fileName: string;
  assignTo?: string;
  organization?: 'Sequential' | 'Indexed' | 'Relative';
  accessMode?: 'Sequential' | 'Random' | 'Dynamic';
  recordKey?: string;
  alternateKeys?: string[];
  fileStatus?: string;
}

/**
 * I-O Control paragraph
 */
export interface IoControlParagraphNode extends ParagraphNode {
  type: 'IoControlParagraph';
  rerunClauses?: RerunClauseNode[];
  sameRecordClauses?: SameRecordClauseNode[];
}

/**
 * Rerun clause
 */
export interface RerunClauseNode extends BaseNode {
  type: 'RerunClause';
  onFile?: string;
  everyRecords?: number;
  everyEndOfReel?: string;
}

/**
 * Same record clause
 */
export interface SameRecordClauseNode extends BaseNode {
  type: 'SameRecordClause';
  files: string[];
}

// ============================================================================
// Type Guards and Utility Types
// ============================================================================

/**
 * Union type for all AST nodes
 */
export type ASTNode = 
  | BaseNode
  | ProgramNode
  | DivisionNode
  | SectionNode
  | ParagraphNode
  | StatementNode
  | DataItemNode
  | ExpressionNode
  | ConditionNode
  | CommentNode;

/**
 * Union type for all statement nodes
 */
export type StatementNodeType =
  | MoveStatementNode
  | PerformStatementNode
  | CallStatementNode
  | IfStatementNode
  | EvaluateStatementNode
  | GoToStatementNode
  | StopStatementNode
  | UseStatementNode
  | SelectStatementNode;

/**
 * Union type for all expression nodes
 */
export type ExpressionNodeType =
  | ArithmeticExpressionNode
  | IdentifierNode
  | LiteralNode
  | FunctionCallNode;

/**
 * Union type for all condition nodes
 */
export type ConditionNodeType =
  | RelationalConditionNode
  | ClassConditionNode
  | CompoundConditionNode;

/**
 * Visitor pattern interface for traversing the AST
 */
export interface ASTVisitor<T = void> {
  visitProgram?(node: ProgramNode): T;
  visitDivision?(node: DivisionNode): T;
  visitSection?(node: SectionNode): T;
  visitParagraph?(node: ParagraphNode): T;
  visitStatement?(node: StatementNode): T;
  visitDataItem?(node: DataItemNode): T;
  visitExpression?(node: ExpressionNode): T;
  visitCondition?(node: ConditionNode): T;
  visitComment?(node: CommentNode): T;
}
