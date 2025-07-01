/**
 * Main entry point for the COBOL Static Program Analyzer
 */

export { CobolProgram } from './ast/nodes/CobolProgram';
export { IdentificationDivision } from './ast/nodes/IdentificationDivision';
export { EnvironmentDivision } from './ast/nodes/EnvironmentDivision';
export { DataDivision } from './ast/nodes/DataDivision';
export { ProcedureDivision } from './ast/nodes/ProcedureDivision';
export { SectionNode } from './ast/nodes/SectionNode';
export { ParagraphNode } from './ast/nodes/ParagraphNode';
export { StatementNode } from './ast/nodes/StatementNode';
export { ASTNode, BaseASTNode } from './ast/nodes/ASTNode';

export {
  BaseASTVisitor,
  NodeCollectorVisitor,
  CallReferenceVisitor,
  NodeCounterVisitor,
  ValidationVisitor,
  traverseAST,
  findNodesByType,
  countNodesByType,
  validateAST
} from './ast/visitor';

export {
  SourceLocation,
  NodeMetadata,
  CallReference,
  VariableDefinition,
  DataType,
  DiagnosticMessage,
  PerformanceMetrics,
  ComplexityMetrics
} from './core/types';

// Version information
export const VERSION = '1.0.0-alpha';

console.log(`🚀 COBOL Static Program Analyzer v${VERSION} initialized`);
console.log('📁 Project structure created');
console.log('🧪 Test infrastructure ready');
console.log('🏗️  AST node interfaces defined');
console.log('👁️  Visitor pattern implemented');
console.log('⚡ Ready for ANTLR4NG upgrade');