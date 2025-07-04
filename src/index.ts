/**
 * Main entry point for the COBOL Static Program Analyzer - Phase 2 Integration
 */

// Main Integration Entry Point
export { CobolAnalyzer, CobolAnalyzerConfig, AnalysisResult, DEFAULT_ANALYZER_CONFIG } from './CobolAnalyzer';
import { CobolAnalyzer } from './CobolAnalyzer';

// Parser Components
export { CobolParser, CobolParserConfig, ParseResult, DEFAULT_COBOL_PARSER_CONFIG } from './parser/cobol-parser';
export { CobolASTBuilder, ASTBuilderConfig, DEFAULT_AST_BUILDER_CONFIG } from './ast/builder';
export { 
  CobolErrorHandler, 
  ErrorRecoveryStrategy,
  SyntaxError,
  SemanticError,
  AnalysisError,
  PreprocessingError,
  ASTConstructionError,
  ParsingError
} from './parser/error-handler';

// AST Nodes
export { CobolProgram } from './ast/nodes/CobolProgram';
export { IdentificationDivision } from './ast/nodes/IdentificationDivision';
export { EnvironmentDivision } from './ast/nodes/EnvironmentDivision';
export { DataDivision } from './ast/nodes/DataDivision';
export { ProcedureDivision } from './ast/nodes/ProcedureDivision';
export { SectionNode } from './ast/nodes/SectionNode';
export { ParagraphNode } from './ast/nodes/ParagraphNode';
export { StatementNode } from './ast/nodes/StatementNode';
export { ASTNode, BaseASTNode } from './ast/nodes/ASTNode';

// Visitor Pattern
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

// Core Types
export {
  SourceLocation,
  NodeMetadata,
  CallReference,
  VariableDefinition,
  DataType,
  DiagnosticMessage,
  PerformanceMetrics,
  ComplexityMetrics,
  StatementType
} from './core/types';

// Phase 3: COPY Statement Processing
export {
  CopyProcessor,
  CopybookResolver,
  FileSystemCopybookFinder,
  InMemoryCopybookCache,
  ReplacingProcessor,
  DependencyTracker,
  CopyStatementVisitor,
  CopyErrorHandler,
  createCopyProcessor,
  getDefaultCopybookConfiguration,
  processCopyStatement,
  // Types
  CopybookConfiguration,
  CopyStatementInfo,
  ReplacingClause,
  ReplacingPattern,
  PseudoText,
  CopybookMetadata,
  ResolvedCopybook,
  CopybookDependency,
  ProcessingResult,
  CopyProcessingError,
  PreprocessedSource,
  DEFAULT_COPYBOOK_CONFIG
} from './copy';

// Version information
export const VERSION = '1.0.0-alpha-phase3';
export const PHASE = 'Phase 3 - COPY Statement Processing';

// Quick start function for integration testing
export async function quickAnalyze(cobolSource: string, fileName?: string) {
  const analyzer = new CobolAnalyzer();
  return await analyzer.analyze(cobolSource, fileName);
}

console.log(`🚀 COBOL Static Program Analyzer v${VERSION} - ${PHASE}`);
console.log('📁 Phase 3 COPY Processing Complete');
console.log('📋 COPY statement resolution implemented');
console.log('🔄 REPLACING clause processing integrated');
console.log('🕸️  Circular dependency detection active');
console.log('🗂️  File system copybook finder ready');
console.log('⚡ Integrated with Phase 2 parsing pipeline');
console.log('💡 Use quickAnalyze() for immediate testing');
console.log('🔧 Use CobolAnalyzer class for full control');