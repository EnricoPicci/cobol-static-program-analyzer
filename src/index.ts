/**
 * Main entry point for the COBOL Static Program Analyzer - Phase 5 Enhanced
 */

// Phase 5: Enhanced Analysis Entry Point
export { 
  EnhancedCobolAnalyzer, 
  EnhancedAnalyzerConfig, 
  EnhancedAnalysisResult,
  QualityGateDetail,
  DEFAULT_ENHANCED_CONFIG 
} from './enhanced-analyzer';

// Base Integration Entry Point (Phase 2-4)
export { CobolAnalyzer, CobolAnalyzerConfig, AnalysisResult, DEFAULT_ANALYZER_CONFIG } from './CobolAnalyzer';
import { EnhancedCobolAnalyzer } from './enhanced-analyzer';

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

// Phase 4: Static Analysis
export {
  StaticAnalyzer,
  StaticAnalysisConfig,
  DEFAULT_STATIC_ANALYSIS_CONFIG
} from './analysis/static-analyzer';

// Phase 5: Enhanced Analysis and Reporting
export {
  ReportGenerator,
  ComprehensiveReport,
  CodeQualityMetrics,
  CodeSmell,
  SecurityIssue,
  PerformanceIssue,
  Recommendation,
  QualityTrend
} from './reporting';

export {
  DashboardGenerator,
  ChartConfig,
  DashboardConfig
} from './visualization';

export {
  APIIntegrationClient,
  WebhookServer,
  SonarQubeConfig,
  JenkinsConfig,
  GitLabConfig,
  IntegrationResult,
  WebhookPayload
} from './integration';

// Version information
export const VERSION = '1.0.0-phase5-enhanced';
export const PHASE = 'Phase 5 - Enhanced Analysis and Reporting';

// Quick start function for enhanced analysis
export async function quickEnhancedAnalyze(
  cobolSource: string, 
  fileName?: string,
  outputDir?: string
) {
  const analyzer = new EnhancedCobolAnalyzer();
  return await analyzer.analyzeWithEnhancements(cobolSource, fileName, outputDir);
}

// Legacy quick analyze for backward compatibility
export async function quickAnalyze(cobolSource: string, fileName?: string) {
  const analyzer = new EnhancedCobolAnalyzer();
  const result = await analyzer.analyzeWithEnhancements(cobolSource, fileName);
  return result; // Return enhanced result with backward compatibility
}

console.log(`🚀 COBOL Static Program Analyzer v${VERSION} - ${PHASE}`);
console.log('✅ Phase 1: Foundation (AST, Visitor Pattern) - Complete');
console.log('✅ Phase 2: Parser Integration (Parsing Pipeline) - Complete');
console.log('✅ Phase 3: COPY Processing (Copybook Resolution) - Complete');
console.log('✅ Phase 4: Static Analysis (Code Quality) - Complete');
console.log('🎉 Phase 5: Enhanced Analysis & Reporting - Complete');
console.log('');
console.log('🔍 Enhanced Features:');
console.log('  📊 Comprehensive Quality Reports');
console.log('  📈 Interactive Dashboards');
console.log('  🚨 Code Smell Detection');
console.log('  🔒 Security Analysis');
console.log('  ⚡ Performance Analysis');
console.log('  💳 Technical Debt Calculation');
console.log('  🎯 Quality Gate Validation');
console.log('  🔗 External Tool Integration');
console.log('  📦 Batch Processing');
console.log('  📋 Multiple Export Formats');
console.log('');
console.log('💡 Use quickEnhancedAnalyze() for full enhanced analysis');
console.log('🔧 Use EnhancedCobolAnalyzer class for complete control');