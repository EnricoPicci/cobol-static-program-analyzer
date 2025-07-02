/**
 * Analysis module exports - Three-tier error handling system
 */

// Semantic validation (Tier 2)
export { 
  SemanticValidator, 
  SemanticValidationConfig, 
  DEFAULT_SEMANTIC_CONFIG 
} from './semantic-validator';

// Static analysis (Tier 3)
export { 
  StaticAnalyzer, 
  StaticAnalysisConfig, 
  DEFAULT_STATIC_ANALYSIS_CONFIG 
} from './static-analyzer';

// Error coordination
export { 
  ErrorCoordinator, 
  ErrorCoordinationConfig, 
  DEFAULT_ERROR_COORDINATION_CONFIG,
  ErrorAnalysisResult,
  ErrorCorrelation
} from './error-coordinator';

// Re-export error handling types
export { 
  CobolParsingError,
  SyntaxError,
  SemanticError,
  AnalysisError,
  PreprocessingError,
  ASTConstructionError,
  ParsingError,
  CobolErrorHandler,
  ErrorRecoveryStrategy
} from '../parser/error-handler';

// Re-export ANTLR error listener - temporarily disabled due to antlr4ng compatibility issues
// export { 
//   CobolErrorListener,
//   CobolRecoveryStrategy
// } from '../parser/antlr-error-listener';