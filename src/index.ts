/**
 * Main entry point for the COBOL AST Parser
 */

export * from './ast/nodes';
export * from './ast/parser';

// Re-export the main parsing function for convenience
export { parseCobol as parse } from './ast/parser';
