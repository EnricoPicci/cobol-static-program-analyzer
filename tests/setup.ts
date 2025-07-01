/**
 * Jest test setup with custom COBOL matchers
 */

import { customMatchers } from './utils/matchers';

// Extend Jest matchers
expect.extend(customMatchers);

// Global test configuration
declare global {
  namespace jest {
    interface Matchers<R> {
      // Parsing matchers
      toParseSuccessfully(): R;
      toFailParsingWith(errorType: string): R;
      toHaveParseErrors(count: number): R;
      
      // AST structure matchers
      toHaveASTStructure(expected: any): R;
      toContainNode(nodeType: string, count?: number): R;
      toHaveNodeAt(path: string, nodeType: string): R;
      
      // COBOL-specific matchers
      toHaveValidCOBOLStructure(): R;
      toContainDivision(divisionType: string): R;
      toHaveWorkingStorageVariable(varName: string): R;
      toHaveProcedureParagraph(paragraphName: string): R;
      toCallParagraph(callerName: string, targetName: string): R;
      
      // COPY statement matchers
      toResolveCopyStatements(): R;
      toIncludeCopybook(copybookName: string): R;
      toHaveCircularDependency(): R;
      
      // Performance matchers
      toParseWithin(maxDuration: number): R;
      toUseMemoryUnder(maxMemoryMB: number): R;
      
      // Source preservation matchers
      toPreserveSourceLocations(): R;
      toMaintainSourceText(): R;
    }
  }
}

// Test timeout configuration
jest.setTimeout(30000); // 30 seconds for complex parsing tests

// Performance monitoring setup
global.performance = global.performance || {
  now: () => Date.now(),
  mark: () => {},
  measure: () => {},
  getEntriesByName: () => [],
  getEntriesByType: () => [],
  clearMarks: () => {},
  clearMeasures: () => {}
} as any;

console.log('🧪 COBOL Parser Test Environment Initialized');