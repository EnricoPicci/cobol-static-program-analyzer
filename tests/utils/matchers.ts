/**
 * Custom Jest matchers for COBOL parsing validation
 */

import { CobolProgram } from '../../src/ast/nodes/CobolProgram';
import { ASTNode } from '../../src/ast/nodes/ASTNode';
import { SourceLocation } from '../../src/core/types';

export const customMatchers = {
  toParseSuccessfully(received: string) {
    try {
      // TODO: Implement actual parser once available
      // const ast = parse(received);
      const mockSuccess = received.includes('IDENTIFICATION DIVISION');
      
      return {
        message: () => `Expected parsing to fail but it succeeded`,
        pass: mockSuccess
      };
    } catch (error) {
      return {
        message: () => `Expected parsing to succeed but failed: ${error instanceof Error ? error.message : String(error)}`,
        pass: false
      };
    }
  },

  toFailParsingWith(received: string, expectedErrorType: string) {
    try {
      // TODO: Implement actual parser once available
      // const ast = parse(received);
      return {
        message: () => `Expected parsing to fail with '${expectedErrorType}' but it succeeded`,
        pass: false
      };
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      const hasExpectedError = errorMessage.includes(expectedErrorType);
      
      return {
        message: () => hasExpectedError
          ? `Expected parsing not to fail with '${expectedErrorType}'`
          : `Expected parsing to fail with '${expectedErrorType}' but failed with: ${errorMessage}`,
        pass: hasExpectedError
      };
    }
  },

  toHaveValidCOBOLStructure(received: CobolProgram) {
    const errors: string[] = [];
    
    // Check required divisions
    if (!received.identificationDivision) {
      errors.push('Missing IDENTIFICATION DIVISION');
    }
    
    // Check program ID
    if (!received.identificationDivision?.programId) {
      errors.push('Missing PROGRAM-ID');
    }
    
    // Validate program name format
    if (received.identificationDivision?.programId) {
      const programId = received.identificationDivision.programId;
      if (!/^[A-Z][A-Z0-9-]{0,29}$/.test(programId)) {
        errors.push(`Invalid PROGRAM-ID format: ${programId}`);
      }
    }
    
    const isValid = errors.length === 0;
    
    return {
      message: () => isValid
        ? 'Expected COBOL program structure to be invalid'
        : `Expected valid COBOL structure but found errors: ${errors.join(', ')}`,
      pass: isValid
    };
  },

  toContainDivision(received: CobolProgram, divisionType: string) {
    const divisions: Record<string, any> = {
      'IDENTIFICATION': received.identificationDivision,
      'ENVIRONMENT': received.environmentDivision,
      'DATA': received.dataDivision,
      'PROCEDURE': received.procedureDivision
    };
    
    const division = divisions[divisionType.toUpperCase()];
    const hasDiv = !!division;
    
    return {
      message: () => hasDiv
        ? `Expected program not to contain ${divisionType} DIVISION`
        : `Expected program to contain ${divisionType} DIVISION`,
      pass: hasDiv
    };
  },

  toHaveWorkingStorageVariable(received: CobolProgram, varName: string) {
    const workingStorage = received.dataDivision?.workingStorage || [];
    const hasVariable = workingStorage.some(variable => variable.name === varName);
    
    return {
      message: () => hasVariable
        ? `Expected not to find working storage variable '${varName}'`
        : `Expected to find working storage variable '${varName}' but it was not found`,
      pass: hasVariable
    };
  },

  toHaveProcedureParagraph(received: CobolProgram, paragraphName: string) {
    const paragraphs = received.procedureDivision?.paragraphs || [];
    const sections = received.procedureDivision?.sections || [];
    
    // Check standalone paragraphs
    const standaloneParagraph = paragraphs.some(para => para.name === paragraphName);
    
    // Check paragraphs within sections
    const sectionParagraph = sections.some(section =>
      section.paragraphs?.some(para => para.name === paragraphName)
    );
    
    const hasParagraph = standaloneParagraph || sectionParagraph;
    
    return {
      message: () => hasParagraph
        ? `Expected not to find procedure paragraph '${paragraphName}'`
        : `Expected to find procedure paragraph '${paragraphName}' but it was not found`,
      pass: hasParagraph
    };
  },

  toCallParagraph(received: CobolProgram, callerName: string, targetName: string) {
    const findParagraph = (name: string) => {
      const paragraphs = received.procedureDivision?.paragraphs || [];
      const sections = received.procedureDivision?.sections || [];
      
      // Check standalone paragraphs
      let found = paragraphs.find(para => para.name === name);
      if (found) return found;
      
      // Check paragraphs within sections
      for (const section of sections) {
        found = section.paragraphs?.find(para => para.name === name);
        if (found) return found;
      }
      
      return null;
    };
    
    const caller = findParagraph(callerName);
    if (!caller) {
      return {
        message: () => `Caller paragraph '${callerName}' not found`,
        pass: false
      };
    }
    
    const callsTarget = caller.calledSectionsParagraphs?.some(call => call.name === targetName);
    
    return {
      message: () => callsTarget
        ? `Expected '${callerName}' not to call '${targetName}'`
        : `Expected '${callerName}' to call '${targetName}' but no such call was found`,
      pass: !!callsTarget
    };
  },

  async toParseWithin(received: () => Promise<any>, maxDuration: number) {
    const startTime = performance.now();
    
    try {
      await received();
      const duration = performance.now() - startTime;
      
      return {
        message: () => duration <= maxDuration
          ? `Expected parsing to take longer than ${maxDuration}ms but took ${duration.toFixed(2)}ms`
          : `Expected parsing to complete within ${maxDuration}ms but took ${duration.toFixed(2)}ms`,
        pass: duration <= maxDuration
      };
    } catch (error) {
      return {
        message: () => `Parsing failed: ${error instanceof Error ? error.message : String(error)}`,
        pass: false
      };
    }
  },

  async toUseMemoryUnder(received: () => Promise<any>, maxMemoryMB: number) {
    const initialMemory = process.memoryUsage().heapUsed;
    
    try {
      await received();
      global.gc && global.gc(); // Force garbage collection if available
      
      const finalMemory = process.memoryUsage().heapUsed;
      const memoryUsedMB = (finalMemory - initialMemory) / (1024 * 1024);
      
      return {
        message: () => memoryUsedMB <= maxMemoryMB
          ? `Expected to use more than ${maxMemoryMB}MB but used ${memoryUsedMB.toFixed(2)}MB`
          : `Expected to use less than ${maxMemoryMB}MB but used ${memoryUsedMB.toFixed(2)}MB`,
        pass: memoryUsedMB <= maxMemoryMB
      };
    } catch (error) {
      return {
        message: () => `Operation failed: ${error instanceof Error ? error.message : String(error)}`,
        pass: false
      };
    }
  },

  toHaveASTStructure(received: ASTNode, expected: any) {
    const compareStructure = (actual: any, expected: any, path = ''): string[] => {
      const errors: string[] = [];
      
      if (typeof expected === 'object' && expected !== null) {
        for (const [key, expectedValue] of Object.entries(expected)) {
          const currentPath = path ? `${path}.${key}` : key;
          
          if (!(key in actual)) {
            errors.push(`Missing property at ${currentPath}`);
          } else {
            errors.push(...compareStructure(actual[key], expectedValue, currentPath));
          }
        }
      } else {
        if (actual !== expected) {
          errors.push(`Expected ${expected} at ${path || 'root'} but got ${actual}`);
        }
      }
      
      return errors;
    };
    
    const errors = compareStructure(received, expected);
    const isValid = errors.length === 0;
    
    return {
      message: () => isValid
        ? 'Expected AST structure not to match'
        : `AST structure mismatch: ${errors.join(', ')}`,
      pass: isValid
    };
  },

  toContainNode(received: ASTNode, nodeType: string, count?: number) {
    const findNodes = (node: ASTNode, type: string): ASTNode[] => {
      const results: ASTNode[] = [];
      
      if (node.type === type) {
        results.push(node);
      }
      
      if (node.children) {
        for (const child of node.children) {
          results.push(...findNodes(child, type));
        }
      }
      
      return results;
    };
    
    const foundNodes = findNodes(received, nodeType);
    const actualCount = foundNodes.length;
    
    if (count !== undefined) {
      const hasExactCount = actualCount === count;
      return {
        message: () => hasExactCount
          ? `Expected not to find exactly ${count} nodes of type '${nodeType}'`
          : `Expected to find exactly ${count} nodes of type '${nodeType}' but found ${actualCount}`,
        pass: hasExactCount
      };
    } else {
      const hasAnyNodes = actualCount > 0;
      return {
        message: () => hasAnyNodes
          ? `Expected not to find any nodes of type '${nodeType}'`
          : `Expected to find at least one node of type '${nodeType}' but found none`,
        pass: hasAnyNodes
      };
    }
  }
};