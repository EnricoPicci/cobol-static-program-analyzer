/**
 * Error Handling System Demonstration
 * Shows how to use the three-tier error handling system
 */

import { CobolParser, DEFAULT_COBOL_PARSER_CONFIG } from '../parser/cobol-parser';
import { ErrorCoordinator, DEFAULT_ERROR_COORDINATION_CONFIG } from '../analysis/error-coordinator';
import { SemanticValidator } from '../analysis/semantic-validator';
import { StaticAnalyzer } from '../analysis/static-analyzer';

/**
 * Demo COBOL program with various types of errors
 */
const DEMO_COBOL_WITH_ERRORS = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. ERROR-DEMO.
      AUTHOR. Demo Author.

      ENVIRONMENT DIVISION
      CONFIGURATION SECTION.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  INVALID-NAME-WITH-MORE-THAN-30-CHARACTERS   PIC X(10).
      01  DUPLICATE-VAR                               PIC 9(5).
      01  DUPLICATE-VAR                               PIC X(20).
      01  UNINITIALIZED-VAR                          PIC 9(3).
      01  UNUSED-VAR                                  PIC X(5) VALUE 'TEST'.

      PROCEDURE DIVISION.
      MAIN-SECTION SECTION.
      
      PARAGRAPH-1.
          MOVE 'HELLO' TO UNDEFINED-VAR
          PERFORM NONEXISTENT-PARAGRAPH
          ADD 1 TO UNINITIALIZED-VAR.
      
      UNREACHABLE-PARAGRAPH.
          DISPLAY 'This will never be reached'
          PERFORM UNREACHABLE-PARAGRAPH.  *> Infinite loop
      
      UNUSED-SECTION SECTION.
      UNUSED-PARA.
          DISPLAY 'This section is never called'.
      
      STOP RUN.
`;

/**
 * Demo COBOL program with correct syntax
 */
const DEMO_COBOL_CORRECT = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. CORRECT-DEMO.
      AUTHOR. Demo Author.

      ENVIRONMENT DIVISION.
      CONFIGURATION SECTION.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  WS-COUNTER                                  PIC 9(3) VALUE 0.
      01  WS-MESSAGE                                  PIC X(30) VALUE 'Hello World'.

      PROCEDURE DIVISION.
      MAIN-SECTION SECTION.
      
      MAIN-PARAGRAPH.
          DISPLAY WS-MESSAGE
          ADD 1 TO WS-COUNTER
          PERFORM HELPER-PARAGRAPH
          STOP RUN.
      
      HELPER-PARAGRAPH.
          DISPLAY 'Counter: ' WS-COUNTER.
`;

/**
 * Demonstrate three-tier error handling system
 */
export async function demonstrateErrorHandling(): Promise<void> {
  console.log('=== COBOL Three-Tier Error Handling System Demo ===\n');

  // Create parser with comprehensive error analysis enabled
  const parser = new CobolParser({
    ...DEFAULT_COBOL_PARSER_CONFIG,
    errorRecovery: 'lenient',
    maxErrors: 50
  });

  console.log('1. Parsing COBOL program with multiple error types...\n');

  try {
    // Parse program with errors
    const result = await parser.parse(DEMO_COBOL_WITH_ERRORS, 'error-demo.cob');
    
    console.log(`Parse Result: ${result.success ? 'SUCCESS' : 'FAILED'}`);
    console.log(`Total Errors: ${result.errors.length}`);
    console.log(`Total Warnings: ${result.warnings.length}\n`);

    // Display errors by tier
    displayErrorsByTier(result.errors, result.warnings);

    // If we have an AST, demonstrate individual validators
    if (result.ast) {
      console.log('\n2. Running individual validators for detailed analysis...\n');
      await demonstrateIndividualValidators(result.ast);
    }

  } catch (error) {
    console.error('Failed to parse COBOL program:', error);
  }

  console.log('\n3. Testing with correct COBOL program...\n');

  try {
    // Parse correct program
    const correctResult = await parser.parse(DEMO_COBOL_CORRECT, 'correct-demo.cob');
    
    console.log(`Parse Result: ${correctResult.success ? 'SUCCESS' : 'FAILED'}`);
    console.log(`Total Errors: ${correctResult.errors.length}`);
    console.log(`Total Warnings: ${correctResult.warnings.length}`);

    if (correctResult.success) {
      console.log('✅ Clean program parsed successfully with no errors!');
    }

  } catch (error) {
    console.error('Failed to parse correct COBOL program:', error);
  }
}

/**
 * Display errors organized by tier
 */
function displayErrorsByTier(errors: any[], warnings: any[]): void {
  const allDiagnostics = [...errors, ...warnings];
  
  // Group by tier
  const tier1 = allDiagnostics.filter(d => d.code?.includes('SYNTAX'));
  const tier2 = allDiagnostics.filter(d => d.code?.includes('SEMANTIC') || d.code?.includes('MISSING') || d.code?.includes('INVALID') || d.code?.includes('DUPLICATE'));
  const tier3 = allDiagnostics.filter(d => d.code?.includes('UNUSED') || d.code?.includes('UNREACHABLE') || d.code?.includes('CIRCULAR') || d.code?.includes('INFINITE'));

  console.log('TIER 1 - SYNTAX ERRORS:');
  if (tier1.length > 0) {
    tier1.forEach(error => {
      console.log(`  ❌ [${error.code}] ${error.message} (${error.location?.line}:${error.location?.column})`);
    });
  } else {
    console.log('  ✅ No syntax errors');
  }

  console.log('\nTIER 2 - SEMANTIC ERRORS:');
  if (tier2.length > 0) {
    tier2.forEach(error => {
      console.log(`  ❌ [${error.code}] ${error.message} (${error.location?.line}:${error.location?.column})`);
      if (error.suggestions) {
        error.suggestions.forEach((suggestion: string) => {
          console.log(`     💡 ${suggestion}`);
        });
      }
    });
  } else {
    console.log('  ✅ No semantic errors');
  }

  console.log('\nTIER 3 - STATIC ANALYSIS ISSUES:');
  if (tier3.length > 0) {
    tier3.forEach(issue => {
      const severity = issue.severity === 'error' ? '❌' : '⚠️';
      console.log(`  ${severity} [${issue.code}] ${issue.message} (${issue.location?.line}:${issue.location?.column})`);
      if (issue.suggestions) {
        issue.suggestions.forEach((suggestion: string) => {
          console.log(`     💡 ${suggestion}`);
        });
      }
    });
  } else {
    console.log('  ✅ No static analysis issues');
  }
}

/**
 * Demonstrate individual validators
 */
async function demonstrateIndividualValidators(ast: any): Promise<void> {
  console.log('Running Semantic Validator...');
  const semanticValidator = new SemanticValidator();
  semanticValidator.validate(ast);
  
  const semanticErrors = semanticValidator.getErrors();
  const semanticWarnings = semanticValidator.getWarnings();
  
  console.log(`  Semantic Errors: ${semanticErrors.length}`);
  console.log(`  Semantic Warnings: ${semanticWarnings.length}`);

  console.log('\nRunning Static Analyzer...');
  const staticAnalyzer = new StaticAnalyzer();
  staticAnalyzer.analyze(ast);
  
  const analysisErrors = staticAnalyzer.getErrors();
  const analysisWarnings = staticAnalyzer.getWarnings();
  
  console.log(`  Analysis Errors: ${analysisErrors.length}`);
  console.log(`  Analysis Warnings: ${analysisWarnings.length}`);

  // Display call graph information
  const callGraph = staticAnalyzer.getCallGraph();
  console.log(`  Call Graph Nodes: ${callGraph.size}`);
  
  // Display variable usage information
  const variableUsage = staticAnalyzer.getVariableUsage();
  console.log(`  Variables Analyzed: ${variableUsage.size}`);
}

/**
 * Demonstrate error recovery strategies
 */
export async function demonstrateErrorRecovery(): Promise<void> {
  console.log('\n=== Error Recovery Strategy Demonstration ===\n');

  const problematicCobol = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. RECOVERY-TEST
      *> Missing period above should be recovered
      
      INVALID-KEYWORD-HERE
      
      DATA DIVISION.
      WORKING-STORAGE SECTION
      *> Another missing period
      
      01  TEST-VAR PIC X(10)
      *> Yet another missing period
      
      PROCEDURE DIVISION.
      MAIN-PARA.
          DISPLAY 'Testing recovery'
          INVALID-STATEMENT-HERE
          STOP RUN.
  `;

  // Test with lenient recovery
  console.log('1. Testing with LENIENT error recovery...');
  const lenientParser = new CobolParser({
    ...DEFAULT_COBOL_PARSER_CONFIG,
    errorRecovery: 'lenient',
    maxErrors: 20
  });

  try {
    const result = await lenientParser.parse(problematicCobol, 'recovery-test.cob');
    console.log(`  Errors found: ${result.errors.length}`);
    console.log(`  Parse completed: ${result.success ? 'YES' : 'NO'}`);
    console.log(`  AST created: ${result.ast ? 'YES' : 'NO'}`);
  } catch (error) {
    console.log(`  Parser threw exception: ${error}`);
  }

  // Test with strict recovery
  console.log('\n2. Testing with STRICT error recovery...');
  const strictParser = new CobolParser({
    ...DEFAULT_COBOL_PARSER_CONFIG,
    errorRecovery: 'strict',
    maxErrors: 1
  });

  try {
    const result = await strictParser.parse(problematicCobol, 'recovery-test.cob');
    console.log(`  Errors found: ${result.errors.length}`);
    console.log(`  Parse completed: ${result.success ? 'YES' : 'NO'}`);
  } catch (error) {
    console.log(`  Parser stopped early due to strict mode`);
  }
}

// Run the demo if this file is executed directly
if (require.main === module) {
  (async () => {
    try {
      await demonstrateErrorHandling();
      await demonstrateErrorRecovery();
    } catch (error) {
      console.error('Demo failed:', error);
    }
  })();
}