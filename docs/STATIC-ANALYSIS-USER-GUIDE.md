# COBOL Static Analysis User Guide

## Overview

The COBOL Static Program Analyzer provides comprehensive static analysis capabilities for COBOL programs, helping developers identify potential issues, unused code, and improve program quality before runtime. This guide covers all aspects of using the static analysis features.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Configuration](#configuration)
3. [Analysis Features](#analysis-features)
4. [Usage Examples](#usage-examples)
5. [Integration Patterns](#integration-patterns)
6. [Troubleshooting](#troubleshooting)
7. [Best Practices](#best-practices)

## Getting Started

### Installation

The static analyzer is part of the COBOL Static Program Analyzer package. Install it using:

```bash
npm install cobol-static-program-analyzer
```

### Basic Usage

```typescript
import { StaticAnalyzer } from 'cobol-static-program-analyzer';
import { CobolProgram } from 'cobol-static-program-analyzer';

// Create analyzer instance
const analyzer = new StaticAnalyzer();

// Analyze a COBOL program
analyzer.analyze(cobolProgram);

// Get results
const warnings = analyzer.getWarnings();
const errors = analyzer.getErrors();
const summary = analyzer.getSummary();
```

### Quick Start Example

```typescript
import { StaticAnalyzer, DEFAULT_STATIC_ANALYSIS_CONFIG } from 'cobol-static-program-analyzer';

// Create analyzer with default configuration
const analyzer = new StaticAnalyzer(DEFAULT_STATIC_ANALYSIS_CONFIG);

// Analyze your COBOL program
analyzer.analyze(parsedProgram);

// Check for issues
if (analyzer.hasErrors()) {
    console.log('Analysis found errors:');
    analyzer.getErrors().forEach(error => {
        console.log(`${error.code}: ${error.message} at ${error.location?.line}:${error.location?.column}`);
    });
}

// Review warnings
const warnings = analyzer.getWarnings();
console.log(`Found ${warnings.length} warnings`);
```

## Configuration

The static analyzer supports comprehensive configuration to control which analyses are performed.

### Configuration Interface

```typescript
interface StaticAnalysisConfig {
  detectDeadCode: boolean;              // Detect unreachable code
  detectUnreachableParagraphs: boolean; // Detect unreachable paragraphs
  detectUnusedVariables: boolean;       // Detect unused variables
  detectCircularDependencies: boolean;  // Detect circular call dependencies
  detectInfiniteLoops: boolean;         // Detect potential infinite loops
  detectUnreferencedSections: boolean;  // Detect unreferenced sections
  checkMissingGoToTargets: boolean;     // Check for missing GO TO targets
  analyzeVariableUsage: boolean;        // Analyze variable usage patterns
}
```

### Default Configuration

```typescript
const DEFAULT_STATIC_ANALYSIS_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: true
};
```

### Custom Configuration

```typescript
// Create custom configuration
const customConfig: StaticAnalysisConfig = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectDeadCode: false,          // Disable dead code detection
  detectUnusedVariables: false,   // Disable unused variable detection
  // Enable other checks
};

// Use custom configuration
const analyzer = new StaticAnalyzer(customConfig);
```

## Analysis Features

### 1. Dead Code Detection

Identifies unreachable code that can never be executed.

**What it detects:**
- Unreachable paragraphs and sections
- Code after unconditional branches
- Orphaned code blocks

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Starting program"
           PERFORM SUB-PROCESS
           STOP RUN.
       
       UNREACHABLE-PARAGRAPH.    *> This will be flagged
           DISPLAY "Never reached"
           .
       
       SUB-PROCESS.
           DISPLAY "This is reachable"
           .
```

### 2. Unused Variable Detection

Identifies variables that are declared but never used.

**What it detects:**
- Variables that are never referenced
- Variables that are only written to but never read
- Variables that are read but never initialized

**Example:**
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USED-VAR        PIC X(10).
       01 UNUSED-VAR      PIC X(10).     *> This will be flagged
       01 WRITE-ONLY-VAR  PIC X(10).     *> This will be flagged
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           MOVE "VALUE" TO USED-VAR
           MOVE "VALUE" TO WRITE-ONLY-VAR
           DISPLAY USED-VAR
           .
```

### 3. Circular Dependency Detection

Identifies circular calling patterns that could lead to infinite recursion.

**What it detects:**
- Direct circular calls (A calls B, B calls A)
- Indirect circular calls (A calls B, B calls C, C calls A)
- Self-referencing paragraphs

**Example:**
```cobol
       PROCEDURE DIVISION.
       PARA-A.
           PERFORM PARA-B    *> Circular dependency detected
           .
       
       PARA-B.
           PERFORM PARA-A    *> Circular dependency detected
           .
```

### 4. GO TO Target Validation

Ensures all GO TO statements have valid targets.

**What it detects:**
- Missing paragraph names in GO TO statements
- Misspelled paragraph names
- References to non-existent labels

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           IF WS-FLAG = 'Y'
               GO TO NONEXISTENT-PARA    *> This will be flagged
           END-IF
           .
       
       EXISTING-PARA.
           DISPLAY "This exists"
           .
```

### 5. Variable Usage Pattern Analysis

Analyzes how variables are used throughout the program.

**What it detects:**
- Variables referenced only in their own definition
- Initialization patterns
- Read/write patterns

### 6. Infinite Loop Detection

Identifies potential infinite loops in the code.

**What it detects:**
- Self-referencing PERFORM statements
- Paragraphs that call themselves
- Potential endless loops

**Example:**
```cobol
       PROCEDURE DIVISION.
       ENDLESS-LOOP.
           DISPLAY "Looping forever"
           PERFORM ENDLESS-LOOP    *> This will be flagged
           .
```

## Usage Examples

### Basic Analysis

```typescript
import { StaticAnalyzer, CobolProgram } from 'cobol-static-program-analyzer';

// Parse your COBOL program first
const program: CobolProgram = /* parsed program */;

// Create analyzer
const analyzer = new StaticAnalyzer();

// Perform analysis
analyzer.analyze(program);

// Get results
const summary = analyzer.getSummary();
console.log(`Analysis complete: ${summary.errorCount} errors, ${summary.warningCount} warnings`);
```

### Selective Analysis

```typescript
// Only check for unused variables and dead code
const config = {
  detectDeadCode: true,
  detectUnreachableParagraphs: false,
  detectUnusedVariables: true,
  detectCircularDependencies: false,
  detectInfiniteLoops: false,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: false,
  analyzeVariableUsage: false
};

const analyzer = new StaticAnalyzer(config);
analyzer.analyze(program);
```

### Detailed Result Processing

```typescript
analyzer.analyze(program);

// Process warnings by type
const warnings = analyzer.getWarnings();
warnings.forEach(warning => {
  switch (warning.code) {
    case 'UNUSED_VARIABLE':
      console.log(`Unused variable: ${warning.message}`);
      break;
    case 'UNREACHABLE_CODE':
      console.log(`Dead code: ${warning.message}`);
      break;
    case 'CIRCULAR_DEPENDENCY':
      console.log(`Circular dependency: ${warning.message}`);
      break;
    // Handle other warning types
  }
});
```

### Call Graph Analysis

```typescript
analyzer.analyze(program);

// Get the call graph
const callGraph = analyzer.getCallGraph();

// Analyze call relationships
callGraph.forEach((node, name) => {
  console.log(`${name} (${node.type}):`);
  console.log(`  Calls: ${node.callees.join(', ')}`);
  console.log(`  Called by: ${node.callers.join(', ')}`);
  console.log(`  Reachable: ${node.reachable}`);
});
```

### Variable Usage Analysis

```typescript
analyzer.analyze(program);

// Get variable usage information
const variableUsage = analyzer.getVariableUsage();

// Analyze variable patterns
variableUsage.forEach((usage, name) => {
  console.log(`Variable ${name}:`);
  console.log(`  Read: ${usage.isRead}`);
  console.log(`  Written: ${usage.isWritten}`);
  console.log(`  Initialized: ${usage.isInitialized}`);
  console.log(`  References: ${usage.references.length}`);
});
```

## Integration Patterns

### With Error Handling

```typescript
import { StaticAnalyzer, CobolErrorHandler } from 'cobol-static-program-analyzer';

try {
  const analyzer = new StaticAnalyzer();
  analyzer.analyze(program);
  
  // Integrate with existing error handler
  const errorHandler: CobolErrorHandler = analyzer.getErrorHandler();
  const allDiagnostics = errorHandler.getAllDiagnostics();
  
  // Process diagnostics
  allDiagnostics.forEach(diagnostic => {
    console.log(`${diagnostic.severity}: ${diagnostic.message}`);
    if (diagnostic.suggestions) {
      diagnostic.suggestions.forEach(suggestion => {
        console.log(`  Suggestion: ${suggestion}`);
      });
    }
  });
} catch (error) {
  console.error('Analysis failed:', error);
}
```

### With Build Pipeline

```typescript
// Integration with build pipeline
class CobolBuildPipeline {
  private analyzer: StaticAnalyzer;
  
  constructor() {
    this.analyzer = new StaticAnalyzer();
  }
  
  async processProgram(program: CobolProgram): Promise<boolean> {
    // Perform static analysis
    this.analyzer.analyze(program);
    
    // Check for blocking issues
    if (this.analyzer.hasErrors()) {
      console.error('Build failed due to analysis errors');
      return false;
    }
    
    // Report warnings but continue
    const warnings = this.analyzer.getWarnings();
    if (warnings.length > 0) {
      console.warn(`Build completed with ${warnings.length} warnings`);
    }
    
    return true;
  }
}
```

### With IDE Integration

```typescript
// IDE integration for real-time analysis
class CobolLanguageServer {
  private analyzer: StaticAnalyzer;
  
  constructor() {
    this.analyzer = new StaticAnalyzer();
  }
  
  analyzeDocument(document: CobolProgram): DiagnosticMessage[] {
    this.analyzer.clear(); // Clear previous results
    this.analyzer.analyze(document);
    
    // Return diagnostics for IDE display
    return this.analyzer.getErrorHandler().getAllDiagnostics();
  }
}
```

## Troubleshooting

### Common Issues

#### 1. No Analysis Results

**Problem:** Analyzer returns no warnings or errors but program has obvious issues.

**Solutions:**
- Ensure the program has proper divisions (PROCEDURE DIVISION, DATA DIVISION)
- Check that the program structure is correctly parsed
- Verify configuration settings are enabled

```typescript
// Debug configuration
const config = analyzer.getConfiguration();
console.log('Current configuration:', config);

// Check analysis summary
const summary = analyzer.getSummary();
console.log('Analysis summary:', summary);
```

#### 2. False Positives

**Problem:** Analyzer reports issues that aren't actually problems.

**Solutions:**
- Review the specific analysis rules
- Adjust configuration to disable problematic checks
- Consider COBOL-specific patterns that might be flagged incorrectly

```typescript
// Disable specific checks that cause false positives
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectDeadCode: false, // If reporting false positives
};
```

#### 3. Performance Issues

**Problem:** Analysis takes too long on large programs.

**Solutions:**
- Disable expensive analysis passes
- Process programs in smaller chunks
- Use selective analysis for development builds

```typescript
// Performance-optimized configuration
const fastConfig = {
  detectDeadCode: false,           // Expensive
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false, // Expensive
  detectInfiniteLoops: true,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false      // Expensive
};
```

### Debug Mode

```typescript
// Enable debug logging
const analyzer = new StaticAnalyzer();

// Analyze with detailed logging
analyzer.analyze(program);

// Get detailed information
const callGraph = analyzer.getCallGraph();
const variableUsage = analyzer.getVariableUsage();

console.log('Call graph nodes:', callGraph.size);
console.log('Variables analyzed:', variableUsage.size);
```

## Best Practices

### 1. Configuration Management

- Use different configurations for different environments
- Enable all checks for comprehensive analysis
- Disable expensive checks for development builds

### 2. Result Processing

- Process warnings and errors separately
- Group similar issues together
- Provide actionable feedback to developers

### 3. Integration

- Run analysis as part of the build process
- Integrate with CI/CD pipelines
- Provide real-time feedback in IDEs

### 4. Performance

- Cache analysis results when possible
- Use incremental analysis for large codebases
- Profile analysis performance regularly

### 5. Quality Gates

- Set quality thresholds for builds
- Fail builds on critical issues
- Allow warnings but track them over time

## Error Codes Reference

| Code | Description | Severity |
|------|-------------|----------|
| `UNREACHABLE_CODE` | Code that can never be executed | Warning |
| `UNREACHABLE_PARAGRAPH` | Paragraph that is never called | Warning |
| `UNUSED_VARIABLE` | Variable that is never used | Warning |
| `WRITE_ONLY_VARIABLE` | Variable that is written but never read | Warning |
| `UNINITIALIZED_VARIABLE` | Variable that is read but never initialized | Warning |
| `CIRCULAR_DEPENDENCY` | Circular calling pattern detected | Warning |
| `INFINITE_LOOP` | Potential infinite loop detected | Warning |
| `UNREFERENCED_SECTION` | Section that is never referenced | Warning |
| `MISSING_GOTO_TARGET` | GO TO statement with missing target | Warning |
| `SELF_REFERENTIAL_VARIABLE` | Variable only used in its own definition | Warning |

## Support

For issues, questions, or contributions:

1. Check the troubleshooting section
2. Review the configuration options
3. Consult the API documentation
4. Submit issues with detailed examples

## Next Steps

- Review the [API Documentation](./STATIC-ANALYSIS-API.md)
- Check the [Configuration Guide](./STATIC-ANALYSIS-CONFIG.md)
- See [Integration Examples](./STATIC-ANALYSIS-INTEGRATION.md)