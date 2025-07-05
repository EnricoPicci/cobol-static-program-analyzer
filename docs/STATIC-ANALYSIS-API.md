# COBOL Static Analysis API Reference

## Overview

The Static Analysis API provides programmatic access to comprehensive static analysis capabilities for COBOL programs. This document covers all public APIs, interfaces, and usage patterns.

## Table of Contents

1. [StaticAnalyzer Class](#staticanalyzer-class)
2. [Configuration Interfaces](#configuration-interfaces)
3. [Data Types and Interfaces](#data-types-and-interfaces)
4. [Error Handling](#error-handling)
5. [Analysis Results](#analysis-results)
6. [Integration APIs](#integration-apis)
7. [Utility Functions](#utility-functions)

## StaticAnalyzer Class

The main class for performing static analysis on COBOL programs.

### Constructor

```typescript
constructor(config?: StaticAnalysisConfig)
```

**Parameters:**
- `config` (optional): Configuration object defining which analysis passes to run

**Example:**
```typescript
import { StaticAnalyzer, DEFAULT_STATIC_ANALYSIS_CONFIG } from 'cobol-static-program-analyzer';

// Default configuration
const analyzer = new StaticAnalyzer();

// Custom configuration
const customConfig = { ...DEFAULT_STATIC_ANALYSIS_CONFIG, detectDeadCode: false };
const customAnalyzer = new StaticAnalyzer(customConfig);
```

### Core Methods

#### analyze(program: CobolProgram): void

Performs static analysis on the provided COBOL program.

**Parameters:**
- `program`: The parsed COBOL program to analyze

**Throws:**
- `Error`: If analysis fails due to internal errors

**Example:**
```typescript
const analyzer = new StaticAnalyzer();
analyzer.analyze(cobolProgram);
```

#### clear(): void

Clears all analysis results and resets internal state.

**Example:**
```typescript
analyzer.clear();
console.log(analyzer.getSummary().errorCount); // 0
```

### Result Access Methods

#### getErrors(): AnalysisError[]

Returns all analysis errors found during the last analysis.

**Returns:**
- Array of `AnalysisError` objects representing critical issues

**Example:**
```typescript
const errors = analyzer.getErrors();
errors.forEach(error => {
    console.log(`Error: ${error.message} at ${error.location?.line}:${error.location?.column}`);
});
```

#### getWarnings(): AnalysisError[]

Returns all analysis warnings found during the last analysis.

**Returns:**
- Array of `AnalysisError` objects representing non-critical issues

**Example:**
```typescript
const warnings = analyzer.getWarnings();
warnings.forEach(warning => {
    console.log(`Warning: ${warning.message}`);
    if (warning.suggestions) {
        warning.suggestions.forEach(suggestion => {
            console.log(`  Suggestion: ${suggestion}`);
        });
    }
});
```

#### hasErrors(): boolean

Checks if the analysis found any errors.

**Returns:**
- `true` if errors were found, `false` otherwise

**Example:**
```typescript
if (analyzer.hasErrors()) {
    console.log('Analysis found errors - build should fail');
}
```

#### getSummary(): AnalysisSummary

Returns a comprehensive summary of the analysis results.

**Returns:**
- `AnalysisSummary` object containing counts and status information

**Example:**
```typescript
const summary = analyzer.getSummary();
console.log(`Analysis complete: ${summary.errorCount} errors, ${summary.warningCount} warnings`);
console.log(`Analyzed ${summary.callGraphNodes} nodes and ${summary.variablesAnalyzed} variables`);
```

### Analysis Data Access Methods

#### getCallGraph(): Map<string, CallGraphNode>

Returns the call graph constructed during analysis.

**Returns:**
- Map of paragraph/section names to `CallGraphNode` objects

**Example:**
```typescript
const callGraph = analyzer.getCallGraph();
callGraph.forEach((node, name) => {
    console.log(`${name} calls: ${node.callees.join(', ')}`);
    console.log(`${name} called by: ${node.callers.join(', ')}`);
});
```

#### getVariableUsage(): Map<string, VariableUsage>

Returns variable usage information collected during analysis.

**Returns:**
- Map of variable names to `VariableUsage` objects

**Example:**
```typescript
const variableUsage = analyzer.getVariableUsage();
variableUsage.forEach((usage, name) => {
    console.log(`${name}: read=${usage.isRead}, written=${usage.isWritten}, initialized=${usage.isInitialized}`);
});
```

#### getErrorHandler(): CobolErrorHandler

Returns the error handler instance for integration with external systems.

**Returns:**
- `CobolErrorHandler` instance containing all diagnostics

**Example:**
```typescript
const errorHandler = analyzer.getErrorHandler();
const diagnostics = errorHandler.getAllDiagnostics();
```

#### getConfiguration(): StaticAnalysisConfig

Returns the current configuration settings.

**Returns:**
- Current `StaticAnalysisConfig` object

**Example:**
```typescript
const config = analyzer.getConfiguration();
console.log('Dead code detection enabled:', config.detectDeadCode);
```

## Configuration Interfaces

### StaticAnalysisConfig

Main configuration interface for controlling analysis behavior.

```typescript
interface StaticAnalysisConfig {
  /** Detect dead code */
  detectDeadCode: boolean;
  
  /** Detect unreachable paragraphs */
  detectUnreachableParagraphs: boolean;
  
  /** Detect unused variables */
  detectUnusedVariables: boolean;
  
  /** Detect circular dependencies */
  detectCircularDependencies: boolean;
  
  /** Detect infinite loops */
  detectInfiniteLoops: boolean;
  
  /** Detect unreferenced sections */
  detectUnreferencedSections: boolean;
  
  /** Check for missing GO TO targets */
  checkMissingGoToTargets: boolean;
  
  /** Analyze variable usage patterns */
  analyzeVariableUsage: boolean;
}
```

### DEFAULT_STATIC_ANALYSIS_CONFIG

Default configuration with all analysis passes enabled.

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

## Data Types and Interfaces

### CallGraphNode

Represents a node in the call graph.

```typescript
interface CallGraphNode {
  /** Name of the section or paragraph */
  name: string;
  
  /** Type of the node */
  type: 'section' | 'paragraph';
  
  /** Source location */
  location: SourceLocation;
  
  /** List of callers (who calls this node) */
  callers: string[];
  
  /** List of callees (who this node calls) */
  callees: string[];
  
  /** Whether this node was visited during analysis */
  visited: boolean;
  
  /** Whether this node is reachable from entry point */
  reachable: boolean;
}
```

### VariableUsage

Represents variable usage information.

```typescript
interface VariableUsage {
  /** Variable name */
  name: string;
  
  /** Variable definition */
  definition: VariableDefinition;
  
  /** All reference locations */
  references: SourceLocation[];
  
  /** Whether variable is read */
  isRead: boolean;
  
  /** Whether variable is written */
  isWritten: boolean;
  
  /** Whether variable is initialized */
  isInitialized: boolean;
}
```

### AnalysisSummary

Summary of analysis results.

```typescript
interface AnalysisSummary {
  /** Number of errors found */
  errorCount: number;
  
  /** Number of warnings found */
  warningCount: number;
  
  /** Whether any errors were found */
  hasErrors: boolean;
  
  /** Whether any warnings were found */
  hasWarnings: boolean;
  
  /** Number of call graph nodes analyzed */
  callGraphNodes: number;
  
  /** Number of variables analyzed */
  variablesAnalyzed: number;
}
```

## Error Handling

### AnalysisError

Represents an analysis error or warning.

```typescript
class AnalysisError extends CobolParsingError {
  /** Type of analysis that generated this error */
  readonly analysisType: string;
  
  /** Suggested fixes */
  readonly suggestions?: string[];
  
  constructor(
    message: string,
    analysisType: string,
    code: string,
    location: SourceLocation,
    suggestions?: string[]
  );
}
```

### Error Codes

Common error codes returned by the static analyzer:

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
| `STATIC_ANALYSIS_FAILED` | General analysis failure | Error |

## Analysis Results

### Processing Results

```typescript
// Basic result processing
analyzer.analyze(program);

const summary = analyzer.getSummary();
console.log(`Analysis Results:`);
console.log(`  Errors: ${summary.errorCount}`);
console.log(`  Warnings: ${summary.warningCount}`);
console.log(`  Call Graph Nodes: ${summary.callGraphNodes}`);
console.log(`  Variables Analyzed: ${summary.variablesAnalyzed}`);

// Detailed error processing
const errors = analyzer.getErrors();
errors.forEach(error => {
    console.log(`${error.severity.toUpperCase()}: ${error.message}`);
    console.log(`  Location: ${error.location?.line}:${error.location?.column}`);
    console.log(`  Analysis Type: ${error.analysisType}`);
    
    if (error.suggestions) {
        console.log(`  Suggestions:`);
        error.suggestions.forEach(suggestion => {
            console.log(`    - ${suggestion}`);
        });
    }
});
```

### Filtering Results

```typescript
// Filter by error code
const unusedVariables = analyzer.getWarnings().filter(w => w.code === 'UNUSED_VARIABLE');

// Filter by analysis type
const deadCodeWarnings = analyzer.getWarnings().filter(w => w.analysisType === 'dead-code-detection');

// Filter by severity
const criticalIssues = analyzer.getWarnings().filter(w => w.severity === 'error');
```

### Grouping Results

```typescript
// Group by error code
const groupedWarnings = analyzer.getWarnings().reduce((groups, warning) => {
    const code = warning.code;
    if (!groups[code]) {
        groups[code] = [];
    }
    groups[code].push(warning);
    return groups;
}, {} as Record<string, AnalysisError[]>);

// Process grouped results
Object.entries(groupedWarnings).forEach(([code, warnings]) => {
    console.log(`${code}: ${warnings.length} occurrences`);
});
```

## Integration APIs

### Error Handler Integration

```typescript
// Get error handler for integration
const errorHandler = analyzer.getErrorHandler();

// Get all diagnostics
const diagnostics = errorHandler.getAllDiagnostics();

// Generate formatted report
const report = errorHandler.generateReport();
console.log(report);
```

### Build System Integration

```typescript
class StaticAnalysisBuildStep {
    private analyzer: StaticAnalyzer;
    
    constructor(config?: StaticAnalysisConfig) {
        this.analyzer = new StaticAnalyzer(config);
    }
    
    async execute(program: CobolProgram): Promise<boolean> {
        this.analyzer.analyze(program);
        
        // Fail build on errors
        if (this.analyzer.hasErrors()) {
            console.error('Build failed due to static analysis errors');
            return false;
        }
        
        // Report warnings
        const warnings = this.analyzer.getWarnings();
        if (warnings.length > 0) {
            console.warn(`Build completed with ${warnings.length} warnings`);
        }
        
        return true;
    }
}
```

### IDE Integration

```typescript
// Language server integration
class CobolLanguageServer {
    private analyzer: StaticAnalyzer;
    
    constructor() {
        this.analyzer = new StaticAnalyzer();
    }
    
    provideDiagnostics(document: CobolProgram): DiagnosticMessage[] {
        this.analyzer.clear();
        this.analyzer.analyze(document);
        
        return this.analyzer.getErrorHandler().getAllDiagnostics();
    }
    
    provideCodeActions(diagnostic: DiagnosticMessage): CodeAction[] {
        // Convert suggestions to code actions
        return diagnostic.suggestions?.map(suggestion => ({
            title: suggestion,
            kind: 'quickfix',
            // Additional code action properties
        })) || [];
    }
}
```

## Utility Functions

### Configuration Helpers

```typescript
// Create configuration for specific use cases
export function createPerformanceConfig(): StaticAnalysisConfig {
    return {
        detectDeadCode: false,           // Expensive
        detectUnreachableParagraphs: true,
        detectUnusedVariables: true,
        detectCircularDependencies: false, // Expensive
        detectInfiniteLoops: true,
        detectUnreferencedSections: false,
        checkMissingGoToTargets: true,
        analyzeVariableUsage: false      // Expensive
    };
}

export function createComprehensiveConfig(): StaticAnalysisConfig {
    return {
        detectDeadCode: true,
        detectUnreachableParagraphs: true,
        detectUnusedVariables: true,
        detectCircularDependencies: true,
        detectInfiniteLoops: true,
        detectUnreferencedSections: true,
        checkMissingGoToTargets: true,
        analyzeVariableUsage: true
    };
}
```

### Result Utilities

```typescript
// Utility function to format analysis results
export function formatAnalysisResults(analyzer: StaticAnalyzer): string {
    const summary = analyzer.getSummary();
    const errors = analyzer.getErrors();
    const warnings = analyzer.getWarnings();
    
    let result = `Analysis Summary:\n`;
    result += `  Errors: ${summary.errorCount}\n`;
    result += `  Warnings: ${summary.warningCount}\n`;
    result += `  Call Graph Nodes: ${summary.callGraphNodes}\n`;
    result += `  Variables Analyzed: ${summary.variablesAnalyzed}\n\n`;
    
    if (errors.length > 0) {
        result += `Errors:\n`;
        errors.forEach(error => {
            result += `  - ${error.message} (${error.location?.line}:${error.location?.column})\n`;
        });
        result += `\n`;
    }
    
    if (warnings.length > 0) {
        result += `Warnings:\n`;
        warnings.forEach(warning => {
            result += `  - ${warning.message} (${warning.location?.line}:${warning.location?.column})\n`;
        });
    }
    
    return result;
}
```

## Performance Considerations

### Memory Usage

- The analyzer keeps call graphs and variable usage maps in memory during analysis
- Call `clear()` to free memory between analyses
- Consider processing large programs in chunks

### Analysis Performance

- Circular dependency detection can be expensive on large programs
- Variable usage analysis processes all statements
- Dead code detection requires full reachability analysis

### Optimization Tips

```typescript
// For development builds - faster analysis
const devConfig = createPerformanceConfig();
const devAnalyzer = new StaticAnalyzer(devConfig);

// For production builds - comprehensive analysis
const prodConfig = createComprehensiveConfig();
const prodAnalyzer = new StaticAnalyzer(prodConfig);

// Reuse analyzer instances when possible
const sharedAnalyzer = new StaticAnalyzer();
programs.forEach(program => {
    sharedAnalyzer.clear();
    sharedAnalyzer.analyze(program);
    // Process results...
});
```

## Examples

### Complete Analysis Workflow

```typescript
import { 
    StaticAnalyzer, 
    DEFAULT_STATIC_ANALYSIS_CONFIG,
    formatAnalysisResults 
} from 'cobol-static-program-analyzer';

async function analyzeProgram(program: CobolProgram): Promise<void> {
    const analyzer = new StaticAnalyzer(DEFAULT_STATIC_ANALYSIS_CONFIG);
    
    try {
        // Perform analysis
        analyzer.analyze(program);
        
        // Get results
        const summary = analyzer.getSummary();
        console.log(`Analysis complete: ${summary.errorCount} errors, ${summary.warningCount} warnings`);
        
        // Process errors
        if (analyzer.hasErrors()) {
            console.error('Critical issues found:');
            analyzer.getErrors().forEach(error => {
                console.error(`  ${error.code}: ${error.message}`);
            });
        }
        
        // Process warnings
        const warnings = analyzer.getWarnings();
        if (warnings.length > 0) {
            console.warn('Warnings found:');
            warnings.forEach(warning => {
                console.warn(`  ${warning.code}: ${warning.message}`);
                if (warning.suggestions) {
                    warning.suggestions.forEach(suggestion => {
                        console.warn(`    Suggestion: ${suggestion}`);
                    });
                }
            });
        }
        
        // Analyze call graph
        const callGraph = analyzer.getCallGraph();
        console.log(`Call graph has ${callGraph.size} nodes`);
        
        // Analyze variable usage
        const variableUsage = analyzer.getVariableUsage();
        console.log(`Analyzed ${variableUsage.size} variables`);
        
    } catch (error) {
        console.error('Analysis failed:', error);
    }
}
```

## Migration Guide

### From Version 1.x to 2.x

Breaking changes and migration steps will be documented here as the API evolves.

## Support

For API questions, bug reports, or feature requests:

1. Check the [User Guide](./STATIC-ANALYSIS-USER-GUIDE.md)
2. Review the [Configuration Guide](./STATIC-ANALYSIS-CONFIG.md)
3. See the [Integration Examples](./STATIC-ANALYSIS-INTEGRATION.md)
4. Submit issues with code examples