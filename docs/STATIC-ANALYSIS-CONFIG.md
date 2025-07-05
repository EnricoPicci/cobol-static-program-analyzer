# COBOL Static Analysis Configuration Guide

## Overview

The COBOL Static Program Analyzer provides extensive configuration options to control which analysis passes are performed and how they behave. This guide covers all configuration options, use cases, and best practices.

## Table of Contents

1. [Configuration Interface](#configuration-interface)
2. [Analysis Options](#analysis-options)
3. [Predefined Configurations](#predefined-configurations)
4. [Use Case Configurations](#use-case-configurations)
5. [Performance Tuning](#performance-tuning)
6. [Environment-Specific Settings](#environment-specific-settings)
7. [Advanced Configuration](#advanced-configuration)

## Configuration Interface

### StaticAnalysisConfig

The main configuration interface that controls all analysis behavior:

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

## Analysis Options

### Dead Code Detection

**Option:** `detectDeadCode`  
**Default:** `true`  
**Performance Impact:** High

Detects code that can never be executed due to control flow analysis.

**What it finds:**
- Unreachable sections after unconditional returns
- Paragraphs that are never called
- Code after STOP RUN statements

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Starting"
           STOP RUN.
           DISPLAY "Never reached"    *> Dead code detected
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectDeadCode: true  // Enable dead code detection
};
```

### Unreachable Paragraph Detection

**Option:** `detectUnreachableParagraphs`  
**Default:** `true`  
**Performance Impact:** Medium

Specifically focuses on paragraphs that are never called via PERFORM or GO TO.

**What it finds:**
- Paragraphs with no incoming calls
- Orphaned paragraphs after code refactoring
- Paragraphs that were previously called but no longer referenced

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM USED-PARA
           .
       
       USED-PARA.
           DISPLAY "This is called"
           .
       
       UNUSED-PARA.              *> Unreachable paragraph detected
           DISPLAY "Never called"
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectUnreachableParagraphs: true
};
```

### Unused Variable Detection

**Option:** `detectUnusedVariables`  
**Default:** `true`  
**Performance Impact:** Medium

Detects variables that are declared but never used in the program.

**What it finds:**
- Variables that are never referenced
- Variables that are only written to but never read
- Variables that are read but never initialized

**Example:**
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USED-VAR        PIC X(10).
       01 UNUSED-VAR      PIC X(10).     *> Unused variable detected
       01 WRITE-ONLY-VAR  PIC X(10).     *> Write-only variable detected
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           MOVE "VALUE" TO USED-VAR
           MOVE "VALUE" TO WRITE-ONLY-VAR
           DISPLAY USED-VAR
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectUnusedVariables: true
};
```

### Circular Dependency Detection

**Option:** `detectCircularDependencies`  
**Default:** `true`  
**Performance Impact:** High

Detects circular calling patterns that could lead to infinite recursion.

**What it finds:**
- Direct circular calls (A calls B, B calls A)
- Indirect circular calls (A calls B, B calls C, C calls A)
- Complex circular patterns

**Example:**
```cobol
       PROCEDURE DIVISION.
       PARA-A.
           PERFORM PARA-B    *> Circular dependency detected
           .
       
       PARA-B.
           PERFORM PARA-C
           .
       
       PARA-C.
           PERFORM PARA-A    *> Completes the circle
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectCircularDependencies: true
};
```

### Infinite Loop Detection

**Option:** `detectInfiniteLoops`  
**Default:** `true`  
**Performance Impact:** Low

Detects obvious infinite loop patterns.

**What it finds:**
- Self-referencing PERFORM statements
- Paragraphs that call themselves directly
- Simple infinite loop patterns

**Example:**
```cobol
       PROCEDURE DIVISION.
       ENDLESS-LOOP.
           DISPLAY "Looping forever"
           PERFORM ENDLESS-LOOP    *> Infinite loop detected
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectInfiniteLoops: true
};
```

### Unreferenced Section Detection

**Option:** `detectUnreferencedSections`  
**Default:** `true`  
**Performance Impact:** Low

Detects sections that are never referenced by any PERFORM statements.

**What it finds:**
- Sections that are never called
- Sections that might be legacy code
- Sections that were previously used but no longer referenced

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       MAIN-PARA.
           DISPLAY "Main processing"
           .
       
       UNUSED-SECTION SECTION.      *> Unreferenced section detected
       UNUSED-PARA.
           DISPLAY "Never called"
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  detectUnreferencedSections: true
};
```

### Missing GO TO Target Checking

**Option:** `checkMissingGoToTargets`  
**Default:** `true`  
**Performance Impact:** Low

Validates that all GO TO statements have valid targets.

**What it finds:**
- GO TO statements with non-existent targets
- Misspelled paragraph names in GO TO
- References to deleted paragraphs

**Example:**
```cobol
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           IF WS-FLAG = 'Y'
               GO TO NONEXISTENT-PARA    *> Missing target detected
           END-IF
           .
       
       EXISTING-PARA.
           DISPLAY "This exists"
           .
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  checkMissingGoToTargets: true
};
```

### Variable Usage Pattern Analysis

**Option:** `analyzeVariableUsage`  
**Default:** `true`  
**Performance Impact:** High

Performs detailed analysis of variable usage patterns.

**What it finds:**
- Variables referenced only in their own definition
- Complex usage patterns
- Initialization issues

**Example:**
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SELF-REF-VAR    PIC X(10) VALUE SELF-REF-VAR.  *> Self-reference detected
```

**Configuration:**
```typescript
const config = {
  ...DEFAULT_STATIC_ANALYSIS_CONFIG,
  analyzeVariableUsage: true
};
```

## Predefined Configurations

### Comprehensive Analysis

For thorough analysis in CI/CD or production builds:

```typescript
const COMPREHENSIVE_CONFIG: StaticAnalysisConfig = {
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

### Performance Optimized

For development builds where speed is important:

```typescript
const PERFORMANCE_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,           // Expensive operation
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false, // Expensive operation
  detectInfiniteLoops: true,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false      // Expensive operation
};
```

### Code Quality Focus

For code quality gates and refactoring:

```typescript
const QUALITY_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: false,  // Less critical for quality
  analyzeVariableUsage: true
};
```

### Maintenance Focus

For legacy code maintenance:

```typescript
const MAINTENANCE_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,            // Important for cleanup
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,     // Important for cleanup
  detectCircularDependencies: false, // May be expected in legacy code
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,   // Critical for correctness
  analyzeVariableUsage: false      // May be noisy in legacy code
};
```

### Basic Safety Checks

Minimal configuration for basic safety:

```typescript
const BASIC_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,
  detectUnreachableParagraphs: false,
  detectUnusedVariables: false,
  detectCircularDependencies: false,
  detectInfiniteLoops: true,       // Always important
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,   // Critical for correctness
  analyzeVariableUsage: false
};
```

## Use Case Configurations

### Development Environment

```typescript
const DEV_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,           // Too slow for development
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,     // Helpful during development
  detectCircularDependencies: false, // Too slow
  detectInfiniteLoops: true,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false
};

const devAnalyzer = new StaticAnalyzer(DEV_CONFIG);
```

### Continuous Integration

```typescript
const CI_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: true
};

const ciAnalyzer = new StaticAnalyzer(CI_CONFIG);
```

### Pre-commit Hooks

```typescript
const PRE_COMMIT_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,           // Too slow for pre-commit
  detectUnreachableParagraphs: false,
  detectUnusedVariables: false,
  detectCircularDependencies: false,
  detectInfiniteLoops: true,       // Quick and important
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,   // Quick and critical
  analyzeVariableUsage: false
};

const preCommitAnalyzer = new StaticAnalyzer(PRE_COMMIT_CONFIG);
```

### Code Review

```typescript
const REVIEW_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: false,  // Assume this is checked elsewhere
  analyzeVariableUsage: true
};

const reviewAnalyzer = new StaticAnalyzer(REVIEW_CONFIG);
```

## Performance Tuning

### Performance Impact Analysis

| Analysis Type | CPU Impact | Memory Impact | I/O Impact |
|---------------|------------|---------------|------------|
| `detectDeadCode` | High | Medium | Low |
| `detectUnreachableParagraphs` | Medium | Low | Low |
| `detectUnusedVariables` | Medium | Medium | Low |
| `detectCircularDependencies` | High | High | Low |
| `detectInfiniteLoops` | Low | Low | Low |
| `detectUnreferencedSections` | Low | Low | Low |
| `checkMissingGoToTargets` | Low | Low | Low |
| `analyzeVariableUsage` | High | High | Low |

### Optimization Strategies

#### Fast Development Builds

```typescript
const FAST_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,           // Disable expensive checks
  detectUnreachableParagraphs: true,
  detectUnusedVariables: false,    // Can be noisy during development
  detectCircularDependencies: false, // Expensive
  detectInfiniteLoops: true,       // Quick and important
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,   // Quick and critical
  analyzeVariableUsage: false      // Expensive
};
```

#### Balanced Configuration

```typescript
const BALANCED_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false, // Disable most expensive
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false      // Disable expensive
};
```

#### Memory-Constrained Environments

```typescript
const MEMORY_EFFICIENT_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: false,           // High memory usage
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false, // High memory usage
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false      // High memory usage
};
```

## Environment-Specific Settings

### Development

```typescript
const createDevelopmentConfig = (): StaticAnalysisConfig => ({
  detectDeadCode: false,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false,
  detectInfiniteLoops: true,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false
});
```

### Testing

```typescript
const createTestingConfig = (): StaticAnalysisConfig => ({
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: false, // May interfere with test setup
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false
});
```

### Production

```typescript
const createProductionConfig = (): StaticAnalysisConfig => ({
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,
  detectInfiniteLoops: true,
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: true
});
```

## Advanced Configuration

### Dynamic Configuration

```typescript
class ConfigurationManager {
  private baseConfig: StaticAnalysisConfig;
  
  constructor(baseConfig: StaticAnalysisConfig = DEFAULT_STATIC_ANALYSIS_CONFIG) {
    this.baseConfig = baseConfig;
  }
  
  getConfigForFileSize(fileSize: number): StaticAnalysisConfig {
    if (fileSize > 100000) { // Large files
      return {
        ...this.baseConfig,
        detectDeadCode: false,
        detectCircularDependencies: false,
        analyzeVariableUsage: false
      };
    }
    return this.baseConfig;
  }
  
  getConfigForBuildType(buildType: 'dev' | 'test' | 'prod'): StaticAnalysisConfig {
    switch (buildType) {
      case 'dev':
        return createDevelopmentConfig();
      case 'test':
        return createTestingConfig();
      case 'prod':
        return createProductionConfig();
      default:
        return this.baseConfig;
    }
  }
}
```

### Configuration Validation

```typescript
function validateConfig(config: StaticAnalysisConfig): string[] {
  const issues: string[] = [];
  
  // Check for potentially expensive combinations
  if (config.detectDeadCode && config.detectCircularDependencies && config.analyzeVariableUsage) {
    issues.push('Warning: All expensive analyses enabled - consider disabling some for better performance');
  }
  
  // Check for minimal configuration
  const enabledChecks = Object.values(config).filter(Boolean).length;
  if (enabledChecks === 0) {
    issues.push('Warning: No analysis checks enabled');
  }
  
  return issues;
}
```

### Configuration Builder

```typescript
class ConfigurationBuilder {
  private config: StaticAnalysisConfig;
  
  constructor() {
    this.config = { ...DEFAULT_STATIC_ANALYSIS_CONFIG };
  }
  
  enableDeadCodeDetection(enable: boolean = true): this {
    this.config.detectDeadCode = enable;
    return this;
  }
  
  enableUnusedVariableDetection(enable: boolean = true): this {
    this.config.detectUnusedVariables = enable;
    return this;
  }
  
  enableCircularDependencyDetection(enable: boolean = true): this {
    this.config.detectCircularDependencies = enable;
    return this;
  }
  
  enableAllChecks(): this {
    Object.keys(this.config).forEach(key => {
      (this.config as any)[key] = true;
    });
    return this;
  }
  
  disableExpensiveChecks(): this {
    this.config.detectDeadCode = false;
    this.config.detectCircularDependencies = false;
    this.config.analyzeVariableUsage = false;
    return this;
  }
  
  build(): StaticAnalysisConfig {
    return { ...this.config };
  }
}

// Usage
const config = new ConfigurationBuilder()
  .enableDeadCodeDetection()
  .enableUnusedVariableDetection()
  .disableExpensiveChecks()
  .build();
```

## Configuration Examples

### Real-World Scenarios

#### Large Legacy System

```typescript
const LEGACY_SYSTEM_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,            // Important for cleanup
  detectUnreachableParagraphs: true,
  detectUnusedVariables: false,    // May be noisy in legacy code
  detectCircularDependencies: false, // May be expected pattern
  detectInfiniteLoops: true,
  detectUnreferencedSections: false, // May be entry points
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false      // May be noisy
};
```

#### Microservice Development

```typescript
const MICROSERVICE_CONFIG: StaticAnalysisConfig = {
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

#### Batch Processing System

```typescript
const BATCH_PROCESSING_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: true,
  detectUnusedVariables: true,
  detectCircularDependencies: true,  // Important for batch jobs
  detectInfiniteLoops: true,         // Critical for batch jobs
  detectUnreferencedSections: true,
  checkMissingGoToTargets: true,
  analyzeVariableUsage: false        // May be too detailed
};
```

## Best Practices

### Configuration Management

1. **Use environment-specific configurations**
2. **Document configuration choices**
3. **Validate configurations before use**
4. **Consider performance impact**
5. **Review and update configurations regularly**

### Performance Guidelines

1. **Disable expensive checks in development**
2. **Use comprehensive analysis in CI/CD**
3. **Profile analysis performance**
4. **Consider file size when configuring**

### Team Coordination

1. **Establish team standards for configurations**
2. **Share configuration files in version control**
3. **Document configuration rationale**
4. **Regular configuration reviews**

## Troubleshooting Configuration Issues

### Common Problems

1. **Analysis too slow**: Disable expensive checks
2. **Too many false positives**: Adjust specific check configurations
3. **Missing important issues**: Enable more comprehensive analysis
4. **Memory issues**: Disable high-memory checks

### Debug Configuration

```typescript
const DEBUG_CONFIG: StaticAnalysisConfig = {
  detectDeadCode: true,
  detectUnreachableParagraphs: false,
  detectUnusedVariables: false,
  detectCircularDependencies: false,
  detectInfiniteLoops: false,
  detectUnreferencedSections: false,
  checkMissingGoToTargets: false,
  analyzeVariableUsage: false
};
```

This configuration enables only dead code detection to isolate issues.