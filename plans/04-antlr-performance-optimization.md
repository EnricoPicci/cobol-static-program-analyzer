# ANTLR Performance Optimization Plan

## Executive Summary

Based on comprehensive research of the existing ANTLR infrastructure, this plan outlines critical performance optimizations for the COBOL Static Program Analyzer. The primary recommendation is upgrading from legacy antlr4ts to modern antlr4ng for significant performance improvements.

## Current State Analysis

### Existing ANTLR Infrastructure ✅
- **Complete COBOL 85 Grammar**: 5,654 lines covering full NIST COBOL 85 specification
- **Preprocessor Grammar**: 1,903 lines handling COPY/REPLACE directives
- **Generated TypeScript Code**: 8 files with 595 visitor methods providing complete coverage
- **Build Integration**: Properly configured with antlr4ts-cli for visitor pattern generation

### Performance Bottlenecks Identified 🔍
- **Legacy Runtime**: antlr4ts is 4 years old and 15x slower than JavaScript equivalent
- **Memory Overhead**: Current implementation has significant memory allocation overhead
- **Cold Start Performance**: Initial parsing significantly slower than optimal
- **Concurrent Processing**: Limited support for parallel parsing operations

## Upgrade Strategy: antlr4ts → antlr4ng

### Performance Improvements Expected
- **2x faster cold runs**: Improved initial parsing performance
- **20% faster warm runs**: Better performance for repeated parsing operations
- **Better memory management**: Reduced garbage collection pressure
- **Improved TypeScript integration**: Better type safety and IDE support

### Migration Implementation Plan

#### Phase 1: Dependency Migration (Week 1)
```typescript
// package.json changes
{
  "dependencies": {
    "antlr4ng": "^3.0.0",  // Replace antlr4ts
    "antlr4-c3": "^3.0.0"  // Code completion support
  },
  "devDependencies": {
    "antlr4ng-cli": "^2.0.0"  // Replace antlr4ts-cli
  }
}
```

#### Phase 2: Code Generation Updates (Week 1-2)
```bash
# New build script for antlr4ng
antlr4ng -Dlanguage=TypeScript -visitor -no-listener \
  -o src/generated/parser/ grammars/Cobol85.g4

antlr4ng -Dlanguage=TypeScript -visitor -no-listener \
  -o src/generated/preprocessor/ grammars/Cobol85Preprocessor.g4
```

#### Phase 3: Import Updates (Week 2)
```typescript
// Old imports (antlr4ts)
import { ParseTreeVisitor } from 'antlr4ts/tree/ParseTreeVisitor';
import { ParserRuleContext } from 'antlr4ts/ParserRuleContext';

// New imports (antlr4ng)
import { ParseTreeVisitor } from 'antlr4ng/tree/ParseTreeVisitor';
import { ParserRuleContext } from 'antlr4ng/ParserRuleContext';
```

## Advanced Parser Optimization

### Visitor Pattern Implementation Strategy

```typescript
/**
 * High-performance AST builder using optimized visitor pattern
 */
class OptimizedCobolASTBuilder extends AbstractParseTreeVisitor<ASTNode> 
                                   implements Cobol85Visitor<ASTNode> {
  
  // Reusable node pool to reduce garbage collection
  private nodePool: Map<string, ASTNode[]> = new Map();
  
  // Pre-allocated result containers
  private resultStack: ASTNode[] = [];
  
  visitProgramUnit(ctx: ProgramUnitContext): CobolProgram {
    const program = this.getPooledNode<CobolProgram>('CobolProgram');
    
    // Optimized child processing with early returns
    program.identificationDivision = this.visit(ctx.identificationDivision());
    
    if (ctx.environmentDivision()) {
      program.environmentDivision = this.visit(ctx.environmentDivision());
    }
    
    if (ctx.dataDivision()) {
      program.dataDivision = this.visit(ctx.dataDivision());
    }
    
    if (ctx.procedureDivision()) {
      program.procedureDivision = this.visit(ctx.procedureDivision());
    }
    
    return program;
  }
  
  private getPooledNode<T extends ASTNode>(type: string): T {
    const pool = this.nodePool.get(type) || [];
    return pool.pop() as T || this.createNode<T>(type);
  }
}
```

### Memory Management Optimization

```typescript
/**
 * Memory-efficient parsing with lazy evaluation
 */
class LazyASTBuilder {
  private lazyNodes: Map<string, () => ASTNode> = new Map();
  
  buildLazyAST(ctx: ParserRuleContext): LazyCobolProgram {
    return {
      get identificationDivision() {
        return this.lazyEvaluate('identificationDivision', 
          () => this.visit(ctx.identificationDivision()));
      },
      
      get procedureDivision() {
        return this.lazyEvaluate('procedureDivision',
          () => this.visit(ctx.procedureDivision()));
      }
    };
  }
  
  private lazyEvaluate<T>(key: string, factory: () => T): T {
    if (!this.lazyNodes.has(key)) {
      this.lazyNodes.set(key, factory);
    }
    return this.lazyNodes.get(key)!() as T;
  }
}
```

## Parsing Pipeline Optimization

### Two-Stage Optimized Pipeline

```typescript
/**
 * High-performance two-stage parsing pipeline
 */
class OptimizedCobolParser {
  private preprocessorParser: Cobol85PreprocessorParser;
  private mainParser: Cobol85Parser;
  private astBuilder: OptimizedCobolASTBuilder;
  
  async parseProgram(source: string, options: ParseOptions): Promise<CobolProgram> {
    // Stage 1: Optimized preprocessing with parallel COPY resolution
    const preprocessResult = await this.optimizedPreprocess(source, options);
    
    // Stage 2: Main parsing with performance monitoring
    const parseResult = await this.timedParse(preprocessResult.source);
    
    // Stage 3: Optimized AST construction
    return this.astBuilder.build(parseResult);
  }
  
  private async optimizedPreprocess(source: string, options: ParseOptions): Promise<PreprocessResult> {
    const copyStatements = this.extractCopyStatements(source);
    
    // Parallel COPY resolution for independent copybooks
    const resolvedCopies = await Promise.all(
      copyStatements.map(copy => this.resolveCopy(copy))
    );
    
    return this.applyCopyResolutions(source, resolvedCopies);
  }
  
  private async timedParse(source: string): Promise<ParseTree> {
    const startTime = performance.now();
    
    try {
      const lexer = new Cobol85Lexer(CharStreams.fromString(source));
      const tokens = new CommonTokenStream(lexer);
      const parser = new Cobol85Parser(tokens);
      
      // Configure error recovery for performance
      parser.errorHandler = new FastErrorRecoveryStrategy();
      
      const result = parser.programUnit();
      
      const parseTime = performance.now() - startTime;
      this.recordPerformanceMetric('parse_time', parseTime);
      
      return result;
    } catch (error) {
      const parseTime = performance.now() - startTime;
      this.recordPerformanceMetric('parse_error_time', parseTime);
      throw error;
    }
  }
}
```

## Concurrent Processing Architecture

### Parallel Copybook Resolution

```typescript
/**
 * High-performance parallel copybook processing
 */
class ParallelCopyProcessor {
  private readonly MAX_CONCURRENT_COPIES = 10;
  private readonly copyCache = new Map<string, Promise<ResolvedCopybook>>();
  
  async resolveMultipleCopies(copyStatements: CopyStatement[]): Promise<ResolvedCopybook[]> {
    // Build dependency graph
    const dependencyGraph = this.buildDependencyGraph(copyStatements);
    
    // Process in dependency order with maximum parallelism
    const processedCopies = new Map<string, ResolvedCopybook>();
    const processingQueue = [...copyStatements];
    
    while (processingQueue.length > 0) {
      // Find copybooks ready for processing (dependencies resolved)
      const readyForProcessing = processingQueue.filter(copy => 
        this.areDependenciesResolved(copy, processedCopies)
      );
      
      // Process up to MAX_CONCURRENT_COPIES in parallel
      const currentBatch = readyForProcessing.slice(0, this.MAX_CONCURRENT_COPIES);
      
      const batchResults = await Promise.all(
        currentBatch.map(copy => this.resolveSingleCopy(copy))
      );
      
      // Add results to processed map
      batchResults.forEach((result, index) => {
        processedCopies.set(currentBatch[index].name, result);
      });
      
      // Remove processed copies from queue
      currentBatch.forEach(copy => {
        const index = processingQueue.indexOf(copy);
        processingQueue.splice(index, 1);
      });
    }
    
    return Array.from(processedCopies.values());
  }
}
```

## Performance Monitoring Integration

### Real-time Performance Metrics

```typescript
/**
 * Performance monitoring and optimization feedback system
 */
class ParserPerformanceMonitor {
  private metrics: Map<string, PerformanceMetric[]> = new Map();
  
  recordParsingMetric(operation: string, duration: number, memoryUsage: number): void {
    const metric: PerformanceMetric = {
      operation,
      duration,
      memoryUsage,
      timestamp: Date.now()
    };
    
    const operationMetrics = this.metrics.get(operation) || [];
    operationMetrics.push(metric);
    
    // Keep only last 100 metrics per operation
    if (operationMetrics.length > 100) {
      operationMetrics.shift();
    }
    
    this.metrics.set(operation, operationMetrics);
    
    // Trigger optimization if performance degrades
    this.checkPerformanceThresholds(operation, metric);
  }
  
  private checkPerformanceThresholds(operation: string, metric: PerformanceMetric): void {
    const thresholds = {
      'parse_program': { duration: 5000, memory: 100_000_000 }, // 5s, 100MB
      'resolve_copy': { duration: 100, memory: 10_000_000 },    // 100ms, 10MB
      'ast_construction': { duration: 1000, memory: 50_000_000 } // 1s, 50MB
    };
    
    const threshold = thresholds[operation];
    if (threshold) {
      if (metric.duration > threshold.duration) {
        this.triggerOptimization(operation, 'duration', metric.duration);
      }
      
      if (metric.memoryUsage > threshold.memory) {
        this.triggerOptimization(operation, 'memory', metric.memoryUsage);
      }
    }
  }
  
  getPerformanceReport(): PerformanceReport {
    const report: PerformanceReport = {
      operations: {},
      recommendations: []
    };
    
    for (const [operation, metrics] of this.metrics) {
      const avgDuration = metrics.reduce((sum, m) => sum + m.duration, 0) / metrics.length;
      const avgMemory = metrics.reduce((sum, m) => sum + m.memoryUsage, 0) / metrics.length;
      
      report.operations[operation] = {
        averageDuration: avgDuration,
        averageMemoryUsage: avgMemory,
        sampleCount: metrics.length
      };
      
      // Generate optimization recommendations
      if (avgDuration > 1000) {
        report.recommendations.push(`Consider optimizing ${operation} - average duration ${avgDuration.toFixed(2)}ms`);
      }
    }
    
    return report;
  }
}
```

## Scalability Targets & Benchmarks

### Performance Goals
- **Small Programs** (<1K lines): **<25ms** (50% improvement from 50ms)
- **Medium Programs** (1K-10K lines): **<250ms** (50% improvement from 500ms)
- **Large Programs** (10K-100K lines): **<2.5s** (50% improvement from 5s)
- **Memory Usage**: **<50MB** for 50K line program (50% improvement from 100MB)

### Benchmark Test Suite

```typescript
describe('Performance Benchmarks', () => {
  const performanceThresholds = {
    small: { lines: 1000, maxDuration: 25 },
    medium: { lines: 10000, maxDuration: 250 },
    large: { lines: 100000, maxDuration: 2500 }
  };
  
  test.each(Object.entries(performanceThresholds))(
    'should parse %s programs within performance targets',
    async (size, threshold) => {
      const testProgram = generateTestProgram(threshold.lines);
      
      const startTime = performance.now();
      const result = await parser.parseProgram(testProgram);
      const duration = performance.now() - startTime;
      
      expect(result).toBeInstanceOf(CobolProgram);
      expect(duration).toBeLessThan(threshold.maxDuration);
    }
  );
  
  test('should handle concurrent parsing efficiently', async () => {
    const programs = Array.from({ length: 10 }, () => generateTestProgram(5000));
    
    const startTime = performance.now();
    const results = await Promise.all(
      programs.map(program => parser.parseProgram(program))
    );
    const totalDuration = performance.now() - startTime;
    
    expect(results).toHaveLength(10);
    expect(totalDuration).toBeLessThan(3000); // 10 programs in <3s
  });
});
```

## Implementation Timeline

### Week 1: Runtime Migration
- [ ] Update package.json dependencies
- [ ] Regenerate ANTLR TypeScript code with antlr4ng
- [ ] Update import statements throughout codebase
- [ ] Run initial performance benchmarks

### Week 2: Visitor Optimization
- [ ] Implement optimized visitor pattern
- [ ] Add object pooling for AST nodes
- [ ] Implement lazy evaluation strategies
- [ ] Memory usage optimization

### Week 3: Pipeline Enhancement
- [ ] Optimize two-stage parsing pipeline
- [ ] Implement parallel copybook resolution
- [ ] Add performance monitoring infrastructure
- [ ] Create benchmark test suite

### Week 4: Testing & Validation
- [ ] Comprehensive performance testing
- [ ] Memory leak detection and fixes
- [ ] Concurrent processing validation
- [ ] Documentation and examples

## Success Metrics

### Functional Requirements ✅
- [ ] All existing tests pass with new runtime
- [ ] Parsing accuracy maintained at 100%
- [ ] AST structure compatibility preserved
- [ ] Error handling behavior unchanged

### Performance Requirements 🚀
- [ ] 2x improvement in cold start performance
- [ ] 20% improvement in warm parsing performance
- [ ] 50% reduction in memory usage for large programs
- [ ] Support for 10+ concurrent parsing operations

### Quality Requirements 🔍
- [ ] Zero regressions in parsing functionality
- [ ] Performance benchmarks pass consistently
- [ ] Memory leak detection shows clean results
- [ ] Load testing validates scalability improvements

This optimization plan provides a clear path to significant performance improvements while maintaining full compatibility with existing functionality and ensuring robust testing coverage.