# COBOL Static Program Analyzer - COPY Statement Processing Plan

## Overview

This document provides a comprehensive plan for implementing COPY statement processing and copybook resolution in the COBOL Static Program Analyzer. The implementation handles complex COBOL preprocessing requirements including nested COPY statements, REPLACING clauses, and dependency tracking.

## COPY Statement Fundamentals

### COBOL COPY Statement Syntax
```cobol
COPY copybook-name
    [OF/IN library-name]
    [REPLACING clause].
```

### Supported COPY Variations
1. **Basic COPY**: `COPY CUSTOMER-RECORD.`
2. **Library Specification**: `COPY CUSTOMER-RECORD OF COPYLIB.`
3. **With REPLACING**: `COPY TEMPLATE REPLACING ==OLD== BY ==NEW==.`
4. **Complex REPLACING**: Multiple replacement patterns with pseudo-text

## Architecture Design

### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                 COPY Processing Pipeline                    │
├─────────────────────────────────────────────────────────────┤
│ 1. Source Preparation                                       │
│    ├── Format Normalization                                │
│    ├── Metadata Extraction                                 │
│    └── Initial Parsing                                     │
├─────────────────────────────────────────────────────────────┤
│ 2. COPY Resolution                                          │
│    ├── Copybook Discovery                                  │
│    ├── Dependency Analysis                                 │
│    ├── Circular Reference Detection                        │
│    └── Inclusion Planning                                  │
├─────────────────────────────────────────────────────────────┤
│ 3. REPLACING Processing                                     │
│    ├── Pseudo-text Parsing                                 │
│    ├── Pattern Matching                                    │
│    ├── Text Replacement                                    │
│    └── Validation                                          │
├─────────────────────────────────────────────────────────────┤
│ 4. Content Integration                                      │
│    ├── Source Merging                                      │
│    ├── Line Number Mapping                                 │
│    ├── Metadata Preservation                               │
│    └── Final Validation                                    │
└─────────────────────────────────────────────────────────────┘
```

### Implementation Strategy

#### Multi-Strategy Copybook Finder Pattern
```typescript
interface CopybookFinder {
  findCopybook(name: string, library?: string): Promise<CopybookResult>;
  supports(location: string): boolean;
}

class FileSystemCopybookFinder implements CopybookFinder {
  async findCopybook(name: string, library?: string): Promise<CopybookResult> {
    // Implementation for local file system
  }
}

class MVSDatasetFinder implements CopybookFinder {
  async findCopybook(name: string, library?: string): Promise<CopybookResult> {
    // Implementation for MVS datasets
  }
}
```

## Detailed Implementation Plan

### Phase 1: Core Infrastructure (Week 1-2)

#### 1.1 Copybook Resolution Framework
```typescript
interface CopybookResolver {
  resolve(copyStatement: CopyStatementContext): Promise<ResolvedCopybook>;
  configure(config: CopybookConfig): void;
}

interface CopybookConfig {
  searchPaths: string[];
  extensions: string[];
  cacheEnabled: boolean;
  maxNestingLevel: number;
  encoding: string;
}

interface ResolvedCopybook {
  name: string;
  content: string;
  path: string;
  metadata: CopybookMetadata;
  dependencies: CopybookDependency[];
}
```

#### 1.2 Dependency Tracking System
```typescript
interface DependencyTracker {
  addDependency(parent: string, child: string): void;
  detectCircularDependencies(): CircularDependency[];
  getDependencyGraph(): DependencyGraph;
  getInclusionOrder(rootCopybook: string): string[];
}

class GraphBasedDependencyTracker implements DependencyTracker {
  private graph: Map<string, Set<string>> = new Map();
  
  detectCircularDependencies(): CircularDependency[] {
    // Depth-first search for cycles
    return this.findCycles();
  }
}
```

### Phase 2: COPY Statement Processing (Week 3-4)

#### 2.1 ANTLR Visitor Implementation
```typescript
class CopyStatementVisitor extends Cobol85PreprocessorBaseVisitor<ProcessingResult> {
  visitCopyStatement(ctx: CopyStatementContext): ProcessingResult {
    const copybookName = this.extractCopybookName(ctx);
    const libraryName = this.extractLibraryName(ctx);
    const replacingClause = this.extractReplacingClause(ctx);
    
    return this.processCopyStatement({
      name: copybookName,
      library: libraryName,
      replacing: replacingClause
    });
  }
  
  private async processCopyStatement(copyInfo: CopyInfo): Promise<ProcessingResult> {
    // 1. Resolve copybook location
    const copybook = await this.resolver.resolve(copyInfo);
    
    // 2. Check for circular dependencies
    if (this.dependencyTracker.hasCircularDependency(copybook.name)) {
      throw new CircularDependencyError(copybook.name);
    }
    
    // 3. Process nested COPY statements
    const processedContent = await this.processNestedCopies(copybook.content);
    
    // 4. Apply REPLACING clause if present
    const finalContent = copyInfo.replacing 
      ? this.applyReplacing(processedContent, copyInfo.replacing)
      : processedContent;
    
    return {
      originalStatement: copyInfo,
      resolvedCopybook: copybook,
      processedContent: finalContent,
      dependencies: copybook.dependencies
    };
  }
}
```

#### 2.2 REPLACING Clause Processing
```typescript
interface ReplacingClause {
  patterns: ReplacingPattern[];
}

interface ReplacingPattern {
  oldText: PseudoText;
  newText: PseudoText;
}

class ReplacingProcessor {
  applyReplacing(content: string, replacing: ReplacingClause): string {
    let result = content;
    
    for (const pattern of replacing.patterns) {
      result = this.replacePattern(result, pattern);
    }
    
    return result;
  }
  
  private replacePattern(content: string, pattern: ReplacingPattern): string {
    // Handle pseudo-text replacement with proper COBOL tokenization
    const oldPattern = this.expandPseudoText(pattern.oldText);
    const newPattern = this.expandPseudoText(pattern.newText);
    
    return content.replace(new RegExp(oldPattern, 'g'), newPattern);
  }
}
```

### Phase 3: File System Integration (Week 5-6)

#### 3.1 Copybook Finder Implementations
```typescript
class FileSystemCopybookFinder implements CopybookFinder {
  private searchPaths: string[];
  private extensions: string[] = ['.cpy', '.copy', '.cbl', '.cob'];
  
  async findCopybook(name: string, library?: string): Promise<CopybookResult> {
    const searchLocations = this.buildSearchLocations(name, library);
    
    for (const location of searchLocations) {
      const result = await this.tryLocation(location);
      if (result.found) {
        return result;
      }
    }
    
    throw new CopybookNotFoundError(name, library);
  }
  
  private buildSearchLocations(name: string, library?: string): string[] {
    const locations: string[] = [];
    
    for (const basePath of this.searchPaths) {
      for (const extension of this.extensions) {
        // Direct file lookup
        locations.push(path.join(basePath, `${name}${extension}`));
        
        // Library subdirectory lookup
        if (library) {
          locations.push(path.join(basePath, library, `${name}${extension}`));
        }
      }
    }
    
    return locations;
  }
}
```

#### 3.2 Caching System
```typescript
interface CopybookCache {
  get(key: string): Promise<CachedCopybook | null>;
  set(key: string, copybook: CachedCopybook): Promise<void>;
  invalidate(key: string): Promise<void>;
  clear(): Promise<void>;
}

class InMemoryCopybookCache implements CopybookCache {
  private cache: Map<string, CachedCopybook> = new Map();
  
  async get(key: string): Promise<CachedCopybook | null> {
    const cached = this.cache.get(key);
    
    if (cached && !this.isExpired(cached)) {
      return cached;
    }
    
    return null;
  }
  
  private isExpired(cached: CachedCopybook): boolean {
    const maxAge = 5 * 60 * 1000; // 5 minutes
    return Date.now() - cached.timestamp > maxAge;
  }
}
```

### Phase 4: Advanced Features (Week 7-8)

#### 4.1 Nested COPY Processing
```typescript
class NestedCopyProcessor {
  private maxNestingLevel: number = 10;
  private currentNestingLevel: number = 0;
  
  async processNestedCopies(content: string): Promise<string> {
    if (this.currentNestingLevel >= this.maxNestingLevel) {
      throw new MaxNestingLevelExceededError(this.maxNestingLevel);
    }
    
    this.currentNestingLevel++;
    
    try {
      const copyStatements = this.findCopyStatements(content);
      
      if (copyStatements.length === 0) {
        return content; // No nested COPY statements
      }
      
      let processedContent = content;
      
      // Process COPY statements in reverse order to maintain correct positions
      for (const copyStatement of copyStatements.reverse()) {
        const resolved = await this.processSingleCopy(copyStatement);
        processedContent = this.replaceCopyStatement(
          processedContent, 
          copyStatement, 
          resolved.content
        );
      }
      
      // Recursively process any newly introduced COPY statements
      return await this.processNestedCopies(processedContent);
    } finally {
      this.currentNestingLevel--;
    }
  }
}
```

#### 4.2 Error Recovery and Reporting
```typescript
interface CopyProcessingError {
  type: 'COPYBOOK_NOT_FOUND' | 'CIRCULAR_DEPENDENCY' | 'REPLACING_ERROR' | 'SYNTAX_ERROR';
  message: string;
  location: SourceLocation;
  copybook?: string;
  details?: any;
}

class CopyErrorHandler {
  private errors: CopyProcessingError[] = [];
  
  handleCopybookNotFound(name: string, location: SourceLocation): void {
    this.errors.push({
      type: 'COPYBOOK_NOT_FOUND',
      message: `Copybook '${name}' not found in search paths`,
      location,
      copybook: name
    });
  }
  
  handleCircularDependency(cycle: string[], location: SourceLocation): void {
    this.errors.push({
      type: 'CIRCULAR_DEPENDENCY',
      message: `Circular dependency detected: ${cycle.join(' -> ')}`,
      location,
      details: { cycle }
    });
  }
  
  getErrors(): CopyProcessingError[] {
    return [...this.errors];
  }
  
  hasErrors(): boolean {
    return this.errors.length > 0;
  }
}
```

## Configuration Management

### Copybook Configuration
```typescript
interface CopybookConfiguration {
  searchPaths: string[];
  extensions: string[];
  cacheEnabled: boolean;
  maxNestingLevel: number;
  encoding: string;
  caseSensitive: boolean;
  parallelProcessing: boolean;
  errorRecovery: 'strict' | 'lenient';
}

class CopybookConfigManager {
  private config: CopybookConfiguration;
  
  constructor(config?: Partial<CopybookConfiguration>) {
    this.config = {
      searchPaths: ['./copybooks', './copy'],
      extensions: ['.cpy', '.copy', '.cbl', '.cob'],
      cacheEnabled: true,
      maxNestingLevel: 10,
      encoding: 'utf8',
      caseSensitive: false,
      parallelProcessing: true,
      errorRecovery: 'lenient',
      ...config
    };
  }
  
  getSearchPaths(): string[] {
    return [...this.config.searchPaths];
  }
  
  addSearchPath(path: string): void {
    if (!this.config.searchPaths.includes(path)) {
      this.config.searchPaths.push(path);
    }
  }
}
```

## Performance Optimizations

### Parallel Processing Strategy
```typescript
class ParallelCopyProcessor {
  async processMultipleCopies(copyStatements: CopyStatement[]): Promise<ProcessingResult[]> {
    // Group independent COPY statements for parallel processing
    const independentGroups = this.groupIndependentCopies(copyStatements);
    
    const results: ProcessingResult[] = [];
    
    for (const group of independentGroups) {
      // Process each group in parallel
      const groupResults = await Promise.all(
        group.map(copy => this.processSingleCopy(copy))
      );
      
      results.push(...groupResults);
    }
    
    return results;
  }
  
  private groupIndependentCopies(copies: CopyStatement[]): CopyStatement[][] {
    // Analyze dependencies and group copies that can be processed in parallel
    // Implementation uses dependency graph analysis
  }
}
```

### Caching Strategy
```typescript
class MultiLevelCache {
  private memoryCache: Map<string, CachedCopybook> = new Map();
  private diskCache?: DiskCache;
  
  async get(key: string): Promise<CachedCopybook | null> {
    // Level 1: Memory cache
    let result = this.memoryCache.get(key);
    if (result && !this.isExpired(result)) {
      return result;
    }
    
    // Level 2: Disk cache
    if (this.diskCache) {
      result = await this.diskCache.get(key);
      if (result && !this.isExpired(result)) {
        this.memoryCache.set(key, result);
        return result;
      }
    }
    
    return null;
  }
}
```

## Integration with Main Parser

### Preprocessor Integration
```typescript
class CobolPreprocessor {
  private copyProcessor: CopyProcessor;
  private replaceProcessor: ReplaceProcessor;
  
  async preprocess(source: string, config: PreprocessorConfig): Promise<PreprocessedSource> {
    // Stage 1: COPY statement processing
    const copyProcessed = await this.copyProcessor.process(source, config);
    
    // Stage 2: REPLACE statement processing
    const replaceProcessed = await this.replaceProcessor.process(copyProcessed.content, config);
    
    // Stage 3: Compiler directive processing
    const finalContent = await this.processCompilerDirectives(replaceProcessed.content);
    
    return {
      originalSource: source,
      processedSource: finalContent,
      includedCopybooks: copyProcessed.includedCopybooks,
      sourceMap: this.buildSourceMap(source, finalContent, copyProcessed.inclusions),
      errors: [...copyProcessed.errors, ...replaceProcessed.errors]
    };
  }
}
```

## Testing Strategy

### Unit Tests
```typescript
describe('CopyProcessor', () => {
  describe('Basic COPY Resolution', () => {
    test('should resolve simple COPY statement', async () => {
      const processor = new CopyProcessor();
      const result = await processor.process('COPY CUSTOMER-RECORD.');
      
      expect(result.success).toBe(true);
      expect(result.includedCopybooks).toHaveLength(1);
      expect(result.includedCopybooks[0].name).toBe('CUSTOMER-RECORD');
    });
  });
  
  describe('REPLACING Clause', () => {
    test('should apply REPLACING clause correctly', async () => {
      const source = 'COPY TEMPLATE REPLACING ==PREFIX== BY ==CUST==.';
      const result = await processor.process(source);
      
      expect(result.processedContent).toContain('CUST-RECORD');
      expect(result.processedContent).not.toContain('PREFIX-RECORD');
    });
  });
  
  describe('Error Handling', () => {
    test('should handle missing copybook gracefully', async () => {
      const result = await processor.process('COPY NONEXISTENT.');
      
      expect(result.success).toBe(false);
      expect(result.errors).toHaveLength(1);
      expect(result.errors[0].type).toBe('COPYBOOK_NOT_FOUND');
    });
  });
});
```

### Integration Tests
```typescript
describe('COPY Integration', () => {
  test('should handle nested COPY statements', async () => {
    // Test with copybook that contains other COPY statements
    const result = await processor.process('COPY MAIN-COPYBOOK.');
    
    expect(result.success).toBe(true);
    expect(result.includedCopybooks.length).toBeGreaterThan(1);
    expect(result.processedContent).toContain('NESTED-CONTENT');
  });
});
```

## Deployment Considerations

### Configuration Templates
```typescript
// Default configuration for different environments
const configurations = {
  development: {
    searchPaths: ['./copybooks', './test-copybooks'],
    cacheEnabled: false,
    errorRecovery: 'strict'
  },
  production: {
    searchPaths: ['/opt/cobol/copybooks', '/usr/local/copybooks'],
    cacheEnabled: true,
    errorRecovery: 'lenient'
  },
  mainframe: {
    searchPaths: ['//PROD.COPYLIB', '//TEST.COPYLIB'],
    caseSensitive: true,
    encoding: 'ebcdic'
  }
};
```

## Success Metrics

### Functional Requirements
- [ ] Resolve 100% of valid COPY statements
- [ ] Handle all REPLACING clause variations
- [ ] Detect and report circular dependencies
- [ ] Support minimum 10 levels of nesting
- [ ] Process large copybooks (>100KB) efficiently

### Performance Requirements
- [ ] Process 1000-line copybook in <100ms
- [ ] Handle 50 concurrent COPY resolutions
- [ ] Cache hit ratio >80% in typical usage
- [ ] Memory usage <50MB for large copybook sets

### Quality Requirements
- [ ] 90%+ test coverage for COPY processing
- [ ] Zero memory leaks in long-running processes
- [ ] Graceful degradation on errors
- [ ] Comprehensive error reporting

This comprehensive plan ensures robust COPY statement processing that meets enterprise COBOL development requirements while maintaining high performance and reliability standards.