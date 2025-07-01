# COBOL Static Program Analyzer - Technical Architecture Plan

## Executive Summary

This document outlines the comprehensive technical architecture for implementing a COBOL Static Program Analyzer using TypeScript and ANTLR4. The architecture is based on extensive research across grammar analysis, AST design, COPY statement handling, TypeScript integration, and test-driven development.

## Architecture Overview

### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                    COBOL Static Analyzer                    │
├─────────────────────────────────────────────────────────────┤
│ Public API Layer                                           │
│ ├── CobolAnalyzer (main entry point)                       │
│ ├── AnalysisResult (output interface)                      │
│ └── Configuration (analyzer settings)                      │
├─────────────────────────────────────────────────────────────┤
│ Analysis Engine                                             │
│ ├── Static Analysis Framework                              │
│ ├── Symbol Table & Cross-Reference                         │
│ ├── Call Graph Construction                                │
│ ├── Dead Code Detection                                    │
│ └── Data Flow Analysis                                     │
├─────────────────────────────────────────────────────────────┤
│ AST Layer                                                   │
│ ├── Custom AST Nodes (50+ typed interfaces)               │
│ ├── AST Builder (ANTLR to Custom AST)                     │
│ ├── Visitor Pattern Infrastructure                         │
│ └── Query & Traversal System                              │
├─────────────────────────────────────────────────────────────┤
│ Parser Layer                                                │
│ ├── Main Parser (Cobol85Parser)                           │
│ ├── Preprocessor (Cobol85PreprocessorParser)              │
│ ├── COPY Statement Resolver                               │
│ └── Error Recovery & Reporting                            │
├─────────────────────────────────────────────────────────────┤
│ Foundation Layer                                            │
│ ├── ANTLR4 Runtime (antlr4ts)                            │
│ ├── Grammar Files (Cobol85.g4, Cobol85Preprocessor.g4)   │
│ ├── Generated Parser Classes                              │
│ └── Utility Functions                                     │
└─────────────────────────────────────────────────────────────┘
```

## Key Architectural Decisions

### 1. Two-Stage Processing Architecture

**Stage 1: Preprocessing**
- COPY statement resolution and inclusion
- REPLACE statement processing
- Compiler directive handling
- Source code normalization

**Stage 2: Parsing & Analysis**
- Main grammar parsing
- Custom AST construction
- Static analysis execution
- Result compilation

### 2. Custom AST vs ANTLR Parse Tree

**Decision**: Transform ANTLR ParseTree to Custom AST
**Rationale**:
- Better suited for static analysis operations
- Type-safe traversal and querying
- Extensible for future analysis features
- Clean separation from ANTLR implementation details

### 3. Visitor Pattern for AST Operations

**Implementation Strategy**:
- Base `ASTVisitor<T>` interface for type-safe traversal
- Specialized visitors for different analysis types
- Composable visitor pattern for complex analyses
- Error handling integrated into visitor framework

## Core Interfaces

### AST Node Hierarchy

```typescript
interface ASTNode {
  type: string;
  location: SourceLocation;
  parent?: ASTNode;
  children: ASTNode[];
  metadata?: NodeMetadata;
}

interface CobolProgram extends ASTNode {
  name: string;
  identificationDivision: IdentificationDivision;
  environmentDivision?: EnvironmentDivision;
  dataDivision?: DataDivision;
  procedureDivision?: ProcedureDivision;
}

interface ProcedureDivision extends ASTNode {
  sections: SectionNode[];
  paragraphs: ParagraphNode[];
  calledSectionsParagraphs: CallReference[];
}

interface SectionNode extends ASTNode {
  name: string;
  sourceCode: string;
  paragraphs: ParagraphNode[];
  calledSectionsParagraphs: CallReference[];
}
```

### Analysis Framework

```typescript
interface StaticAnalyzer {
  analyze(program: CobolProgram): AnalysisResult;
}

interface AnalysisResult {
  program: CobolProgram;
  symbolTable: SymbolTable;
  callGraph: CallGraph;
  deadCode: DeadCode[];
  unusedVariables: Variable[];
  cyclomaticComplexity: ComplexityMetrics;
  dataFlowAnalysis: DataFlowResult;
}
```

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-2)
- Project structure setup
- Basic AST node definitions
- ANTLR integration infrastructure
- Initial visitor pattern implementation

### Phase 2: Parser Integration (Weeks 3-4)
- AST builder implementation
- Basic parsing pipeline
- Error handling framework
- Unit test infrastructure

### Phase 3: COPY Processing (Weeks 5-6)
- Copybook resolver implementation
- Dependency tracking system
- Nested COPY handling
- Preprocessor integration

### Phase 4: Static Analysis (Weeks 7-8)
- Symbol table construction
- Call graph generation
- Dead code detection
- Variable usage analysis

### Phase 5: Advanced Features (Weeks 9-10)
- Data flow analysis
- Cyclomatic complexity
- Cross-reference generation
- Performance optimization

## Performance Considerations

### Optimization Strategies

1. **Lazy AST Construction**: Build AST nodes only when needed
2. **Incremental Parsing**: Parse only changed parts of programs
3. **Caching System**: Cache parsed ASTs and analysis results
4. **Parallel Processing**: Process independent copybooks concurrently
5. **Memory Management**: Efficient node allocation and garbage collection

### Scalability Targets

- **Small Programs** (< 1K lines): Parse in < 50ms
- **Medium Programs** (1K-10K lines): Parse in < 500ms  
- **Large Programs** (10K-100K lines): Parse in < 5s
- **Memory Usage**: < 100MB for 50K line program
- **Concurrent Processing**: Support 10+ programs simultaneously

## Error Handling Strategy

### Three-Tier Error System

1. **Syntax Errors**: ANTLR parse-time errors with recovery
2. **Semantic Errors**: COBOL language rule violations
3. **Analysis Errors**: Static analysis specific issues

### Error Recovery

- **Partial AST Construction**: Continue parsing after errors
- **Error Annotation**: Mark problematic nodes for later processing
- **Diagnostic Information**: Detailed error messages with context
- **IDE Integration**: LSP-compatible error reporting

## Quality Assurance

### Testing Strategy

- **Unit Tests**: 80% coverage for core components
- **Integration Tests**: End-to-end parsing scenarios
- **Performance Tests**: Parsing speed and memory benchmarks  
- **Compatibility Tests**: Various COBOL dialects and styles

### Code Quality

- **TypeScript Strict Mode**: Full type safety enforcement
- **ESLint Configuration**: Comprehensive linting rules
- **Prettier Integration**: Consistent code formatting
- **Documentation**: TSDoc comments for all public APIs

## Technology Stack

### Core Dependencies
- **TypeScript**: v5.8.3+ with strict type checking
- **antlr4ts**: v0.5.0-alpha.4 for parser runtime
- **Jest**: v29+ for testing framework
- **ESLint**: v8+ for code quality

### Development Tools
- **Prettier**: Code formatting
- **Husky**: Git hooks for quality gates
- **TypeDoc**: API documentation generation
- **ts-node**: Development execution

## File Organization

```
src/
├── core/              # Main analyzer logic
│   ├── analyzer.ts    # CobolAnalyzer main class
│   ├── config.ts      # Configuration management
│   └── types.ts       # Core type definitions
├── ast/               # AST node definitions
│   ├── nodes/         # Individual node types
│   ├── builder.ts     # AST construction
│   └── visitor.ts     # Visitor pattern base
├── parser/            # Parser integration
│   ├── cobol-parser.ts
│   ├── preprocessor.ts
│   └── error-handler.ts
├── analysis/          # Static analysis components
│   ├── symbol-table.ts
│   ├── call-graph.ts
│   ├── dead-code.ts
│   └── data-flow.ts
├── copy/              # COPY statement handling
│   ├── resolver.ts
│   ├── dependency.ts
│   └── cache.ts
├── utils/             # Utility functions
└── generated/         # ANTLR generated files
```

## Next Steps

1. **Review and Approve Architecture**: Stakeholder review of this plan
2. **Setup Development Environment**: TypeScript, testing, and tooling
3. **Begin Phase 1 Implementation**: Foundation layer development
4. **Establish CI/CD Pipeline**: Automated testing and quality gates
5. **Create Development Milestones**: Detailed sprint planning

This architecture provides a solid foundation for building a production-quality COBOL static program analyzer with extensibility for future enhancements and enterprise-grade performance characteristics.