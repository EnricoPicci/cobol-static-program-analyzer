# COBOL Static Program Analyzer - TDD Implementation Strategy

## Test-Driven Development Plan

This document outlines the comprehensive Test-Driven Development strategy for implementing the COBOL Static Program Analyzer. The approach ensures robust testing coverage across all components, from basic tokenization through complex static analysis operations.

## Testing Philosophy

### Red-Green-Refactor Cycle
1. **Red**: Write failing tests that define desired functionality
2. **Green**: Write minimal code to make tests pass
3. **Refactor**: Improve code quality while maintaining test coverage

### Testing Pyramid Strategy
```
                    ┌──────────────────┐
                    │   E2E Tests      │  ← Few, but comprehensive
                    │   (5-10 tests)   │
                ┌───┴──────────────────┴───┐
                │   Integration Tests      │  ← Component interactions
                │   (30-50 tests)         │
            ┌───┴──────────────────────────┴───┐
            │        Unit Tests                │  ← Many, fast, isolated
            │        (200+ tests)              │
            └──────────────────────────────────┘
```

## Testing Framework Architecture

### Core Setup
- **Framework**: Jest v29+ with TypeScript support
- **Test Runner**: ts-jest for TypeScript compilation
- **Coverage Target**: 80% minimum for critical components
- **Assertion Library**: Jest built-in assertions + custom matchers

### Project Configuration

```javascript
// jest.config.js
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  collectCoverageFrom: [
    'src/**/*.ts',
    '!src/generated/**',
    '!src/**/*.d.ts'
  ],
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80
    }
  },
  setupFilesAfterEnv: ['<rootDir>/tests/setup.ts']
};
```

## Test Structure Organization

### Directory Layout
```
tests/
├── data/                    # Test data files
│   ├── valid/              # Valid COBOL programs
│   │   ├── basic/          # Simple programs
│   │   ├── intermediate/   # Complex control flow
│   │   └── advanced/       # Real-world examples
│   ├── invalid/            # Programs with errors
│   │   ├── syntax/         # Syntax errors
│   │   ├── semantic/       # COBOL rule violations
│   │   └── preprocessor/   # COPY/REPLACE errors
│   ├── copybooks/          # Test copybook files
│   ├── expected-asts/      # Expected AST outputs (JSON)
│   └── configs/            # Test configuration files
├── unit/                   # Unit tests
│   ├── lexer/             # Tokenization tests
│   ├── parser/            # Parsing tests
│   ├── ast/               # AST construction tests
│   ├── analysis/          # Static analysis tests
│   └── utils/             # Utility function tests
├── integration/            # Integration tests
│   ├── preprocessing/     # COPY/REPLACE integration
│   ├── parsing/           # End-to-end parsing
│   └── analysis/          # Complete analysis workflow
├── e2e/                   # End-to-end tests
│   ├── real-world/        # Actual COBOL programs
│   └── performance/       # Performance benchmarks
└── utils/                 # Testing utilities
    ├── helpers.ts         # Test helper functions
    ├── matchers.ts        # Custom Jest matchers
    └── fixtures.ts        # Common test fixtures
```

## Test Categories & Specifications

### 1. Lexer Unit Tests (`tests/unit/lexer/`)

**Test Cases**:
```typescript
describe('CobolLexer', () => {
  describe('Keywords', () => {
    test('should tokenize COBOL keywords correctly', () => {
      const tokens = tokenize('IDENTIFICATION DIVISION');
      expect(tokens).toHaveTokens([
        { type: 'IDENTIFICATION', text: 'IDENTIFICATION' },
        { type: 'DIVISION', text: 'DIVISION' }
      ]);
    });
  });

  describe('Literals', () => {
    test('should handle numeric literals', () => {
      expect(tokenize('123')).toHaveToken({ type: 'NUMERIC_LITERAL', value: 123 });
      expect(tokenize('123.45')).toHaveToken({ type: 'NUMERIC_LITERAL', value: 123.45 });
    });

    test('should handle string literals', () => {
      expect(tokenize('"Hello World"')).toHaveToken({ 
        type: 'STRING_LITERAL', 
        value: 'Hello World' 
      });
    });
  });

  describe('Identifiers', () => {
    test('should tokenize COBOL identifiers', () => {
      expect(tokenize('CUSTOMER-NAME')).toHaveToken({
        type: 'IDENTIFIER',
        text: 'CUSTOMER-NAME'
      });
    });
  });

  describe('Error Handling', () => {
    test('should handle invalid characters gracefully', () => {
      expect(() => tokenize('INVALID@CHAR')).toThrow('Invalid character');
    });
  });
});
```

### 2. Parser Unit Tests (`tests/unit/parser/`)

**Division Parsing Tests**:
```typescript
describe('CobolParser', () => {
  describe('Identification Division', () => {
    test('should parse minimal identification division', () => {
      const source = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-PROGRAM.
      `;
      const ast = parse(source);
      expect(ast.identificationDivision.programId).toBe('TEST-PROGRAM');
    });
  });

  describe('Data Division', () => {
    test('should parse working storage section', () => {
      const source = `
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 CUSTOMER-RECORD.
           05 CUSTOMER-NAME    PIC X(30).
           05 CUSTOMER-ID      PIC 9(5).
      `;
      const ast = parse(source);
      expect(ast.dataDivision.workingStorage).toHaveLength(1);
      expect(ast.dataDivision.workingStorage[0].name).toBe('CUSTOMER-RECORD');
    });
  });

  describe('Procedure Division', () => {
    test('should parse sections and paragraphs', () => {
      const source = `
        PROCEDURE DIVISION.
        MAIN-SECTION SECTION.
        PROCESS-DATA.
            PERFORM CALCULATE-TOTALS.
            DISPLAY "Processing complete".
        CALCULATE-TOTALS.
            ADD 1 TO COUNTER.
      `;
      const ast = parse(source);
      expect(ast.procedureDivision.sections).toHaveLength(1);
      expect(ast.procedureDivision.sections[0].paragraphs).toHaveLength(2);
    });
  });
});
```

### 3. Preprocessor Tests (`tests/unit/preprocessor/`)

**COPY Statement Tests**:
```typescript
describe('CobolPreprocessor', () => {
  describe('COPY Statement Processing', () => {
    test('should resolve simple COPY statements', async () => {
      const source = 'COPY CUSTOMER-RECORD.';
      const result = await preprocessor.process(source, { 
        copyPaths: ['tests/data/copybooks'] 
      });
      expect(result.expandedSource).toContain('01 CUSTOMER-RECORD');
    });

    test('should handle COPY with REPLACING', async () => {
      const source = 'COPY TEMPLATE REPLACING ==PREFIX== BY ==CUST==.';
      const result = await preprocessor.process(source);
      expect(result.expandedSource).toContain('CUST-RECORD');
    });

    test('should detect circular COPY references', async () => {
      const source = 'COPY CIRCULAR1.';
      await expect(preprocessor.process(source)).rejects.toThrow('Circular dependency');
    });
  });
});
```

### 4. AST Construction Tests (`tests/unit/ast/`)

**Node Creation Tests**:
```typescript
describe('ASTBuilder', () => {
  test('should create correct node hierarchy', () => {
    const parseTree = parseToTree('IDENTIFICATION DIVISION. PROGRAM-ID. TEST.');
    const ast = astBuilder.build(parseTree);
    
    expect(ast).toBeInstanceOf(CobolProgram);
    expect(ast.identificationDivision).toBeInstanceOf(IdentificationDivision);
    expect(ast.identificationDivision.programId).toBe('TEST');
  });

  test('should preserve source locations', () => {
    const ast = parseAndBuild('PROGRAM-ID. TEST.');
    expect(ast.identificationDivision.location).toEqual({
      line: 1,
      column: 1,
      endLine: 1,
      endColumn: 18
    });
  });
});
```

### 5. Static Analysis Tests (`tests/unit/analysis/`)

**Analysis Component Tests**:
```typescript
describe('StaticAnalyzer', () => {
  describe('Dead Code Detection', () => {
    test('should identify unreachable paragraphs', () => {
      const program = parseProgram(`
        PROCEDURE DIVISION.
        MAIN-PARA.
            PERFORM SUB-PARA.
            STOP RUN.
        SUB-PARA.
            DISPLAY "Called".
        UNREACHABLE-PARA.
            DISPLAY "Never called".
      `);
      
      const analysis = analyzer.analyze(program);
      expect(analysis.deadCode).toHaveLength(1);
      expect(analysis.deadCode[0].name).toBe('UNREACHABLE-PARA');
    });
  });

  describe('Variable Usage Analysis', () => {
    test('should identify unused variables', () => {
      const program = parseProgram(`
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 USED-VAR    PIC X(10).
        01 UNUSED-VAR  PIC X(10).
        
        PROCEDURE DIVISION.
        MAIN-PARA.
            MOVE "TEST" TO USED-VAR.
            DISPLAY USED-VAR.
      `);
      
      const analysis = analyzer.analyze(program);
      expect(analysis.unusedVariables).toHaveLength(1);
      expect(analysis.unusedVariables[0].name).toBe('UNUSED-VAR');
    });
  });
});
```

## Test Data Management

### Sample Program Collection

**Basic Programs** (`tests/data/valid/basic/`):
- `hello-world.cbl`: Minimal COBOL program
- `arithmetic.cbl`: Basic arithmetic operations
- `string-handling.cbl`: String manipulation
- `file-io.cbl`: Simple file operations

**Intermediate Programs** (`tests/data/valid/intermediate/`):
- `nested-performs.cbl`: Complex PERFORM logic
- `file-processing.cbl`: Sequential file processing
- `subprograms.cbl`: CALL statement usage
- `table-processing.cbl`: Array/table operations

**Advanced Programs** (`tests/data/valid/advanced/`):
- `inventory-system.cbl`: Real-world business logic
- `report-generator.cbl`: Complex reporting
- `database-interface.cbl`: SQL integration
- `multi-program.cbl`: Program composition

**Error Test Cases** (`tests/data/invalid/`):
- `syntax-errors.cbl`: Various syntax violations
- `semantic-errors.cbl`: COBOL rule violations
- `missing-divisions.cbl`: Incomplete program structure

### Copybook Test Files (`tests/data/copybooks/`):
- `customer-record.cpy`: Basic data structure
- `error-codes.cpy`: Constant definitions
- `nested-copy.cpy`: Copybook that includes other copybooks
- `with-replacing.cpy`: Template copybook for REPLACING tests

## Testing Utilities

### Custom Jest Matchers

```typescript
// tests/utils/matchers.ts
declare global {
  namespace jest {
    interface Matchers<R> {
      toParseSuccessfully(): R;
      toHaveASTStructure(expected: any): R;
      toContainNode(nodeType: string, count?: number): R;
      toHaveToken(expectedToken: Token): R;
      toHaveTokens(expectedTokens: Token[]): R;
    }
  }
}

export function toParseSuccessfully(received: string) {
  try {
    const ast = parse(received);
    return {
      message: () => `Expected parsing to fail but it succeeded`,
      pass: true
    };
  } catch (error) {
    return {
      message: () => `Expected parsing to succeed but failed: ${error.message}`,
      pass: false
    };
  }
}
```

### Test Helper Functions

```typescript
// tests/utils/helpers.ts
export function parseProgram(source: string): CobolProgram {
  const preprocessed = preprocessor.process(source);
  return parser.parse(preprocessed.expandedSource);
}

export function parseAndAnalyze(source: string): AnalysisResult {
  const program = parseProgram(source);
  return analyzer.analyze(program);
}

export function loadTestProgram(filename: string): string {
  return fs.readFileSync(path.join(__dirname, '../data/valid', filename), 'utf8');
}

export function createMockCopybook(name: string, content: string): void {
  const copybookPath = path.join(__dirname, '../data/copybooks', `${name}.cpy`);
  fs.writeFileSync(copybookPath, content);
}
```

## Quality Gates & Coverage

### Coverage Requirements
- **Critical Components**: 90%+ coverage
  - Parser core functionality
  - AST construction
  - COPY statement processing
- **Important Components**: 80%+ coverage
  - Static analysis algorithms
  - Error handling
  - Utility functions
- **Supporting Components**: 70%+ coverage
  - Configuration management
  - Logging and diagnostics

### Performance Benchmarks
- **Unit Tests**: Each test completes in < 100ms
- **Integration Tests**: Each test completes in < 1s
- **E2E Tests**: Each test completes in < 10s
- **Memory Usage**: Tests use < 256MB peak memory

### Continuous Integration
```yaml
# .github/workflows/test.yml
name: Test Suite
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
      - run: npm install
      - run: npm run test:unit
      - run: npm run test:integration
      - run: npm run test:e2e
      - run: npm run test:coverage
```

## Implementation Timeline

### Week 1-2: Test Infrastructure
- [ ] Setup Jest configuration and test directory structure
- [ ] Implement testing utilities and custom matchers
- [ ] Create basic test data files
- [ ] Setup CI/CD pipeline with quality gates

### Week 3-4: Parser Tests
- [ ] Lexer unit tests (tokenization)
- [ ] Parser unit tests (division parsing)
- [ ] AST construction tests
- [ ] Error handling tests

### Week 5-6: Preprocessor Tests
- [ ] COPY statement processing tests
- [ ] REPLACE statement tests
- [ ] Nested copybook tests
- [ ] Error recovery tests

### Week 7-8: Integration & Analysis Tests
- [ ] End-to-end parsing integration tests
- [ ] Static analysis component tests
- [ ] Performance benchmark tests
- [ ] Real-world program tests

### Week 9-10: Advanced Testing
- [ ] Large program stress tests
- [ ] Memory usage and leak tests
- [ ] Concurrent processing tests
- [ ] Edge case and regression tests

## Success Criteria

### Functional Testing
- [ ] All COBOL language constructs parse correctly
- [ ] COPY statement resolution works for all test cases
- [ ] Static analysis produces accurate results
- [ ] Error messages are clear and actionable

### Quality Metrics
- [ ] 80%+ test coverage achieved
- [ ] All tests pass consistently
- [ ] Performance benchmarks met
- [ ] Memory usage within limits

### Development Experience
- [ ] Fast test execution (full suite < 30s)
- [ ] Clear test failure messages
- [ ] Easy test data management
- [ ] Productive TDD workflow

This comprehensive TDD strategy ensures robust testing coverage across all aspects of the COBOL Static Program Analyzer, from basic parsing through advanced static analysis capabilities.