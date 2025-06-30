# Cobol AST Parser - Technical Implementation Plan (Updated)

## 1. Overview and Goals

This document outlines the technical plan for building a robust Abstract Syntax Tree (AST) parser for COBOL. The primary goal is to convert COBOL source code into a structured, machine-readable JSON format (the AST) that can be used for static analysis. The parser must accurately represent the program's structure, including a clear hierarchy of sections and paragraphs, and correctly handle comments and whitespace. Crucially, the AST nodes for leaf paragraphs and sections (those that do not call other procedures) must contain sufficient information to enable an LLM to generate a detailed summary.

## 2. Technology Stack & Key Libraries

- **Language:** TypeScript
- **Runtime:** Node.js
- **Package Manager:** npm
- **Testing Framework:** Jest
- **Parser Generator:** ANTLR.js (TypeScript target)

**Rationale for ANTLR.js:** ANTLR is a powerful and mature parser generator. Research has confirmed the availability of a high-quality, open-source [COBOL 85 grammar](https://github.com/antlr/grammars-v4/tree/master/cobol85) in the official ANTLR grammars repository. This will significantly accelerate development.

## 3. Project Setup (Phase 1)

This phase establishes the foundation for the project.

1.  **Initialize Project:**
    - Run `npm init -y` to create a `package.json` file.

2.  **Install Dependencies:**
    - **Development Dependencies:** `npm install --save-dev typescript ts-node jest ts-jest @types/jest @types/node`
    - **Parser Dependencies:** `npm install --save-dev antlr4ts-cli`
    - **Runtime Dependencies:** `npm install antlr4ts`

3.  **Configure TypeScript:**
    - Run `npx tsc --init` to generate a `tsconfig.json` file.
    - Key settings will include:
        - `"target": "es2020"`
        - `"module": "commonjs"`
        - `"rootDir": "./src"`
        - `"outDir": "./dist"`
        - `"esModuleInterop": true`
        - `"strict": true`

4.  **Configure Jest for TDD:**
    - Create a `jest.config.js` file.
    - Configure it to use `ts-jest` for running tests written in TypeScript.
    ```javascript
    module.exports = {
      preset: 'ts-jest',
      testEnvironment: 'node',
      testMatch: ['**/tests/**/*.test.ts'],
    };
    ```

5.  **Establish Directory Structure:**
    ```
    /
    ├── grammars/      # To store the ANTLR .g4 grammar file
    ├── src/           # Main TypeScript source code
    │   ├── ast/       # TypeScript interfaces for AST nodes
    │   ├── generated/ # Generated ANTLR parser code
    │   ├── parser.ts  # Core parser logic
    │   └── index.ts   # Main entry point for the module
    ├── tests/         # Jest test files
    │   └── parser.test.ts
    ├── package.json
    └── tsconfig.json
    ```

## 4. Grammar and Parser Generation (Phase 2)

1.  **Acquire COBOL Grammar:**
    - Download the COBOL 85 grammar files from the [ANTLR grammars repository](https://github.com/antlr/grammars-v4/tree/master/cobol85).
    - Specifically, we need:
        - `Cobol85.g4`
        - `Cobol85Preprocessor.g4`
    - Place these files into the `/grammars` directory.

2.  **Generate Parser Code:**
    - Add an npm script to `package.json` to automate the ANTLR code generation for both grammars.
    ```json
    "scripts": {
      "generate-parser": "antlr4ts -visitor -o src/generated/preprocessor grammars/Cobol85Preprocessor.g4 && antlr4ts -visitor -o src/generated/parser grammars/Cobol85.g4"
    }
    ```
    - Running `npm run generate-parser` will create two sets of lexer, parser, and visitor classes. This two-stage approach is necessary to first process `COPY` and `REPLACE` statements before parsing the main COBOL logic.

## 5. AST Node Definition (Phase 3)

Before writing tests, we will define the structure of our AST. Create TypeScript interfaces for each node type in `src/ast/nodes.ts`. This ensures a strongly-typed and consistent AST structure.

**Updated AST Node Interfaces:**

```typescript
interface Position {
  line: number;
  column: number;
}

interface BaseNode {
  type: string;
  location: {
    start: Position;
    end: Position;
  };
  comments: CommentNode[];
  sourceCode: string; // Raw source code of the node
}

interface CommentNode {
    type: 'Comment';
    value: string;
    location: {
        start: Position;
        end: Position;
    };
}

interface ProgramNode extends BaseNode {
  type: 'Program';
  programId: string;
  body: (DataDivisionNode | ProcedureDivisionNode)[];
}

interface SectionNode extends BaseNode {
    type: 'Section';
    name: string;
    body: (ParagraphNode | StatementNode)[];
    dataFlow: DataFlowInfo;
}

interface ParagraphNode extends BaseNode {
    type: 'Paragraph';
    name: string;
    body: StatementNode[];
    dataFlow: DataFlowInfo;
}

interface DataFlowInfo {
    read: string[];
    written: string[];
    // Potentially more detailed information in the future
}

// ... and so on for all other COBOL constructs.
```

## 6. Parser Implementation via TDD (Phase 4)

This phase will be iterative. For each feature, we will first write a failing test, then implement the parser logic to make it pass.

---

### **Step 6.1: Handling Comments and Whitespace**

-   **Strategy:** ANTLR can be configured to capture tokens on a "hidden" channel. We will configure the lexer to place comments and whitespace on this hidden channel. During the parsing phase, we will access these hidden tokens and attach them to the appropriate AST nodes.
-   **Test (`tests/comments.test.ts`):**
    - Write a test with comments in various positions (before a section, before a paragraph, inline with a statement).
    - Assert that the `comments` property of the corresponding `SectionNode`, `ParagraphNode`, or other relevant AST node contains the correct comment text and location information.
-   **Implementation:**
    - Modify the ANTLR grammar (`.g4` file) to send comments to a hidden channel.
    - In the `CobolAstVisitor`, before visiting a node, access the hidden token stream to collect any preceding comments.
    - Add these collected comments to the `comments` array of the AST node being constructed.

---

### **Step 6.2: Procedure Division - Sections and Paragraphs**

-   **Test (`tests/structure.test.ts`):**
    - Write a test with a `PROCEDURE DIVISION` that includes both sections and paragraphs.
    - The test should cover:
        - A section containing multiple paragraphs.
        - Standalone paragraphs outside of any section.
    - Assert that the AST correctly represents this hierarchy: `ProcedureDivisionNode` contains an array of `SectionNode` and `ParagraphNode` objects. Each `SectionNode` should, in turn, contain its own array of `ParagraphNode` objects.
-   **Implementation (`src/parser.ts`):**
    - Override `visitSection()` and `visitParagraph()` in the `CobolAstVisitor`.
    - When `visitSection()` is entered, create a `SectionNode`. Then, for all paragraphs visited within that section, add them to the `body` of the `SectionNode`.
    - When `visitParagraph()` is entered, create a `ParagraphNode` and populate its `body` with the statements it contains.
    - For both node types, capture the raw source code from the input stream and store it in the `sourceCode` property.

---

### **Step 6.3: Data Flow Analysis for Leaf Nodes**

-   **Strategy:** After constructing the basic AST for a paragraph or section, we will perform a local analysis to identify which variables are read from and written to.
-   **Test (`tests/dataflow.test.ts`):**
    - Write a test for a paragraph that contains statements like `MOVE`, `ADD`, `COMPUTE`, and `IF`.
    - Assert that the `dataFlow` property of the `ParagraphNode` contains two arrays: `read` and `written`, with the correct variable names in each.
-   **Implementation (`src/parser.ts`):**
    - Create a new visitor, `DataFlowVisitor`, that specifically analyzes the statements within a single paragraph or section.
    - This visitor will inspect each statement node (e.g., `MoveStatementNode`, `AddStatementNode`) and identify the source and destination identifiers.
    - The `read` and `written` arrays in the `dataFlow` property of the corresponding `ParagraphNode` or `SectionNode` will be populated based on this analysis.

---

### **Step 6.4: Preprocessor for COPY Statements**

-   **Test (`tests/preprocessor.test.ts`):**
    - Create a test that simulates a COBOL file with a `COPY` statement.
    - Provide the content of the copybook as part of the test setup.
    - Assert that the preprocessor correctly replaces the `COPY` statement with the content of the copybook.
-   **Implementation (`src/preprocessor.ts`):**
    - Create a function that takes the source code and a map of copybook contents.
    - Use the generated `Cobol85Preprocessor` to walk the token stream and perform the replacements.

---

### **Step 6.5 - 6.8: (Previously 6.2 - 6.6) Other Parser Features**

-   The remaining implementation steps (Basic Program Structure, Data Division, Statements, Control Flow) will proceed as previously outlined, but with the added context of being nested within the appropriate section or paragraph nodes.

## 7. Error Handling

-   **Strategy:** The parser must be resilient to syntax errors.
-   **Implementation:**
    - ANTLR provides an `ANTLRErrorListener` interface. We will implement a custom error listener that collects syntax errors (message, line, column) instead of printing them to the console.
    - The main `parse` function will return an object containing both the `ast` (which may be partial) and a list of `errors`.
-   **Test:**
    - Write a test with invalid COBOL syntax and assert that the `errors` array is populated correctly.

## 8. Integration with CLI

The final output of this parser module will be a single function:

```typescript
// in src/index.ts
export function parseCobol(sourceCode: string): { ast: ProgramNode | null; errors: SyntaxError[] } {
  // ... implementation ...
}
```

This function will be imported and used by the main command-line interface tool to get the AST for a given COBOL file, which can then be used for further analysis as described in the project's `README.md`.