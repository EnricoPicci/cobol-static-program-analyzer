# COBOL Line-by-Line Parser Implementation Plan

## 1. Introduction

This document outlines a detailed technical implementation plan for a line-by-line COBOL parser. The parser will read COBOL source code, construct a hierarchical tree representation of the program, identify and classify comments, handle errors gracefully, and provide logging capabilities. The primary goal is to create a structured representation of the COBOL program that can be easily analyzed and summarized, particularly for use with LLM models.

## 2. Technology Stack

*   **Language:** TypeScript
*   **Runtime:** Node.js
*   **Package Manager:** npm
*   **Testing Framework:** Jest (with `ts-jest` for TypeScript support)

## 3. Core Components

*   `CobolParser` Class: The main entry point for parsing COBOL source code.
*   `CopybookHandler` Utility: Responsible for pre-processing COBOL code to resolve `COPY` statements.
*   Tree Node Definitions: TypeScript interfaces/classes defining the structure of the parsed COBOL program.
*   `CommentExtractor` Utility: Responsible for identifying and extracting comments from COBOL lines.
*   `CommentClassifier` Utility: Heuristically determines if a comment is commented-out code or an actual comment.
*   `ErrorHandling` Module: Manages custom error types and detailed error reporting.
*   `Logging` Module: Provides capabilities to track the parsing process and issues.

## 4. Data Structures (Tree Representation)

The parser will build a tree structure with the following node types:

*   **`ProgramNode`**
    *   `name: string`: The program ID.
    *   `divisions: DivisionNode[]`: An array of division nodes.
    *   `comments: CommentNode[]`: Comments associated with the program (e.g., before any division).

*   **`DivisionNode`** (Abstract Base or Interface)
    *   `type: 'IDENTIFICATION' | 'ENVIRONMENT' | 'DATA' | 'PROCEDURE'`: The type of the division.
    *   `name: string`: The full name of the division (e.g., "IDENTIFICATION DIVISION.").
    *   `rawContent: CobolLine[]`: The raw lines of code belonging to this division.
    *   `comments: CommentNode[]`: Comments found within this division.

*   **Specific Division Nodes** (extending `DivisionNode` or implementing specific interfaces)
    *   `IdentificationDivisionNode`: Properties like `programId: string`, `author: string`, etc.
    *   `EnvironmentDivisionNode`: Properties like `fileControl: string[]`, `inputOutputSection: string[]`, etc.
    *   `DataDivisionNode`: Properties like `workingStorageSection: string[]`, `linkageSection: string[]`, etc.
    *   `ProcedureDivisionNode`: Contains `sections: SectionNode[]` and `paragraphs: ParagraphNode[]` (for paragraphs not within a section).

*   **`SectionNode`** (for Procedure Division)
    *   `name: string`: The section name.
    *   `paragraphs: ParagraphNode[]`: An array of paragraphs within this section.
    *   `calledBy: string[]`: Names of sections/paragraphs that `PERFORM` this section.
    *   `calls: string[]`: Names of sections/paragraphs `PERFORMED` by this section.
    *   `summaryInfo: { purpose: string, variables: string[], operations: string[] }`: Information for LLM summarization.
    *   `comments: CommentNode[]`: Comments within this section.
    *   `rawContent: CobolLine[]`: Raw lines of code for this section.
    *   `sourceFile: string`: The file path from which this section originated (main file or copybook).

*   **`ParagraphNode`** (for Procedure Division)
    *   `name: string`: The paragraph name.
    *   `statements: CobolLine[]`: Raw COBOL statements within the paragraph.
    *   `calledBy: string[]`: Names of sections/paragraphs that `PERFORM` this paragraph.
    *   `calls: string[]`: Names of sections/paragraphs `PERFORMED` by this paragraph.
    *   `summaryInfo: { purpose: string, variables: string[], operations: string[] }`: Information for LLM summarization.
    *   `comments: CommentNode[]`: Comments within this paragraph.
    *   `rawContent: CobolLine[]`: Raw lines of code for this paragraph.
    *   `sourceFile: string`: The file path from which this paragraph originated (main file or copybook).

*   **`CommentNode`**
    *   `content: string`: The text content of the comment.
    *   `lineNumber: number`: The original line number of the comment.
    *   `isCodeComment: boolean`: `true` if the comment is likely commented-out code, `false` otherwise.
    *   `originalLine: CobolLine`: The full original line containing the comment.

*   **`CobolLine`**
    *   `content: string`: The actual text content of the line.
    *   `lineNumber: number`: The 1-based line number in the original file.
    *   `sourceFile: string`: The absolute path to the file this line originated from.

## 5. Test-Driven Development (TDD) Workflow

The implementation will follow a pragmatic TDD approach:

1.  **Define Tests:** For a given piece of functionality, define all necessary unit tests that cover expected behavior, edge cases, and error conditions.
2.  **Implement Code:** Write the code to implement the functionality, aiming to satisfy all defined tests.
3.  **Run Tests:** Execute all relevant tests.
4.  **Refactor and Iterate:** If tests fail, refactor the code to make them pass. This cycle of implementation, testing, and refactoring continues until all tests for the functionality pass and the code is clean and maintainable.
5.  **Repeat:** Move to the next piece of functionality.

## 6. Phased Implementation Plan

### Phase 1: Project Setup

*   **Objective:** Initialize the project, set up TypeScript and Jest.
*   **Steps:**
    1.  `npm init -y`
    2.  `npm install typescript ts-node @types/node jest ts-jest @types/jest --save-dev`
    3.  `npx tsc --init` (Configure `tsconfig.json` for `outDir`, `rootDir`, `esModuleInterop`, `strict`, etc.)
    4.  Configure `jest.config.js` for `ts-jest`.
    5.  Create `src/parser.ts`, `src/copybookHandler.ts`, and `tests/parser.test.ts`, `tests/copybookHandler.test.ts`.

### Phase 2: Copybook Handling (Pre-processing)

*   **Objective:** Implement the `CopybookHandler` to resolve `COPY` statements, including recursive copies, and handle file-related errors/warnings.
*   **Steps:**
    1.  **Define Tests:**
        *   Test `CopybookHandler.process()` with a COBOL source containing a single `COPY` statement.
        *   Test with nested `COPY` statements (recursive handling).
        *   Test with a `COPY` statement referencing a non-existent file, asserting a detailed error message is returned.
        *   Test with a `COPY` statement referencing an empty file, asserting a warning message is returned.
        *   Test that the processed content correctly replaces `COPY` statements with the copybook content.
        *   Test that the `sourceFile` property is correctly set for lines originating from copybooks.
    2.  **Implement:** `CopybookHandler` class with a `process(cobolSource: string, filePath: string)` method that:
        *   Reads the initial COBOL source file line by line.
        *   Identifies `COPY` statements (e.g., using regex `COPY\s+([a-zA-Z0-9-]+)\s*\.?`).
        *   Recursively calls itself to process nested copybooks.
        *   Handles file I/O (reading copybook files).
        *   Generates detailed error messages for file not found scenarios.
        *   Generates warning messages for empty copybook files.
        *   Returns the pre-processed COBOL source as an array of lines, with each line augmented to include its original `sourceFile` information.
    3.  **Integrate:** Modify `CobolParser.parse()` to first call `CopybookHandler.process()` to get the expanded COBOL source before proceeding with its own parsing logic.

### Phase 3: Basic Program Structure Parsing

*   **Objective:** Parse the basic program structure (Program ID and Division headers) from the pre-processed COBOL source.
*   **Steps:**
    1.  **Define Tests:**
        *   Test `CobolParser.parse()` with a simple COBOL program string (already pre-processed by `CopybookHandler`) that asserts a `ProgramNode` with the correct `name`.
        *   Test programs containing `IDENTIFICATION DIVISION.`, `ENVIRONMENT DIVISION.`, `DATA DIVISION.`, and `PROCEDURE DIVISION.`, asserting that correct `DivisionNode` types are created as children of `ProgramNode`.
        *   Ensure that `rawContent` for each division correctly retains the `sourceFile` information from the pre-processed lines.
    2.  **Implement:** Logic within `CobolParser` to identify `PROGRAM-ID` and division headers and create corresponding `ProgramNode` and `DivisionNode` instances, storing their `rawContent` and `sourceFile` information.

### Phase 4: Comment Extraction and Classification

*   **Objective:** Accurately identify, extract, and classify comments within the COBOL source code.
*   **Steps:**
    1.  **Define Tests:**
        *   Test `CommentExtractor` utility for full-line comments (e.g., `* This is a comment.`).
        *   Test inline comments (if applicable to the COBOL dialect, e.g., `*>`).
        *   Test comments at various line positions.
        *   Ensure `CommentNode` includes the correct `sourceFile`.
    2.  **Implement:** `CommentExtractor` utility to identify comment lines based on COBOL comment indicators (e.g., `*` in column 7).
    3.  **Define Tests:**
        *   Test `CommentClassifier` utility for comments that are clearly descriptive (e.g., `* This routine calculates total.`).
        *   Test comments that appear to be commented-out code (e.g., `* MOVE 1 TO WS-VAR.`, `* IF WS-FLAG = 'Y' THEN`).
        *   Assert `isCodeComment` property is correctly set.
    4.  **Implement:** `CommentClassifier` utility using heuristic analysis (e.g., looking for COBOL keywords, variable patterns) to determine `isCodeComment`.
    5.  Integrate `CommentExtractor` and `CommentClassifier` into the main `CobolParser` to populate `CommentNode` instances within `ProgramNode` and `DivisionNode`s, ensuring `sourceFile` is propagated.

### Phase 5: Procedure Division - Sections and Paragraphs

*   **Objective:** Parse the `PROCEDURE DIVISION` to identify and structure sections and paragraphs.
*   **Steps:**
    1.  **Define Tests:**
        *   Test a `PROCEDURE DIVISION` with multiple sections and paragraphs.
        *   Assert that `SectionNode` and `ParagraphNode` are correctly identified, named, and nested.
        *   Assert `rawContent` for each section and paragraph, including their `sourceFile`.
    2.  **Implement:** Specific parsing logic within `ProcedureDivisionNode` to identify section and paragraph headers and populate their respective arrays, ensuring `sourceFile` is correctly assigned to each node.

### Phase 6: Procedure Division - `PERFORM` Statement Analysis

*   **Objective:** Identify `PERFORM` statements and establish call relationships between sections and paragraphs.
*   **Steps:**
    1.  **Define Tests:**
        *   Test `PERFORM paragraph-name`.
        *   Test `PERFORM section-name`.
        *   Test `PERFORM paragraph-name THRU another-paragraph`.
        *   Assert that the `calls` array in the calling `ParagraphNode` or `SectionNode` is correctly populated with target names.
    2.  **Implement:** Regex or string parsing within `ParagraphNode` and `SectionNode` processing to identify `PERFORM` statements and extract target names.
    3.  **Define Tests:** Write tests to verify `calledBy` relationships are correctly established after parsing.
    4.  **Implement:** A post-parsing pass or a lookup mechanism within `CobolParser` to resolve and populate `calledBy` arrays for all `SectionNode` and `ParagraphNode` instances.

### Phase 7: Procedure Division - LLM Summary Information Extraction

*   **Objective:** Extract key information from paragraphs and sections to facilitate LLM summarization.
*   **Steps:**
    1.  **Define Tests:**
        *   Test a paragraph containing `MOVE`, `ADD`, `COMPUTE` statements with various variables.
        *   Assert that `summaryInfo.variables` is populated with relevant variable names.
        *   Assert that `summaryInfo.operations` is populated with relevant COBOL verbs (e.g., `MOVE`, `ADD`, `IF`, `PERFORM`).
    2.  **Implement:** Heuristic-based extraction logic (e.g., pattern matching for COBOL verbs and subsequent operands) to populate `summaryInfo.variables` and `summaryInfo.operations` for `ParagraphNode` and `SectionNode`.
    3.  (Note: `summaryInfo.purpose` will initially be a placeholder or require more advanced NLP/contextual analysis in a later phase, beyond the scope of this initial parser).

### Phase 8: Error Handling and Logging

*   **Objective:** Implement robust error handling and comprehensive logging for the parsing process.
*   **Steps:**
    1.  **Define Tests:**
        *   Test malformed COBOL syntax (e.g., missing `DIVISION` keyword, misspelled `PROGRAM-ID`).
        *   Assert that specific custom error types (e.g., `CobolSyntaxError`) are thrown or handled, including line numbers and detailed messages.
        *   Test that `CopybookHandler` errors (file not found, empty file) are properly logged/handled.
        *   Test that a warning is logged for sections or paragraphs that are not called by any other section or paragraph.
        *   Test that a warning is logged for `REPLACE` statements, including the file name and line number.
        *   Test that a warning is logged for `GO TO` statements, including the file name and line number.
    2.  **Implement:** Custom error classes and `try-catch` blocks within the parsing logic to gracefully handle syntax errors and provide informative messages.
    3.  **Implement:** Logic to identify uncalled sections/paragraphs and log warnings. This will likely require a post-parsing analysis step.
    4.  **Implement:** Logic to identify `REPLACE` and `GO TO` statements during parsing and log warnings with file name and line number.
    5.  **Define Tests:** (Requires mocking a logging framework) Write tests to ensure that parsing events (e.g., parser start/end, division parsed, section/paragraph identified, errors encountered) are logged at appropriate levels.
    6.  **Implement:** Integrate a simple logging utility (e.g., a custom console logger or a lightweight library) and add log statements at key points throughout the `CobolParser` and its sub-components, including `CopybookHandler`.

## 7. Testing Strategy

*   **Unit Tests:** Extensive unit tests will be written for each class and utility (e.g., `CobolParser`, `CommentExtractor`, `CommentClassifier`, individual node parsing logic) to ensure their isolated functionality is correct.
*   **Integration Tests:** Tests will be written to verify the end-to-end parsing process, ensuring that all components work together correctly to produce the expected tree structure for various COBOL programs.
*   **Regression Tests:** A suite of regression tests will be maintained to prevent new changes from breaking existing functionality.

## 8. Future Considerations (Beyond Initial Scope)

*   Support for more complex COBOL statements and syntax variations.
*   Handling of COBOL Copybooks (inclusion of external source code).
*   Performance optimizations for very large COBOL programs.
*   Advanced semantic analysis for more accurate LLM summarization.
*   Integration with a symbol table for comprehensive variable and procedure tracking.
