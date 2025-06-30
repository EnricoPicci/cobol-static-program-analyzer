# COBOL Static Program Analyzer

In the Corporate IT world, there are still many COBOL programs running critical business operations. One of the biggest challenges is understanding how these programs work and identifying the business logic and rules implemented within them. This tool leverages the power of static code analysis and Large Language Models (LLMs) to generate clear, concise, and structured documentation of the business logic and business rules implemented in COBOL programs.

## Core Ideas and Approach

### Program Structure Analysis
COBOL programs are structured using paragraphs and sections that function as callable units via the `PERFORM` statement. Our approach:
- Creates a hierarchical tree of paragraphs and sections based on their call relationships
- Performs bottom-up analysis starting with leaf nodes (paragraphs/sections that don't call others)
- Uses LLM summaries of child nodes to enhance parent node analysis
- Builds a complete tree of summaries representing the program's structure and business logic

### Comprehensive COBOL Analysis
The tool provides complete coverage of COBOL program components:

#### Data Division Analysis
- **WORKING-STORAGE SECTION**: Analyzes data structures, business constants, and temporary variables
- **FILE SECTION**: Documents file layouts, record structures, and data relationships
- **LINKAGE SECTION**: Maps external data interfaces and parameter definitions
- **LOCAL-STORAGE SECTION**: Identifies thread-local and procedure-local data

#### Complete Control Flow Analysis
Beyond simple `PERFORM` statements, the tool handles:
- **CALL statements**: External program invocations and parameter passing
- **GO TO statements**: Legacy control flow patterns
- **EXIT and STOP statements**: Program termination points
- **Conditional Logic**: IF-THEN-ELSE structures and EVALUATE statements for business rule extraction
- **Loop Constructs**: PERFORM UNTIL, PERFORM VARYING, and other iterative patterns

#### Enhanced COPY Statement Resolution
- **Advanced COPY Processing**: Handles REPLACING clauses and text substitution
- **Nested COPY Statements**: Resolves multi-level includes and dependencies
- **Conditional Compilation**: Processes compiler directives and conditional includes
- **Dependency Mapping**: Creates complete dependency trees of included copybooks

## Architecture

### Tool Design
- **Command Line Interface**: Cross-platform CLI tool with comprehensive options
- **Implementation**: Python-based for robust COBOL parsing library ecosystem
- **Modular Architecture**: Separate parsing, analysis, and documentation generation components
- **Progress Reporting**: Real-time console output showing analysis progress

### LLM Integration Strategy

#### Multi-Model Support
- **Configurable LLM Providers**: Support for OpenAI, Anthropic, Google, and local models
- **Model Selection**: Different models optimized for different analysis tasks
- **Fallback Mechanisms**: Automatic fallback to alternative models on failure

#### Cost Optimization
- **Intelligent Caching**: Reuses existing summaries to avoid redundant LLM calls
- **Incremental Analysis**: Only processes changed components in subsequent runs
- **Batch Processing**: Groups similar analysis tasks for efficient API usage

### Analysis Strategies

#### 1. AST-Based Parsing (Primary)
- Generates complete Abstract Syntax Tree representation
- Provides semantic understanding of COBOL constructs
- Enables precise control flow and data flow analysis
- **Graceful Degradation**: Falls back to line-by-line parsing on AST failures

#### 2. Line-by-Line Parsing (Fallback)
- Direct source code processing for non-standard or complex COBOL
- Handles legacy code that may not conform to modern COBOL standards
- Provides basic structure analysis when AST parsing fails

### Error Handling and Validation

#### Robust Processing
- **Syntax Validation**: Pre-validates COBOL syntax before analysis
- **Partial Analysis**: Continues processing even when some components fail
- **Error Reporting**: Detailed logs of parsing issues and resolution attempts
- **Progress Tracking**: Real-time console updates on analysis status

#### Quality Assurance
- **Cross-Validation**: Compares AST and line-by-line results for consistency
- **Business Logic Validation**: Identifies potential business rule conflicts
- **Comment Validation**: Ensures business context preservation

## Business Context Enhancement

### Comment Processing
- **Business Context Preservation**: Extracts and analyzes existing comments containing business rules and explanations
- **Code Comment Removal**: Strips commented-out code lines before LLM processing to reduce noise
- **Documentation Integration**: Incorporates meaningful comments into generated summaries

### Business Rule Extraction
- **Pattern Recognition**: Identifies common business logic patterns in COBOL code
- **Rule Categorization**: Separates business rules from technical implementation details
- **Context Enrichment**: Uses preserved comments to enhance business understanding

## Output Format

The tool generates comprehensive JSON output containing:

### Program Structure
```json
{
  "program_name": "CUSTOMER-PROCESSING",
  "analysis_metadata": {
    "timestamp": "2025-06-30T10:30:00Z",
    "llm_model": "gpt-4",
    "analysis_strategy": "ast_primary_with_fallback",
    "total_components": 45,
    "processed_components": 43,
    "failed_components": 2
  },
  "data_divisions": {
    "working_storage": [...],
    "file_section": [...],
    "linkage_section": [...]
  },
  "procedure_components": {
    "sections": [...],
    "paragraphs": [...]
  }
}
```

### Component Details
Each program component includes:
- **Identification**: Name, file location, line numbers
- **Source Code**: Original COBOL code (with comments preserved separately)
- **Business Context**: Extracted comments and business rule explanations
- **Analysis Results**: LLM-generated summaries from both parsing strategies
- **Relationships**: Called components, data dependencies, control flow
- **Business Rules**: Identified business logic patterns and rules

### Dependencies and Relationships
- **Call Graph**: Visual representation of PERFORM and CALL relationships
- **Data Flow**: How data moves through the program
- **COPY Dependencies**: Complete include dependency tree
- **Business Rule Mapping**: Relationships between business rules and implementation

## Usage Examples

### Basic Analysis
```bash
cobol-analyzer analyze --input program.cbl --output analysis.json
```

### Advanced Configuration
```bash
cobol-analyzer analyze \
  --input program.cbl \
  --output analysis.json \
  --llm-provider openai \
  --model gpt-4 \
  --include-copybooks ./copybooks \
  --cache-summaries \
  --progress-console
```

### Incremental Analysis
```bash
cobol-analyzer analyze \
  --input program.cbl \
  --output analysis.json \
  --incremental \
  --previous-analysis previous_analysis.json
```

## Configuration

The tool supports extensive configuration through:
- **Command-line arguments**: For immediate usage
- **Configuration files**: For project-specific settings
- **Environment variables**: For sensitive information like API keys
- **Interactive setup**: Guided configuration for first-time users

