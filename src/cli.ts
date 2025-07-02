#!/usr/bin/env node
/**
 * CLI Interface for COBOL Analyzer - Phase 2 Integration Testing
 */

import { CobolAnalyzer, DEFAULT_ANALYZER_CONFIG, CobolAnalyzerConfig } from './CobolAnalyzer';
import { readFileSync, writeFileSync } from 'fs';
import { resolve } from 'path';

interface CLIOptions {
  input?: string;
  output?: string;
  format?: 'json' | 'xml' | 'yaml';
  config?: string;
  validate?: boolean;
  profile?: boolean;
  verbose?: boolean;
  help?: boolean;
  version?: boolean;
  example?: boolean;
}

class CobolCLI {
  private analyzer: CobolAnalyzer;

  constructor() {
    this.analyzer = new CobolAnalyzer();
  }

  async run(args: string[]): Promise<void> {
    const options = this.parseArgs(args);

    if (options.help) {
      this.showHelp();
      return;
    }

    if (options.version) {
      console.log(`COBOL Static Program Analyzer ${this.analyzer.getVersion()}`);
      console.log(`Features: ${this.analyzer.getFeatures().join(', ')}`);
      return;
    }

    if (options.example) {
      await this.runExample();
      return;
    }

    if (!options.input) {
      console.error('Error: Input file is required');
      this.showHelp();
      process.exit(1);
    }

    try {
      // Load custom configuration if provided
      if (options.config) {
        const configData = JSON.parse(readFileSync(options.config, 'utf8'));
        this.analyzer.updateConfig(configData);
      }

      // Configure output format
      if (options.format || options.profile) {
        const currentConfig = this.analyzer.getConfig();
        this.analyzer.updateConfig({
          ...currentConfig,
          analysis: {
            ...currentConfig.analysis,
            enableProfiling: options.profile || currentConfig.analysis.enableProfiling
          },
          output: {
            ...currentConfig.output,
            astFormat: options.format || currentConfig.output.astFormat,
            prettyPrint: options.verbose || currentConfig.output.prettyPrint
          }
        });
      }

      // Read input file
      const sourceCode = readFileSync(resolve(options.input), 'utf8');
      const fileName = options.input;

      if (options.verbose) {
        console.log(`Analyzing: ${fileName}`);
        console.log(`Size: ${sourceCode.length} characters`);
        console.log(`Lines: ${sourceCode.split('\n').length}`);
      }

      // Perform analysis
      const startTime = performance.now();
      
      let result;
      if (options.validate) {
        const diagnostics = await this.analyzer.validate(sourceCode, fileName);
        result = {
          validation: true,
          success: diagnostics.filter(d => d.severity === 'error').length === 0,
          diagnostics
        };
      } else {
        const analysisResult = await this.analyzer.analyze(sourceCode, fileName);
        result = this.analyzer.formatResult(analysisResult);
      }

      const endTime = performance.now();

      if (options.verbose) {
        console.log(`Analysis completed in ${(endTime - startTime).toFixed(2)}ms`);
      }

      // Output results
      if (options.output) {
        if (typeof result === 'string') {
          writeFileSync(options.output, result);
        } else {
          writeFileSync(options.output, JSON.stringify(result, null, 2));
        }
        if (options.verbose) {
          console.log(`Results written to: ${options.output}`);
        }
      } else {
        if (typeof result === 'string') {
          console.log(result);
        } else {
          console.log(JSON.stringify(result, null, 2));
        }
      }

    } catch (error) {
      console.error(`Error: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }
  }

  private parseArgs(args: string[]): CLIOptions {
    const options: CLIOptions = {};
    
    for (let i = 0; i < args.length; i++) {
      const arg = args[i];
      
      switch (arg) {
        case '-i':
        case '--input':
          options.input = args[++i];
          break;
        case '-o':
        case '--output':
          options.output = args[++i];
          break;
        case '-f':
        case '--format':
          options.format = args[++i] as 'json' | 'xml' | 'yaml';
          break;
        case '-c':
        case '--config':
          options.config = args[++i];
          break;
        case '--validate':
          options.validate = true;
          break;
        case '-p':
        case '--profile':
          options.profile = true;
          break;
        case '-v':
        case '--verbose':
          options.verbose = true;
          break;
        case '-h':
        case '--help':
          options.help = true;
          break;
        case '--version':
          options.version = true;
          break;
        case '--example':
          options.example = true;
          break;
        default:
          if (!options.input && !arg.startsWith('-')) {
            options.input = arg;
          }
          break;
      }
    }
    
    return options;
  }

  private showHelp(): void {
    console.log(`
COBOL Static Program Analyzer - CLI Interface

Usage: node cli.js [options] [input-file]

Options:
  -i, --input <file>     Input COBOL source file (required)
  -o, --output <file>    Output file for results
  -f, --format <format>  Output format: json, xml, yaml (default: json)
  -c, --config <file>    Configuration file (JSON)
  --validate             Validation mode only (no full analysis)
  -p, --profile          Enable performance profiling
  -v, --verbose          Verbose output
  -h, --help             Show this help
  --version              Show version information
  --example              Run built-in example

Examples:
  node cli.js program.cbl
  node cli.js -i program.cbl -o results.json --profile
  node cli.js program.cbl --validate
  node cli.js --example
`);
  }

  private async runExample(): Promise<void> {
    console.log('Running Phase 2 Integration Example...\n');

    const exampleProgram = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. INTEGRATION-EXAMPLE.
      AUTHOR. PHASE-2-COORDINATOR.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-COUNTER     PIC 9(3) VALUE 0.
      01 WS-MESSAGE     PIC X(30) VALUE "Phase 2 Integration Test".
      01 WS-RESULT      PIC 9(5)V99 VALUE 0.
      
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          DISPLAY "Starting Integration Example"
          PERFORM INITIALIZATION
          PERFORM CALCULATION
          PERFORM DISPLAY-RESULTS
          STOP RUN.
      
      INITIALIZATION.
          MOVE 100 TO WS-COUNTER
          DISPLAY "Initialized counter to: " WS-COUNTER.
      
      CALCULATION.
          COMPUTE WS-RESULT = WS-COUNTER * 1.5
          DISPLAY "Calculated result: " WS-RESULT.
      
      DISPLAY-RESULTS.
          DISPLAY "Final message: " WS-MESSAGE
          DISPLAY "Final result: " WS-RESULT
          DISPLAY "Integration Example Complete".
    `.trim();

    try {
      console.log('Source Code:');
      console.log('=' .repeat(50));
      console.log(exampleProgram);
      console.log('=' .repeat(50));
      console.log();

      const result = await this.analyzer.analyze(exampleProgram, 'integration-example.cbl');
      
      console.log('Analysis Results:');
      console.log('-' .repeat(30));
      console.log(`Success: ${result.success}`);
      console.log(`Source Lines: ${result.parseResult.sourceInfo.lineCount}`);
      console.log(`Source Length: ${result.parseResult.sourceInfo.originalLength} characters`);
      
      if (result.performance) {
        console.log(`Parse Time: ${result.performance.parseTime.toFixed(2)}ms`);
        console.log(`Total Time: ${result.performance.totalTime.toFixed(2)}ms`);
        console.log(`Memory Usage: ${(result.performance.memoryUsage / 1024).toFixed(2)}KB`);
      }

      if (result.analysis?.complexity) {
        console.log('\nComplexity Metrics:');
        console.log(`Statements: ${result.analysis.complexity.totalStatements}`);
        console.log(`Paragraphs: ${result.analysis.complexity.totalParagraphs}`);
        console.log(`Sections: ${result.analysis.complexity.totalSections}`);
        console.log(`Nesting Depth: ${result.analysis.complexity.nestingDepth}`);
      }

      if (result.allDiagnostics.length > 0) {
        console.log('\nDiagnostics:');
        result.allDiagnostics.forEach(diagnostic => {
          console.log(`${diagnostic.severity.toUpperCase()}: ${diagnostic.message} (${diagnostic.location.line}:${diagnostic.location.column})`);
        });
      } else {
        console.log('\nNo errors or warnings detected!');
      }

      console.log('\n✅ Phase 2 Integration Example completed successfully!');
      
    } catch (error) {
      console.error(`Example failed: ${error instanceof Error ? error.message : String(error)}`);
    }
  }
}

// CLI Entry Point
if (require.main === module) {
  const cli = new CobolCLI();
  cli.run(process.argv.slice(2)).catch(error => {
    console.error(`Fatal error: ${error instanceof Error ? error.message : String(error)}`);
    process.exit(1);
  });
}

export { CobolCLI };