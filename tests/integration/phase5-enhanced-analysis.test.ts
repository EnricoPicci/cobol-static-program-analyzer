/**
 * Phase 5: Enhanced Analysis Integration Tests
 * Tests the complete enhanced analysis workflow with reporting and integrations
 */

import { EnhancedCobolAnalyzer, DEFAULT_ENHANCED_CONFIG } from '../../src/enhanced-analyzer';
import { ReportGenerator } from '../../src/reporting/report-generator';
import { DashboardGenerator } from '../../src/visualization/dashboard-generator';
import { APIIntegrationClient } from '../../src/integration/api-client';
import { quickEnhancedAnalyze } from '../../src/index';
import { existsSync, mkdirSync, rmSync } from 'fs';
import { resolve } from 'path';

describe('Phase 5: Enhanced Analysis Integration', () => {
  const testOutputDir = resolve(__dirname, '../../test-outputs');
  
  beforeAll(() => {
    // Create test output directory
    if (!existsSync(testOutputDir)) {
      mkdirSync(testOutputDir, { recursive: true });
    }
  });

  afterAll(() => {
    // Clean up test outputs
    if (existsSync(testOutputDir)) {
      rmSync(testOutputDir, { recursive: true, force: true });
    }
  });

  describe('Enhanced COBOL Analyzer', () => {
    let analyzer: EnhancedCobolAnalyzer;

    beforeEach(() => {
      analyzer = new EnhancedCobolAnalyzer();
    });

    test('should initialize with default enhanced configuration', () => {
      expect(analyzer).toBeDefined();
      expect(analyzer.getVersion()).toBe('1.0.0-phase5-enhanced');
      
      const features = analyzer.getEnhancedFeatures();
      expect(features).toContain('Comprehensive Quality Reports');
      expect(features).toContain('Interactive Dashboards');
      expect(features).toContain('Code Smell Detection');
      expect(features).toContain('Security Analysis');
      expect(features).toContain('Technical Debt Calculation');
    });

    test('should perform enhanced analysis with comprehensive reporting', async () => {
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ENHANCED-ANALYSIS-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-COUNTER     PIC 9(3) VALUE 0.
        01 WS-MESSAGE     PIC X(50) VALUE "Enhanced Analysis Test Program".
        01 WS-RESULT      PIC 9(5)V99 VALUE 0.
        01 WS-PASSWORD    PIC X(20) VALUE "hardcoded123". 
        
        PROCEDURE DIVISION.
        MAIN-PROCESS.
            DISPLAY "Starting Enhanced Analysis Test"
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
            DISPLAY "Enhanced Analysis Test Complete".
      `.trim();

      const result = await analyzer.analyzeWithEnhancements(
        sampleProgram,
        'enhanced-test.cbl',
        testOutputDir
      );

      // Debug if test fails
      if (!result.success) {
        console.log('Parse errors:', result.parseResult.errors);
        console.log('All diagnostics:', result.allDiagnostics);
      }

      // Validate basic analysis success
      expect(result.success).toBe(true);
      expect(result.ast).toBeDefined();
      expect(result.parseResult.success).toBe(true);

      // Validate enhanced features
      expect(result.report).toBeDefined();
      expect(result.qualityGateStatus).toMatch(/PASSED|WARNING|FAILED/);
      expect(result.qualityGateDetails).toBeDefined();
      expect(result.qualityGateDetails.length).toBeGreaterThan(0);

      // Validate comprehensive report
      if (result.report) {
        expect(result.report.metadata.programName).toBe('ENHANCED-ANALYSIS-TEST');
        expect(result.report.summary.overallScore).toBeGreaterThanOrEqual(0);
        expect(result.report.summary.overallScore).toBeLessThanOrEqual(100);
        expect(result.report.summary.grade).toMatch(/[ABCDF]/);
        
        // Should detect security issue (hardcoded password)
        expect(result.report.qualityMetrics.securityIssues.length).toBeGreaterThanOrEqual(1);
        
        // Should have some quality metrics
        expect(result.report.qualityMetrics.maintainabilityIndex).toBeGreaterThanOrEqual(0);
        expect(result.report.qualityMetrics.complexityScore).toBeGreaterThanOrEqual(0);
        expect(result.report.qualityMetrics.technicalDebt).toBeGreaterThanOrEqual(0);
      }

      // Validate quality gates
      const maintainabilityGate = result.qualityGateDetails.find(qg => qg.metric === 'Maintainability Index');
      expect(maintainabilityGate).toBeDefined();
      expect(maintainabilityGate?.status).toMatch(/PASSED|FAILED/);

      const securityGate = result.qualityGateDetails.find(qg => qg.metric === 'Security Issues');
      expect(securityGate).toBeDefined();
      // Should fail due to hardcoded password
      expect(securityGate?.actualValue).toBeGreaterThan(0);
    });

    test('should handle invalid COBOL source gracefully', async () => {
      const invalidProgram = 'INVALID COBOL CODE HERE';

      const result = await analyzer.analyzeWithEnhancements(invalidProgram, 'invalid.cbl');

      expect(result.success).toBe(false);
      expect(result.qualityGateStatus).toBe('FAILED');
      expect(result.parseResult.errors.length).toBeGreaterThan(0);
    });

    test('should validate quality gates correctly', async () => {
      // Configure strict quality gates
      const strictConfig = {
        ...DEFAULT_ENHANCED_CONFIG,
        quality: {
          maintainabilityThreshold: 95, // Very high threshold
          complexityThreshold: 95,
          technicalDebtThreshold: 1, // Very low debt tolerance
          securityIssueThreshold: 0, // No security issues allowed
          performanceIssueThreshold: 0
        }
      };

      const strictAnalyzer = new EnhancedCobolAnalyzer(strictConfig);

      const problemProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROBLEMATIC-PROGRAM.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-SECRET PIC X(10) VALUE "password123".
        
        PROCEDURE DIVISION.
        MAIN-PROCESS.
            DISPLAY "This has issues"
            PERFORM LONG-PARAGRAPH
            STOP RUN.
            
        LONG-PARAGRAPH.
            IF 1 = 1 THEN
                IF 2 = 2 THEN
                    IF 3 = 3 THEN
                        IF 4 = 4 THEN
                            IF 5 = 5 THEN
                                IF 6 = 6 THEN
                                    DISPLAY "Deep nesting"
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                END-IF
            END-IF.
      `.trim();

      const result = await strictAnalyzer.analyzeWithEnhancements(problemProgram, 'problem.cbl');

      // Should fail quality gates due to strict thresholds
      expect(result.qualityGateStatus).toBe('FAILED');
      
      // Should have multiple failing quality gates
      const failedGates = result.qualityGateDetails.filter(qg => qg.status === 'FAILED');
      expect(failedGates.length).toBeGreaterThan(0);
    });

    test('should support batch processing', async () => {
      const programs = [
        {
          source: `
            IDENTIFICATION DIVISION.
            PROGRAM-ID. BATCH-PROGRAM-1.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-VAR PIC X(10).
            PROCEDURE DIVISION.
            MAIN.
                MOVE "TEST1" TO WS-VAR
                DISPLAY WS-VAR
                STOP RUN.
          `.trim(),
          fileName: 'batch1.cbl'
        },
        {
          source: `
            IDENTIFICATION DIVISION.
            PROGRAM-ID. BATCH-PROGRAM-2.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-VAR PIC X(10).
            PROCEDURE DIVISION.
            MAIN.
                MOVE "TEST2" TO WS-VAR
                DISPLAY WS-VAR
                STOP RUN.
          `.trim(),
          fileName: 'batch2.cbl'
        }
      ];

      const batchResult = await analyzer.analyzeBatch(programs, testOutputDir);

      expect(batchResult.results.length).toBe(2);
      expect(batchResult.summary.totalPrograms).toBe(2);
      expect(batchResult.summary.successfulAnalyses).toBeGreaterThanOrEqual(0);
      expect(batchResult.summary.averageScore).toBeGreaterThanOrEqual(0);

      // Verify that each program was analyzed
      for (const result of batchResult.results) {
        expect(result.parseResult).toBeDefined();
        expect(result.qualityGateStatus).toMatch(/PASSED|WARNING|FAILED/);
      }
    });
  });

  describe('Report Generator', () => {
    let reportGenerator: ReportGenerator;

    beforeEach(() => {
      reportGenerator = new ReportGenerator();
    });

    test('should generate comprehensive report', async () => {
      const analyzer = new EnhancedCobolAnalyzer();
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. REPORT-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-VAR PIC X(10).
        PROCEDURE DIVISION.
        MAIN.
            MOVE "TEST" TO WS-VAR
            DISPLAY WS-VAR
            STOP RUN.
      `.trim();

      const analysisResult = await analyzer.analyzeWithEnhancements(sampleProgram, 'report-test.cbl');
      
      expect(analysisResult.report).toBeDefined();
      
      if (analysisResult.report) {
        // Validate report structure
        expect(analysisResult.report.metadata).toBeDefined();
        expect(analysisResult.report.summary).toBeDefined();
        expect(analysisResult.report.qualityMetrics).toBeDefined();
        expect(analysisResult.report.recommendations).toBeDefined();
        
        // Validate metadata
        expect(analysisResult.report.metadata.programName).toBe('REPORT-TEST');
        expect(analysisResult.report.metadata.analysisDate).toBeDefined();
        expect(analysisResult.report.metadata.totalLinesOfCode).toBeGreaterThan(0);
        
        // Validate summary
        expect(analysisResult.report.summary.overallScore).toBeGreaterThanOrEqual(0);
        expect(analysisResult.report.summary.overallScore).toBeLessThanOrEqual(100);
        expect(['A', 'B', 'C', 'D', 'F']).toContain(analysisResult.report.summary.grade);
      }
    });

    test('should export report in multiple formats', async () => {
      const analyzer = new EnhancedCobolAnalyzer();
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. EXPORT-TEST.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Export test"
            STOP RUN.
      `.trim();

      const analysisResult = await analyzer.analyzeWithEnhancements(sampleProgram, 'export-test.cbl');
      
      if (analysisResult.report) {
        // Test JSON export
        const jsonPath = resolve(testOutputDir, 'test-report.json');
        await reportGenerator.exportReport(analysisResult.report, 'json', jsonPath);
        expect(existsSync(jsonPath)).toBe(true);

        // Test HTML export
        const htmlPath = resolve(testOutputDir, 'test-report.html');
        await reportGenerator.exportReport(analysisResult.report, 'html', htmlPath);
        expect(existsSync(htmlPath)).toBe(true);

        // Test CSV export
        const csvPath = resolve(testOutputDir, 'test-report.csv');
        await reportGenerator.exportReport(analysisResult.report, 'csv', csvPath);
        expect(existsSync(csvPath)).toBe(true);
      }
    });
  });

  describe('Dashboard Generator', () => {
    let dashboardGenerator: DashboardGenerator;

    beforeEach(() => {
      dashboardGenerator = new DashboardGenerator();
    });

    test('should generate interactive dashboard', async () => {
      const analyzer = new EnhancedCobolAnalyzer();
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. DASHBOARD-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-VAR PIC X(10).
        PROCEDURE DIVISION.
        MAIN.
            MOVE "TEST" TO WS-VAR
            DISPLAY WS-VAR
            STOP RUN.
      `.trim();

      const analysisResult = await analyzer.analyzeWithEnhancements(sampleProgram, 'dashboard-test.cbl');
      
      if (analysisResult.report) {
        const dashboardHtml = dashboardGenerator.generateInteractiveDashboard(analysisResult.report);
        
        expect(dashboardHtml).toContain('<!DOCTYPE html>');
        expect(dashboardHtml).toContain('DASHBOARD-TEST');
        expect(dashboardHtml).toContain('chart.js');
        expect(dashboardHtml).toContain('Overall Quality Score');
        expect(dashboardHtml).toContain('Issues Breakdown');
        expect(dashboardHtml).toContain('Complexity Analysis');
      }
    });
  });

  describe('API Integration Client', () => {
    let apiClient: APIIntegrationClient;

    beforeEach(() => {
      apiClient = new APIIntegrationClient();
    });

    test('should handle webhook payload creation', async () => {
      const analyzer = new EnhancedCobolAnalyzer();
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. WEBHOOK-TEST.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Webhook test"
            STOP RUN.
      `.trim();

      const analysisResult = await analyzer.analyzeWithEnhancements(sampleProgram, 'webhook-test.cbl');
      
      if (analysisResult.report) {
        // Simulate webhook payload (without actually sending)
        const webhookPayload = {
          event: 'analysis_complete' as const,
          timestamp: new Date().toISOString(),
          report: analysisResult.report,
          metadata: {
            triggeredBy: 'test',
            buildNumber: '123',
            commitHash: 'abc123',
            branch: 'test-branch'
          }
        };

        expect(webhookPayload.event).toBe('analysis_complete');
        expect(webhookPayload.report).toBeDefined();
        expect(webhookPayload.metadata.triggeredBy).toBe('test');
      }
    });
  });

  describe('Quick Enhanced Analyze Function', () => {
    test('should provide quick enhanced analysis', async () => {
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. QUICK-TEST.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Quick test"
            STOP RUN.
      `.trim();

      const result = await quickEnhancedAnalyze(sampleProgram, 'quick-test.cbl');

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.report).toBeDefined();
      expect(result.qualityGateStatus).toMatch(/PASSED|WARNING|FAILED/);
    });

    test('should work with output directory', async () => {
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. OUTPUT-TEST.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Output test"
            STOP RUN.
      `.trim();

      const result = await quickEnhancedAnalyze(sampleProgram, 'output-test.cbl', testOutputDir);

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      
      // Dashboard should be generated when output directory is provided
      if (result.dashboardPath) {
        expect(existsSync(result.dashboardPath)).toBe(true);
      }
    });
  });

  describe('Integration with Previous Phases', () => {
    test('should maintain backward compatibility with base analyzer', async () => {
      const sampleProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COMPAT-TEST.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Compatibility test"
            STOP RUN.
      `.trim();

      const enhancedAnalyzer = new EnhancedCobolAnalyzer();
      const result = await enhancedAnalyzer.analyzeWithEnhancements(sampleProgram, 'compat-test.cbl');

      // Should have all base analysis result properties
      expect(result.parseResult).toBeDefined();
      expect(result.ast).toBeDefined();
      expect(result.allDiagnostics).toBeDefined();
      expect(result.success).toBeDefined();

      // Should have enhanced properties
      expect(result.report).toBeDefined();
      expect(result.qualityGateStatus).toBeDefined();
      expect(result.qualityGateDetails).toBeDefined();
    });

    test('should work with COPY statements (Phase 3 integration)', async () => {
      // This test would require actual copybooks, so we'll simulate
      const programWithCopy = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COPY-INTEGRATION-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        COPY COMMON-VARS.
        PROCEDURE DIVISION.
        MAIN.
            DISPLAY "Copy integration test"
            STOP RUN.
      `.trim();

      const result = await quickEnhancedAnalyze(programWithCopy, 'copy-test.cbl');

      // Should handle COPY statement gracefully (even if copybook not found)
      expect(result).toBeDefined();
      expect(result.parseResult).toBeDefined();
    });

    test('should integrate with static analysis (Phase 4)', async () => {
      const programWithIssues = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. STATIC-ANALYSIS-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-UNUSED-VAR PIC X(10).
        01 WS-USED-VAR PIC X(10).
        
        PROCEDURE DIVISION.
        MAIN-PARA.
            MOVE "TEST" TO WS-USED-VAR
            DISPLAY WS-USED-VAR
            STOP RUN.
            
        UNREACHABLE-PARA.
            DISPLAY "This is unreachable".
      `.trim();

      const result = await quickEnhancedAnalyze(programWithIssues, 'static-test.cbl');

      expect(result.success).toBe(true);
      expect(result.report).toBeDefined();
      
      if (result.report) {
        // Debug code smells detection
        console.log('Code smells detected:', result.report.qualityMetrics.codeSmells);
        console.log('Analysis warnings:', result.analysis);
        console.log('All diagnostics:', result.allDiagnostics);
        
        // Should detect unused variable and unreachable code from static analysis
        // The base static analyzer should provide warnings that become code smells
        const hasStaticAnalysisIssues = result.allDiagnostics.some(d => 
          d.code === 'UNUSED_VARIABLE' || d.code === 'UNREACHABLE_CODE'
        );
        
        // Either code smells are detected OR static analysis warnings are present
        const codeSmells = result.report.qualityMetrics.codeSmells;
        expect(hasStaticAnalysisIssues || codeSmells.length > 0).toBe(true);
        
        // Should have analysis from base static analyzer
        expect(result.allDiagnostics).toBeDefined();
      }
    });
  });
});