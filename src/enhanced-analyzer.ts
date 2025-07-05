/**
 * Phase 5: Enhanced COBOL Analyzer
 * Main coordinator for enhanced analysis and reporting features
 */

import { CobolAnalyzer, AnalysisResult, CobolAnalyzerConfig, DEFAULT_ANALYZER_CONFIG } from './CobolAnalyzer';
import { ReportGenerator, ComprehensiveReport, CodeQualityMetrics } from './reporting/report-generator';
import { DashboardGenerator, DashboardConfig } from './visualization/dashboard-generator';
import { APIIntegrationClient, SonarQubeConfig, JenkinsConfig, GitLabConfig, WebhookPayload } from './integration/api-client';
import { CobolProgram } from './ast/nodes/CobolProgram';
import { DiagnosticMessage } from './core/types';
import { StaticAnalyzer, DEFAULT_STATIC_ANALYSIS_CONFIG } from './analysis/static-analyzer';

export interface EnhancedAnalyzerConfig extends CobolAnalyzerConfig {
  reporting: {
    generateReport: boolean;
    generateDashboard: boolean;
    includeCodeSmells: boolean;
    includeSecurityAnalysis: boolean;
    includePerformanceAnalysis: boolean;
    outputFormats: ('json' | 'html' | 'pdf' | 'csv')[];
  };
  
  integration: {
    enableWebhooks: boolean;
    webhookUrl?: string;
    webhookSecret?: string;
    sonarQube?: SonarQubeConfig;
    jenkins?: JenkinsConfig;
    gitLab?: GitLabConfig;
  };
  
  quality: {
    maintainabilityThreshold: number;
    complexityThreshold: number;
    technicalDebtThreshold: number; // in hours
    securityIssueThreshold: number;
    performanceIssueThreshold: number;
  };
}

export const DEFAULT_ENHANCED_CONFIG: EnhancedAnalyzerConfig = {
  ...DEFAULT_ANALYZER_CONFIG,
  reporting: {
    generateReport: true,
    generateDashboard: true,
    includeCodeSmells: true,
    includeSecurityAnalysis: true,
    includePerformanceAnalysis: true,
    outputFormats: ['json', 'html']
  },
  integration: {
    enableWebhooks: false
  },
  quality: {
    maintainabilityThreshold: 70,
    complexityThreshold: 80,
    technicalDebtThreshold: 8,
    securityIssueThreshold: 0,
    performanceIssueThreshold: 3
  }
};

export interface EnhancedAnalysisResult extends AnalysisResult {
  report?: ComprehensiveReport;
  dashboardPath?: string;
  qualityGateStatus: 'PASSED' | 'FAILED' | 'WARNING';
  qualityGateDetails: QualityGateDetail[];
  integrationResults?: {
    sonarQube?: any;
    jenkins?: any;
    gitLab?: any;
    webhook?: any;
  };
}

export interface QualityGateDetail {
  metric: string;
  threshold: number;
  actualValue: number;
  status: 'PASSED' | 'FAILED';
  message: string;
}

/**
 * Enhanced COBOL Analyzer with advanced reporting and integration capabilities
 */
export class EnhancedCobolAnalyzer {
  private baseAnalyzer: CobolAnalyzer;
  private reportGenerator: ReportGenerator;
  private dashboardGenerator: DashboardGenerator;
  private apiClient: APIIntegrationClient;
  private config: EnhancedAnalyzerConfig;

  constructor(config: EnhancedAnalyzerConfig = DEFAULT_ENHANCED_CONFIG) {
    this.config = config;
    this.baseAnalyzer = new CobolAnalyzer(config);
    this.reportGenerator = new ReportGenerator();
    this.dashboardGenerator = new DashboardGenerator();
    this.apiClient = new APIIntegrationClient();
  }

  /**
   * Perform enhanced analysis with comprehensive reporting
   */
  async analyzeWithEnhancements(
    source: string, 
    fileName?: string,
    outputDir?: string
  ): Promise<EnhancedAnalysisResult> {
    try {
      // Perform base analysis
      const baseResult = await this.baseAnalyzer.analyze(source, fileName);
      
      if (!baseResult.success || !baseResult.ast) {
        return {
          ...baseResult,
          qualityGateStatus: 'FAILED',
          qualityGateDetails: [{
            metric: 'Parsing',
            threshold: 0,
            actualValue: baseResult.allDiagnostics.filter(d => d.severity === 'error').length,
            status: 'FAILED',
            message: 'Failed to parse COBOL source code'
          }]
        };
      }

      // Run static analysis to get additional diagnostics
      const staticAnalyzer = new StaticAnalyzer(DEFAULT_STATIC_ANALYSIS_CONFIG);
      staticAnalyzer.analyze(baseResult.ast);
      const staticAnalysisWarnings = staticAnalyzer.getWarnings();
      
      // Merge static analysis warnings with base diagnostics
      const enhancedDiagnostics = [
        ...baseResult.allDiagnostics,
        ...staticAnalysisWarnings.map((warning: any) => ({
          severity: 'warning' as const,
          code: warning.code,
          message: warning.message,
          location: warning.location
        }))
      ];

      // Update the base result with enhanced diagnostics
      const enhancedBaseResult = {
        ...baseResult,
        allDiagnostics: enhancedDiagnostics
      };

      // Generate comprehensive report
      let report: ComprehensiveReport | undefined;
      let dashboardPath: string | undefined;

      if (this.config.reporting.generateReport) {
        report = await this.reportGenerator.generateReport(
          enhancedBaseResult,
          baseResult.ast,
          fileName || 'program.cbl'
        );

        // Export in requested formats
        if (outputDir) {
          for (const format of this.config.reporting.outputFormats) {
            const outputPath = `${outputDir}/report.${format}`;
            await this.reportGenerator.exportReport(report, format, outputPath);
          }
        }
      }

      // Generate dashboard
      if (this.config.reporting.generateDashboard && report && outputDir) {
        const dashboardHtml = this.dashboardGenerator.generateInteractiveDashboard(report);
        dashboardPath = `${outputDir}/dashboard.html`;
        
        const fs = await import('fs');
        fs.writeFileSync(dashboardPath, dashboardHtml);
      }

      // Check quality gates
      const qualityGateResult = this.checkQualityGates(report);

      // Handle integrations
      const integrationResults = await this.handleIntegrations(report, enhancedBaseResult, qualityGateResult);

      const enhancedResult: EnhancedAnalysisResult = {
        ...enhancedBaseResult,
        report,
        dashboardPath,
        qualityGateStatus: qualityGateResult.status,
        qualityGateDetails: qualityGateResult.details,
        integrationResults
      };

      return enhancedResult;

    } catch (error) {
      return {
        parseResult: {
          errors: [{
            severity: 'error',
            code: 'ENHANCED_ANALYSIS_FAILED',
            message: `Enhanced analysis failed: ${error instanceof Error ? error.message : String(error)}`,
            location: { line: 1, column: 1, endLine: 1, endColumn: 1 }
          }],
          warnings: [],
          success: false,
          sourceInfo: {
            originalLength: source.length,
            processedLength: 0,
            lineCount: source.split('\n').length,
            copybooksIncluded: []
          }
        },
        allDiagnostics: [{
          severity: 'error',
          code: 'ENHANCED_ANALYSIS_FAILED',
          message: `Enhanced analysis failed: ${error instanceof Error ? error.message : String(error)}`,
          location: { line: 1, column: 1, endLine: 1, endColumn: 1 }
        }],
        success: false,
        qualityGateStatus: 'FAILED',
        qualityGateDetails: [{
          metric: 'Analysis',
          threshold: 0,
          actualValue: 1,
          status: 'FAILED',
          message: 'Enhanced analysis encountered an error'
        }]
      };
    }
  }

  /**
   * Check quality gates against configured thresholds
   */
  private checkQualityGates(report?: ComprehensiveReport): { status: 'PASSED' | 'FAILED' | 'WARNING'; details: QualityGateDetail[] } {
    if (!report) {
      return {
        status: 'FAILED',
        details: [{
          metric: 'Report Generation',
          threshold: 0,
          actualValue: 1,
          status: 'FAILED',
          message: 'Failed to generate comprehensive report'
        }]
      };
    }

    const details: QualityGateDetail[] = [];
    let hasFailures = false;
    let hasWarnings = false;

    // Check maintainability threshold
    const maintainabilityDetail: QualityGateDetail = {
      metric: 'Maintainability Index',
      threshold: this.config.quality.maintainabilityThreshold,
      actualValue: report.qualityMetrics.maintainabilityIndex,
      status: report.qualityMetrics.maintainabilityIndex >= this.config.quality.maintainabilityThreshold ? 'PASSED' : 'FAILED',
      message: `Maintainability index is ${report.qualityMetrics.maintainabilityIndex} (threshold: ${this.config.quality.maintainabilityThreshold})`
    };
    details.push(maintainabilityDetail);
    if (maintainabilityDetail.status === 'FAILED') hasFailures = true;

    // Check complexity threshold
    const complexityDetail: QualityGateDetail = {
      metric: 'Complexity Score',
      threshold: this.config.quality.complexityThreshold,
      actualValue: report.qualityMetrics.complexityScore,
      status: report.qualityMetrics.complexityScore >= this.config.quality.complexityThreshold ? 'PASSED' : 'FAILED',
      message: `Complexity score is ${report.qualityMetrics.complexityScore} (threshold: ${this.config.quality.complexityThreshold})`
    };
    details.push(complexityDetail);
    if (complexityDetail.status === 'FAILED') hasFailures = true;

    // Check technical debt threshold
    const debtDetail: QualityGateDetail = {
      metric: 'Technical Debt',
      threshold: this.config.quality.technicalDebtThreshold,
      actualValue: report.qualityMetrics.technicalDebt,
      status: report.qualityMetrics.technicalDebt <= this.config.quality.technicalDebtThreshold ? 'PASSED' : 'FAILED',
      message: `Technical debt is ${report.qualityMetrics.technicalDebt.toFixed(1)} hours (threshold: ${this.config.quality.technicalDebtThreshold})`
    };
    details.push(debtDetail);
    if (debtDetail.status === 'FAILED') hasFailures = true;

    // Check security issues threshold
    const securityDetail: QualityGateDetail = {
      metric: 'Security Issues',
      threshold: this.config.quality.securityIssueThreshold,
      actualValue: report.qualityMetrics.securityIssues.length,
      status: report.qualityMetrics.securityIssues.length <= this.config.quality.securityIssueThreshold ? 'PASSED' : 'FAILED',
      message: `Found ${report.qualityMetrics.securityIssues.length} security issues (threshold: ${this.config.quality.securityIssueThreshold})`
    };
    details.push(securityDetail);
    if (securityDetail.status === 'FAILED') hasFailures = true;

    // Check performance issues threshold
    const performanceDetail: QualityGateDetail = {
      metric: 'Performance Issues',
      threshold: this.config.quality.performanceIssueThreshold,
      actualValue: report.qualityMetrics.performanceIssues.length,
      status: report.qualityMetrics.performanceIssues.length <= this.config.quality.performanceIssueThreshold ? 'PASSED' : 'FAILED',
      message: `Found ${report.qualityMetrics.performanceIssues.length} performance issues (threshold: ${this.config.quality.performanceIssueThreshold})`
    };
    details.push(performanceDetail);
    if (performanceDetail.status === 'FAILED') hasWarnings = true; // Performance issues are warnings, not failures

    // Determine overall status
    let status: 'PASSED' | 'FAILED' | 'WARNING' = 'PASSED';
    if (hasFailures) {
      status = 'FAILED';
    } else if (hasWarnings) {
      status = 'WARNING';
    }

    return { status, details };
  }

  /**
   * Handle external integrations
   */
  private async handleIntegrations(
    report?: ComprehensiveReport,
    baseResult?: AnalysisResult,
    qualityGateResult?: { status: string; details: QualityGateDetail[] }
  ): Promise<any> {
    if (!report || !baseResult) return {};

    const results: any = {};

    try {
      // SonarQube integration
      if (this.config.integration.sonarQube) {
        results.sonarQube = await this.apiClient.sendToSonarQube(
          report,
          this.config.integration.sonarQube
        );
      }

      // Jenkins integration
      if (this.config.integration.jenkins) {
        results.jenkins = await this.apiClient.triggerJenkinsBuild(
          report,
          this.config.integration.jenkins
        );
      }

      // GitLab integration (would need merge request ID)
      if (this.config.integration.gitLab) {
        // This would typically be called with a specific merge request ID
        // results.gitLab = await this.apiClient.createGitLabComment(report, this.config.integration.gitLab, mrId);
      }

      // Webhook notifications
      if (this.config.integration.enableWebhooks && this.config.integration.webhookUrl) {
        const webhookPayload: WebhookPayload = {
          event: qualityGateResult?.status === 'FAILED' ? 'quality_gate_failed' : 'analysis_complete',
          timestamp: new Date().toISOString(),
          report,
          metadata: {
            triggeredBy: 'enhanced-analyzer',
            buildNumber: process.env.BUILD_NUMBER,
            commitHash: process.env.COMMIT_HASH,
            branch: process.env.BRANCH_NAME
          }
        };

        results.webhook = await this.apiClient.sendWebhook(
          this.config.integration.webhookUrl,
          webhookPayload,
          this.config.integration.webhookSecret
        );
      }
    } catch (error) {
      console.error('Integration error:', error);
    }

    return results;
  }

  /**
   * Analyze multiple COBOL programs (batch processing)
   */
  async analyzeBatch(
    programs: Array<{ source: string; fileName: string }>,
    outputDir: string
  ): Promise<{
    results: EnhancedAnalysisResult[];
    summary: {
      totalPrograms: number;
      successfulAnalyses: number;
      failedAnalyses: number;
      totalIssues: number;
      averageScore: number;
      qualityGatesPassed: number;
    };
  }> {
    const results: EnhancedAnalysisResult[] = [];
    
    for (const program of programs) {
      const programOutputDir = `${outputDir}/${program.fileName.replace('.cbl', '')}`;
      const fs = await import('fs');
      
      // Create program-specific directory
      try {
        fs.mkdirSync(programOutputDir, { recursive: true });
      } catch (error) {
        // Directory already exists or creation failed
      }
      
      const result = await this.analyzeWithEnhancements(
        program.source,
        program.fileName,
        programOutputDir
      );
      
      results.push(result);
    }

    // Generate batch summary
    const summary = {
      totalPrograms: programs.length,
      successfulAnalyses: results.filter(r => r.success).length,
      failedAnalyses: results.filter(r => !r.success).length,
      totalIssues: results.reduce((sum, r) => sum + r.allDiagnostics.length, 0),
      averageScore: results
        .filter(r => r.report)
        .reduce((sum, r) => sum + r.report!.summary.overallScore, 0) / 
        results.filter(r => r.report).length || 0,
      qualityGatesPassed: results.filter(r => r.qualityGateStatus === 'PASSED').length
    };

    // Generate batch summary report
    await this.generateBatchSummary(results, summary, outputDir);

    return { results, summary };
  }

  /**
   * Generate batch summary report
   */
  private async generateBatchSummary(
    results: EnhancedAnalysisResult[],
    summary: any,
    outputDir: string
  ): Promise<void> {
    const summaryReport = {
      generatedAt: new Date().toISOString(),
      summary,
      programs: results.map(r => ({
        fileName: r.parseResult.sourceInfo.copybooksIncluded?.[0] || 'unknown',
        success: r.success,
        overallScore: r.report?.summary.overallScore || 0,
        grade: r.report?.summary.grade || 'F',
        qualityGateStatus: r.qualityGateStatus,
        issueCount: r.allDiagnostics.length,
        technicalDebt: r.report?.qualityMetrics.technicalDebt || 0
      }))
    };

    const fs = await import('fs');
    fs.writeFileSync(
      `${outputDir}/batch-summary.json`,
      JSON.stringify(summaryReport, null, 2)
    );

    // Generate batch dashboard
    const batchDashboard = this.generateBatchDashboard(summaryReport);
    fs.writeFileSync(`${outputDir}/batch-dashboard.html`, batchDashboard);
  }

  /**
   * Generate batch dashboard HTML
   */
  private generateBatchDashboard(summaryReport: any): string {
    return `
<!DOCTYPE html>
<html>
<head>
    <title>COBOL Batch Analysis Summary</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .summary-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin: 20px 0; }
        .summary-card { padding: 20px; background: #f8f9fa; border-radius: 8px; text-align: center; }
        .chart-container { height: 400px; margin: 20px 0; }
        .programs-table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        .programs-table th, .programs-table td { padding: 10px; border: 1px solid #ddd; text-align: left; }
        .grade-A { color: #28a745; font-weight: bold; }
        .grade-B { color: #ffc107; font-weight: bold; }
        .grade-C { color: #fd7e14; font-weight: bold; }
        .grade-D { color: #dc3545; font-weight: bold; }
        .grade-F { color: #6c757d; font-weight: bold; }
        .status-PASSED { color: #28a745; }
        .status-FAILED { color: #dc3545; }
        .status-WARNING { color: #ffc107; }
    </style>
</head>
<body>
    <h1>🚀 COBOL Batch Analysis Summary</h1>
    <p>Generated: ${summaryReport.generatedAt}</p>

    <div class="summary-grid">
        <div class="summary-card">
            <h3>Total Programs</h3>
            <div style="font-size: 2em; color: #007bff;">${summaryReport.summary.totalPrograms}</div>
        </div>
        <div class="summary-card">
            <h3>Success Rate</h3>
            <div style="font-size: 2em; color: #28a745;">${((summaryReport.summary.successfulAnalyses / summaryReport.summary.totalPrograms) * 100).toFixed(1)}%</div>
        </div>
        <div class="summary-card">
            <h3>Average Score</h3>
            <div style="font-size: 2em; color: #fd7e14;">${summaryReport.summary.averageScore.toFixed(1)}</div>
        </div>
        <div class="summary-card">
            <h3>Quality Gates Passed</h3>
            <div style="font-size: 2em; color: #6f42c1;">${summaryReport.summary.qualityGatesPassed}</div>
        </div>
    </div>

    <h2>📊 Analysis Overview</h2>
    <div class="chart-container">
        <canvas id="overviewChart"></canvas>
    </div>

    <h2>📋 Program Details</h2>
    <table class="programs-table">
        <thead>
            <tr>
                <th>Program</th>
                <th>Status</th>
                <th>Score</th>
                <th>Grade</th>
                <th>Quality Gate</th>
                <th>Issues</th>
                <th>Technical Debt</th>
            </tr>
        </thead>
        <tbody>
            ${summaryReport.programs.map((prog: any) => `
                <tr>
                    <td>${prog.fileName}</td>
                    <td>${prog.success ? '✅ Success' : '❌ Failed'}</td>
                    <td>${prog.overallScore}</td>
                    <td class="grade-${prog.grade}">${prog.grade}</td>
                    <td class="status-${prog.qualityGateStatus}">${prog.qualityGateStatus}</td>
                    <td>${prog.issueCount}</td>
                    <td>${prog.technicalDebt.toFixed(1)}h</td>
                </tr>
            `).join('')}
        </tbody>
    </table>

    <script>
        // Overview Chart
        const ctx = document.getElementById('overviewChart');
        new Chart(ctx, {
            type: 'bar',
            data: {
                labels: ['Successful', 'Failed', 'Quality Gates Passed', 'Quality Gates Failed'],
                datasets: [{
                    label: 'Count',
                    data: [
                        ${summaryReport.summary.successfulAnalyses},
                        ${summaryReport.summary.failedAnalyses},
                        ${summaryReport.summary.qualityGatesPassed},
                        ${summaryReport.summary.totalPrograms - summaryReport.summary.qualityGatesPassed}
                    ],
                    backgroundColor: ['#28a745', '#dc3545', '#6f42c1', '#fd7e14'],
                    borderRadius: 4
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: { display: false }
                },
                scales: {
                    y: { beginAtZero: true }
                }
            }
        });
    </script>
</body>
</html>`;
  }

  /**
   * Update configuration
   */
  updateConfig(config: Partial<EnhancedAnalyzerConfig>): void {
    this.config = { ...this.config, ...config };
    this.baseAnalyzer.updateConfig(config);
  }

  /**
   * Get current configuration
   */
  getConfig(): EnhancedAnalyzerConfig {
    return { ...this.config };
  }

  /**
   * Get version information
   */
  getVersion(): string {
    return '1.0.0-phase5-enhanced';
  }

  /**
   * Get enhanced features list
   */
  getEnhancedFeatures(): string[] {
    return [
      ...this.baseAnalyzer.getFeatures(),
      'Comprehensive Quality Reports',
      'Interactive Dashboards',
      'Code Smell Detection',
      'Security Analysis',
      'Performance Analysis',
      'Technical Debt Calculation',
      'Quality Gate Validation',
      'SonarQube Integration',
      'Jenkins Integration',
      'GitLab Integration',
      'Webhook Notifications',
      'Batch Processing',
      'Multiple Export Formats',
      'Trend Analysis',
      'Maintainability Metrics'
    ];
  }
}