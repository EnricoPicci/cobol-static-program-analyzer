/**
 * Phase 5: Dashboard and Visualization Generator
 * Creates interactive dashboards and charts for COBOL analysis results
 */

import { ComprehensiveReport } from '../reporting/report-generator';
import { writeFileSync } from 'fs';

export interface ChartConfig {
  type: 'bar' | 'line' | 'pie' | 'radar' | 'scatter' | 'gauge';
  title: string;
  data: any;
  options?: any;
}

export interface DashboardConfig {
  title: string;
  description: string;
  theme: 'light' | 'dark' | 'auto';
  layout: 'grid' | 'tabs' | 'sections';
  refreshInterval?: number;
  exportOptions: string[];
}

/**
 * Dashboard Generator for comprehensive reporting
 */
export class DashboardGenerator {
  
  /**
   * Generate interactive HTML dashboard
   */
  generateInteractiveDashboard(
    report: ComprehensiveReport, 
    config: DashboardConfig = this.getDefaultConfig()
  ): string {
    const charts = this.generateCharts(report);
    const metrics = this.generateMetricsCards(report);
    const timeline = this.generateTimeline(report);
    
    return `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${config.title} - ${report.metadata.programName}</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/date-fns@2.29.3/index.min.js"></script>
    <style>
        ${this.getDashboardStyles(config.theme)}
    </style>
</head>
<body class="theme-${config.theme}">
    <div class="dashboard-container">
        <header class="dashboard-header">
            <h1>${config.title}</h1>
            <div class="header-info">
                <span class="program-name">${report.metadata.programName}</span>
                <span class="analysis-date">${new Date(report.metadata.analysisDate).toLocaleString()}</span>
                <span class="overall-grade grade-${report.summary.grade}">${report.summary.grade}</span>
            </div>
        </header>

        <main class="dashboard-main ${config.layout}">
            <!-- Metrics Overview -->
            <section class="metrics-overview">
                <h2>📊 Key Metrics</h2>
                <div class="metrics-grid">
                    ${metrics}
                </div>
            </section>

            <!-- Quality Score Gauge -->
            <section class="quality-gauge">
                <h2>🎯 Overall Quality Score</h2>
                <div class="gauge-container">
                    <canvas id="qualityGauge" width="300" height="300"></canvas>
                    <div class="gauge-info">
                        <div class="score">${report.summary.overallScore}</div>
                        <div class="grade">${report.summary.grade}</div>
                    </div>
                </div>
            </section>

            <!-- Issues Breakdown -->
            <section class="issues-breakdown">
                <h2>🚨 Issues Breakdown</h2>
                <div class="chart-container">
                    <canvas id="issuesChart"></canvas>
                </div>
                <div class="issues-summary">
                    <div class="issue-type critical">
                        <span class="count">${report.summary.criticalIssues}</span>
                        <span class="label">Critical</span>
                    </div>
                    <div class="issue-type major">
                        <span class="count">${report.summary.majorIssues}</span>
                        <span class="label">Major</span>
                    </div>
                    <div class="issue-type minor">
                        <span class="count">${report.summary.minorIssues}</span>
                        <span class="label">Minor</span>
                    </div>
                </div>
            </section>

            <!-- Complexity Radar -->
            <section class="complexity-radar">
                <h2>🕸️ Complexity Analysis</h2>
                <div class="chart-container">
                    <canvas id="complexityRadar"></canvas>
                </div>
            </section>

            <!-- Code Quality Trends -->
            <section class="quality-trends">
                <h2>📈 Quality Trends</h2>
                <div class="chart-container">
                    <canvas id="trendsChart"></canvas>
                </div>
            </section>

            <!-- Security Overview -->
            <section class="security-overview">
                <h2>🔒 Security Analysis</h2>
                <div class="security-grid">
                    ${this.generateSecuritySection(report)}
                </div>
            </section>

            <!-- Performance Issues -->
            <section class="performance-section">
                <h2>⚡ Performance Analysis</h2>
                <div class="performance-grid">
                    ${this.generatePerformanceSection(report)}
                </div>
            </section>

            <!-- Recommendations -->
            <section class="recommendations-section">
                <h2>💡 Recommendations</h2>
                <div class="recommendations-list">
                    ${this.generateRecommendationsSection(report)}
                </div>
            </section>

            <!-- Technical Debt -->
            <section class="technical-debt">
                <h2>💳 Technical Debt</h2>
                <div class="debt-visualization">
                    <div class="debt-amount">
                        <span class="value">${report.qualityMetrics.technicalDebt.toFixed(1)}</span>
                        <span class="unit">hours</span>
                    </div>
                    <div class="debt-breakdown">
                        <canvas id="debtChart"></canvas>
                    </div>
                </div>
            </section>

            <!-- File Structure -->
            <section class="file-structure">
                <h2>🗂️ Program Structure</h2>
                <div class="structure-visualization">
                    ${this.generateStructureVisualization(report)}
                </div>
            </section>
        </main>

        <footer class="dashboard-footer">
            <div class="export-options">
                <button onclick="exportToPDF()">📄 Export PDF</button>
                <button onclick="exportToExcel()">📊 Export Excel</button>
                <button onclick="exportToJSON()">📋 Export JSON</button>
            </div>
            <div class="version-info">
                Generated by COBOL Static Program Analyzer v${report.metadata.analysisVersion}
            </div>
        </footer>
    </div>

    <script>
        ${this.generateChartScripts(report)}
        ${this.generateInteractivityScripts()}
    </script>
</body>
</html>`;
  }

  /**
   * Generate chart configurations
   */
  private generateCharts(report: ComprehensiveReport): ChartConfig[] {
    return [
      {
        type: 'gauge',
        title: 'Overall Quality Score',
        data: {
          value: report.summary.overallScore,
          max: 100,
          threshold: [60, 80, 90]
        }
      },
      {
        type: 'pie',
        title: 'Issues by Severity',
        data: {
          labels: ['Critical', 'Major', 'Minor'],
          datasets: [{
            data: [
              report.summary.criticalIssues,
              report.summary.majorIssues,
              report.summary.minorIssues
            ],
            backgroundColor: ['#dc3545', '#fd7e14', '#ffc107'],
            borderWidth: 2
          }]
        }
      },
      {
        type: 'radar',
        title: 'Complexity Metrics',
        data: {
          labels: ['Maintainability', 'Complexity', 'Security', 'Performance', 'Reliability'],
          datasets: [{
            label: 'Current',
            data: [
              report.qualityMetrics.maintainabilityIndex,
              report.qualityMetrics.complexityScore,
              this.calculateSecurityScore(report),
              this.calculatePerformanceScore(report),
              this.calculateReliabilityScore(report)
            ],
            fill: true,
            backgroundColor: 'rgba(54, 162, 235, 0.2)',
            borderColor: 'rgb(54, 162, 235)',
            pointBackgroundColor: 'rgb(54, 162, 235)'
          }]
        }
      }
    ];
  }

  /**
   * Generate metrics cards HTML
   */
  private generateMetricsCards(report: ComprehensiveReport): string {
    return `
        <div class="metric-card">
            <div class="metric-icon">📄</div>
            <div class="metric-value">${report.metadata.totalLinesOfCode}</div>
            <div class="metric-label">Lines of Code</div>
        </div>
        <div class="metric-card">
            <div class="metric-icon">🔧</div>
            <div class="metric-value">${report.metadata.totalStatements}</div>
            <div class="metric-label">Statements</div>
        </div>
        <div class="metric-card">
            <div class="metric-icon">📝</div>
            <div class="metric-value">${report.metadata.totalParagraphs}</div>
            <div class="metric-label">Paragraphs</div>
        </div>
        <div class="metric-card">
            <div class="metric-icon">🏗️</div>
            <div class="metric-value">${report.metadata.totalSections}</div>
            <div class="metric-label">Sections</div>
        </div>
        <div class="metric-card ${report.qualityMetrics.maintainabilityIndex < 70 ? 'warning' : ''}">
            <div class="metric-icon">⚙️</div>
            <div class="metric-value">${report.qualityMetrics.maintainabilityIndex}</div>
            <div class="metric-label">Maintainability</div>
        </div>
        <div class="metric-card ${report.qualityMetrics.technicalDebt > 10 ? 'danger' : ''}">
            <div class="metric-icon">💳</div>
            <div class="metric-value">${report.qualityMetrics.technicalDebt.toFixed(1)}h</div>
            <div class="metric-label">Technical Debt</div>
        </div>
    `;
  }

  /**
   * Generate security section
   */
  private generateSecuritySection(report: ComprehensiveReport): string {
    const securityIssues = report.qualityMetrics.securityIssues;
    
    if (securityIssues.length === 0) {
      return '<div class="security-status success">✅ No security issues found</div>';
    }

    return securityIssues.map(issue => `
        <div class="security-issue ${issue.severity.toLowerCase()}">
            <div class="issue-header">
                <span class="issue-type">${issue.type}</span>
                <span class="issue-severity">${issue.severity}</span>
            </div>
            <div class="issue-message">${issue.message}</div>
            <div class="issue-remediation">💡 ${issue.remediation}</div>
            ${issue.cweId ? `<div class="cwe-id">CWE: ${issue.cweId}</div>` : ''}
        </div>
    `).join('');
  }

  /**
   * Generate performance section
   */
  private generatePerformanceSection(report: ComprehensiveReport): string {
    const performanceIssues = report.qualityMetrics.performanceIssues;
    
    if (performanceIssues.length === 0) {
      return '<div class="performance-status success">⚡ No performance issues found</div>';
    }

    return performanceIssues.map(issue => `
        <div class="performance-issue ${issue.severity.toLowerCase()}">
            <div class="issue-header">
                <span class="issue-type">${issue.type}</span>
                <span class="issue-severity">${issue.severity}</span>
            </div>
            <div class="issue-message">${issue.message}</div>
            <div class="issue-impact">📊 Impact: ${issue.impact}</div>
            <div class="issue-optimization">🚀 ${issue.optimization}</div>
        </div>
    `).join('');
  }

  /**
   * Generate recommendations section
   */
  private generateRecommendationsSection(report: ComprehensiveReport): string {
    return report.recommendations.map(rec => `
        <div class="recommendation ${rec.priority.toLowerCase()}">
            <div class="rec-header">
                <h3>${rec.title}</h3>
                <span class="rec-priority">${rec.priority}</span>
                <span class="rec-category">${rec.category}</span>
            </div>
            <div class="rec-description">${rec.description}</div>
            <div class="rec-impact">📈 Impact: ${rec.impact}</div>
            <div class="rec-effort">⏱️ Effort: ${rec.effort}</div>
            ${rec.examples.length > 0 ? `
                <div class="rec-examples">
                    <strong>Examples:</strong>
                    <ul>
                        ${rec.examples.map(ex => `<li>${ex}</li>`).join('')}
                    </ul>
                </div>
            ` : ''}
        </div>
    `).join('');
  }

  /**
   * Generate structure visualization
   */
  private generateStructureVisualization(report: ComprehensiveReport): string {
    return `
        <div class="structure-tree">
            <div class="structure-node program">
                <span class="node-icon">📋</span>
                <span class="node-name">${report.metadata.programName}</span>
                <div class="structure-children">
                    <div class="structure-node division">
                        <span class="node-icon">🏗️</span>
                        <span class="node-name">Procedure Division</span>
                        <div class="node-stats">
                            ${report.metadata.totalSections} sections, 
                            ${report.metadata.totalParagraphs} paragraphs
                        </div>
                    </div>
                    <div class="structure-node division">
                        <span class="node-icon">📊</span>
                        <span class="node-name">Data Division</span>
                        <div class="node-stats">Variables and file descriptions</div>
                    </div>
                </div>
            </div>
        </div>
    `;
  }

  /**
   * Generate timeline visualization
   */
  private generateTimeline(report: ComprehensiveReport): string {
    return `
        <div class="analysis-timeline">
            <div class="timeline-item">
                <div class="timeline-marker">📥</div>
                <div class="timeline-content">
                    <h4>Analysis Started</h4>
                    <span>${new Date(report.metadata.analysisDate).toLocaleString()}</span>
                </div>
            </div>
            <div class="timeline-item">
                <div class="timeline-marker">🔍</div>
                <div class="timeline-content">
                    <h4>Parsing Completed</h4>
                    <span>${report.metadata.totalLinesOfCode} lines processed</span>
                </div>
            </div>
            <div class="timeline-item">
                <div class="timeline-marker">📊</div>
                <div class="timeline-content">
                    <h4>Analysis Completed</h4>
                    <span>${report.summary.totalIssues} issues found</span>
                </div>
            </div>
        </div>
    `;
  }

  /**
   * Generate dashboard styles
   */
  private getDashboardStyles(theme: string): string {
    return `
        :root {
            --primary-color: #2c3e50;
            --secondary-color: #3498db;
            --success-color: #27ae60;
            --warning-color: #f39c12;
            --danger-color: #e74c3c;
            --info-color: #17a2b8;
            --background-color: ${theme === 'dark' ? '#1a1a1a' : '#f8f9fa'};
            --card-background: ${theme === 'dark' ? '#2d3748' : '#ffffff'};
            --text-color: ${theme === 'dark' ? '#e2e8f0' : '#2d3748'};
            --border-color: ${theme === 'dark' ? '#4a5568' : '#e2e8f0'};
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background-color: var(--background-color);
            color: var(--text-color);
            line-height: 1.6;
        }

        .dashboard-container {
            min-height: 100vh;
            display: flex;
            flex-direction: column;
        }

        .dashboard-header {
            background: linear-gradient(135deg, var(--primary-color), var(--secondary-color));
            color: white;
            padding: 2rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
            flex-wrap: wrap;
        }

        .header-info {
            display: flex;
            gap: 2rem;
            align-items: center;
            flex-wrap: wrap;
        }

        .overall-grade {
            font-size: 2rem;
            font-weight: bold;
            padding: 0.5rem 1rem;
            border-radius: 50%;
            background: rgba(255, 255, 255, 0.2);
        }

        .grade-A { color: #27ae60; }
        .grade-B { color: #f39c12; }
        .grade-C { color: #e67e22; }
        .grade-D { color: #e74c3c; }
        .grade-F { color: #c0392b; }

        .dashboard-main {
            flex: 1;
            padding: 2rem;
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
            gap: 2rem;
        }

        .dashboard-main.grid {
            grid-template-areas:
                "metrics metrics"
                "gauge issues"
                "complexity trends"
                "security performance"
                "recommendations debt"
                "structure structure";
        }

        section {
            background: var(--card-background);
            border: 1px solid var(--border-color);
            border-radius: 12px;
            padding: 1.5rem;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }

        .metrics-overview {
            grid-area: metrics;
        }

        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
            gap: 1rem;
            margin-top: 1rem;
        }

        .metric-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 1.5rem;
            border-radius: 8px;
            text-align: center;
            transition: transform 0.2s;
        }

        .metric-card:hover {
            transform: translateY(-2px);
        }

        .metric-card.warning {
            background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        }

        .metric-card.danger {
            background: linear-gradient(135deg, #ff9a9e 0%, #fecfef 100%);
        }

        .metric-icon {
            font-size: 2rem;
            margin-bottom: 0.5rem;
        }

        .metric-value {
            font-size: 2rem;
            font-weight: bold;
            margin-bottom: 0.25rem;
        }

        .metric-label {
            font-size: 0.9rem;
            opacity: 0.9;
        }

        .chart-container {
            height: 300px;
            margin-top: 1rem;
            position: relative;
        }

        .gauge-container {
            position: relative;
            text-align: center;
        }

        .gauge-info {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            text-align: center;
        }

        .gauge-info .score {
            font-size: 2rem;
            font-weight: bold;
            color: var(--secondary-color);
        }

        .gauge-info .grade {
            font-size: 1.2rem;
            font-weight: bold;
        }

        .issues-summary {
            display: flex;
            justify-content: space-around;
            margin-top: 1rem;
        }

        .issue-type {
            text-align: center;
            padding: 1rem;
        }

        .issue-type .count {
            display: block;
            font-size: 2rem;
            font-weight: bold;
        }

        .issue-type.critical .count { color: var(--danger-color); }
        .issue-type.major .count { color: var(--warning-color); }
        .issue-type.minor .count { color: var(--info-color); }

        .security-issue, .performance-issue, .recommendation {
            background: var(--card-background);
            border: 1px solid var(--border-color);
            border-radius: 8px;
            padding: 1rem;
            margin-bottom: 1rem;
        }

        .issue-header, .rec-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 0.5rem;
        }

        .issue-severity, .rec-priority {
            padding: 0.25rem 0.5rem;
            border-radius: 4px;
            font-size: 0.8rem;
            font-weight: bold;
            text-transform: uppercase;
        }

        .critical, .high { background: var(--danger-color); color: white; }
        .major, .medium { background: var(--warning-color); color: white; }
        .minor, .low { background: var(--info-color); color: white; }

        .dashboard-footer {
            background: var(--card-background);
            border-top: 1px solid var(--border-color);
            padding: 1rem 2rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        .export-options button {
            background: var(--secondary-color);
            color: white;
            border: none;
            padding: 0.5rem 1rem;
            border-radius: 4px;
            margin-right: 0.5rem;
            cursor: pointer;
            transition: background 0.2s;
        }

        .export-options button:hover {
            background: var(--primary-color);
        }

        @media (max-width: 768px) {
            .dashboard-main {
                grid-template-columns: 1fr;
                grid-template-areas: none;
            }
            
            .metrics-grid {
                grid-template-columns: repeat(2, 1fr);
            }
        }
    `;
  }

  /**
   * Generate chart scripts
   */
  private generateChartScripts(report: ComprehensiveReport): string {
    return `
        // Quality Gauge Chart
        const gaugeCtx = document.getElementById('qualityGauge');
        if (gaugeCtx) {
            new Chart(gaugeCtx, {
                type: 'doughnut',
                data: {
                    datasets: [{
                        data: [${report.summary.overallScore}, ${100 - report.summary.overallScore}],
                        backgroundColor: [
                            '${this.getScoreColor(report.summary.overallScore)}',
                            '#e9ecef'
                        ],
                        borderWidth: 0,
                        cutout: '70%'
                    }]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: { display: false },
                        tooltip: { enabled: false }
                    }
                }
            });
        }

        // Issues Breakdown Chart
        const issuesCtx = document.getElementById('issuesChart');
        if (issuesCtx) {
            new Chart(issuesCtx, {
                type: 'pie',
                data: {
                    labels: ['Critical', 'Major', 'Minor'],
                    datasets: [{
                        data: [${report.summary.criticalIssues}, ${report.summary.majorIssues}, ${report.summary.minorIssues}],
                        backgroundColor: ['#dc3545', '#fd7e14', '#ffc107'],
                        borderWidth: 2,
                        borderColor: '#fff'
                    }]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            position: 'bottom'
                        }
                    }
                }
            });
        }

        // Complexity Radar Chart
        const radarCtx = document.getElementById('complexityRadar');
        if (radarCtx) {
            new Chart(radarCtx, {
                type: 'radar',
                data: {
                    labels: ['Maintainability', 'Complexity', 'Security', 'Performance', 'Reliability'],
                    datasets: [{
                        label: 'Current Score',
                        data: [
                            ${report.qualityMetrics.maintainabilityIndex},
                            ${report.qualityMetrics.complexityScore},
                            ${this.calculateSecurityScore(report)},
                            ${this.calculatePerformanceScore(report)},
                            ${this.calculateReliabilityScore(report)}
                        ],
                        fill: true,
                        backgroundColor: 'rgba(54, 162, 235, 0.2)',
                        borderColor: 'rgb(54, 162, 235)',
                        pointBackgroundColor: 'rgb(54, 162, 235)',
                        pointBorderColor: '#fff',
                        pointHoverBackgroundColor: '#fff',
                        pointHoverBorderColor: 'rgb(54, 162, 235)'
                    }]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    elements: {
                        line: { borderWidth: 3 }
                    },
                    scales: {
                        r: {
                            angleLines: { display: false },
                            suggestedMin: 0,
                            suggestedMax: 100
                        }
                    }
                }
            });
        }

        // Technical Debt Chart
        const debtCtx = document.getElementById('debtChart');
        if (debtCtx) {
            const codeSmellsDebt = ${report.qualityMetrics.codeSmells.reduce((sum, smell) => sum + smell.effortMinutes, 0) / 60};
            const securityDebt = ${report.qualityMetrics.securityIssues.length};
            const performanceDebt = ${report.qualityMetrics.performanceIssues.length * 0.75};
            
            new Chart(debtCtx, {
                type: 'bar',
                data: {
                    labels: ['Code Smells', 'Security Issues', 'Performance Issues'],
                    datasets: [{
                        label: 'Hours',
                        data: [codeSmellsDebt, securityDebt, performanceDebt],
                        backgroundColor: ['#6f42c1', '#dc3545', '#fd7e14'],
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
                        y: {
                            beginAtZero: true,
                            title: {
                                display: true,
                                text: 'Hours'
                            }
                        }
                    }
                }
            });
        }
    `;
  }

  /**
   * Generate interactivity scripts
   */
  private generateInteractivityScripts(): string {
    return `
        function exportToPDF() {
            alert('PDF export functionality would be implemented here');
        }

        function exportToExcel() {
            alert('Excel export functionality would be implemented here');
        }

        function exportToJSON() {
            const data = {
                metadata: ${JSON.stringify('report.metadata')},
                summary: ${JSON.stringify('report.summary')},
                exportDate: new Date().toISOString()
            };
            
            const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'cobol-analysis-report.json';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
        }

        // Auto-refresh functionality
        setInterval(() => {
            const timestamp = document.querySelector('.analysis-date');
            if (timestamp) {
                timestamp.title = 'Last updated: ' + new Date().toLocaleString();
            }
        }, 60000);

        // Responsive chart resizing
        window.addEventListener('resize', () => {
            Chart.helpers.each(Chart.instances, (instance) => {
                instance.resize();
            });
        });
    `;
  }

  /**
   * Helper methods for calculations
   */
  private calculateSecurityScore(report: ComprehensiveReport): number {
    const securityIssues = report.qualityMetrics.securityIssues.length;
    return Math.max(0, 100 - (securityIssues * 20));
  }

  private calculatePerformanceScore(report: ComprehensiveReport): number {
    const performanceIssues = report.qualityMetrics.performanceIssues.length;
    return Math.max(0, 100 - (performanceIssues * 15));
  }

  private calculateReliabilityScore(report: ComprehensiveReport): number {
    const codeSmells = report.qualityMetrics.codeSmells.filter(s => 
      s.type === 'DEAD_CODE' || s.type === 'COMPLEX_CONDITIONAL'
    ).length;
    return Math.max(0, 100 - (codeSmells * 10));
  }

  private getScoreColor(score: number): string {
    if (score >= 90) return '#28a745';
    if (score >= 80) return '#ffc107';
    if (score >= 70) return '#fd7e14';
    if (score >= 60) return '#dc3545';
    return '#6c757d';
  }

  private getDefaultConfig(): DashboardConfig {
    return {
      title: 'COBOL Static Analysis Dashboard',
      description: 'Comprehensive analysis results and quality metrics',
      theme: 'light',
      layout: 'grid',
      exportOptions: ['pdf', 'excel', 'json']
    };
  }
}