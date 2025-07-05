/**
 * Phase 5: Enhanced Analysis and Reporting
 * Comprehensive reporting system for COBOL static analysis
 */

import { CobolProgram } from '../ast/nodes/CobolProgram';
import { AnalysisResult } from '../CobolAnalyzer';
import { DiagnosticMessage, PerformanceMetrics, SourceLocation } from '../core/types';
import { writeFileSync } from 'fs';

export interface CodeQualityMetrics {
  maintainabilityIndex: number;
  technicalDebt: number;
  codeSmells: CodeSmell[];
  securityIssues: SecurityIssue[];
  performanceIssues: PerformanceIssue[];
  complexityScore: number;
  duplicationPercentage: number;
  testCoverage: number;
}

export interface CodeSmell {
  type: 'LONG_PARAGRAPH' | 'DEEP_NESTING' | 'DEAD_CODE' | 'DUPLICATE_CODE' | 'COMPLEX_CONDITIONAL' | 'MAGIC_NUMBER';
  severity: 'INFO' | 'MINOR' | 'MAJOR' | 'CRITICAL';
  message: string;
  location: SourceLocation;
  suggestion: string;
  effortMinutes: number;
}

export interface SecurityIssue {
  type: 'HARDCODED_CREDENTIAL' | 'SQL_INJECTION' | 'PATH_TRAVERSAL' | 'BUFFER_OVERFLOW' | 'UNVALIDATED_INPUT';
  severity: 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';
  message: string;
  location: SourceLocation;
  cweId?: string;
  remediation: string;
}

export interface PerformanceIssue {
  type: 'INEFFICIENT_LOOP' | 'EXCESSIVE_FILE_IO' | 'MEMORY_LEAK' | 'SLOW_QUERY' | 'UNNECESSARY_COMPUTATION';
  severity: 'LOW' | 'MEDIUM' | 'HIGH';
  message: string;
  location: SourceLocation;
  impact: string;
  optimization: string;
}

export interface ComprehensiveReport {
  metadata: {
    programName: string;
    fileName: string;
    analysisDate: string;
    analysisVersion: string;
    totalLinesOfCode: number;
    totalStatements: number;
    totalParagraphs: number;
    totalSections: number;
  };
  
  summary: {
    overallScore: number;
    grade: 'A' | 'B' | 'C' | 'D' | 'F';
    totalIssues: number;
    criticalIssues: number;
    majorIssues: number;
    minorIssues: number;
    infoIssues: number;
  };
  
  qualityMetrics: CodeQualityMetrics;
  analysisResult: AnalysisResult;
  recommendations: Recommendation[];
  trends: QualityTrend[];
}

export interface Recommendation {
  priority: 'HIGH' | 'MEDIUM' | 'LOW';
  category: 'MAINTAINABILITY' | 'PERFORMANCE' | 'SECURITY' | 'RELIABILITY';
  title: string;
  description: string;
  impact: string;
  effort: string;
  examples: string[];
}

export interface QualityTrend {
  metric: string;
  currentValue: number;
  previousValue?: number;
  trend: 'IMPROVING' | 'STABLE' | 'DEGRADING';
  changePercentage: number;
}

export class ReportGenerator {
  private version: string = '1.0.0-phase5';

  /**
   * Generate comprehensive report for COBOL program
   */
  async generateReport(
    analysisResult: AnalysisResult,
    ast: CobolProgram,
    fileName: string = 'program.cbl'
  ): Promise<ComprehensiveReport> {
    const qualityMetrics = this.calculateQualityMetrics(analysisResult, ast);
    const metadata = this.generateMetadata(analysisResult, ast, fileName);
    const summary = this.generateSummary(qualityMetrics, analysisResult);
    const recommendations = this.generateRecommendations(qualityMetrics, analysisResult);
    const trends = this.generateTrends(qualityMetrics);

    return {
      metadata,
      summary,
      qualityMetrics,
      analysisResult,
      recommendations,
      trends
    };
  }

  /**
   * Calculate comprehensive code quality metrics
   */
  private calculateQualityMetrics(result: AnalysisResult, ast: CobolProgram): CodeQualityMetrics {
    const codeSmells = this.detectCodeSmells(result, ast);
    const securityIssues = this.detectSecurityIssues(result, ast);
    const performanceIssues = this.detectPerformanceIssues(result, ast);
    
    const complexityScore = this.calculateComplexityScore(result);
    const maintainabilityIndex = this.calculateMaintainabilityIndex(result, codeSmells);
    const technicalDebt = this.calculateTechnicalDebt(codeSmells, securityIssues, performanceIssues);
    
    return {
      maintainabilityIndex,
      technicalDebt,
      codeSmells,
      securityIssues,
      performanceIssues,
      complexityScore,
      duplicationPercentage: this.calculateDuplicationPercentage(ast),
      testCoverage: 0 // Would need actual test coverage data
    };
  }

  /**
   * Detect code smells in the program
   */
  private detectCodeSmells(result: AnalysisResult, ast: CobolProgram): CodeSmell[] {
    const smells: CodeSmell[] = [];

    // Long paragraph detection
    if (ast.procedureDivision) {
      ast.procedureDivision.paragraphs.forEach(paragraph => {
        if (paragraph.statements.length > 50) {
          smells.push({
            type: 'LONG_PARAGRAPH',
            severity: 'MAJOR',
            message: `Paragraph '${paragraph.name}' has ${paragraph.statements.length} statements (>50)`,
            location: paragraph.location,
            suggestion: 'Consider breaking down into smaller paragraphs',
            effortMinutes: 30
          });
        }
      });
    }

    // Deep nesting detection
    if (result.analysis?.complexity && result.analysis.complexity.nestingDepth > 5) {
      smells.push({
        type: 'DEEP_NESTING',
        severity: 'MAJOR',
        message: `Deep nesting detected (depth: ${result.analysis.complexity.nestingDepth})`,
        location: { line: 1, column: 1, endLine: 1, endColumn: 1 },
        suggestion: 'Refactor nested conditions to improve readability',
        effortMinutes: 60
      });
    }

    // Dead code detection from static analysis
    result.allDiagnostics.forEach(diagnostic => {
      if (diagnostic.code === 'UNREACHABLE_CODE') {
        smells.push({
          type: 'DEAD_CODE',
          severity: 'MINOR',
          message: diagnostic.message,
          location: diagnostic.location,
          suggestion: 'Remove unreachable code to improve maintainability',
          effortMinutes: 15
        });
      }
    });

    return smells;
  }

  /**
   * Detect security issues in the program
   */
  private detectSecurityIssues(result: AnalysisResult, ast: CobolProgram): SecurityIssue[] {
    const issues: SecurityIssue[] = [];

    // Hardcoded credentials detection (simplified)
    if (ast.dataDivision && ast.dataDivision.workingStorage) {
      ast.dataDivision.workingStorage.forEach((variable: any) => {
        if (variable.name && (variable.name.includes('PASSWORD') || variable.name.includes('SECRET'))) {
          issues.push({
            type: 'HARDCODED_CREDENTIAL',
            severity: 'HIGH',
            message: `Potential hardcoded credential in variable '${variable.name}'`,
            location: variable.location || { line: 1, column: 1, endLine: 1, endColumn: 1 },
            cweId: 'CWE-798',
            remediation: 'Use secure credential management or external configuration'
          });
        }
      });
    }

    // SQL injection potential (simplified)
    if (ast.procedureDivision) {
      ast.procedureDivision.paragraphs.forEach(paragraph => {
        paragraph.statements.forEach(statement => {
          if (statement.sourceText && statement.sourceText.includes('EXEC SQL')) {
            issues.push({
              type: 'SQL_INJECTION',
              severity: 'MEDIUM',
              message: 'SQL statement found - review for injection vulnerabilities',
              location: statement.location,
              cweId: 'CWE-89',
              remediation: 'Use parameterized queries and input validation'
            });
          }
        });
      });
    }

    return issues;
  }

  /**
   * Detect performance issues in the program
   */
  private detectPerformanceIssues(result: AnalysisResult, ast: CobolProgram): PerformanceIssue[] {
    const issues: PerformanceIssue[] = [];

    // Inefficient loop detection
    if (ast.procedureDivision) {
      ast.procedureDivision.paragraphs.forEach(paragraph => {
        const performCount = paragraph.statements.filter(s => 
          s.sourceText && s.sourceText.includes('PERFORM')
        ).length;
        
        if (performCount > 10) {
          issues.push({
            type: 'INEFFICIENT_LOOP',
            severity: 'MEDIUM',
            message: `Paragraph '${paragraph.name}' has ${performCount} PERFORM statements`,
            location: paragraph.location,
            impact: 'May cause performance degradation with large datasets',
            optimization: 'Consider consolidating loops or using bulk operations'
          });
        }
      });
    }

    return issues;
  }

  /**
   * Calculate complexity score (0-100)
   */
  private calculateComplexityScore(result: AnalysisResult): number {
    if (!result.analysis?.complexity) return 50;
    
    const complexity = result.analysis.complexity;
    let score = 100;
    
    // Deduct points for high complexity
    if (complexity.cyclomaticComplexity > 10) score -= 20;
    if (complexity.nestingDepth > 5) score -= 15;
    if (complexity.totalStatements > 500) score -= 10;
    if (complexity.totalParagraphs > 50) score -= 10;
    
    return Math.max(0, Math.min(100, score));
  }

  /**
   * Calculate maintainability index (0-100)
   */
  private calculateMaintainabilityIndex(result: AnalysisResult, codeSmells: CodeSmell[]): number {
    let index = 85; // Base score
    
    // Deduct for code smells
    const criticalSmells = codeSmells.filter(s => s.severity === 'CRITICAL').length;
    const majorSmells = codeSmells.filter(s => s.severity === 'MAJOR').length;
    const minorSmells = codeSmells.filter(s => s.severity === 'MINOR').length;
    
    index -= (criticalSmells * 10) + (majorSmells * 5) + (minorSmells * 2);
    
    // Deduct for complexity
    if (result.analysis?.complexity) {
      const complexity = result.analysis.complexity;
      if (complexity.cyclomaticComplexity > 15) index -= 10;
      if (complexity.nestingDepth > 6) index -= 5;
    }
    
    return Math.max(0, Math.min(100, index));
  }

  /**
   * Calculate technical debt in hours
   */
  private calculateTechnicalDebt(
    codeSmells: CodeSmell[],
    securityIssues: SecurityIssue[],
    performanceIssues: PerformanceIssue[]
  ): number {
    let totalMinutes = 0;
    
    // Add effort for code smells
    totalMinutes += codeSmells.reduce((sum, smell) => sum + smell.effortMinutes, 0);
    
    // Add effort for security issues
    totalMinutes += securityIssues.length * 60; // 1 hour per security issue
    
    // Add effort for performance issues
    totalMinutes += performanceIssues.length * 45; // 45 minutes per performance issue
    
    return totalMinutes / 60; // Convert to hours
  }

  /**
   * Calculate code duplication percentage
   */
  private calculateDuplicationPercentage(ast: CobolProgram): number {
    // Simplified duplication detection
    if (!ast.procedureDivision) return 0;
    
    const statements = ast.procedureDivision.paragraphs
      .flatMap(p => p.statements)
      .map(s => s.sourceText?.trim())
      .filter(s => s && s.length > 10);
    
    const duplicates = statements.filter((statement, index) => 
      statements.indexOf(statement) !== index
    );
    
    return statements.length > 0 ? (duplicates.length / statements.length) * 100 : 0;
  }

  /**
   * Generate report metadata
   */
  private generateMetadata(result: AnalysisResult, ast: CobolProgram, fileName: string) {
    return {
      programName: ast.name,
      fileName,
      analysisDate: new Date().toISOString(),
      analysisVersion: this.version,
      totalLinesOfCode: result.parseResult.sourceInfo.lineCount,
      totalStatements: result.analysis?.complexity?.totalStatements || 0,
      totalParagraphs: result.analysis?.complexity?.totalParagraphs || 0,
      totalSections: result.analysis?.complexity?.totalSections || 0
    };
  }

  /**
   * Generate summary section
   */
  private generateSummary(qualityMetrics: CodeQualityMetrics, result: AnalysisResult) {
    const totalIssues = qualityMetrics.codeSmells.length + 
                       qualityMetrics.securityIssues.length + 
                       qualityMetrics.performanceIssues.length;
    
    const criticalIssues = qualityMetrics.codeSmells.filter(s => s.severity === 'CRITICAL').length +
                          qualityMetrics.securityIssues.filter(s => s.severity === 'CRITICAL').length;
    
    const majorIssues = qualityMetrics.codeSmells.filter(s => s.severity === 'MAJOR').length +
                       qualityMetrics.securityIssues.filter(s => s.severity === 'HIGH').length;
    
    const overallScore = Math.round(
      (qualityMetrics.maintainabilityIndex + qualityMetrics.complexityScore) / 2
    );
    
    let grade: 'A' | 'B' | 'C' | 'D' | 'F' = 'F';
    if (overallScore >= 90) grade = 'A';
    else if (overallScore >= 80) grade = 'B';
    else if (overallScore >= 70) grade = 'C';
    else if (overallScore >= 60) grade = 'D';
    
    return {
      overallScore,
      grade,
      totalIssues,
      criticalIssues,
      majorIssues,
      minorIssues: totalIssues - criticalIssues - majorIssues,
      infoIssues: 0
    };
  }

  /**
   * Generate recommendations
   */
  private generateRecommendations(qualityMetrics: CodeQualityMetrics, result: AnalysisResult): Recommendation[] {
    const recommendations: Recommendation[] = [];
    
    // Maintainability recommendations
    if (qualityMetrics.maintainabilityIndex < 70) {
      recommendations.push({
        priority: 'HIGH',
        category: 'MAINTAINABILITY',
        title: 'Improve Code Maintainability',
        description: 'The maintainability index is below acceptable threshold',
        impact: 'Reduces development velocity and increases bug risk',
        effort: '2-4 hours',
        examples: ['Break down large paragraphs', 'Reduce nesting depth', 'Remove dead code']
      });
    }
    
    // Security recommendations
    if (qualityMetrics.securityIssues.length > 0) {
      recommendations.push({
        priority: 'HIGH',
        category: 'SECURITY',
        title: 'Address Security Issues',
        description: `${qualityMetrics.securityIssues.length} security issues found`,
        impact: 'May expose system to security vulnerabilities',
        effort: '1-2 hours per issue',
        examples: ['Remove hardcoded credentials', 'Validate input parameters', 'Use secure coding practices']
      });
    }
    
    // Performance recommendations
    if (qualityMetrics.performanceIssues.length > 0) {
      recommendations.push({
        priority: 'MEDIUM',
        category: 'PERFORMANCE',
        title: 'Optimize Performance',
        description: `${qualityMetrics.performanceIssues.length} performance issues identified`,
        impact: 'May cause slow response times and poor user experience',
        effort: '30-60 minutes per issue',
        examples: ['Optimize loop structures', 'Reduce file I/O operations', 'Cache frequently used data']
      });
    }
    
    return recommendations;
  }

  /**
   * Generate quality trends (placeholder)
   */
  private generateTrends(qualityMetrics: CodeQualityMetrics): QualityTrend[] {
    return [
      {
        metric: 'Maintainability Index',
        currentValue: qualityMetrics.maintainabilityIndex,
        trend: 'STABLE',
        changePercentage: 0
      },
      {
        metric: 'Technical Debt',
        currentValue: qualityMetrics.technicalDebt,
        trend: 'STABLE',
        changePercentage: 0
      }
    ];
  }

  /**
   * Export report to various formats
   */
  async exportReport(report: ComprehensiveReport, format: 'json' | 'html' | 'pdf' | 'csv', outputPath: string): Promise<void> {
    switch (format) {
      case 'json':
        writeFileSync(outputPath, JSON.stringify(report, null, 2));
        break;
      case 'html':
        const html = this.generateHTMLReport(report);
        writeFileSync(outputPath, html);
        break;
      case 'csv':
        const csv = this.generateCSVReport(report);
        writeFileSync(outputPath, csv);
        break;
      case 'pdf':
        // Would need PDF generation library
        throw new Error('PDF export not implemented yet');
      default:
        throw new Error(`Unsupported format: ${format}`);
    }
  }

  /**
   * Generate HTML report
   */
  private generateHTMLReport(report: ComprehensiveReport): string {
    return `
<!DOCTYPE html>
<html>
<head>
    <title>COBOL Static Analysis Report - ${report.metadata.programName}</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background: #f0f0f0; padding: 20px; border-radius: 5px; }
        .score { font-size: 24px; font-weight: bold; color: ${this.getScoreColor(report.summary.overallScore)}; }
        .grade { font-size: 48px; font-weight: bold; color: ${this.getGradeColor(report.summary.grade)}; }
        .metrics { display: flex; gap: 20px; margin: 20px 0; }
        .metric { flex: 1; padding: 15px; background: #f9f9f9; border-radius: 5px; }
        .issues { margin: 20px 0; }
        .issue { padding: 10px; margin: 5px 0; border-left: 4px solid #ccc; }
        .critical { border-left-color: #ff4444; }
        .major { border-left-color: #ff8800; }
        .minor { border-left-color: #ffaa00; }
        .recommendations { margin: 20px 0; }
        .recommendation { padding: 15px; margin: 10px 0; background: #e8f4f8; border-radius: 5px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>COBOL Static Analysis Report</h1>
        <h2>${report.metadata.programName}</h2>
        <p>Analysis Date: ${report.metadata.analysisDate}</p>
        <p>File: ${report.metadata.fileName}</p>
        <div class="score">Overall Score: ${report.summary.overallScore}/100</div>
        <div class="grade">Grade: ${report.summary.grade}</div>
    </div>

    <div class="metrics">
        <div class="metric">
            <h3>Lines of Code</h3>
            <div>${report.metadata.totalLinesOfCode}</div>
        </div>
        <div class="metric">
            <h3>Statements</h3>
            <div>${report.metadata.totalStatements}</div>
        </div>
        <div class="metric">
            <h3>Paragraphs</h3>
            <div>${report.metadata.totalParagraphs}</div>
        </div>
        <div class="metric">
            <h3>Technical Debt</h3>
            <div>${report.qualityMetrics.technicalDebt.toFixed(1)} hours</div>
        </div>
    </div>

    <div class="issues">
        <h3>Code Quality Issues (${report.summary.totalIssues})</h3>
        ${report.qualityMetrics.codeSmells.map(smell => `
            <div class="issue ${smell.severity.toLowerCase()}">
                <strong>${smell.type}</strong> - ${smell.message}
                <br><small>Suggestion: ${smell.suggestion}</small>
            </div>
        `).join('')}
    </div>

    <div class="recommendations">
        <h3>Recommendations</h3>
        ${report.recommendations.map(rec => `
            <div class="recommendation">
                <h4>${rec.title} (${rec.priority})</h4>
                <p>${rec.description}</p>
                <p><strong>Impact:</strong> ${rec.impact}</p>
                <p><strong>Effort:</strong> ${rec.effort}</p>
            </div>
        `).join('')}
    </div>
</body>
</html>`;
  }

  /**
   * Generate CSV report
   */
  private generateCSVReport(report: ComprehensiveReport): string {
    const lines = [
      'Category,Type,Severity,Message,Location,Suggestion',
      ...report.qualityMetrics.codeSmells.map(smell => 
        `Code Smell,${smell.type},${smell.severity},"${smell.message}","${smell.location.line}:${smell.location.column}","${smell.suggestion}"`
      ),
      ...report.qualityMetrics.securityIssues.map(issue => 
        `Security,${issue.type},${issue.severity},"${issue.message}","${issue.location.line}:${issue.location.column}","${issue.remediation}"`
      ),
      ...report.qualityMetrics.performanceIssues.map(issue => 
        `Performance,${issue.type},${issue.severity},"${issue.message}","${issue.location.line}:${issue.location.column}","${issue.optimization}"`
      )
    ];
    
    return lines.join('\n');
  }

  private getScoreColor(score: number): string {
    if (score >= 90) return '#28a745';
    if (score >= 80) return '#ffc107';
    if (score >= 70) return '#fd7e14';
    if (score >= 60) return '#dc3545';
    return '#6c757d';
  }

  private getGradeColor(grade: string): string {
    const colors = { A: '#28a745', B: '#ffc107', C: '#fd7e14', D: '#dc3545', F: '#6c757d' };
    return colors[grade as keyof typeof colors] || '#6c757d';
  }
}