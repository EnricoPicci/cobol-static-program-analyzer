/**
 * Phase 5: API Integration System
 * Provides integration with external static analysis tools and platforms
 */

import { ComprehensiveReport } from '../reporting/report-generator';
import { AnalysisResult } from '../CobolAnalyzer';

export interface SonarQubeConfig {
  serverUrl: string;
  token: string;
  projectKey: string;
  organizationKey?: string;
}

export interface JenkinsConfig {
  serverUrl: string;
  username: string;
  token: string;
  jobName: string;
}

export interface GitLabConfig {
  serverUrl: string;
  token: string;
  projectId: string;
}

export interface IntegrationResult {
  success: boolean;
  message: string;
  externalId?: string;
  dashboardUrl?: string;
  details?: any;
}

export interface WebhookPayload {
  event: 'analysis_complete' | 'quality_gate_failed' | 'security_issue_found';
  timestamp: string;
  report: ComprehensiveReport;
  metadata: {
    triggeredBy?: string;
    buildNumber?: string;
    commitHash?: string;
    branch?: string;
  };
}

/**
 * API Integration Client for external tool integration
 */
export class APIIntegrationClient {
  
  /**
   * Send analysis results to SonarQube
   */
  async sendToSonarQube(report: ComprehensiveReport, config: SonarQubeConfig): Promise<IntegrationResult> {
    try {
      const sonarFormat = this.convertToSonarQubeFormat(report);
      
      const response = await fetch(`${config.serverUrl}/api/issues/bulk_change`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${config.token}`,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          projectKey: config.projectKey,
          organization: config.organizationKey,
          issues: sonarFormat.issues,
          metrics: sonarFormat.metrics
        })
      });

      if (response.ok) {
        const result = await response.json();
        return {
          success: true,
          message: 'Successfully sent to SonarQube',
          externalId: result.id,
          dashboardUrl: `${config.serverUrl}/dashboard?id=${config.projectKey}`
        };
      } else {
        return {
          success: false,
          message: `SonarQube API error: ${response.status} ${response.statusText}`
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Failed to send to SonarQube: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Trigger Jenkins build with analysis results
   */
  async triggerJenkinsBuild(report: ComprehensiveReport, config: JenkinsConfig): Promise<IntegrationResult> {
    try {
      const buildParams = {
        ANALYSIS_SCORE: report.summary.overallScore.toString(),
        QUALITY_GATE: report.summary.grade,
        CRITICAL_ISSUES: report.summary.criticalIssues.toString(),
        MAJOR_ISSUES: report.summary.majorIssues.toString(),
        TECHNICAL_DEBT: report.qualityMetrics.technicalDebt.toString(),
        PROGRAM_NAME: report.metadata.programName
      };

      const response = await fetch(`${config.serverUrl}/job/${config.jobName}/buildWithParameters`, {
        method: 'POST',
        headers: {
          'Authorization': `Basic ${Buffer.from(`${config.username}:${config.token}`).toString('base64')}`,
          'Content-Type': 'application/x-www-form-urlencoded'
        },
        body: new URLSearchParams(buildParams).toString()
      });

      if (response.ok) {
        return {
          success: true,
          message: 'Jenkins build triggered successfully',
          dashboardUrl: `${config.serverUrl}/job/${config.jobName}`
        };
      } else {
        return {
          success: false,
          message: `Jenkins API error: ${response.status} ${response.statusText}`
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Failed to trigger Jenkins build: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Create GitLab merge request comment with analysis results
   */
  async createGitLabComment(
    report: ComprehensiveReport, 
    config: GitLabConfig, 
    mergeRequestIid: number
  ): Promise<IntegrationResult> {
    try {
      const comment = this.generateGitLabComment(report);
      
      const response = await fetch(
        `${config.serverUrl}/api/v4/projects/${config.projectId}/merge_requests/${mergeRequestIid}/notes`,
        {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${config.token}`,
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({
            body: comment
          })
        }
      );

      if (response.ok) {
        const result = await response.json();
        return {
          success: true,
          message: 'GitLab comment created successfully',
          externalId: result.id,
          dashboardUrl: result.web_url
        };
      } else {
        return {
          success: false,
          message: `GitLab API error: ${response.status} ${response.statusText}`
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Failed to create GitLab comment: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Send webhook notification
   */
  async sendWebhook(
    webhookUrl: string, 
    payload: WebhookPayload, 
    secret?: string
  ): Promise<IntegrationResult> {
    try {
      const headers: Record<string, string> = {
        'Content-Type': 'application/json',
        'User-Agent': 'COBOL-Static-Analyzer/1.0'
      };

      // Add signature if secret provided
      if (secret) {
        const crypto = await import('crypto');
        const signature = crypto
          .createHmac('sha256', secret)
          .update(JSON.stringify(payload))
          .digest('hex');
        headers['X-Hub-Signature-256'] = `sha256=${signature}`;
      }

      const response = await fetch(webhookUrl, {
        method: 'POST',
        headers,
        body: JSON.stringify(payload)
      });

      if (response.ok) {
        return {
          success: true,
          message: 'Webhook sent successfully'
        };
      } else {
        return {
          success: false,
          message: `Webhook failed: ${response.status} ${response.statusText}`
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Failed to send webhook: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Get analysis history from external system
   */
  async getAnalysisHistory(
    config: SonarQubeConfig, 
    days: number = 30
  ): Promise<{ success: boolean; data?: any; message?: string }> {
    try {
      const response = await fetch(
        `${config.serverUrl}/api/measures/search_history?component=${config.projectKey}&metrics=ncloc,complexity,violations&from=${this.getDateString(days)}`,
        {
          headers: {
            'Authorization': `Bearer ${config.token}`
          }
        }
      );

      if (response.ok) {
        const data = await response.json();
        return { success: true, data };
      } else {
        return { 
          success: false, 
          message: `Failed to fetch history: ${response.status} ${response.statusText}` 
        };
      }
    } catch (error) {
      return {
        success: false,
        message: `Error fetching analysis history: ${error instanceof Error ? error.message : String(error)}`
      };
    }
  }

  /**
   * Convert report to SonarQube format
   */
  private convertToSonarQubeFormat(report: ComprehensiveReport) {
    return {
      issues: [
        ...report.qualityMetrics.codeSmells.map(smell => ({
          rule: `cobol:${smell.type.toLowerCase()}`,
          severity: this.mapSeverityToSonar(smell.severity),
          message: smell.message,
          component: report.metadata.fileName,
          line: smell.location.line,
          type: 'CODE_SMELL'
        })),
        ...report.qualityMetrics.securityIssues.map(issue => ({
          rule: `cobol:${issue.type.toLowerCase()}`,
          severity: this.mapSecuritySeverityToSonar(issue.severity),
          message: issue.message,
          component: report.metadata.fileName,
          line: issue.location.line,
          type: 'VULNERABILITY'
        }))
      ],
      metrics: {
        ncloc: report.metadata.totalLinesOfCode,
        complexity: report.qualityMetrics.complexityScore,
        violations: report.summary.totalIssues,
        technical_debt: Math.round(report.qualityMetrics.technicalDebt * 60), // Convert to minutes
        maintainability_rating: this.gradeToRating(report.summary.grade),
        reliability_rating: this.calculateReliabilityRating(report),
        security_rating: this.calculateSecurityRating(report)
      }
    };
  }

  /**
   * Generate GitLab merge request comment
   */
  private generateGitLabComment(report: ComprehensiveReport): string {
    const gradeEmoji = this.getGradeEmoji(report.summary.grade);
    
    return `
## 🔍 COBOL Static Analysis Report ${gradeEmoji}

**Program:** \`${report.metadata.programName}\`  
**Overall Score:** ${report.summary.overallScore}/100 (Grade: **${report.summary.grade}**)  
**Analysis Date:** ${new Date(report.metadata.analysisDate).toLocaleString()}

### 📊 Summary
- **Lines of Code:** ${report.metadata.totalLinesOfCode}
- **Total Issues:** ${report.summary.totalIssues}
- **Critical Issues:** ${report.summary.criticalIssues}
- **Major Issues:** ${report.summary.majorIssues}
- **Technical Debt:** ${report.qualityMetrics.technicalDebt.toFixed(1)} hours

### 📈 Quality Metrics
- **Maintainability Index:** ${report.qualityMetrics.maintainabilityIndex}/100
- **Complexity Score:** ${report.qualityMetrics.complexityScore}/100
- **Code Duplication:** ${report.qualityMetrics.duplicationPercentage.toFixed(1)}%

### 🚨 Top Issues
${report.qualityMetrics.codeSmells.slice(0, 5).map(smell => 
  `- **${smell.type}** (${smell.severity}): ${smell.message}`
).join('\n')}

${report.recommendations.length > 0 ? `
### 💡 Key Recommendations
${report.recommendations.slice(0, 3).map(rec => 
  `- **${rec.title}**: ${rec.description}`
).join('\n')}
` : ''}

---
*Generated by COBOL Static Program Analyzer v${report.metadata.analysisVersion}*
`;
  }

  /**
   * Helper methods
   */
  private mapSeverityToSonar(severity: string): string {
    const mapping: Record<string, string> = {
      'CRITICAL': 'BLOCKER',
      'MAJOR': 'MAJOR',
      'MINOR': 'MINOR',
      'INFO': 'INFO'
    };
    return mapping[severity] || 'MINOR';
  }

  private mapSecuritySeverityToSonar(severity: string): string {
    const mapping: Record<string, string> = {
      'CRITICAL': 'BLOCKER',
      'HIGH': 'CRITICAL',
      'MEDIUM': 'MAJOR',
      'LOW': 'MINOR'
    };
    return mapping[severity] || 'MINOR';
  }

  private gradeToRating(grade: string): number {
    const mapping: Record<string, number> = {
      'A': 1, 'B': 2, 'C': 3, 'D': 4, 'F': 5
    };
    return mapping[grade] || 5;
  }

  private calculateReliabilityRating(report: ComprehensiveReport): number {
    const bugs = report.qualityMetrics.codeSmells.filter(s => 
      s.type === 'DEAD_CODE' || s.type === 'COMPLEX_CONDITIONAL'
    ).length;
    
    if (bugs === 0) return 1;
    if (bugs <= 3) return 2;
    if (bugs <= 7) return 3;
    if (bugs <= 15) return 4;
    return 5;
  }

  private calculateSecurityRating(report: ComprehensiveReport): number {
    const securityIssues = report.qualityMetrics.securityIssues;
    const criticalIssues = securityIssues.filter(i => i.severity === 'CRITICAL').length;
    const highIssues = securityIssues.filter(i => i.severity === 'HIGH').length;
    
    if (criticalIssues > 0) return 5;
    if (highIssues > 0) return 4;
    if (securityIssues.length > 5) return 3;
    if (securityIssues.length > 0) return 2;
    return 1;
  }

  private getGradeEmoji(grade: string): string {
    const mapping: Record<string, string> = {
      'A': '🟢', 'B': '🟡', 'C': '🟠', 'D': '🔴', 'F': '❌'
    };
    return mapping[grade] || '❓';
  }

  private getDateString(daysAgo: number): string {
    const date = new Date();
    date.setDate(date.getDate() - daysAgo);
    return date.toISOString().split('T')[0];
  }
}

/**
 * Webhook server for receiving external events
 */
export class WebhookServer {
  private port: number;
  private handlers: Map<string, (payload: any) => void> = new Map();

  constructor(port: number = 3000) {
    this.port = port;
  }

  /**
   * Register webhook handler
   */
  onWebhook(event: string, handler: (payload: any) => void): void {
    this.handlers.set(event, handler);
  }

  /**
   * Start webhook server
   */
  async start(): Promise<void> {
    // Implementation would depend on chosen HTTP framework (Express, Fastify, etc.)
    console.log(`Webhook server would start on port ${this.port}`);
    console.log(`Registered handlers: ${Array.from(this.handlers.keys()).join(', ')}`);
  }

  /**
   * Stop webhook server
   */
  async stop(): Promise<void> {
    console.log('Webhook server stopped');
  }
}