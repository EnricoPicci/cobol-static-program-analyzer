/**
 * Specialized error handler for COPY statement processing
 */

import {
  CopyProcessingError,
  SourceLocation
} from './types';

export class CopyErrorHandler {
  private errors: CopyProcessingError[] = [];

  handleCopybookNotFound(name: string, library: string | undefined, location: SourceLocation): void {
    const message = library 
      ? `Copybook '${name}' not found in library '${library}'`
      : `Copybook '${name}' not found in search paths`;
    
    this.errors.push({
      type: 'COPYBOOK_NOT_FOUND',
      message,
      location,
      copybook: name
    });
  }

  handleCircularDependency(cycle: string[], location: SourceLocation): void {
    this.errors.push({
      type: 'CIRCULAR_DEPENDENCY',
      message: `Circular dependency detected: ${cycle.join(' -> ')}`,
      location,
      details: { cycle }
    });
  }

  handleReplacingError(message: string, location: SourceLocation, details?: any): void {
    this.errors.push({
      type: 'REPLACING_ERROR',
      message,
      location,
      details
    });
  }

  handleSyntaxError(message: string, location: SourceLocation, copybook?: string): void {
    this.errors.push({
      type: 'SYNTAX_ERROR',
      message,
      location,
      copybook
    });
  }

  handleIOError(message: string, location: SourceLocation, copybook?: string): void {
    this.errors.push({
      type: 'IO_ERROR',
      message,
      location,
      copybook
    });
  }

  handleMaxNestingLevelExceeded(maxLevel: number, location: SourceLocation): void {
    this.errors.push({
      type: 'SYNTAX_ERROR',
      message: `Maximum nesting level of ${maxLevel} exceeded`,
      location
    });
  }

  handleParsingError(error: Error, location: SourceLocation): void {
    this.errors.push({
      type: 'SYNTAX_ERROR',
      message: `Parsing error: ${error.message}`,
      location,
      details: { originalError: error.toString() }
    });
  }

  handleCopyProcessingError(error: Error, location: SourceLocation, copybook?: string): void {
    this.errors.push({
      type: 'SYNTAX_ERROR',
      message: `Copy processing error: ${error.message}`,
      location,
      copybook,
      details: { originalError: error.toString() }
    });
  }

  handleUnexpectedError(error: Error, location: SourceLocation): void {
    this.errors.push({
      type: 'SYNTAX_ERROR',
      message: `Unexpected error: ${error.message}`,
      location,
      details: { 
        originalError: error.toString(),
        stack: error.stack 
      }
    });
  }

  getErrors(): CopyProcessingError[] {
    return [...this.errors];
  }

  hasErrors(): boolean {
    return this.errors.length > 0;
  }

  getErrorCount(): number {
    return this.errors.length;
  }

  getErrorsByType(type: CopyProcessingError['type']): CopyProcessingError[] {
    return this.errors.filter(error => error.type === type);
  }

  clear(): void {
    this.errors = [];
  }

  addError(error: CopyProcessingError): void {
    this.errors.push(error);
  }

  removeError(index: number): boolean {
    if (index >= 0 && index < this.errors.length) {
      this.errors.splice(index, 1);
      return true;
    }
    return false;
  }

  // Utility methods for error analysis
  hasErrorType(type: CopyProcessingError['type']): boolean {
    return this.errors.some(error => error.type === type);
  }

  getFirstError(): CopyProcessingError | null {
    return this.errors.length > 0 ? this.errors[0] : null;
  }

  getLastError(): CopyProcessingError | null {
    return this.errors.length > 0 ? this.errors[this.errors.length - 1] : null;
  }

  getErrorsForCopybook(copybook: string): CopyProcessingError[] {
    return this.errors.filter(error => error.copybook === copybook);
  }

  getErrorsAtLocation(line: number, column?: number): CopyProcessingError[] {
    return this.errors.filter(error => {
      if (error.location.line !== line) {
        return false;
      }
      if (column !== undefined && error.location.column !== column) {
        return false;
      }
      return true;
    });
  }

  // Generate error report
  generateReport(): string {
    if (this.errors.length === 0) {
      return 'No copy processing errors';
    }

    const lines: string[] = [
      `Copy Processing Errors (${this.errors.length} total):`,
      ''
    ];

    this.errors.forEach((error, index) => {
      lines.push(`${index + 1}. ${error.type}: ${error.message}`);
      lines.push(`   Location: Line ${error.location.line}, Column ${error.location.column}`);
      
      if (error.copybook) {
        lines.push(`   Copybook: ${error.copybook}`);
      }
      
      if (error.details) {
        lines.push(`   Details: ${JSON.stringify(error.details, null, 2)}`);
      }
      
      lines.push('');
    });

    return lines.join('\n');
  }

  // Generate summary
  generateSummary(): string {
    if (this.errors.length === 0) {
      return 'No copy processing errors';
    }

    const errorCounts: Record<string, number> = {};
    
    this.errors.forEach(error => {
      errorCounts[error.type] = (errorCounts[error.type] || 0) + 1;
    });

    const lines: string[] = [
      `Copy Processing Error Summary (${this.errors.length} total):`
    ];

    Object.entries(errorCounts).forEach(([type, count]) => {
      lines.push(`  ${type}: ${count}`);
    });

    return lines.join('\n');
  }

  // Merge errors from another handler
  mergeErrors(other: CopyErrorHandler): void {
    this.errors.push(...other.getErrors());
  }

  // Filter errors
  filterErrors(predicate: (error: CopyProcessingError) => boolean): CopyProcessingError[] {
    return this.errors.filter(predicate);
  }

  // Sort errors
  sortErrors(compareFn?: (a: CopyProcessingError, b: CopyProcessingError) => number): void {
    if (compareFn) {
      this.errors.sort(compareFn);
    } else {
      // Default sort by location
      this.errors.sort((a, b) => {
        if (a.location.line !== b.location.line) {
          return a.location.line - b.location.line;
        }
        return a.location.column - b.location.column;
      });
    }
  }
}
