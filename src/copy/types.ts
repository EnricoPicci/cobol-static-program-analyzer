/**
 * Type definitions for COPY statement processing
 */

export interface CopybookConfiguration {
  searchPaths: string[];
  extensions: string[];
  cacheEnabled: boolean;
  maxNestingLevel: number;
  encoding: string;
  caseSensitive: boolean;
  parallelProcessing: boolean;
  errorRecovery: 'strict' | 'lenient';
}

export interface CopyStatementInfo {
  name: string;
  library?: string;
  replacingClause?: ReplacingClause;
  sourceLocation: SourceLocation;
}

export interface ReplacingClause {
  patterns: ReplacingPattern[];
}

export interface ReplacingPattern {
  oldText: PseudoText;
  newText: PseudoText;
}

export interface PseudoText {
  content: string;
  type: 'literal' | 'identifier' | 'pseudotext';
}

export interface SourceLocation {
  line: number;
  column: number;
  filename?: string;
}

export interface CopybookMetadata {
  name: string;
  path: string;
  size: number;
  lastModified: Date;
  encoding: string;
  dependencies: string[];
}

export interface ResolvedCopybook {
  name: string;
  content: string;
  path: string;
  metadata: CopybookMetadata;
  dependencies: CopybookDependency[];
}

export interface CopybookDependency {
  name: string;
  path: string;
  type: 'direct' | 'indirect';
}

export interface ProcessingResult {
  success: boolean;
  originalStatement: CopyStatementInfo;
  resolvedCopybook?: ResolvedCopybook;
  processedContent?: string;
  dependencies: CopybookDependency[];
  errors: CopyProcessingError[];
  sourceMap?: SourceMapping[];
}

export interface CopyProcessingError {
  type: 'COPYBOOK_NOT_FOUND' | 'CIRCULAR_DEPENDENCY' | 'REPLACING_ERROR' | 'SYNTAX_ERROR' | 'IO_ERROR';
  message: string;
  location: SourceLocation;
  copybook?: string;
  details?: any;
}

export interface SourceMapping {
  originalLine: number;
  originalColumn: number;
  processedLine: number;
  processedColumn: number;
  source: 'main' | 'copybook';
  copybook?: string;
}

export interface CircularDependency {
  cycle: string[];
  entryPoint: string;
}

export interface DependencyGraph {
  nodes: Map<string, Set<string>>;
  reversed: Map<string, Set<string>>;
}

export interface CachedCopybook {
  copybook: ResolvedCopybook;
  timestamp: number;
  hits: number;
}

export interface CopybookResult {
  found: boolean;
  content?: string;
  path?: string;
  metadata?: CopybookMetadata;
  error?: string;
}

export interface PreprocessedSource {
  originalSource: string;
  processedSource: string;
  includedCopybooks: ResolvedCopybook[];
  sourceMap: SourceMapping[];
  errors: CopyProcessingError[];
}

export interface CopybookFinder {
  findCopybook(name: string, library?: string): Promise<CopybookResult>;
  supports(location: string): boolean;
}

export interface CopybookCache {
  get(key: string): Promise<CachedCopybook | null>;
  set(key: string, copybook: CachedCopybook): Promise<void>;
  invalidate(key: string): Promise<void>;
  clear(): Promise<void>;
  getStats(): CacheStats;
}

export interface CacheStats {
  size: number;
  hits: number;
  misses: number;
  hitRatio: number;
}

export interface DependencyTracker {
  addDependency(parent: string, child: string): void;
  detectCircularDependencies(): CircularDependency[];
  getDependencyGraph(): DependencyGraph;
  getInclusionOrder(rootCopybook: string): string[];
  clear(): void;
}

export interface CopybookResolver {
  resolve(copyStatement: CopyStatementInfo): Promise<ResolvedCopybook>;
  configure(config: Partial<CopybookConfiguration>): void;
  getConfiguration(): CopybookConfiguration;
}

export interface ReplacingProcessor {
  applyReplacing(content: string, replacing: ReplacingClause): string;
  validateReplacingClause(replacing: ReplacingClause): CopyProcessingError[];
}

export interface CopyProcessor {
  process(source: string, config?: Partial<CopybookConfiguration>): Promise<PreprocessedSource>;
  processNestedCopies(content: string, currentDepth?: number): Promise<string>;
  getConfiguration(): CopybookConfiguration;
  updateConfiguration(config: Partial<CopybookConfiguration>): void;
}

// Default configuration
export const DEFAULT_COPYBOOK_CONFIG: CopybookConfiguration = {
  searchPaths: ['./copybooks', './copy', '.'],
  extensions: ['.cpy', '.copy', '.cbl', '.cob'],
  cacheEnabled: true,
  maxNestingLevel: 10,
  encoding: 'utf8',
  caseSensitive: false,
  parallelProcessing: true,
  errorRecovery: 'lenient'
};
