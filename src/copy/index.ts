/**
 * COPY statement processing module exports
 */

// Core processor
export { CopyProcessor } from './copy-processor';

// Resolution and finding
export { CopybookResolver } from './copybook-resolver';
export { FileSystemCopybookFinder } from './filesystem-finder';

// Caching
export { InMemoryCopybookCache } from './copybook-cache';

// Processing components
export { ReplacingProcessor } from './replacing-processor';
export { DependencyTracker } from './dependency-tracker';
export { CopyStatementVisitor } from './copy-visitor';
export { CopyErrorHandler } from './error-handler';

// Types and interfaces
export * from './types';

// Convenience factory function
import { CopyProcessor } from './copy-processor';
import { CopybookConfiguration, DEFAULT_COPYBOOK_CONFIG } from './types';

/**
 * Create a new copy processor with default or custom configuration
 */
export function createCopyProcessor(config?: Partial<CopybookConfiguration>): CopyProcessor {
  return new CopyProcessor(config);
}

/**
 * Get the default copybook configuration
 */
export function getDefaultCopybookConfiguration(): CopybookConfiguration {
  return { ...DEFAULT_COPYBOOK_CONFIG };
}

/**
 * Quick function to process a single COPY statement
 */
export async function processCopyStatement(
  source: string,
  config?: Partial<CopybookConfiguration>
) {
  const processor = createCopyProcessor(config);
  return await processor.process(source, config);
}
