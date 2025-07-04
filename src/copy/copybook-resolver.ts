/**
 * Copybook resolution and file finding implementation
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import {
  CopybookResolver as ICopybookResolver,
  CopybookConfiguration,
  CopyStatementInfo,
  ResolvedCopybook,
  CopybookMetadata,
  CopybookFinder,
  CopybookResult,
  CopybookCache,
  DEFAULT_COPYBOOK_CONFIG
} from './types';
import { FileSystemCopybookFinder } from './filesystem-finder';
import { InMemoryCopybookCache } from './copybook-cache';

export class CopybookResolver implements ICopybookResolver {
  private config: CopybookConfiguration;
  private finders: CopybookFinder[];
  private cache: CopybookCache;

  constructor(config?: Partial<CopybookConfiguration>) {
    this.config = { ...DEFAULT_COPYBOOK_CONFIG, ...config };
    this.cache = new InMemoryCopybookCache();
    this.finders = [
      new FileSystemCopybookFinder(this.config)
    ];
  }

  async resolve(copyStatement: CopyStatementInfo): Promise<ResolvedCopybook> {
    const cacheKey = this.buildCacheKey(copyStatement.name, copyStatement.library);
    
    // Check cache first if enabled
    if (this.config.cacheEnabled) {
      const cached = await this.cache.get(cacheKey);
      if (cached) {
        return cached.copybook;
      }
    }

    // Try each finder until one succeeds
    for (const finder of this.finders) {
      try {
        const result = await finder.findCopybook(copyStatement.name, copyStatement.library);
        
        if (result.found && result.content && result.path) {
          const resolvedCopybook = await this.buildResolvedCopybook(
            copyStatement.name,
            result.content,
            result.path,
            result.metadata
          );
          
          // Cache the result if caching is enabled
          if (this.config.cacheEnabled) {
            await this.cache.set(cacheKey, {
              copybook: resolvedCopybook,
              timestamp: Date.now(),
              hits: 1
            });
          }
          
          return resolvedCopybook;
        }
      } catch (error) {
        // Continue to next finder
        continue;
      }
    }

    throw new Error(`Copybook '${copyStatement.name}' not found in any search location`);
  }

  private async buildResolvedCopybook(
    name: string,
    content: string,
    filePath: string,
    metadata?: CopybookMetadata
  ): Promise<ResolvedCopybook> {
    const stats = await fs.stat(filePath);
    
    const copybookMetadata: CopybookMetadata = metadata || {
      name,
      path: filePath,
      size: stats.size,
      lastModified: stats.mtime,
      encoding: this.config.encoding,
      dependencies: []
    };

    return {
      name,
      content,
      path: filePath,
      metadata: copybookMetadata,
      dependencies: [] // Will be populated during processing
    };
  }

  private buildCacheKey(name: string, library?: string): string {
    return library ? `${library}:${name}` : name;
  }

  configure(config: Partial<CopybookConfiguration>): void {
    this.config = { ...this.config, ...config };
    
    // Update finders with new configuration
    this.finders = [
      new FileSystemCopybookFinder(this.config)
    ];
  }

  getConfiguration(): CopybookConfiguration {
    return { ...this.config };
  }

  addFinder(finder: CopybookFinder): void {
    this.finders.push(finder);
  }

  getCache(): CopybookCache {
    return this.cache;
  }

  async clearCache(): Promise<void> {
    await this.cache.clear();
  }
}
