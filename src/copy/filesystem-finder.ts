/**
 * File system based copybook finder implementation
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import {
  CopybookFinder,
  CopybookResult,
  CopybookConfiguration,
  CopybookMetadata
} from './types';

export class FileSystemCopybookFinder implements CopybookFinder {
  private searchPaths: string[];
  private extensions: string[];
  private encoding: string;
  private caseSensitive: boolean;

  constructor(config: CopybookConfiguration) {
    this.searchPaths = config.searchPaths;
    this.extensions = config.extensions;
    this.encoding = config.encoding;
    this.caseSensitive = config.caseSensitive;
  }

  async findCopybook(name: string, library?: string): Promise<CopybookResult> {
    const searchLocations = this.buildSearchLocations(name, library);
    
    for (const location of searchLocations) {
      const result = await this.tryLocation(location, name);
      if (result.found) {
        return result;
      }
    }
    
    return {
      found: false,
      error: `Copybook '${name}' not found in search paths: ${this.searchPaths.join(', ')}`
    };
  }

  supports(location: string): boolean {
    // This finder supports local file system paths
    return !location.startsWith('http') && !location.includes('://');
  }

  private buildSearchLocations(name: string, library?: string): string[] {
    const locations: string[] = [];
    const nameVariations = this.getNameVariations(name);
    
    for (const basePath of this.searchPaths) {
      for (const nameVariation of nameVariations) {
        for (const extension of this.extensions) {
          // Direct file lookup
          locations.push(path.join(basePath, `${nameVariation}${extension}`));
          
          // Library subdirectory lookup
          if (library) {
            const libraryVariations = this.getNameVariations(library);
            for (const libraryVar of libraryVariations) {
              locations.push(path.join(basePath, libraryVar, `${nameVariation}${extension}`));
            }
          }
        }
      }
    }
    
    return locations;
  }

  private getNameVariations(name: string): string[] {
    if (this.caseSensitive) {
      return [name];
    }
    
    return [
      name,
      name.toLowerCase(),
      name.toUpperCase()
    ];
  }

  private async tryLocation(location: string, name: string): Promise<CopybookResult> {
    try {
      const stats = await fs.stat(location);
      
      if (stats.isFile()) {
        const content = await fs.readFile(location, { encoding: this.encoding as BufferEncoding });
        const metadata: CopybookMetadata = {
          name,
          path: location,
          size: stats.size,
          lastModified: stats.mtime,
          encoding: this.encoding,
          dependencies: []
        };
        
        return {
          found: true,
          content,
          path: location,
          metadata
        };
      }
    } catch (error) {
      // File doesn't exist or can't be read, continue to next location
    }
    
    return { found: false };
  }

  updateConfiguration(config: Partial<CopybookConfiguration>): void {
    if (config.searchPaths) this.searchPaths = config.searchPaths;
    if (config.extensions) this.extensions = config.extensions;
    if (config.encoding) this.encoding = config.encoding;
    if (config.caseSensitive !== undefined) this.caseSensitive = config.caseSensitive;
  }

  getSearchPaths(): string[] {
    return [...this.searchPaths];
  }

  addSearchPath(searchPath: string): void {
    if (!this.searchPaths.includes(searchPath)) {
      this.searchPaths.push(searchPath);
    }
  }

  removeSearchPath(searchPath: string): void {
    const index = this.searchPaths.indexOf(searchPath);
    if (index > -1) {
      this.searchPaths.splice(index, 1);
    }
  }
}
