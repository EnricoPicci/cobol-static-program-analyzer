/**
 * In-memory copybook cache implementation
 */

import {
  CopybookCache,
  CachedCopybook,
  CacheStats
} from './types';

export class InMemoryCopybookCache implements CopybookCache {
  private cache: Map<string, CachedCopybook> = new Map();
  private maxAge: number = 5 * 60 * 1000; // 5 minutes default
  private maxSize: number = 100; // Maximum number of cached copybooks
  private hits: number = 0;
  private misses: number = 0;

  constructor(maxAge?: number, maxSize?: number) {
    if (maxAge !== undefined) this.maxAge = maxAge;
    if (maxSize !== undefined) this.maxSize = maxSize;
  }

  async get(key: string): Promise<CachedCopybook | null> {
    const cached = this.cache.get(key);
    
    if (cached && !this.isExpired(cached)) {
      cached.hits++;
      this.hits++;
      return cached;
    }
    
    if (cached && this.isExpired(cached)) {
      this.cache.delete(key);
    }
    
    this.misses++;
    return null;
  }

  async set(key: string, copybook: CachedCopybook): Promise<void> {
    // Implement LRU eviction if cache is full
    if (this.cache.size >= this.maxSize && !this.cache.has(key)) {
      this.evictLeastRecentlyUsed();
    }
    
    this.cache.set(key, {
      ...copybook,
      timestamp: Date.now()
    });
  }

  async invalidate(key: string): Promise<void> {
    this.cache.delete(key);
  }

  async clear(): Promise<void> {
    this.cache.clear();
    this.hits = 0;
    this.misses = 0;
  }

  getStats(): CacheStats {
    const totalRequests = this.hits + this.misses;
    const hitRatio = totalRequests > 0 ? this.hits / totalRequests : 0;
    
    return {
      size: this.cache.size,
      hits: this.hits,
      misses: this.misses,
      hitRatio
    };
  }

  private isExpired(cached: CachedCopybook): boolean {
    return Date.now() - cached.timestamp > this.maxAge;
  }

  private evictLeastRecentlyUsed(): void {
    let oldestKey: string | null = null;
    let oldestTimestamp = Date.now();
    let leastHits = Infinity;
    
    for (const [key, cached] of this.cache) {
      // Prioritize by least hits, then by oldest timestamp
      if (cached.hits < leastHits || 
          (cached.hits === leastHits && cached.timestamp < oldestTimestamp)) {
        oldestKey = key;
        oldestTimestamp = cached.timestamp;
        leastHits = cached.hits;
      }
    }
    
    if (oldestKey) {
      this.cache.delete(oldestKey);
    }
  }

  // Additional utility methods
  getSize(): number {
    return this.cache.size;
  }

  getMaxSize(): number {
    return this.maxSize;
  }

  setMaxSize(maxSize: number): void {
    this.maxSize = maxSize;
    
    // Evict entries if current size exceeds new max size
    while (this.cache.size > this.maxSize) {
      this.evictLeastRecentlyUsed();
    }
  }

  getMaxAge(): number {
    return this.maxAge;
  }

  setMaxAge(maxAge: number): void {
    this.maxAge = maxAge;
    this.cleanupExpired();
  }

  private cleanupExpired(): void {
    const expiredKeys: string[] = [];
    
    for (const [key, cached] of this.cache) {
      if (this.isExpired(cached)) {
        expiredKeys.push(key);
      }
    }
    
    for (const key of expiredKeys) {
      this.cache.delete(key);
    }
  }

  // Get all cache keys
  getKeys(): string[] {
    return Array.from(this.cache.keys());
  }

  // Check if a key exists in cache (without affecting hit/miss stats)
  has(key: string): boolean {
    const cached = this.cache.get(key);
    return cached !== undefined && !this.isExpired(cached);
  }
}
