/**
 * Unit tests for DependencyTracker
 */

import '../setup';
import { DependencyTracker } from '../../../src/copy';

describe('DependencyTracker', () => {
  let tracker: DependencyTracker;

  beforeEach(() => {
    tracker = new DependencyTracker();
  });

  describe('Basic Dependency Management', () => {
    test('should add and track dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('A', 'D');

      expect(tracker.hasDependency('A', 'B')).toBe(true);
      expect(tracker.hasDependency('B', 'C')).toBe(true);
      expect(tracker.hasDependency('A', 'D')).toBe(true);
      expect(tracker.hasDependency('B', 'A')).toBe(false);
    });

    test('should get direct dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('A', 'C');
      tracker.addDependency('B', 'D');

      const aDeps = tracker.getDependencies('A');
      expect(aDeps).toHaveLength(2);
      expect(aDeps).toContain('B');
      expect(aDeps).toContain('C');

      const bDeps = tracker.getDependencies('B');
      expect(bDeps).toHaveLength(1);
      expect(bDeps).toContain('D');
    });

    test('should get dependents (reverse dependencies)', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('C', 'B');
      tracker.addDependency('B', 'D');

      const bDependents = tracker.getDependents('B');
      expect(bDependents).toHaveLength(2);
      expect(bDependents).toContain('A');
      expect(bDependents).toContain('C');

      const dDependents = tracker.getDependents('D');
      expect(dDependents).toHaveLength(1);
      expect(dDependents).toContain('B');
    });

    test('should remove dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('A', 'C');
      
      expect(tracker.hasDependency('A', 'B')).toBe(true);
      
      tracker.removeDependency('A', 'B');
      
      expect(tracker.hasDependency('A', 'B')).toBe(false);
      expect(tracker.hasDependency('A', 'C')).toBe(true);
    });
  });

  describe('Circular Dependency Detection', () => {
    test('should detect simple circular dependency', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'A');

      const cycles = tracker.detectCircularDependencies();
      
      expect(cycles).toHaveLength(1);
      expect(cycles[0].cycle).toEqual(expect.arrayContaining(['A', 'B']));
    });

    test('should detect complex circular dependency', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('C', 'D');
      tracker.addDependency('D', 'B'); // Creates cycle B -> C -> D -> B

      const cycles = tracker.detectCircularDependencies();
      
      expect(cycles).toHaveLength(1);
      expect(cycles[0].cycle).toEqual(expect.arrayContaining(['B', 'C', 'D']));
    });

    test('should detect multiple separate cycles', () => {
      // First cycle: A -> B -> A
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'A');
      
      // Second cycle: X -> Y -> Z -> X
      tracker.addDependency('X', 'Y');
      tracker.addDependency('Y', 'Z');
      tracker.addDependency('Z', 'X');

      const cycles = tracker.detectCircularDependencies();
      
      expect(cycles).toHaveLength(2);
    });

    test('should return empty array for acyclic graph', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('A', 'D');
      tracker.addDependency('D', 'E');

      const cycles = tracker.detectCircularDependencies();
      
      expect(cycles).toHaveLength(0);
      expect(tracker.isAcyclic()).toBe(true);
    });
  });

  describe('Inclusion Order', () => {
    test('should generate correct inclusion order for linear dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('C', 'D');

      const order = tracker.getInclusionOrder('A');
      
      expect(order).toEqual(['D', 'C', 'B', 'A']);
    });

    test('should generate correct inclusion order for complex dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('A', 'C');
      tracker.addDependency('B', 'D');
      tracker.addDependency('C', 'D');

      const order = tracker.getInclusionOrder('A');
      
      // D should come first (deepest dependency)
      // Then B and C (can be in either order)
      // Finally A (root)
      expect(order[0]).toBe('D');
      expect(order[3]).toBe('A');
      expect(order).toContain('B');
      expect(order).toContain('C');
    });

    test('should throw error for circular dependencies in inclusion order', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('C', 'A'); // Creates cycle

      expect(() => {
        tracker.getInclusionOrder('A');
      }).toThrow('Circular dependency detected');
    });
  });

  describe('Topological Sort', () => {
    test('should generate topological sort for acyclic graph', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'D');
      tracker.addDependency('C', 'D');
      tracker.addDependency('E', 'F');

      const sorted = tracker.getTopologicalSort();
      
      expect(sorted).not.toBeNull();
      expect(sorted!).toHaveLength(6);
      
      // Check that dependencies come before dependents
      const aIndex = sorted!.indexOf('A');
      const bIndex = sorted!.indexOf('B');
      const dIndex = sorted!.indexOf('D');
      
      expect(bIndex).toBeLessThan(aIndex);
      expect(dIndex).toBeLessThan(bIndex);
    });

    test('should return null for cyclic graph', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('C', 'A');

      const sorted = tracker.getTopologicalSort();
      
      expect(sorted).toBeNull();
    });
  });

  describe('Graph Statistics', () => {
    test('should count nodes correctly', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      tracker.addDependency('D', 'E');

      expect(tracker.getNodeCount()).toBe(5);
    });

    test('should count edges correctly', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('A', 'C');
      tracker.addDependency('B', 'D');

      expect(tracker.getEdgeCount()).toBe(3);
    });

    test('should get all nodes', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('C', 'D');

      const nodes = tracker.getAllNodes();
      
      expect(nodes).toHaveLength(4);
      expect(nodes).toContain('A');
      expect(nodes).toContain('B');
      expect(nodes).toContain('C');
      expect(nodes).toContain('D');
    });
  });

  describe('Graph Operations', () => {
    test('should clear all dependencies', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');
      
      expect(tracker.getNodeCount()).toBe(3);
      
      tracker.clear();
      
      expect(tracker.getNodeCount()).toBe(0);
      expect(tracker.detectCircularDependencies()).toHaveLength(0);
    });

    test('should get dependency graph', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('B', 'C');

      const graph = tracker.getDependencyGraph();
      
      expect(graph.nodes.has('A')).toBe(true);
      expect(graph.nodes.get('A')!.has('B')).toBe(true);
      expect(graph.reversed.has('B')).toBe(true);
      expect(graph.reversed.get('B')!.has('A')).toBe(true);
    });
  });

  describe('Edge Cases', () => {
    test('should handle self-dependency', () => {
      tracker.addDependency('A', 'A');

      const cycles = tracker.detectCircularDependencies();
      
      expect(cycles).toHaveLength(1);
      expect(cycles[0].cycle).toContain('A');
    });

    test('should handle empty graph', () => {
      expect(tracker.getNodeCount()).toBe(0);
      expect(tracker.getEdgeCount()).toBe(0);
      expect(tracker.detectCircularDependencies()).toHaveLength(0);
      expect(tracker.isAcyclic()).toBe(true);
    });

    test('should handle single node', () => {
      tracker.addDependency('A', 'B');
      
      expect(tracker.getDependencies('B')).toHaveLength(0);
      expect(tracker.getDependents('A')).toHaveLength(0);
    });

    test('should handle duplicate dependencies gracefully', () => {
      tracker.addDependency('A', 'B');
      tracker.addDependency('A', 'B'); // Duplicate
      
      // Should not create multiple edges
      expect(tracker.getDependencies('A')).toHaveLength(1);
      expect(tracker.getEdgeCount()).toBe(1);
    });
  });
});
