/**
 * Dependency tracking and circular dependency detection
 */

import {
  DependencyTracker as IDependencyTracker,
  CircularDependency,
  DependencyGraph
} from './types';

export class DependencyTracker implements IDependencyTracker {
  private graph: Map<string, Set<string>> = new Map();
  private reversed: Map<string, Set<string>> = new Map();

  addDependency(parent: string, child: string): void {
    // Add to forward graph (parent -> child)
    if (!this.graph.has(parent)) {
      this.graph.set(parent, new Set());
    }
    this.graph.get(parent)!.add(child);

    // Add to reverse graph (child -> parent)
    if (!this.reversed.has(child)) {
      this.reversed.set(child, new Set());
    }
    this.reversed.get(child)!.add(parent);
  }

  detectCircularDependencies(): CircularDependency[] {
    const visited = new Set<string>();
    const recursionStack = new Set<string>();
    const cycles: CircularDependency[] = [];

    for (const node of this.graph.keys()) {
      if (!visited.has(node)) {
        const cycle = this.detectCycleFromNode(node, visited, recursionStack, []);
        if (cycle) {
          cycles.push({
            cycle: cycle,
            entryPoint: node
          });
        }
      }
    }

    return cycles;
  }

  getDependencyGraph(): DependencyGraph {
    return {
      nodes: new Map(this.graph),
      reversed: new Map(this.reversed)
    };
  }

  getInclusionOrder(rootCopybook: string): string[] {
    const order: string[] = [];
    const visited = new Set<string>();
    const visiting = new Set<string>();

    const visit = (node: string): boolean => {
      if (visiting.has(node)) {
        // Circular dependency detected
        return false;
      }
      
      if (visited.has(node)) {
        return true;
      }

      visiting.add(node);
      
      const dependencies = this.graph.get(node);
      if (dependencies) {
        for (const dep of dependencies) {
          if (!visit(dep)) {
            return false; // Circular dependency
          }
        }
      }

      visiting.delete(node);
      visited.add(node);
      order.push(node);
      
      return true;
    };

    if (visit(rootCopybook)) {
      return order; // Don't reverse - post-order traversal gives correct inclusion order
    } else {
      throw new Error(`Circular dependency detected starting from ${rootCopybook}`);
    }
  }

  clear(): void {
    this.graph.clear();
    this.reversed.clear();
  }

  private detectCycleFromNode(
    node: string,
    visited: Set<string>,
    recursionStack: Set<string>,
    path: string[]
  ): string[] | null {
    visited.add(node);
    recursionStack.add(node);
    path.push(node);

    const dependencies = this.graph.get(node);
    if (dependencies) {
      for (const neighbor of dependencies) {
        if (!visited.has(neighbor)) {
          const cycle = this.detectCycleFromNode(neighbor, visited, recursionStack, [...path]);
          if (cycle) {
            return cycle;
          }
        } else if (recursionStack.has(neighbor)) {
          // Found a back edge - cycle detected
          const cycleStart = path.indexOf(neighbor);
          return path.slice(cycleStart).concat([neighbor]);
        }
      }
    }

    recursionStack.delete(node);
    return null;
  }

  // Utility methods
  hasDependency(parent: string, child: string): boolean {
    const dependencies = this.graph.get(parent);
    return dependencies ? dependencies.has(child) : false;
  }

  getDependencies(node: string): string[] {
    const dependencies = this.graph.get(node);
    return dependencies ? Array.from(dependencies) : [];
  }

  getDependents(node: string): string[] {
    const dependents = this.reversed.get(node);
    return dependents ? Array.from(dependents) : [];
  }

  removeDependency(parent: string, child: string): void {
    const dependencies = this.graph.get(parent);
    if (dependencies) {
      dependencies.delete(child);
      if (dependencies.size === 0) {
        this.graph.delete(parent);
      }
    }

    const dependents = this.reversed.get(child);
    if (dependents) {
      dependents.delete(parent);
      if (dependents.size === 0) {
        this.reversed.delete(child);
      }
    }
  }

  getNodeCount(): number {
    const allNodes = new Set<string>();
    
    for (const node of this.graph.keys()) {
      allNodes.add(node);
    }
    
    for (const [node, dependencies] of this.graph) {
      allNodes.add(node);
      for (const dep of dependencies) {
        allNodes.add(dep);
      }
    }
    
    return allNodes.size;
  }

  getEdgeCount(): number {
    let count = 0;
    for (const dependencies of this.graph.values()) {
      count += dependencies.size;
    }
    return count;
  }

  getAllNodes(): string[] {
    const allNodes = new Set<string>();
    
    for (const node of this.graph.keys()) {
      allNodes.add(node);
    }
    
    for (const dependencies of this.graph.values()) {
      for (const dep of dependencies) {
        allNodes.add(dep);
      }
    }
    
    return Array.from(allNodes);
  }

  // Check if the graph is acyclic
  isAcyclic(): boolean {
    return this.detectCircularDependencies().length === 0;
  }

  // Get topological sort if graph is acyclic
  getTopologicalSort(): string[] | null {
    if (!this.isAcyclic()) {
      return null;
    }

    const result: string[] = [];
    const visited = new Set<string>();
    const temp = new Set<string>();

    const visit = (node: string): boolean => {
      if (temp.has(node)) {
        return false; // Cycle detected
      }
      if (visited.has(node)) {
        return true;
      }

      temp.add(node);
      const dependencies = this.graph.get(node);
      if (dependencies) {
        for (const dep of dependencies) {
          if (!visit(dep)) {
            return false;
          }
        }
      }
      temp.delete(node);
      visited.add(node);
      result.push(node); // Add to end instead of beginning
      return true;
    };

    for (const node of this.getAllNodes()) {
      if (!visited.has(node)) {
        if (!visit(node)) {
          return null;
        }
      }
    }

    return result;
  }
}
