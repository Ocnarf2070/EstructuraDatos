package dataStructures.graph;
/**
 * APELLIDOS :                         NOMBRE:
 *
 * TITULACION: .
 *
 * Computes Topological Sorting for DiGraphs
 */


import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.queue.Queue;
import dataStructures.tuple.Tuple2;
import dataStructures.queue.LinkedQueue;

public class TopologicalSortingDic<V> {

    private List<V> topSort;
    private boolean hasCycle;

    public TopologicalSortingDic(DiGraph<V> graph) {

        topSort = new ArrayList<V>();
        // dictionary: vertex -> # of pending predecessors
        Dictionary<V, Integer> pendingPredecessors = new HashDictionary<V, Integer>();
        Queue<V> sources = new LinkedQueue<V>();
        hasCycle = false;
        for (V ver : graph.vertices()) {
        	pendingPredecessors.insert(ver, graph.inDegree(ver));
        }
        while(!hasCycle && ! pendingPredecessors.isEmpty()) {
        	for (Tuple2<V, Integer> tuple : pendingPredecessors.keysValues()) {
        		if(tuple._2()==0) {
        			sources.enqueue(tuple._1());
        			pendingPredecessors.delete(tuple._1());
        		}
        	}
        	if(sources.isEmpty())hasCycle = true;
        	else {
        		while(!sources.isEmpty()) {
        			for(V pred : graph.successors(sources.first())) {
        				pendingPredecessors.insert(pred, pendingPredecessors.valueOf(pred)-1);
        			}
        			topSort.append(sources.first());
        			sources.dequeue();
        		}
        	}
        }
        // completar
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<V> order() {
        return hasCycle ? null : topSort;
    }
}