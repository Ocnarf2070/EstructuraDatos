package dataStructures.graph;
/**
 * APELLIDOS :                         NOMBRE:
 *
 * TITULACION: .
 *
 * Computes Topological Sorting for DiGraphs
 */


import java.util.Iterator;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;
import dataStructures.set.HashSet;

public class TopologicalSortingDicPar<V> {

    private List<Set<V>> topSort;
    private boolean hasCycle;

    public TopologicalSortingDicPar(DiGraph<V> graph) {
    	
        topSort = new ArrayList<Set<V>>();
        // dictionary: vertex -> # of pending predecessors
        Dictionary<V, Integer> pendingPredecessors = new HashDictionary<V, Integer>();
        Set<V> sources = new HashSet<>();
        hasCycle = false;
        for (V ver : graph.vertices()) {
        	pendingPredecessors.insert(ver, graph.inDegree(ver));
        }
        while (!hasCycle && !pendingPredecessors.isEmpty()) {
        	Set <V> aux = new HashSet<>();
        	for(Tuple2<V, Integer> tuple : pendingPredecessors.keysValues()) {
        		if(tuple._2()==0) {
        			sources.insert(tuple._1());
        			aux.insert(tuple._1());
        			pendingPredecessors.delete(tuple._1());
        		}
        	}
        	if(sources.isEmpty())hasCycle=true;
        	else {
        		topSort.append(aux);
        		Iterator<V> it = sources.iterator();
        		while(it.hasNext()) {
        			V ver = it.next();
        			for(V pred : graph.successors(ver)) {
        				pendingPredecessors.insert(pred, pendingPredecessors.valueOf(pred)-1);
        			}
        			sources.delete(ver);;
        		}
        	}
        }
        // completar
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<Set<V>> order() {
        return hasCycle ? null : topSort;
    }

}