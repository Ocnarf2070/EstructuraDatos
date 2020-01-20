/**
 * APELLIDOS :   González Sánchez     NOMBRE:Franco Emanuel
 *
 * TITULACION: .
 *
 * Computes Topological Sorting for DiGraphs
 */

package dataStructures.graph;

import javax.sound.midi.SoundbankResource;
import javax.xml.transform.Source;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.List;
import dataStructures.list.ArrayList;
import dataStructures.queue.Queue;
import dataStructures.set.Set;
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
        
        for(V vertice:graph.vertices()){
        	pendingPredecessors.insert(vertice, graph.inDegree(vertice));
        	if(graph.inDegree(vertice)==0)sources.enqueue(vertice);
        }
        int i=0;
        while(!sources.isEmpty()){
        	topSort.insert(i, sources.first());
        	Set<V> pred = null;
        	
        	for(V vertice:graph.vertices()){
        		Set<V> next=graph.predecessors(vertice);
        		if(next.isElem(sources.first()))pred.insert(vertice);
        	}
        	for(Tuple2 <V,Integer> tuple :pendingPredecessors.keysValues()){
        		if(pred.isElem(tuple._1())){
        		pendingPredecessors.insert(tuple._1(),tuple._2()-1);
        		}
        		if(tuple._2()==0)sources.enqueue(tuple._1());
        	}
        	sources.dequeue();
        	i++;
        	
        }
        if(!pendingPredecessors.isEmpty())hasCycle=true;
        
        // completar
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<V> order() {
        return hasCycle ? null : topSort;
    }
}
