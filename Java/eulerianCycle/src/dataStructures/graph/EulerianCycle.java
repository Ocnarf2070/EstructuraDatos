/**
 * Student's name:
 * Student's group:
 *
 * Data Structures. Grado en Informática. UMA.
 */

package dataStructures.graph;

import java.util.Iterator;

import dataStructures.list.*;
import dataStructures.set.Set;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {
        if(g.numVertices()<2){
        	return true;
        }else{
        	boolean par=true;
        	Iterator <V> it = g.vertices().iterator();
        	while(par && it.hasNext()){
        		if(g.degree(it.next())%2 == 1) par=false;
        	}
        	return par;
        }
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v, u);
        for(V ver:g.vertices()){
        	if(g.degree(ver)==0)g.deleteVertex(ver);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
           List <V> cycle=new ArrayList<>();
           V v=v0,vt;
           while(!g.successors(v).isEmpty()){
        	   cycle.append(v);
        	   vt=v;
        	   v=g.successors(v).iterator().next();
        	   if(v!=null)remove(g, vt, v);
           }
           cycle.append(v);
		
        return cycle;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
    	if(xs.isEmpty()){
			for(V v:ys){
				xs.append(v);
			}
		}else{
			V f=ys.iterator().next();
			Iterator <V> it= xs.iterator();
			int i=0;
			while(it.hasNext()&&!it.next().equals(f)){
				i++;
			}
			xs.remove(i);
			it=ys.iterator();
			while(it.hasNext()){
				xs.insert(i,it.next());
				i++;
			}
		}
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
    		Set<V> set=g.vertices();
			Iterator<V> it = cycle.iterator();
			V v=null;
			boolean encontrado=false;
			//while(it.hasNext() && set.isElem(v=it.hasNext())){
			while(it.hasNext() && !encontrado )	{
				v=it.next();
				if(set.isElem(v) ){
					encontrado=true;
				}
			}
    		return encontrado?v:null;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
    		if(!isEulerian(g)){
				return null;
			}else{
				List<V> eulerian = new ArrayList<>();
				V v =g.vertices().iterator().next();
				do{
					List<V> cycle=extractCycle(g,v);
					connectCycles(eulerian,cycle);
					if(!g.isEmpty()){
						v=vertexInCommon(g,cycle);
					}
				}while(!g.isEmpty());
				return eulerian;
				
			}
    }
}
