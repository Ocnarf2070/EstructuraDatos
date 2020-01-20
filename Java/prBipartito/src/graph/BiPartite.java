/**
 * @author Blas Ruiz, Data Structures, Grado en Inform�tica. UMA.
 *
 * Control 2. 13-Febrero-2012
 * Estudio de grafos bipartitos por coloreado con b�squeda en profundidad
 */

package graph;

import java.util.Iterator;

import dictionary.Dictionary;
import dictionary.HashDictionary;
import stack.Stack;
import stack.StackList;


public class BiPartite<V> {
	
	public static enum Color {Red, Blue;
	}

	private static Color nextColor(Color c) {
		return (c == Color.Blue) ?Color.Red:Color.Blue; 
	}
	
	private Stack<Pair<V,Color>> stack; // stack with pair of vertex and color
	private Dictionary<V,Color> dict; // dictionary: Vertices -> Color
	private boolean isBiColored;

	public BiPartite(Graph<V> graph) {
		dict      = new HashDictionary<V, Color>();
		stack = new StackList<Pair<V,Color>>();
		isBiColored       = true;
		if (graph.numVertices() == 0)
			return; 

		V src = graph.vertices().iterator().next(); // initial vertex
		
		stack.push(new Pair<V,Color>(src,Color.Red));
		
		while (!stack.isEmpty()) {
			Pair<V,Color> vColor = stack.top();
			stack.pop();
			boolean encontrado=false;
			Iterator <V> it=dict.iterator();
			while(!dict.isEmpty()&&it.hasNext())if(it.next()==vColor.first())encontrado=true;
			if(!encontrado){
				dict.insert(vColor.first(), vColor.second());
				for(V v : graph.successors(vColor.first())){
					//if(vColor.second()==Color.Red){ stack.push(new Pair <V,Color> (v,Color.Blue));
						stack.push(new Pair <V,Color> (v,nextColor(vColor.second())));
					/*}else{
						stack.push(new Pair <V,Color> (v,Color.Red));
					}*/
				}
			}else if(dict.valueOf(vColor.first())!=vColor.second()){
				isBiColored=false;
				while (!stack.isEmpty()) {
					stack.pop();
				}
			}
         	// completad desde aqu� 		
		} 
	}	
	
	public Dictionary<V,Color> biColored() {
		return dict;
	}
	
	public boolean isBicolored() {
		return isBiColored;
	}
	
}
