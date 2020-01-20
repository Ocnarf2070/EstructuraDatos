package dataStructures.graph;
import java.util.Iterator;

import dataStructures.set.HashSet;
import dataStructures.set.Set;
public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		DiGraph<Character> graph = new DictionaryDiGraph<>();
		for (char c = 'A'; c <= 'H'; c++) {
            graph.addVertex(c);
        }
		
		graph.addDiEdge('A', 'B');
		graph.addDiEdge('B', 'F');
		graph.addDiEdge('B', 'E');
		graph.addDiEdge('C', 'D');
		graph.addDiEdge('C', 'G');
		graph.addDiEdge('D', 'C');
		graph.addDiEdge('D', 'H');
		graph.addDiEdge('E', 'F');
		graph.addDiEdge('E', 'A');
		graph.addDiEdge('F', 'G');
		graph.addDiEdge('G', 'F');
		graph.addDiEdge('H', 'D');
		graph.addDiEdge('H', 'G');
		System.out.println(graph.toString());
		new SCCDiGraph();
		System.out.println(SCCDiGraph.reverseDiGraph(graph));
		Set <Character> ver = new HashSet<>();
		ver.insert('A');ver.insert('B');ver.insert('H');ver.insert('I');ver.insert('j');
		System.out.println(SCCDiGraph.restrictDiGraph(graph, ver));
		for(Character v : graph.vertices())System.out.println(SCCDiGraph.sccOf(graph, v));
		System.out.println(SCCDiGraph.stronglyConnectedComponentsDiGraph(graph));
	}

}