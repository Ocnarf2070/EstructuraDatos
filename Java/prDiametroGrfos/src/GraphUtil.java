/** ------------------------------------------------------------------------------
  * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
  *
  * Control del día 4-9-2013
  * 
  * Diámetro de un grafo conexo 
  *
  * (completa y sustituye los siguientes datos)
  * Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
  *
  * Alumno: APELLIDOS, NOMBRE
  *
  * -------------------------------------------------------------------------------
  */

import java.util.Iterator;

import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.Graph;
import dataStructures.list.List;

public class GraphUtil {

	/**
	 * LENGTH: Calcula el número de elementos que contiene un iterable
	 * 
	 * @param it  El iterador
	 * @return   Número de elementos en el iterador
	 */
	public static <T> int length(Iterable<T> it) {
		Iterator <T> elem = it.iterator();
		int i=0;
		while(elem.hasNext()){
			elem.next();
			i++;
		}
	    return i;
	}

	/**
	 * ECCENTRICITY: Calcula la excentricidad de un vértice en un grafo El algoritmo toma la
	 * longitud del camino máximo en un recorrido en profundidad del grafo
	 * comenzando en el vértice dado.
	 * 
	 * @param graph    Grafo
	 * @param v        Vértice del grafo
	 * @return         Excentricidad del vértice
	 */
	public static <T> int eccentricity(Graph<T> graph, T v) {
		int max=0;
		BreadthFirstTraversal <T> bft = new BreadthFirstTraversal<T>(graph, v);
		Iterator<Iterable<T>> l = bft.pathsIterator();
		while (l.hasNext()){
			int num = length(l.next());
			if(num>max){
				max=num;
			}
		;
		}
		// TO DO
	    return max;
	}

	/**
	 * DIAMETER: Se define como la máxima excentricidad de los vértices del grafo.
	 * 
	 * @param graph
	 * @return
	 */

	public static <T> int diameter(Graph<T> graph) {
		int max=0;
		for (T v : graph.vertices()){
			int num = eccentricity(graph, v);
			if (num>max)max=num;
		}
	    return max;
	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
}
