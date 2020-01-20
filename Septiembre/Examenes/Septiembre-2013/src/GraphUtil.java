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

public class GraphUtil {

	/**
	 * LENGTH: Calcula el número de elementos que contiene un iterable
	 * 
	 * @param it  El iterador
	 * @return   Número de elementos en el iterador
	 */
	public static <T> int length(Iterable<T> it) {
		// TO DO
		int i = 0;
		Iterator <T> ite = it.iterator();
		while (ite.hasNext()) {
			i++;
			ite.next();
		}
	    return i-1;
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
		BreadthFirstTraversal<T> bft = new BreadthFirstTraversal<>(graph, v);
		Iterator<Iterable<T>> it = bft.pathsIterator();
		int max = Integer.MIN_VALUE;
		while (it.hasNext()) {
			int len = length(it.next());
			if (max < len) max = len;
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
		// TO DO
		int max = Integer.MIN_VALUE;
		for(T ver : graph.vertices()) {
			int ecc = eccentricity(graph, ver);
			if (max < ecc)max = ecc;
		}
	    return max;
	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
}
