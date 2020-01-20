/**
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 *
 * PRACTICA 5ª. Ejercicio 9 de la cuarta relación (montículos maxifóbicos en Java)
 *
 * (completa y sustituye los siguientes datos)
 * Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
 * Alumno: APELLIDOS, NOMBRE
 * Fecha de entrega:  DIA | MES | AÑO
 */

package dataStructures.heap;


/**
 * Heap implemented using maxiphobic heap-ordered binary trees.
 * @param <T> Type of elements in heap.
 */
public class MaxiphobicHeap<T extends Comparable<? super T>> implements	Heap<T> {

	private static class Tree<E> {
		private E elem;
		private int size;
		private Tree<E> left;
		private Tree<E> right;
	}

	private static int size(Tree<?> heap) {
		return heap == null ? 0 : heap.size;
	}

	private static <T extends Comparable<? super T>> Tree<T> merge(Tree<T> h1,	Tree<T> h2) {
		if (h1 == null)
			return h2;
		if (h2 == null)
			return h1;

		// COMPLETAR AQUI
		if (h2.elem.compareTo(h1.elem)<0) {
			Tree<T> aux = new Tree<>();
			aux = h1;
			h1 = h2;
			h2 = aux;
		}
		h1.size = h1.size + h2.size;
		Tree<T> A = h1.left;
		Tree<T> B = h1.right;
		Tree<T> C = h2;
		if (size(B) > size(A)) {
			Tree<T> aux = new Tree<>();
			aux = A;
			A = B;
			B = aux;
		}
		if (size(C) > size(A)) {
			Tree<T> aux = new Tree<>();
			aux = A;
			A = C;
			C = aux;
		}
		h1.left = A;
		h1.right = merge(B, C);		
		return h1;
	}

	private Tree<T> root;

	/**
	 * Creates an empty Maxiphobic Heap.
	 * <p>Time complexity: O(1)
	 */
	public MaxiphobicHeap() {
		root = null;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return root == null;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public int size() {
		return root == null ? 0 : root.size;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public T minElem() {
		if (isEmpty())
			throw new EmptyHeapException("minElem on empty heap");
		else
			return root.elem;
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public void delMin() {
		if (isEmpty())
			throw new EmptyHeapException("delMin on empty heap");
		else
			root = merge(root.left, root.right);
	}

	/**
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public void insert(T value) {
		Tree<T> newHeap = new Tree<T>();
		newHeap.elem = value;
		newHeap.size = 1;
		newHeap.left = null;
		newHeap.right = null;

		root = merge(root, newHeap);
	}

	public void clear() {
		root = null;
	}

	private static String toStringRec(Tree<?> tree) {
		return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
				+ tree.elem + "," + toStringRec(tree.right) + ">";
	}

	/**
	 * Returns representation of heap as a String.
	 */
  @Override public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);

  	return className+"("+toStringRec(this.root)+")";
  }

}
