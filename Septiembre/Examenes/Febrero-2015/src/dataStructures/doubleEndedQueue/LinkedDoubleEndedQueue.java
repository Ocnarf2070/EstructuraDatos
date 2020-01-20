/**
 * Estructuras de Datos. Grado en Informática, IS e IC. UMA.
 * Examen de Febrero 2015.
 *
 * Implementación del TAD Deque
 *
 * Apellidos:
 * Nombre:
 * Grado en Ingeniería ...
 * Grupo:
 * Número de PC:
 */

package dataStructures.doubleEndedQueue;

public class LinkedDoubleEndedQueue<T> implements DoubleEndedQueue<T> {

    private static class Node<E> {
        private E elem;
        private Node<E> next;
        private Node<E> prev;

        public Node(E x, Node<E> nxt, Node<E> prv) {
            elem = x;
            next = nxt;
            prev = prv;
        }
    }

    private Node<T> first, last;

    /**
     *  Invariants:
     *  if queue is empty then both first and last are null
     *  if queue is non-empty:
     *      * first is a reference to first node and last is ref to last node
     *      * first.prev is null
     *      * last.next is null
     *      * rest of nodes are doubly linked
     */

    /**
     * Complexity: O(1)
     */
    public LinkedDoubleEndedQueue() {
    	first = null;
    	last = null;
        // TODO Auto-generated method stub

    }

    /**
     * Complexity: O(1)
     */
    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return first==null && last == null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
    	Node <T> node = new Node<>(x, null, null);
    	if(isEmpty()) {
    		first = node;
    		last = node;
    	}else {
    		node.next = first;
    		first.prev = node;
    		first = node;
    	}
        // TODO Auto-generated method stub

    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addLast(T x) {
    	Node <T> node = new Node<>(x, null, null);
    	if(isEmpty()) {
    		first = node;
    		last = node;
    	}else {
    		node.prev=last;
    		last.next=node;
    		last = node;
    	}
        // TODO Auto-generated method stub

    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T first() {
        // TODO Auto-generated method stub
        return first.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T last() {
        // TODO Auto-generated method stub
        return last.elem;
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteFirst() {
        // TODO Auto-generated method stub
    	if(isEmpty())throw new EmptyDoubleEndedQueueException("deleteFirst on empty queue");
    	if (first.next!=null)first.next.prev=null;
    	else last = null;
        first = first.next;
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteLast() {
    	if(isEmpty())throw new EmptyDoubleEndedQueueException("deleteLast on empty queue");
    	if (last.prev!=null) last.prev.next=null;
    	else first = null;
        last = last.prev;
        // TODO Auto-generated method stub

    }

    /**
     * Returns representation of queue as a String.
     */
    @Override
    public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String s = className+"(";
        for (Node<T> node = first; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
