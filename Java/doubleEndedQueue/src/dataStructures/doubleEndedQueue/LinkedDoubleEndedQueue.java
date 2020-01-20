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
     * Complexity:
     */
    public LinkedDoubleEndedQueue() {
        // TODO Auto-generated method stub
    		first=null;
    		last=null;
    }

    /**
     * Complexity:
     */
    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return (first==null&&last==null)?true:false;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
        // TODO Auto-generated method stub
    	Node <T>primero=new Node<>(x, first, null);
    	if(!isEmpty())first.prev=primero;
    	else last=primero;
    	first=primero;
    }

    /**
     * Complexity:
     */
    @Override
    public void addLast(T x) {
        // TODO Auto-generated method stub
    	Node <T>ultimo=new Node<>(x, null, last);
    	if(!isEmpty())last.next=ultimo;
    	else first=ultimo;
    	last=ultimo;
    }

    /**
     * Complexity:
     */
    @Override
    public T first() {
        // TODO Auto-generated method stub
    	if(!isEmpty()){
        return first.elem;
    	}else{
		throw new EmptyDoubleEndedQueueException("first on empty queue");
	}
    }

    /**
     * Complexity:
     */
    @Override
    public T last() {
        // TODO Auto-generated method stub
    	if(!isEmpty()){
        return last.elem;
    	}else{
    		throw new EmptyDoubleEndedQueueException("last on empty queue");
    	}
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteFirst() {
        // TODO Auto-generated method stub
    	
    	first=first.next;
    	if(first!=null)first.prev=null;
    	else last = null;
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteLast() {
        // TODO Auto-generated method stub
    	last=last.prev;
    	if(last!=null)last.next=null;
    	else first=null;
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
