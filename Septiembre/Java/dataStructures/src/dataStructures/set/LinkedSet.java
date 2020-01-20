package dataStructures.set;

import java.util.Iterator;
import java.util.NoSuchElementException;


public class LinkedSet <T> implements Set<T>{
	private static class Node <E>{
		E elem;
		Node <E> next;
		public Node(E x, Node<E> node) {
			elem =x;
			next =node;
			// TODO Auto-generated constructor stub
		}
	}
	
	private Node <T> first;
	private int size;

	@Override
	public boolean isElem(T el) {
		Node <T> current = first;
		while (current != null && !current.elem.equals(el))current = current.next;
		// TODO Auto-generated method stub
		return (current != null);
	}

	@Override
	public void insert(T el) {
		if(!isElem(el)) {
			Node <T> aux = new Node<>(el, first);
			first = aux;
			size ++;
		}
		// TODO Auto-generated method stub
		
	}

	@Override
	public void delete(T el) {
		Node <T> current = first,previous = null;
		while (current != null && !current.elem.equals(el)) {
			previous = current;
			current = current.next;
		}
		
		boolean found = (current != null) && current.elem.equals(el);
		if(found) {
			if(previous == null) first = current.next;
			else previous.next = current.next;
			size--;
		}
		// TODO Auto-generated method stub	
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return size == 0;
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return size;
	}
	
	public String toString() {
        String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String text = className+"(";
        for (Node<T> p = first; p != null; p = p.next) {
            text +=  p.elem + (p.next != null ? "," : "");
        }
        return text + ")";
    }
	
	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return new LinkedSetIterator();
	}
	
	private class LinkedSetIterator implements Iterator<T>{
		Node<T> current;
		
		public LinkedSetIterator() {
			current = first;
			// TODO Auto-generated constructor stub
		}
		
		@Override
		public boolean hasNext() {
			// TODO Auto-generated method stub
			return current != null;
		}

		@Override
		public T next() {
			if(!hasNext())throw new NoSuchElementException();
			// TODO Auto-generated method stub
			T x = current.elem;
			current=current.next;
			return x;
		}
		
	}

}
