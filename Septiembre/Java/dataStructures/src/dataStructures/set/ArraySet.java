package dataStructures.set;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class ArraySet <T> implements Set<T>{
	protected T[] elements;
	protected int size;
	
	private final int INITIAL_CAPACITY = 10;
	
	@SuppressWarnings("unchecked")
	public ArraySet() {
		elements = (T[]) new Object [INITIAL_CAPACITY];
		// TODO Auto-generated constructor stub
	}
	
	private void ensureCapacity () {
		if (size == elements.length) elements = Arrays.copyOf(elements, 2 * elements.length);
	}
	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return new ArraySetIterator();
	}

	@Override
	public boolean isElem(T el) {
		// TODO Auto-generated method stub
		int i=0;
		while (i<size&&!elements[i].equals(el)) i++;
		return i<size;
	}

	@Override
	public void insert(T el) {
		if(!isElem(el)) {
			ensureCapacity();
			elements[size] = el;
			size++;
		}
		// TODO Auto-generated method stub
		
	}

	@Override
	public void delete(T el) {
		int i=0;
		while (i<size&&!elements[i].equals(el)) i++;
		if(i<size) {
			for(int j=i;j<size;j++) {
				elements[j]=elements[j+1];
			}
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
	
	@Override public String toString() {
    	String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
		String s = className+"(";
		for(int i=0; i<size; i++) 
			s += elements[i] + (i<size-1 ? "," : "");
		s += ")";
		return s;			
	}	
	
	private class ArraySetIterator implements Iterator<T>{
		int current;
		
		public ArraySetIterator() {
			current = 0;
			// TODO Auto-generated constructor stub
		}
		
		@Override
		public boolean hasNext() {
			// TODO Auto-generated method stub
			return current != size;
		}

		@Override
		public T next() {
			// TODO Auto-generated method stub
			if(!hasNext()) throw new NoSuchElementException();
			T x = elements [current];
			current++;
			return x;
		}
		
	}
}
