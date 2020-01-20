package dataStructures.set;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class SortedArraySet <T extends Comparable<? super T>> implements Set<T>{
	protected T[] elements;
	protected int size;
	
	private final int INITIAL_CAPACITY = 10;
	
	@SuppressWarnings("unchecked")
	public SortedArraySet() {
		elements = (T[]) new Comparable [INITIAL_CAPACITY];
		// TODO Auto-generated constructor stub
	}
	
	private void ensureCapacity () {
		if (size == elements.length) elements = Arrays.copyOf(elements, 2 * elements.length);
	}
	private boolean busquedaBinariaRec(T[] a,int inf,int sup, T x){
		if (inf <= sup && !isEmpty()){
			int medio = (inf + sup)/2; //Cuidado con los indices!
			if (x.equals(a[medio])) return true;
			else if (x.compareTo(a[medio])<0)
				return busquedaBinariaRec(a,inf,medio-1,x);
			else
				return busquedaBinariaRec(a,medio+1,sup,x);
		} else return false;
	}

	@Override
	public boolean isElem(T el) {
		// TODO Auto-generated method stub
		return busquedaBinariaRec(elements, 0, size-1 , el);
	}

	@Override
	public void insert(T el) {
		if(!isElem(el)) {
			if (isEmpty()) elements[size]=el;
			// TODO Auto-generated method stub´
			else {
				ensureCapacity();
				int i = 0;
				while (i<size && elements[i].compareTo(el)<0) {
					i++;
				}
				for (int pos = size; pos > i; pos--)elements[pos] = elements[pos - 1];
				elements[i] = el;
			}
			size++;

		}
	}

	@Override
	public void delete(T el) {
		// TODO Auto-generated method stub
		int i = 0;
		while (i<size && !elements[i].equals(el))i++;
		if(i<size) {
			for (int pos = i; pos < size; pos++)elements[pos] = elements[pos + 1];
			size--;
		}
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
	
	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return new ArraySetIterator();
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
