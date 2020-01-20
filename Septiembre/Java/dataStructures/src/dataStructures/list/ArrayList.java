package dataStructures.list;

import java.util.Arrays;

public class ArrayList <T> implements List<T> {
	protected T[] elements;
	protected int size;
	
	private final int INITIAL_CAPACITY = 10;
	
	@SuppressWarnings("unchecked")
	public ArrayList() {
		elements = (T[]) new Object [INITIAL_CAPACITY];
		size = 0;
		// TODO Auto-generated constructor stub
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

	@Override
	public T get(int i) {
		// TODO Auto-generated method stub
		return elements[i];
	}
	
	private void ensureCapacity () {
		if (size == elements.length) elements = Arrays.copyOf(elements, 2 * elements.length);
	}
	
	private void validateIndex(int i) {
		if (i < 0 || i >= size()) throw new ListException("Invalid position " + i);
	} 

	@Override
	public void set(int i, T elem) {
		validateIndex(i);
		elements[i] = elem;
		// TODO Auto-generated method stub
		
	}

	@Override
	public void insert(int i, T elem) {
		ensureCapacity();
		if(i != size) {
			validateIndex(i);
			for (int pos = size; pos > i; pos--)elements[pos] = elements[pos - 1];
		}
		elements[i] = elem;
		size++;
		// TODO Auto-generated method stub
		
	}

	@Override
	public void remove(int i) {
		validateIndex(i);
		for (int pos = i; pos < size - 1; pos++) {
			elements[pos]= elements[pos+1];
		}
		size--;
		
		// TODO Auto-generated method stub
		
	}

}
