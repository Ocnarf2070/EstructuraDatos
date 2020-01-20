package dataStructures.stack;

import java.util.Arrays;

public class ArrayStack<T> implements Stack<T> {
	protected T[] elements;
	protected int nextFree;
	private final int INITIAL_CAPACITY = 10;
	@SuppressWarnings("unchecked")
	public ArrayStack() {
		elements = (T[]) new Object[INITIAL_CAPACITY];
		nextFree = 0;
	}
	private void ensureCapacity() {
		if (nextFree == elements.length)
			elements = Arrays.copyOf(elements, 2*elements.length);
	}
	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return nextFree==0;
	}
	@Override
	public void push(T elem) {
		ensureCapacity();
		elements[nextFree] = elem;
		nextFree++;
		// TODO Auto-generated method stub
		
	}
	@Override
	public T top() {
		if (isEmpty()) throw new EmptyStackException("top on empty stack");
		// TODO Auto-generated method stub
		else
		return elements[nextFree-1];
	}
	@Override
	public void pop() {
		if (isEmpty()) throw new EmptyStackException("top on empty stack");
		else
		nextFree--;
		// TODO Auto-generated method stub
		
	}
	@Override public String toString() {
	    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);  
			String s = className+"(";
			for(int i=nextFree-1; i>=0; i--) 
				s += elements[i] + (i>0 ? "," : "");
			s += ")";
			return s;			
		}
}
