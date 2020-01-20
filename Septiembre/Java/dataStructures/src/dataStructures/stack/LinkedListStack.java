package dataStructures.stack;

import java.util.*;

public class LinkedListStack <T> implements Stack <T> {
	protected LinkedList <T> elements;
	
	public LinkedListStack() {
		elements = new LinkedList<>();
		// TODO Auto-generated constructor stub
	}
	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return elements.isEmpty();
	}

	@Override
	public void push(T x) {
		if (isEmpty()) throw new EmptyStackException("top on esmpty stack");
		elements.addFirst(x);
		// TODO Auto-generated method stub
		
	}

	@Override
	public T top() {
		if (isEmpty()) throw new EmptyStackException("pop on esmpty stack");
		// TODO Auto-generated method stub
		return elements.getFirst();
	}

	@Override
	public void pop() {
		elements.removeFirst();
		// TODO Auto-generated method stub
		
	}

}
