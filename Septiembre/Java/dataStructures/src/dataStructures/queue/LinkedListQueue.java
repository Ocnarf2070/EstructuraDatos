package dataStructures.queue;

import java.util.*;

public class LinkedListQueue<T> implements Queue<T> {
	protected LinkedList<T> elements;
	
	public LinkedListQueue() {
		elements = new LinkedList<>();
		// TODO Auto-generated constructor stub
	}

	@Override
	public void enqueue(T elem) {
		elements.addLast(elem);
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dequeue() {
		if (isEmpty()) throw new EmptyQueueException("dequeue on empty queue");
		else
		elements.removeFirst();
		// TODO Auto-generated method stub
		
	}

	@Override
	public T first() {
		if (isEmpty()) throw new EmptyQueueException("first on empty queue");
		else
		// TODO Auto-generated method stub
		return elements.getFirst();
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return elements.isEmpty();
	}
}