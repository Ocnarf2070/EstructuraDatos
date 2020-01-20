package dataStructures.queue;
import dataStructures.list.ArrayList;

public class ListQueue <T> implements Queue<T>{
	protected ArrayList<T> elements;
	
	public ListQueue() {
		elements = new ArrayList<>();
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public void enqueue(T elem) {
		elements.insert(elements.size(), elem);
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dequeue() {
		if (isEmpty()) throw new EmptyQueueException("dequeue on empty queue");
		else elements.remove(0);
		// TODO Auto-generated method stub
		
	}

	@Override
	public T first() {
		if (isEmpty()) throw new EmptyQueueException("first on empty queue");
		else 
		// TODO Auto-generated method stub
		return elements.get(0);
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return elements.isEmpty();
	}

}
