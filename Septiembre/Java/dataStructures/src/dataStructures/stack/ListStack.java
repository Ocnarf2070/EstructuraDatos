package dataStructures.stack;
import dataStructures.list.ArrayList;
import dataStructures.queue.EmptyQueueException;;

public class ListStack <T> implements Stack<T>{
	ArrayList<T> elements;
	
	public ListStack() {
		elements = new ArrayList<>();
		// TODO Auto-generated constructor stub
	}
	
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return elements.isEmpty();
	}

	@Override
	public void push(T x) {
		elements.insert(0, x);
		// TODO Auto-generated method stub
		
	}

	@Override
	public T top() {
		if (isEmpty()) throw new EmptyQueueException("top on empty stack");
		else 
		// TODO Auto-generated method stub
		return elements.get(0);
	}

	@Override
	public void pop() {
		if (isEmpty()) throw new EmptyQueueException("pop on empty stack");
		else  elements.remove(0);
		// TODO Auto-generated method stub
		
	}

}
