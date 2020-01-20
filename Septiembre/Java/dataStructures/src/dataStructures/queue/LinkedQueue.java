package dataStructures.queue;

public class LinkedQueue <T> implements Queue<T> {
	private static class Node <E>{
		private E elem;
		private Node <E> next;
		public Node (E x, Node<E> node) {
			elem = x;
			next = node;
		}
	}
	
	private Node<T> first,last;
	
	public LinkedQueue() {
		first = null;
		last = null;
		// TODO Auto-generated constructor stub
	}

	@Override
	public void enqueue(T elem) {
		Node <T> aux = new Node<T>(elem, null);
		if (first == null) {
			first = aux;
			last = aux;
		}else {
			last.next = aux;
			last = last.next;
		}
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dequeue() {
		// TODO Auto-generated method stub
		if (isEmpty()) throw new EmptyQueueException("dequeue on empty queue");
		else {
			first = first.next;
			if (first == null)last = null;
		}
	}

	@Override
	public T first() {
		if (isEmpty()) throw new EmptyQueueException("first on empty queue");
		// TODO Auto-generated method stub
		else return first.elem;
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return first == null;
	}

}
