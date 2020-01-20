package dataStructures.priorityQueue;

public class LinkedNOPriorityQueue <T extends Comparable<? super T>> implements PriorityQueue<T> {

	private static class Node <E>{
		E elem;
		private Node<E> next;
		public Node(E x, Node <E> node) {
			elem=x;
			next = node;
		}
	}
	
	private Node <T> first;
	private int size;
	
	public LinkedNOPriorityQueue() {
		first = null;
		size = 0;
		// TODO Auto-generated constructor stub
	}

	@Override
	public void enqueue(T elem) {
		Node <T> aux = new Node<>(elem, first);
		first = aux;
		// TODO Auto-generated method stub
		
	}
	private T minimo (){
		T minimo = first.elem;
		Node <T> current = first;
		while(current!=null) {
			if(current.elem.compareTo(minimo)<0)minimo = current.elem;
			current = current.next;
		}
		return minimo;
	}
	@Override
	public void dequeue() {
		T elim = minimo();
		Node <T> current = first, previous = null;
		while(current!=null&&!current.elem.equals(elim)) {
			previous = current;
			current = current.next;
		}
		if(previous == null)first = first.next;
		else previous.next=current.next;
		// TODO Auto-generated method stub
		
	}

	@Override
	public T first() {
		// TODO Auto-generated method stub
		return minimo();
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return first == null;
	}
	
	public String toString() {
		String text = "LinkedNOPriorityQueue (";
		Node<T> p;
		for (p = first; p.next != null; p = p.next) {
			text +=  p.elem +"->";
		}
		return text + p.elem + ")";
	}
}
