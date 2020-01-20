package dataStructures.priorityQueue;

public class LinkedPriorityQueue <T extends Comparable<? super T>> implements PriorityQueue<T> {
	
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
	
	public LinkedPriorityQueue() {
		first = null;
		size = 0;
		// TODO Auto-generated constructor stub
	}
	
	public void enqueue(T elem) {
		Node <T> aux = new Node<>(elem, null);
		if(first != null) {
			Node <T> prev=null,cur=first;
			while(cur != null&&cur.elem.compareTo(elem)<0) {
				prev=cur;
				cur=cur.next;
			}
			if(prev == null) {
				aux.next = first;
				first = aux;
			}else {
				aux.next = cur;
				prev.next = aux;
			}
		} else first = aux;
		size++;

	}

	@Override
	public void dequeue() {
		first = first.next;
		size--;
		// TODO Auto-generated method stub
		
	}

	@Override
	public T first() {
		// TODO Auto-generated method stub
		return first.elem;
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return first == null;
	}
	
	public String toString() {
		String text = "LinkedPriorityQueue (";
		Node<T> p;
		for (p = first; p.next != null; p = p.next) {
			text +=  p.elem +"->";
		}
		return text + p.elem + ")";
	}
}

