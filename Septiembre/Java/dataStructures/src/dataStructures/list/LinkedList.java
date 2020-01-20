package dataStructures.list;

public class LinkedList <T> implements List<T>{

	private static class Node <E> {
		E elem;
		private Node<E> next;
		public Node(E x, Node <E> node) {
			elem=x;
			next = node;
		}
	}
	private Node <T> first, last;
	private int size;

	public LinkedList() {
		first = null;
		last = null;
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

	private Node<T> atIndex(int i) {
		Node<T> aux = first;
		for (int pos = 0; pos < i; pos++)
			aux = aux.next;

		return aux;
	}

	private void validateIndex(int i) {
		if (i < 0 || i >= size())
			throw new ListException("Invalid position " + i);
	} 
	@Override
	public T get(int i) {
		validateIndex(i);
		return atIndex(i).elem;
	}

	@Override
	public void set(int i, T elem) {
		validateIndex(i);
		atIndex(i).elem = elem;

	}

	@Override
	public void insert(int i, T elem) {
		if (i == size) {
			Node<T> node = new Node<>(elem, null);
			if(size == 0)first = node;
			else last.next = node;
			last=node;
		}else if (i==0) first = new Node<>(elem,first);
		else {
			validateIndex(i);
			Node<T> prev = atIndex(i-1);
			prev.next = new Node<>(elem,prev.next);
		}
		size++;
		// TODO Auto-generated method stub

	}

	@Override
	public void remove(int i) {
		validateIndex(i);
		if(i == 0) {
			first = first.next;
			if(first == null)last = null;
		}else {
			Node<T> prev = atIndex(i-1);
			prev.next=prev.next.next;
			if(i==size-1)last = prev;
		}
		size--;
		// TODO Auto-generated method stub

	}


}
