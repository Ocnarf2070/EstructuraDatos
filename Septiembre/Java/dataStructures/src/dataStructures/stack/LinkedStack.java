package dataStructures.stack;

public class LinkedStack<T> implements Stack<T> {
	static private class Node<E> {
		E elem;
		Node<E> next;
		public Node(E x, Node<E> node) {
			elem = x;
			next = node;
		}
	}
	private Node<T> top;

	public LinkedStack() {
		top = null;
	}

	public boolean isEmpty() {
		return top == null;
	}

	@Override
	public void push(T x) {
		Node <T> node = new Node<>(x, top);
		top = node;
		// TODO Auto-generated method stub

	}

	@Override
	public T top() {
		if(isEmpty()) throw new EmptyStackException("top on empty stack");
		// TODO Auto-generated method stub
		return top.elem;
	}

	@Override
	public void pop() {
		if(isEmpty()) throw new EmptyStackException("top on empty stack");
		top = top.next;
		// TODO Auto-generated method stub

	}

}
