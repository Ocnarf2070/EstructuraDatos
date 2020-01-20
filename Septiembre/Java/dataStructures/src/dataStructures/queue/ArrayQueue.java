package dataStructures.queue;

public class ArrayQueue <T> implements Queue<T>{
	protected T [] elements;
	protected int first, last, size;
	
	private final int INITIAL_CAPACITY = 10;
	
	@SuppressWarnings("unchecked")
	public ArrayQueue() {
		elements = (T[]) new Object [INITIAL_CAPACITY];
		size = 0;
		first = 0;
		last = elements.length-1;
		// TODO Auto-generated constructor stub
	}
	
	private int advance (int i) {
		return (i+1) % elements.length;
	}
	
	@SuppressWarnings("unchecked")
	private void ensureCapacity () {
		if (size == elements.length) {
			T[] extension = (T[]) new Object [2 * elements.length];
			for (int i=0; i<size; i++) {
				extension[i] = elements[first];
				first = advance(first);
			}
			elements = extension;
			first = 0;
			last = size - 1 ;
		}
	}
	@Override
	public void enqueue(T elem) {
		ensureCapacity();
		last = advance(last);
		elements [last] = elem;
		size++;
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dequeue() {
		if (isEmpty()) throw new EmptyQueueException("firts on empty list");
		else {
			first = advance(first);
			size--;
		}
		// TODO Auto-generated method stub
		
	}

	@Override
	public T first() {
		if (isEmpty()) throw new EmptyQueueException("firts on empty list");
		else
		// TODO Auto-generated method stub
		return elements[first];
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return size == 0;
	}
	

}
