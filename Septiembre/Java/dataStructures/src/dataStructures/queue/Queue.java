package dataStructures.queue;

public interface Queue<T> {
	void enqueue(T elem);
	void dequeue();
	T first();
	boolean isEmpty();
}

