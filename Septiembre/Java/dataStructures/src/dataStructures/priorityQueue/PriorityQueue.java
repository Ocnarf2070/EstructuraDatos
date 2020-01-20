package dataStructures.priorityQueue;

public interface PriorityQueue<T> {
	void enqueue(T elem);
	void dequeue();
	T first();
	boolean isEmpty();
}