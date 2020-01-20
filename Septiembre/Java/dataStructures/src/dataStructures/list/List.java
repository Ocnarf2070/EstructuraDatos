package dataStructures.list;

public interface List <T> {
	boolean isEmpty();
	int size();
	T get(int i);
	void set(int i, T elem);
	void insert(int i, T elem);
	void remove(int i);
}
