package dataStructures.set;

public interface Set<T> extends Iterable<T> {
	 boolean isElem(T el);
	 void insert(T el);
	 void delete(T el);
	 boolean isEmpty();
	 int size();
}
