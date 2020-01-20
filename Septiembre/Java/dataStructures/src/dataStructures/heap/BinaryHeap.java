package dataStructures.heap;

import java.util.Arrays;

public class BinaryHeap <T extends Comparable<? super T>> implements Heap<T>{
	private T elements[];
	private int size;
	private static int INITIAL_CAPACITY = 128;
	
	public BinaryHeap() {
		elements = (T[]) new Comparable[INITIAL_CAPACITY];
		// TODO Auto-generated constructor stub
	}
	
	private void ensureCapacity () {
		if(size == elements.length)elements = Arrays.copyOf(elements, 2*elements.length);
	}
	
	private static int ROOT_INDEX = 0;
	
	private static boolean isRoot(int idx) {
		return ROOT_INDEX==idx;
	}
	
	private static int parent (int idx) {
		return (idx-1)/2;
	}
	
	private static int leftChild (int idx) {
		return 2*idx+1;
	}
	
	private static int rightChild (int idx) {
		return leftChild(idx)+1;
	}
	
	private boolean isNode (int idx) {
		return idx < size;
	}
	
	private boolean hasLeftChild (int idx) {
		return leftChild(idx)<size;
	}
	
	private boolean isLeaf (int idx) {
		return !hasLeftChild(idx);
	}
	
	private void heapifyUp (int idx) {
		while(!isRoot(idx)) {
			int idxParent = parent(idx);
			
			if(lessThan(idx, idxParent)) {
				swap(idx, idxParent);
				idx = idxParent;
			}else break;
		}
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

	private boolean lessThan (int idx1, int idx2) {
		return elements[idx1].compareTo(elements[idx2])<0;
	}
	
	private void swap (int idx1, int idx2){
		T aux = elements [idx1];
		elements [idx1] = elements [idx2];
		elements [idx2] = aux;
	}
	
	@Override
	public void insert(T x) {
		// TODO Auto-generated method stub
		ensureCapacity();
		elements [size] = x;
		heapifyUp(size);
		size++;
	}

	@Override
	public T minElem() {
		if(isEmpty()) throw new EmptyHeapException("minElem on empty heap");
		// TODO Auto-generated method stub
		return elements[ROOT_INDEX];
	}
		
	private void heapifyDown() {
		int idx = ROOT_INDEX;
		while(!isLeaf(idx)) {
			int idxChild = leftChild(idx);
			int idxRightChild = rightChild(idx);
			if(isNode(idxRightChild)&&lessThan(idxRightChild, idxChild))
				idxChild = idxRightChild;
			if(lessThan(idxChild, idx)) {
				swap(idxChild, idx);
				idx = idxChild;
			}else break;
		}
	}
	
	@Override
	public void delMin() {
		if (isEmpty()) throw new EmptyHeapException("delMin on empty heap");
		// TODO Auto-generated method stub
		else {
			elements[ROOT_INDEX]=elements[size-1];
			size--;
			heapifyDown();
		}
		
	}

}
