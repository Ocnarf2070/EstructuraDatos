package dataStructures.heap;

public class WBLeftistHeap <T extends Comparable<? super T>> implements Heap<T>{

	protected static class Tree<E>{
		E elem;
		int weight;
		Tree<E> left;
		Tree<E> right;
	}
	
	protected Tree <T> root;
	
	public WBLeftistHeap() {
		root = null;
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return root == null;
	}
	
	private static <T> int weight(Tree<T> t) {
		return t == null ? 0 : t.weight;
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return isEmpty() ? 0 : root.weight;
	}

/*	private static <T> Tree<T> node (T x, Tree<T> h1, Tree<T> h2){
		int w1 = weight(h1),w2=weight(h2);
		Tree<T> tree = new Tree<>();
		tree.elem = x;
		tree.weight = w1+w2+1;
		if(w1 >= w2) {
			tree.left = h1;
			tree.right = h2;
		}else {
			tree.left = h2;
			tree.right = h1;
		}
		return tree;
	}*/
	
	private static <T extends Comparable<? super T>> Tree<T> merge (Tree<T> h1, Tree<T> h2){
		if(h1 == null)return h2;
		if(h2 == null)return h1;
		
		if (h2.elem.compareTo(h1.elem) < 0) {
			Tree<T> tmp = h1;
			h1 = h2;
			h2 = tmp;
		}

		h1.right = merge(h1.right, h2);
		
		int wL = weight(h1.left),wR = weight(h1.right);
		h1.weight = wL + wR + 1; // set new weight

		// put always heavier heap on left side
		if (wL < wR) {
			Tree<T> aux = h1.left;
			h1.left = h1.right;
			h1.right = aux;
		}
		
		return h1;

	}
	@Override
	public void insert(T x) {
		// TODO Auto-generated method stub
		Tree<T> tree = new Tree<>();
		tree.elem = x;
		tree.weight = 1;
		tree.left = null;
		tree.right = null;
		root =  merge(root, tree);		
	}

	@Override
	public T minElem() {
		if(isEmpty())throw new EmptyHeapException("minElem on empty heap");
		// TODO Auto-generated method stub
		else return root.elem;
	}

	@Override
	public void delMin() {
		if(isEmpty())throw new EmptyHeapException("delMin on empty heap");
		// TODO Auto-generated method stub
		else root = merge(root.left, root.right);
	}

}
