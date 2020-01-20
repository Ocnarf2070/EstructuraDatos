package dataStructures.searchTree;

import dataStructures.tuple.Tuple2;

public class BST <K extends Comparable<? super K>, V> implements SearchTree<K, V> {
	
	private static class Tree <C,D> {
		private C key;
		private D value;
		private Tree <C,D> left,right;
		
		public Tree(C k, D v) {
			key=k;
			value=v;
			left=null;
			right=null;
			// TODO Auto-generated constructor stub
		}
	}
	
	protected Tree <K,V> root;
	private int size;
	
	public BST() {
		root=null;
		size = 0;
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return root == null;
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return size;
	}

	@Override
	public int height() {
		return heightRec(root);
	}	
	
	private static int heightRec(Tree<?,?> tree) {
		return tree==null ? 0 : 1 + Math.max(heightRec(tree.left), heightRec(tree.right));
	}	

	@Override
	public void insert(K k, V v) {
		root = insertRec(root,k,v);
		// TODO Auto-generated method stub
		
	}

	private Tree<K, V> insertRec(Tree<K, V> node, K key, V value) {
		if(node == null) {
			node = new Tree<> (key,value);
			size++;
		}else if (key.compareTo(node.key)==0) {
			node.value = value;
		}else if (key.compareTo(node.key)<0) node.left=insertRec(node.left,key,value);
		else node.right = insertRec(node.right, key, value);
		return node;
	}

	@Override
	public V search(K k) {
		// TODO Auto-generated method stub
		return searchRec(root,k);
	}
	
	private static <C extends Comparable<? super C>,D> D searchRec (Tree<C, D> tree, C key) {
		if(tree == null) return null;
		else if(key.compareTo(tree.key)==0) return tree.value;
		else if(key.compareTo(tree.key)<0) return searchRec(tree.left, key);
		else return searchRec(tree.right, key);
		
	}

	@Override
	public boolean isElem(K k) {
		// TODO Auto-generated method stub
		return search(k)!=null;
	}
	
	private static <K extends Comparable<? super K>,V> Tree<K, V> split(Tree<K, V> node, Tree<K, V> temp) {
		if (node.left == null) {
			temp.key = node.key;
			temp.value = node.value;
			return node.right; 
		} else {
			node.left = split(node.left, temp);
			return node;
		}
	}

	@Override
	public void delete(K key) {
		root = deleteRec(root, key);
	}

	private Tree<K, V> deleteRec(Tree<K, V> node, K key) {
		if (node == null);
		else if (key.compareTo(node.key) < 0) node.left = deleteRec(node.left, key);
		else if (key.compareTo(node.key) > 0) node.right = deleteRec(node.right, key);
		else {
			if (node.left == null) node = node.right;
			else if (node.right == null) node = node.left;
			else node.right = split(node.right, node);
			size--;
		}		
		return node;
	}

	@Override
	public Iterable<K> inOrden() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<K> postOrder() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<K> preOrder() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<V> values() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterable<Tuple2<K, V>> keysValues() {
		// TODO Auto-generated method stub
		return null;
	}
	
	private static String toStringRec(Tree<?, ?> tree) {
		return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
				+ tree.key + "," + tree.value + "," + toStringRec(tree.right) + ">";
	}
	
	/** 
	 * Returns representation of tree as a String.
	 */
  @Override public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);  
  	return className+"("+toStringRec(this.root)+")";
  }	

}
