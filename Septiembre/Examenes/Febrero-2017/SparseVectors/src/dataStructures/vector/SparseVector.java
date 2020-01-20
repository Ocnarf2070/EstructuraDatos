/******************************************************************************
 * Student's name:
 * Student's group:
 * Data Structures. Grado en Informática. UMA.
******************************************************************************/

package dataStructures.vector;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class SparseVector<T> implements Iterable<T> {

    protected interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    protected static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
            // TODO
            return elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            // TODO
        	if (sz == 1) return new Unif<T>(x);
           	else if (sz == 2){
        		if (i%2 == 0)return new Node<T>(new Unif<T> (x), new Unif<T>(elem)).simplify();
        		else return new Node<T>(new Unif<T> (elem), new Unif<T>(x)).simplify();
        	}else return new Node<T>(new Unif<T>(elem), new Unif<T>(elem)).set(sz, i,x);
        }

        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    protected static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
            // TODO
        	int media = sz/2;
        	if (media>i) return left.get(media, i);
        	else return right.get(media, i-media);
            
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            // TODO
        	
        	int media = sz/2;
        	Tree<T> tree;
        	if (media>i) 
        		tree = new Node<T>(left = left.set(media, i,x),right).simplify();
        	
        	else 
        		tree = new Node<T>(left,right = right.set(media, i-media,x)).simplify();
        	
        	return tree;
        	
        }

        protected Tree<T> simplify() {
            // TODO
        	if (isUnif(left) && isUnif(right) && left.get(0, 0).equals(right.get(0, 0)))  return new Unif<T>(right.get(0, 0));
        	else return this;
        }
        private boolean isUnif(Tree<T> tree) {
        	return tree instanceof Unif<?>;
        }
        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
    	if (n<0) throw new VectorException("negative vector not supported");
    	else {
    		size = (int) Math.pow(2,n);
    		root = new Unif<T>(elem);
    	}
        // TODO
    }

    public int size() {
        // TODO
        return size;
    }

    public T get(int i) {
        // TODO
    	if (i<0 && i>=size) throw new VectorException("index not valide");
    	return root.get(size, i);
        
    }

    public void set(int i, T x) {
    	if (i<0 && i>=size) throw new VectorException("index not valide");
    	root = root.set(size, i, x);
        // TODO
    }

    @Override
    public Iterator<T> iterator() {
        // TODO
    	
        return new MyIterator();
    }

    private class MyIterator implements Iterator<T> {
    	int i = 0;
		@Override
		public boolean hasNext() {
			// TODO Auto-generated method stub
			return i<size;
		}

		@Override
		public T next() {
			if (!hasNext()) throw new NoSuchElementException();
			// TODO Auto-generated method stub
			T next = root.get(size, i);
			i++;
			return next;
		}
		// TODO Auto-generated method stub
	}

	@Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
	
	public SparseVector<T> clone(){
		Iterator<T> it = this.iterator(); 
		SparseVector<T> clone = new SparseVector<>((int)(Math.log(size)/Math.log(2)), it.next());
		int i = 1;
		while(it.hasNext()) {
			clone.set(i, it.next());
			i++;
		}
		return clone;
	}
	
	public int depthOf(int i) {
		SparseVector<T> aux = this.clone();
		Tree<T> t = aux.root;
		int n = 0,sz=size;
		while(!(t instanceof Unif<?>)) {
			sz/=2;
			Node<T> u = (Node <T>) t;
        	if (sz>i) t = u.left;
        	else {
        		t = u.right;
        		i -= sz;
        	}
        	
        	n++;
		}
		return n;
	}
}