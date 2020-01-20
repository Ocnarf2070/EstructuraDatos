package dataStructure.dictionary;

import dataStructures.searchTree.AVL;
import dataStructures.searchTree.SearchTree;
import dataStructures.tuple.Tuple2;

public class AVLDictionary <K extends Comparable<? super K>,V> implements Dictionary<K,V>{
	
	SearchTree<K, V> tree;
	
	public AVLDictionary() {
		tree = new AVL<K,V>();
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return tree.isEmpty();
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return tree.size();
	}

	@Override
	public void insert(K k, V v) {
		// TODO Auto-generated method stub
		tree.insert(k, v);
		
	}

	@Override
	public V valueOf(K k) {
		// TODO Auto-generated method stub
		return tree.search(k);
	}

	@Override
	public boolean isDefinedAt(K k) {
		// TODO Auto-generated method stub
		return tree.search(k)!=null;
	}

	@Override
	public void delete(K k) {
		// TODO Auto-generated method stub
		tree.delete(k);
		
	}

	@Override
	public Iterable<K> keys() {
		// TODO Auto-generated method stub
		return tree.inOrden();
	}

	@Override
	public Iterable<V> values() {
		// TODO Auto-generated method stub
		return tree.values();
	}

	@Override
	public Iterable<Tuple2<K, V>> keysValues() {
		// TODO Auto-generated method stub
		return tree.keysValues();
	}
	
	public String toString() {
	    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);  
			String s = className+"(";
			if(!tree.isEmpty()) {
				for(Tuple2<K,V> t : tree.keysValues())
					s += t._1()+"->"+t._2()+",";
				s = s.substring(0, s.length()-1);
			}
			s += ")";
		  return s;
		}

}
