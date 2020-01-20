package dataStructures.searchTree;
import dataStructures.tuple.*;
public interface SearchTree<K extends Comparable<? super K>, V> {

public boolean isEmpty();
public int size();
public int height();
public void insert(K k, V v);
public V search (K k); 
public boolean isElem(K k);
public void delete(K k);

public Iterable<K> inOrden();
public Iterable<K> postOrder();
public Iterable<K> preOrder();
public Iterable<V> values();
public Iterable<Tuple2<K,V>> keysValues();

}
 

