package dataStructure.dictionary;

import dataStructures.tuple.Tuple2;

public interface Dictionary<K, V> {
	boolean isEmpty();
	int size();
	void insert(K k, V v);
	V valueOf(K k);
	boolean isDefinedAt(K k);
	void delete(K k);
	Iterable<K> keys();
	Iterable<V> values();
	Iterable<Tuple2<K,V>> keysValues();
}