
/**
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name:
 * Student's group:
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
    	Dictionary<Character, Integer> dict = new AVLDictionary<>();
    	char [] str = s.toCharArray();
    	for(char c : str){
    		if(dict.isEmpty()||!dict.isDefinedAt(c)){
    			int i=0;
    			for(char cs:str)if(c==cs)i++;
    			dict.insert(c, i);
    		}
    	}
    return dict;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	//to do 
    	PriorityQueue<WLeafTree<Character>> queue = new BinaryHeapPriorityQueue<>();
    	Dictionary<Character, Integer> dict = weights(s);
    	for (Tuple2<Character,Integer> tuple:dict.keysValues()){
    		queue.enqueue(new WLeafTree<Character>(tuple._1(), tuple._2()));
    	}
    	return queue;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
    	PriorityQueue<WLeafTree<Character>> queue = huffmanLeaves(s);
    	while(!queue.isEmpty()){
    		WLeafTree<Character> l = queue.first();
    		queue.dequeue();
    		if(queue.isEmpty()){
    			return l;
    		}
    		WLeafTree<Character> r = queue.first();
    		queue.dequeue();
    		if(r==null)throw new HuffmanException("No se puede hacer el árbol");
    		WLeafTree<Character> tree= new WLeafTree<>(l, r);
    		queue.enqueue(tree);
    		}
		return queue.first();
    	}

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        //to do 
    	return null;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        //to do 
    	return null;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        //to do 
    	return null;
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        //to do 
    	return null;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        //to do 
    	return null;
    }
}
