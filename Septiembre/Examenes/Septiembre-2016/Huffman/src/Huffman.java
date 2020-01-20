
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
    	for(int i=0;i<s.length();i++) {
    		char c = s.charAt(i);
    		dict.insert(c, cont(c,s));
    	}
    	
    	//to do 
        return dict;
    }
    

    private static Integer cont(char c, String s) {
		// TODO Auto-generated method stub
    	int num=0;
    	for(int i=0;i<s.length();i++) {
    		if (c == s.charAt(i))num++;
    	}
		return num;
	}


	// Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	//to do 
    	Dictionary<Character, Integer> dict = weights(s);
    	PriorityQueue<WLeafTree<Character>> pqueue = new BinaryHeapPriorityQueue<>();
    	for(Tuple2<Character, Integer> tuple : dict.keysValues()) {
    		WLeafTree<Character> wleaf = new WLeafTree<Character>(tuple._1(), tuple._2());
    		pqueue.enqueue(wleaf);
    	}
    	return pqueue;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
    	//to do
    	PriorityQueue<WLeafTree<Character>> pqueue = huffmanLeaves(s);
    	int num = 0;
    	WLeafTree<Character>elem1;
    	while (true) {
    		elem1 = pqueue.first();
    		pqueue.dequeue();
    		num++;
    		if(pqueue.isEmpty())break;
    		WLeafTree<Character>elem2 = pqueue.first();
    		pqueue.dequeue();
    		WLeafTree<Character> node = new WLeafTree<>(elem1, elem2);
    		pqueue.enqueue(node);
    	}
    	if(num<=1)throw new HuffmanException("HuffmanTree: the string must have at least two different symbols");
    	else return elem1;
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        //to do 
    	Dictionary<Character, List<Integer>> d3 = new AVLDictionary<>();
    	for (Tuple2<Character, List<Integer>> tuple : d1.keysValues()) {
    		d3.insert(tuple._1(), tuple._2());
    	}
    	for (Tuple2<Character, List<Integer>> tuple : d2.keysValues()) {
    		if(d3.isDefinedAt(tuple._1())) throw new HuffmanException("joinDics: Dictionaries must be disjointed");
    		else d3.insert(tuple._1(), tuple._2());
    	}
    	return d3;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        //to do 
    	for (Tuple2<Character, List<Integer>> tuple : d.keysValues()) {
    		List<Integer> list = tuple._2();
    		list.prepend(i);
    		d.insert(tuple._1(),list);
    	}
    	return d;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        //to do 
    	
    	if(ht.isLeaf()) {
    		Dictionary<Character, List<Integer>> huffCode = new AVLDictionary<>();
    		List<Integer> list= new LinkedList<>();
    		huffCode.insert(ht.elem(), list);
    		return huffCode;
    	}else {
    		Dictionary<Character, List<Integer>> huffCode = joinDics(prefixWith(0,huffmanCode(ht.leftChild())),prefixWith(1,huffmanCode(ht.rightChild())));
    		return huffCode;
    	}
    	
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        //to do 
    	List <Integer> encode = new LinkedList<>();
    	for(int i = 0; i<s.length();i++) {
    		for(Integer l : hc.valueOf(s.charAt(i))) {
    			encode.append(l);
    		}
    	}
    	return encode;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        //to do 
    	String pal="";
    	while(!bits.isEmpty()) {
    		pal+=takeSymbol(bits,ht);
    	}
    	return pal;
    }


	private static Character takeSymbol(List<Integer> bits, WLeafTree<Character> ht) {
		// TODO Auto-generated method stub
		if(ht.isLeaf())return ht.elem();
		else {
			if(bits.isEmpty()) throw new HuffmanException("Codex not supported");
			else if(bits.get(0)==0) {
				bits.remove(0);
				return takeSymbol(bits, ht.leftChild());
			}
			else {
				bits.remove(0);
				return takeSymbol(bits,ht.rightChild());
			}
		}
	}
}