import java.util.Random;

import dataStructures.hashTable.HashTable;
import dataStructures.hashTable.LinearProbingHashTable;
import dataStructures.hashTable.SeparateChainingHashTable;
import dataStructures.tuple.Tuple2;

public class MainHash {
	
	final static int maxValue = 10000;
	final static int numElems = 1;
	
	//static void initTables(Random rnd, HashTable<Integer, String> t1, HashTable<Integer, String> t2) {
	static void initTables(int [] a,String [] b, HashTable<Integer, String> t1, HashTable<Integer, String> t2) {
		
		for(int i=0; i<numElems; i++) {
			//Integer n = rnd.nextInt(maxValue);
			t1.insert(a[i], b[i].toString());
			t2.insert(a[i], b[i].toString());			
		}			
	}
	
	
	static void remove(Random rnd, HashTable<Integer, String> t1, HashTable<Integer, String> t2) {
		for(int i=0; i<numElems/2; i++) {
			Integer n = rnd.nextInt(maxValue);
			t1.delete(n);
			t2.delete(n);			
		}			
	}
	
	
	static void testEq(HashTable<Integer, String> t1, HashTable<Integer, String> t2) {
		for(int k=0; k<maxValue; k++) {
			String s1 = t1.search(k);
			String s2 = t2.search(k);
			if(s1==null) {
				if(s2!=null) {
					System.out.printf("\nERROR on search for %d",k);
					System.exit(1);
				}
				
			} else if(!s1.equals(s2)) {
				System.out.printf("\nERROR on search for %d: %s %s",k,s1,s2);
				System.exit(1);
			}			
		}
		System.out.println("OK");
	}
	
	static void oneTest(int seed) {
		//Random rnd = new Random(seed);
		int [] a = {1,25,3,15,369,21,24,256,2152,323,6589,125,203,8542,12,3,5,6};
		String [] b = new String [numElems];
		
		for (int i = 0; i<b.length;i++) {
			if(i%2==0) b[i] = "a";
			else b[i]="b";
		}
		
		HashTable<Integer,String> scHashTable = new SeparateChainingHashTable<Integer,String>(97,0.5);
		HashTable<Integer,String> lpHashTable = new LinearProbingHashTable<Integer,String>(97,0.5);

		initTables(a,b,scHashTable,lpHashTable);
		System.out.println("TEST for insert and search");
		System.out.println(scHashTable.toString());
		//System.out.println(lpHashTable.toString());
		System.out.println(lpHashTable.isEmpty());
		//for(Tuple2<Integer, String> tuple : lpHashTable.keysValues())System.out.print(tuple._1()+"->"+tuple._2()+",");
		System.out.println();
		int m = 16, n = 1;
		System.out.println("Is "+ n +" in the table? sc->"+(scHashTable.isElem(n)?"true":"false")+" lp->"+(lpHashTable.isElem(n)?"true":"false"));
		System.out.println("Is "+m+" in the table?sc->"+(scHashTable.isElem(m)?"true":"false")+" lp->"+(lpHashTable.isElem(m)?"true":"false"));
		
		
	}
	
	public static void main(String[] args) {		
		//for(int seed=0; seed<10; seed++)
			oneTest(0);
	}	
}
