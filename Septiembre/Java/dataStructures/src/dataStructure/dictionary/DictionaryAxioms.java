package dataStructure.dictionary;

import static org.junit.Assert.*;
import java.util.Random;

import org.junit.*;

import Repeat.Repeat;
import Repeat.RepeatRule;
import dataStructures.tuple.Tuple2;

public class DictionaryAxioms {
	Dictionary<Integer, Character> empty = new AVLDictionary<>();
	Dictionary<Integer, Character> dict = null;
	Random rnd = new Random();
	@Before
	public void init() {
		dict = new AVLDictionary<>();
		int limit = Math.abs(rnd.nextInt(100));
		for(int i = 0; i<limit;i++) {
			dict.insert(rnd.nextInt(), (char) rnd.nextInt(255));
		}
	}
	
	@Test
	public void ax1() {
		assertTrue(empty.isEmpty());
	}
	
	@Test
	public void ax2() {
		dict.insert(rnd.nextInt(), (char) rnd.nextInt(255));
		assertFalse(dict.isEmpty());
	}
	
	@Test
	public void ax3() {
		assertFalse(empty.isDefinedAt(rnd.nextInt()));
	}
	
	@Test
	public void ax4() {
		int k1 = rnd.nextInt(),k2=rnd.nextInt();
		Dictionary<Integer, Character> aux = new AVLDictionary<>();
		for(Tuple2<Integer,Character> tuple : dict.keysValues()) {
			aux.insert(tuple._1(), tuple._2());
		}
		aux.insert(k2, (char) rnd.nextInt(255));
		assertTrue(aux.isDefinedAt(k1) == (k1 == k2) || dict.isDefinedAt(k1));
	}
	
	@Test
	public void ax5() {
		assertNull(empty.valueOf(rnd.nextInt()));
	}
	
	@Test
	public void ax6() {
		int k1 = rnd.nextInt(),k2=k1;
		char v2;
		dict.insert(k2, v2 = (char)rnd.nextInt(255));
		assertTrue(dict.valueOf(k1).equals(v2));
	}

	@Test
	public void ax7() {
		int k1 = rnd.nextInt(),k2 = rnd.nextInt();
		Dictionary<Integer, Character> aux = new AVLDictionary<>();
		for(Tuple2<Integer,Character> tuple : dict.keysValues()) {
			aux.insert(tuple._1(), tuple._2());
		}
		aux.insert(k2, (char)rnd.nextInt(255));
		assertEquals(aux.valueOf(k1),dict.valueOf(k1));
	}
	
	@Test
	public void ax8() {
		assertEquals(empty.size(), 0);		
	}
	
	@Test
	public void ax9() {
		int size = dict.size();
		int num = dict.keys().iterator().next();
		dict.insert(num , (char) rnd.nextInt(255));
		assertEquals(size, dict.size());
	}
	
	@Test
	public void ax10() {
		int size = dict.size();
		dict.insert(rnd.nextInt(), (char)rnd.nextInt(255));
		assertEquals(size+1, dict.size());
	}
	
	@Test
	public void ax11() {
		empty.delete(rnd.nextInt());
		assertTrue(empty.isEmpty());
	}
	
	@Test
	public void ax12() {
		int k1 = rnd.nextInt(),k2=k1;
		Dictionary<Integer, Character> aux = new AVLDictionary<>();
		for(Tuple2<Integer,Character> tuple : dict.keysValues()) {
			aux.insert(tuple._1(), tuple._2());
		}
		aux.insert(k2, (char)rnd.nextInt(255));
		aux.delete(k1);dict.delete(k1);
		assertEquals(aux.toString(),dict.toString());
		
	}
	
	@Test
	public void ax13() {
		int k1 = rnd.nextInt(), k2 = rnd.nextInt();
		char v2 = (char) rnd.nextInt(255);
		Dictionary<Integer, Character> aux = new AVLDictionary<>();
		for(Tuple2<Integer,Character> tuple : dict.keysValues()) {
			aux.insert(tuple._1(), tuple._2());
		}
		aux.insert(k2, v2);aux.delete(k1);
		dict.delete(k1);dict.insert(k2, v2);
		assertEquals(aux.toString(), dict.toString());
	}
	
	@Rule
    public RepeatRule repeatRule = new RepeatRule();
	@Test
	@Repeat(100)
	public void dictionaryAxioms() {
		ax1();
		ax2();
		ax3();
		ax4();
		ax5();
		ax6();
		ax7();
		ax8();
		ax9();
		ax10();
		ax11();
		ax12();
		ax13();
	}

	@After
	public void fin () {
		dict = null;
	}

	
}
