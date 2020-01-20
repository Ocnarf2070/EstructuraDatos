package dataStructures.priorityQueue;

import static org.junit.Assert.*;

import java.util.Random;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class AxiomsPriorityQueue {
	PriorityQueue<Integer> queue = null;
	Random rnd = new Random();
	@Before
	public void init() {
		queue = new LinkedNOPriorityQueue<>();
	}
	@Test
	public void ax1() {
		assertTrue(queue.isEmpty());;
	}
	
	@Test
	public void ax2() {
		queue.enqueue(rnd.nextInt());
		assertFalse(queue.isEmpty());
	}
	
	@Test
	public void ax3() {
		int elem = rnd.nextInt();
		queue.enqueue(elem);
		assertTrue(queue.first().equals(elem));
	}
	
	@Test
	public void ax4() {
		PriorityQueue<Integer> aux = new LinkedNOPriorityQueue<>();
		int x = rnd.nextInt(),y=rnd.nextInt();
		queue.enqueue(x);
		queue.enqueue(y);
		aux.enqueue(Math.min(x,y));
		assertTrue(queue.first().equals(aux.first()));
	}
	
	@Test
	public void ax5() {
		queue.enqueue(rnd.nextInt());
		queue.dequeue();
		assertTrue(queue.isEmpty());
	}
	
	@Test
	public void ax6() {
		PriorityQueue<Integer> aux = new LinkedNOPriorityQueue<>();
		int x = rnd.nextInt(),y=rnd.nextInt();
		queue.enqueue(x);queue.enqueue(y);queue.dequeue();
		aux.enqueue(Math.min(x, y));aux.dequeue();aux.enqueue(Math.max(x, y));
		assertEquals(queue.toString(), aux.toString());
	}
	@After
	public void finish() {
		queue = null;
	}

}
