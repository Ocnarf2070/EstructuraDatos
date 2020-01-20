package pruebas;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class ArithmeticSeq implements Iterable<Integer> {
	private int from, to, step;
	
	public ArithmeticSeq(int n, int m) {
		from = n;
		to = m;
		step = 1;
		// TODO Auto-generated constructor stub
	}
	
	public ArithmeticSeq(int n, int m, int s) {
		from = n;
		to = m;
		if(s>0)step = s;
		else throw new ExceptionInInitializerError("Los pasos deben ser positivos");
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public Iterator<Integer> iterator() {
		// TODO Auto-generated method stub
		return new ArithmeticIter();
	}
	
	private class ArithmeticIter implements Iterator<Integer>{
		int current;
		
		public ArithmeticIter() {
			current=from;
			// TODO Auto-generated constructor stub
		}
		@Override
		public boolean hasNext() {
			// TODO Auto-generated method stub current <= to : current >= to
			return (from<to) ? current <= to : current >= to;
		}

		@Override
		public Integer next() {
			if (!hasNext()) throw new NoSuchElementException();
			// TODO Auto-generated method stub
			else {
				int x = current;
				if(from<to)current += step;
				else current -= step; 
				return x;
			}
		}
		
	}

}
