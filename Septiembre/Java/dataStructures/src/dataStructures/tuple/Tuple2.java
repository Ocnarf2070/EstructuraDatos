package dataStructures.tuple;

public class Tuple2 <A,B>{
	A elem1;
	B elem2;
	public Tuple2(A a,B b) {
		elem1 = a;
		elem2 = b;
		// TODO Auto-generated constructor stub
	}
	public A _1() {
		return elem1;
	}
	
	public B _2() {
		return elem2;
	}
	
	public String toString() {
		return "Tuple2("+elem1+","+elem2+")";
	}
}
