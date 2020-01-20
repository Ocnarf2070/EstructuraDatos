package pruebas;

public class MainAr {
	public static void main(String[] args) throws InterruptedException {
		ArithmeticSeq seq = new ArithmeticSeq(1,9,2);
		for (int i : seq) {
			System.out.println(i);
		}
	}
	
}
