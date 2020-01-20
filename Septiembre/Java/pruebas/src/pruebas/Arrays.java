package pruebas;

public class Arrays {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Object [] a = new Object [10];
		if (a[5] == null)a[5]=5;
		for(int i = 0; i<5; i++)a[i]=i;
		System.out.println(a[5]);
		for(Object l : a)System.out.println(l);
		a[3]=null;
		for(Object l : a)System.out.println(l);
	}

}
