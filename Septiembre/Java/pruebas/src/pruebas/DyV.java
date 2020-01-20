package pruebas;

public class DyV {
	public static int busquedaBinariaRec(int[] a,int inf,int sup, int x){
		if (inf <= sup && a.length != 0){
			int medio = (inf + sup)/2; //Cuidado con los indices!
			if (x == a[medio]) return medio;
			else if (x < a[medio])
				return busquedaBinariaRec(a,inf,medio-1,x);
			else
				return busquedaBinariaRec(a,medio,sup,x);
		} else return -1;
	}
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		int [] a = {0,1,2,3,4,5,6,7,8,9,10};
		System.out.println(busquedaBinariaRec(a, 0, a.length, 6));
	}

}
