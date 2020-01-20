import dataStructures.doubleEndedQueue.*;
public class main {

	public static void main(String[] args) {
		LinkedDoubleEndedQueue<Integer> queue = new LinkedDoubleEndedQueue<>();
		for(int i=0;i<10;i++) {
			if(i%2==0)queue.addFirst(i);
			else queue.addLast(i);
			System.out.println(queue.toString());
		}
		queue.deleteFirst();
		System.out.println(queue.toString());
		queue.deleteLast();
		System.out.println(queue.toString());
		// TODO Auto-generated method stub

	}

}
