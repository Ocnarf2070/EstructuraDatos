package demos.heap;
import dataStructures.heap.MaxiphobicHeap;;
public class MaxophobicHeapMain {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		MaxiphobicHeap<Integer> heap = new MaxiphobicHeap<>();
		int [] array = {3,4,2,5,6};
		for (int x : array) {
			heap.insert(x);
			System.out.println(heap.toString());
		}
		
	}

}
