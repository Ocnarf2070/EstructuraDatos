import dataStructures.set.*;
public class MainSet {

	public static void main(String[] args) {
		//SortedLinkedSet<Integer> set = new SortedLinkedSet<>();
		//LinkedSet<Integer> set = new LinkedSet<>();
		//LinkedListSet<Integer> set = new LinkedListSet<>();
		//ArraySet<Integer> set = new ArraySet<>();
		SortedArraySet<Integer> set = new SortedArraySet<>();
		for(int i=0;i<=3;i++)set.insert(i);
		System.out.println(set.toString());
		for(int i=8;i<=10;i++)set.insert(i);
		System.out.println(set.toString());
		for(int i=4;i<=7;i++)set.insert(i);
		System.out.println(set.toString());
		System.out.println("size of the set: " + set.size());
		System.out.println("Is the set empty? "+ (set.isEmpty()?"true":"false"));
		int num=7;
		System.out.println("Is "+num+" in the set? "+(set.isElem(num)?"true":"false"));
		set.delete(5);
		set.delete(8);
		set.delete(11);
		System.out.println(set.toString());
		set.insert(num);
		System.out.println(set.toString());
		System.out.println("size of the set: " +set.size());
		// TODO Auto-generated method stub

	}

}
