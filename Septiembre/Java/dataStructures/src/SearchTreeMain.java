import dataStructures.searchTree.BST;;
public class SearchTreeMain {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		BST<Integer, ?> tree = new BST<>();
		int [] a = {2,3,5,1,2,10,3,5,10,6};
		for(int i : a) {
			tree.insert(i, null);
			System.out.println(tree.toString());
		}
		
	}

}
