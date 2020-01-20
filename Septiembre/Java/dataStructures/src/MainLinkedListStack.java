import dataStructures.stack.*;
public class MainLinkedListStack {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		ListStack<Integer> stack = new ListStack<>();
		System.out.println(stack.isEmpty()?"True":"False"); 
		try {
			System.out.println(stack.top());
		}catch (EmptyStackException ms) {
			// TODO: handle exception
		}
		finally {
			stack.push(1);
			stack.push(2);
			stack.push(3);
			while (!stack.isEmpty( )) {
				System.out.print(stack.isEmpty()?"True ":"False "); 
				System.out.println(stack.top( ));
				stack.pop( );
			}
		}

	}

}
