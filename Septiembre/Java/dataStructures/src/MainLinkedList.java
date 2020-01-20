import dataStructures.stack.*;
public class MainLinkedList {

	public static void main(String[] args) {
		LinkedStack<Integer> stack = new LinkedStack<>();
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
