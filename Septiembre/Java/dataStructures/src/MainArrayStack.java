import dataStructures.stack.*;

public class MainArrayStack {
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		ArrayStack<Integer> stack = new ArrayStack<>();
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
