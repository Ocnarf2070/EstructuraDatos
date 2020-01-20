package wellBalanced;
import dataStructures.stack.*;
public class WellBalanced {
	private final static String OPEN_PARENTHESES ="{[(";
	private final static String CLOSED_PARENTHESES = "}])";
	public static void main(String [] args) {
		Stack <Character> stack = new ArrayStack<>();
		System.out.println(WellBalanced.wellBalanced("v(hg(jij)hags{ss[dd]dd})", stack));
		System.out.println(WellBalanced.wellBalanced("ff(h([sds)sds]ss)hags", stack));
	}
	public static boolean wellBalanced(String exp, Stack<Character> stack) {
		String expr = exp;
		while(!expr.isEmpty()) {
			char car = expr.charAt(0);
			if(isOpenParentheses(car))stack.push(car);
			else if (isClosedParentheses(car)) {
				if(match(stack.top(),car)) stack.pop();
				else break;
			}else;
			expr = expr.substring(1);
		}
		return stack.isEmpty();
	}


	public static boolean isOpenParentheses(char c) {
		return OPEN_PARENTHESES.indexOf(new Character(c).toString()) >= 0;
	}
	public static boolean isClosedParentheses(char c) {
		return CLOSED_PARENTHESES.indexOf(new Character(c).toString()) >= 0;
	}
	public static boolean match(char x, char y) {
		return OPEN_PARENTHESES.indexOf(new Character(x).toString()) ==
				CLOSED_PARENTHESES.indexOf(new Character(y).toString());
	}
}