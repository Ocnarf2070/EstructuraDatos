package item;

import dataStructures.stack.ArrayStack;
import dataStructures.stack.Stack;

public class postFix {
	public static int evaluate (Item[] exprList) {
		Stack<Integer> stack = new ArrayStack<>();
		for(Item item : exprList) {
			if(item.isData())stack.push(item.getValue());
			else {
				int a2=stack.top(),a1;
				stack.pop();
				a1 = stack.top();
				stack.pop();
				stack.push(item.evaluate(a1, a2));
			}
		}
		return stack.top();
	}
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Item [] sample = {
				new Data(5),
				new Data(6),
				new Data(2),
				new Dif(),
				new Data(3),
				new Mul(),
				new Add() };

		System.out.println(postFix.evaluate(sample));
	}

}
