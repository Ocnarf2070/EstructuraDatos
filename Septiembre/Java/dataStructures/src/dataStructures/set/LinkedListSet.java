package dataStructures.set;

import java.util.LinkedList;
import java.util.Iterator;

public class LinkedListSet <T> implements Set <T>{
	private LinkedList<T> elements;
	
	
	public LinkedListSet() {
		elements = new LinkedList<>();
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return elements.iterator();
	}

	@Override
	public boolean isElem(T el) {
		// TODO Auto-generated method stub
		return elements.contains(el);
	}

	@Override
	public void insert(T el) {
		if(!isElem(el))elements.add(el);
		// TODO Auto-generated method stub
		
	}

	@Override
	public void delete(T el) {
		elements.remove(el);
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return elements.isEmpty();
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return elements.size();
	}
	
    public String toString() {
        String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String text = className+"(";
        Iterator<T> it = elements.iterator();
        while(it.hasNext()) {
            text +=  it.next() + (it.hasNext() ? "," : "");
        }
        return text + ")";
    }
	
}
