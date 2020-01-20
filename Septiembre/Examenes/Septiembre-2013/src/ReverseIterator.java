import dataStructures.stack.ArrayStack;
import dataStructures.stack.Stack;
import java.util.Iterator;
import java.util.NoSuchElementException;


public class ReverseIterator<T> implements Iterator<T> {
	Stack <T> stack;
	// ..
	
	/** 
	 * Crea un ReverseIterator a partir de un iterable
	 * Esta clase implementa la interfaz Iterator<T>
	 * @param it  El iterable que se quiere invertir
	 */
	public ReverseIterator(Iterable<T> it) {
		stack = new ArrayStack<>();
		Iterator <T> iter = it.iterator();
		while(iter.hasNext()) {
			stack.push(iter.next());
		}
		// ...
	}
	
	/**
	 * Hay más elementos en el iterador?
	 */
	public boolean hasNext() {
		return !stack.isEmpty();
	}
	
	/**
	 * Siguiente elemento del iterador.
	 * Avanza el iterador
	 * @ return El siguiente elemento
	 */
	public T next() {
		if(!hasNext()) throw new NoSuchElementException();
		// ...
		T x = stack.top();
		stack.pop();
		return x;
	}
	
	/**
	 * Elimina el elemento actual de la estructura sobre la que se itera
	 *
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Crea y devuelve un iterable que utiliza un reverse iterator 
	 * @param iterable   El iterador a invertir
	 * @return  Un iterable invertido del que se pasa como parámetro
	 */
	public static <T> Iterable<T> elements(final Iterable<T> iterable) {
		// ..
		return null;
	}
}
