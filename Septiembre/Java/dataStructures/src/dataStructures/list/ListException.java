package dataStructures.list;

@SuppressWarnings("serial")
public class ListException extends RuntimeException {
	 public ListException( ) {
		 super();
	 }
	 
	 public ListException(String msg) {
		 super(msg);
	 }
}