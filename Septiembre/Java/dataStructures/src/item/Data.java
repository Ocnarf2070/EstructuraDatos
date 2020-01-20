package item;

public class Data extends Item{
	private int data;
	public Data(int x) {
		data = x;
	}
	@Override
	public boolean isData() {
		return true;
	}
	@Override
	public int getValue() {
		return data;
	}
}
