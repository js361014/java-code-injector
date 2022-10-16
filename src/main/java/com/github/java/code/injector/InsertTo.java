package com.github.java.code.injector;

public class InsertTo implements Comparable<InsertTo> {

	private final int line;

	private final int col;

	public InsertTo(int line, int col) {
		this.line = line;
		this.col = col;
	}

	@Override
	public int compareTo(InsertTo other) {
		if (line != other.getLine()) {
			return Integer.compare(line, other.getLine());
		}
		return Integer.compare(col, other.getCol());
	}

	public int getLine() {
		return line;
	}

	public int getCol() {
		return col;
	}
}
