package com.github.java.code.injector;

import java.util.function.Supplier;

public class StatementSupplier implements Supplier<String> {

	private int counter;

	public StatementSupplier(int counter) {
		this.counter = counter;
	}

	@Override
	public String get() {
		return String.format("android.util.Log.d(\"DEBUG_INFO\" ,\"MSG_%d\");", counter++);
	}
}
