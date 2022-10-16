package com.github.java.code.injector;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.Namespace;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Main {

	public static void main(String[] args) throws Exception {
		ArgumentParser parser = ArgumentParsers.newFor("Injector").build()
				.defaultHelp(true)
				.description("Inject statement into java source files.");
		parser.addArgument("file").nargs("*")
				.help("Java source file to inject statement. Directories are also supported.");

		Namespace ns = null;
		try {
			ns = parser.parseArgs(args);
		} catch (ArgumentParserException e) {
			parser.handleError(e);
			System.exit(1);
		}

		List<File> files = new ArrayList<>();

		for (String name : ns.<String> getList("file")) {
			files.addAll(Files.walk(Paths.get(
							name
					))
					.filter(Files::isRegularFile)
					.map(Path::toFile)
					.collect(Collectors.toList()));
		}
		StatementSupplier statementSupplier = new StatementSupplier(1);
		for(File file : files) {
			Injector.insertStatementToSourceFile(file.toPath(), file.getName().replace(".java", ""),
					statementSupplier);
			System.out.println(file.toPath());
		}
	}
}