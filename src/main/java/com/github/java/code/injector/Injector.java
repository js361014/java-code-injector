package com.github.java.code.injector;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.Range;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.comments.Comment;
import com.github.javaparser.ast.expr.AssignExpr;
import com.github.javaparser.ast.expr.CastExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.AssertStmt;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.BreakStmt;
import com.github.javaparser.ast.stmt.ContinueStmt;
import com.github.javaparser.ast.stmt.DoStmt;
import com.github.javaparser.ast.stmt.EmptyStmt;
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForEachStmt;
import com.github.javaparser.ast.stmt.ForStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.LabeledStmt;
import com.github.javaparser.ast.stmt.LocalClassDeclarationStmt;
import com.github.javaparser.ast.stmt.LocalRecordDeclarationStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SwitchEntry;
import com.github.javaparser.ast.stmt.SwitchStmt;
import com.github.javaparser.ast.stmt.SynchronizedStmt;
import com.github.javaparser.ast.stmt.ThrowStmt;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.stmt.UnparsableStmt;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.github.javaparser.ast.stmt.YieldStmt;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.javaparser.ParseStart.COMPILATION_UNIT;
import static com.github.javaparser.Providers.provider;

public class Injector {

	private static void processBody(Statement body, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		if (body instanceof BlockStmt) {
			processBlockStmt((BlockStmt) body, insertBefore, insertAfter);
		} else {
			insertBefore.add(body);
			insertAfter.add(body);
		}
	}

	private static void processVariables(NodeList<VariableDeclarator> variableDeclarators, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		for (VariableDeclarator variableDeclarator : variableDeclarators) {
			Optional<Expression> initializer = variableDeclarator.getInitializer();
			if (initializer.isPresent()) {
				Expression initializerExpression = initializer.get();
				processExpression(initializerExpression, insertBefore, insertAfter);
			}
		}
	}

	private static void processExpression(Expression expression, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		if (expression instanceof VariableDeclarationExpr) {
			VariableDeclarationExpr variableDeclarationExpr = (VariableDeclarationExpr) expression;
			processVariables(variableDeclarationExpr.getVariables(), insertBefore, insertAfter);
		} else if (expression instanceof MethodCallExpr) {
			MethodCallExpr methodCallExpr = (MethodCallExpr) expression;
			Optional<Expression> scope = methodCallExpr.getScope();
			scope.ifPresent(value -> processExpression(value, insertBefore, insertAfter));
			for (Node node : methodCallExpr.getChildNodes()) {
				if (node instanceof Expression) {
					processExpression((Expression) node, insertBefore, insertAfter);
				}
			}
			for(Expression exp : methodCallExpr.getArguments()) {
				processExpression(exp, insertBefore, insertAfter);
			}
		} else if (expression instanceof LambdaExpr) {
			LambdaExpr lambdaExpr = (LambdaExpr) expression;
			Statement body = lambdaExpr.getBody();
			if (!(body instanceof ExpressionStmt)) {
				processBody(body, insertBefore, insertAfter);
			} else {
				processExpression(((ExpressionStmt) body).getExpression(), insertBefore, insertAfter);
			}
		} else if (expression instanceof CastExpr) {
			processExpression(((CastExpr) expression).getExpression(), insertBefore, insertAfter);
		} else if (expression instanceof ObjectCreationExpr) {
			ObjectCreationExpr objectCreationExpr = (ObjectCreationExpr) expression;
			Optional<NodeList<BodyDeclaration<?>>> anonymousClassBody = objectCreationExpr.getAnonymousClassBody();
			if (anonymousClassBody.isPresent()) {
				NodeList<BodyDeclaration<?>> bodyDeclarations = anonymousClassBody.get();
				for (BodyDeclaration<?> bodyDeclaration : bodyDeclarations) {
					if (bodyDeclaration instanceof MethodDeclaration) {
						MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
						Optional<BlockStmt> blockStmt = methodDeclaration.getBody();
						blockStmt.ifPresent(stmt -> processBlockStmt(stmt, insertBefore,
								insertAfter));
					}
				}
			}
			for(Expression exp : objectCreationExpr.getArguments()) {
				processExpression(exp, insertBefore, insertAfter);
			}
		} else if (expression instanceof AssignExpr) {
			processExpression(((AssignExpr) expression).getValue(), insertBefore, insertAfter);
		}
	}

	private static boolean allPathsHasReturnStatement(NodeList<Statement> statements) {
		for(Statement statement : statements) {
			if (statement instanceof ReturnStmt) {
				return true;
			} else if (statement instanceof ThrowStmt) {
				return true;
			} else if (statement instanceof IfStmt) {
				if (allPathsHasReturnStatement((IfStmt)statement)) {
					return true;
				}
			}
		}
		return false;
	}

	private static boolean allPathsHasReturnStatement(BlockStmt blockStmt) {
		return allPathsHasReturnStatement(blockStmt.getStatements());
	}

	private static boolean allPathsHasReturnStatement(IfStmt ifStmt) {
		Statement thenStmt = ifStmt.getThenStmt();
		boolean thenAllPathsHasReturnStatement;
		if (thenStmt instanceof BlockStmt) {
			thenAllPathsHasReturnStatement = allPathsHasReturnStatement((BlockStmt)thenStmt);
		} else if (thenStmt instanceof ReturnStmt) {
			thenAllPathsHasReturnStatement = true;
		} else {
			throw new RuntimeException("Not implemented handling thenStmt" + thenStmt.getClass().getName());
		}
		Optional<Statement> elseStmt = ifStmt.getElseStmt();
		if (elseStmt.isPresent()) {
			Statement st = elseStmt.get();
			boolean elseAllPathsHasReturnStatement;
			if (st instanceof BlockStmt) {
				elseAllPathsHasReturnStatement = allPathsHasReturnStatement((BlockStmt)st);
			} else if (st instanceof ReturnStmt) {
				elseAllPathsHasReturnStatement = true;
			} else if (st instanceof IfStmt) {
				elseAllPathsHasReturnStatement = allPathsHasReturnStatement((IfStmt)st);
			} else {
				throw new RuntimeException("Not implemented handling st " + st.getClass().getName());
			}
			return thenAllPathsHasReturnStatement && elseAllPathsHasReturnStatement;
		} else {
			return thenAllPathsHasReturnStatement;
		}
	}

	private static void processIfStmt(IfStmt ifStmt, boolean onlyProcessBody, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		Statement thenStmt = ifStmt.getThenStmt();
		processBody(thenStmt, insertBefore, insertAfter);
		Optional<Statement> elseStmt = ifStmt.getElseStmt();
		if (elseStmt.isPresent()) {
			Statement st = elseStmt.get();
			if (st instanceof IfStmt) {
				processIfStmt((IfStmt) st, true, insertBefore, insertAfter);
			} else {
				processBody(st, insertBefore, insertAfter);
			}
		}

		if (!onlyProcessBody) {
			insertBefore.add(ifStmt);
			insertAfter.add(ifStmt);
		}
	}

	private static void processStatements(NodeList<Statement> statements,
			List<Statement> insertBefore,
			List<Statement> insertAfter) {
		for (Statement statement : statements) {
			if (statement instanceof AssertStmt
					|| statement instanceof YieldStmt || statement instanceof LabeledStmt) {
				insertBefore.add(statement);
				insertAfter.add(statement);
			} else if (statement instanceof BlockStmt) {
				processBlockStmt((BlockStmt) statement, insertBefore, insertAfter);
			} else if (statement instanceof BreakStmt || statement instanceof ContinueStmt
					|| statement instanceof ReturnStmt || statement instanceof ThrowStmt) {
				insertBefore.add(statement);

			} else if (statement instanceof DoStmt) {
				DoStmt doStmt = (DoStmt) statement;
				Statement body = doStmt.getBody();
				processBody(body, insertBefore, insertAfter);

				insertBefore.add(statement);
				insertAfter.add(statement);

			} else if (statement instanceof EmptyStmt
					|| statement instanceof LocalRecordDeclarationStmt
					|| statement instanceof UnparsableStmt) {
				return;
			} else if (statement instanceof ExpressionStmt) {
				ExpressionStmt expressionStmt = (ExpressionStmt) statement;
				Expression expression = expressionStmt.getExpression();
				processExpression(expression, insertBefore, insertAfter);
				insertBefore.add(statement);
				insertAfter.add(statement);
			} else if (statement instanceof ForEachStmt) {
				ForEachStmt forEachStmt = (ForEachStmt) statement;
				Statement body = forEachStmt.getBody();
				processBody(body, insertBefore, insertAfter);

				insertBefore.add(statement);
				insertAfter.add(statement);
			} else if (statement instanceof ForStmt) {
				ForStmt forStmt = (ForStmt) statement;
				Statement body = forStmt.getBody();
				processBody(body, insertBefore, insertAfter);

				insertBefore.add(statement);
				insertAfter.add(statement);

			} else if (statement instanceof IfStmt) {
				IfStmt ifStmt = (IfStmt) statement;
				processIfStmt(ifStmt, allPathsHasReturnStatement(ifStmt), insertBefore, insertAfter);
			} else if (statement instanceof LocalClassDeclarationStmt) {
				LocalClassDeclarationStmt localClassDeclarationStmt = (LocalClassDeclarationStmt) statement;
				ClassOrInterfaceDeclaration classDeclaration = localClassDeclarationStmt.getClassDeclaration();
				processClassOrInterfaceDeclaration(classDeclaration, insertBefore, insertAfter);
			} else if (statement instanceof SwitchStmt) {
				SwitchStmt switchStmt = (SwitchStmt) statement;
				boolean allEntriesHasReturnStatement = true;
				for (SwitchEntry switchEntry : switchStmt.getEntries()) {
					if (switchEntry.getType() == SwitchEntry.Type.BLOCK || switchEntry.getType() == SwitchEntry.Type.STATEMENT_GROUP) {
						if (!allPathsHasReturnStatement(switchEntry.getStatements())) {
							allEntriesHasReturnStatement = false;
						}
						processStatements(switchEntry.getStatements(), insertBefore, insertAfter);
					}
				}
				if (!allEntriesHasReturnStatement) {
					insertBefore.add(statement);
					insertAfter.add(statement);
				}
			} else if (statement instanceof SynchronizedStmt) {
				SynchronizedStmt synchronizedStmt = (SynchronizedStmt) statement;
				Statement body = synchronizedStmt.getBody();
				processBody(body, insertBefore, insertAfter);
			} else if (statement instanceof TryStmt) {
				TryStmt tryStmt = (TryStmt) statement;
				processBlockStmt(tryStmt.getTryBlock(), insertBefore, insertAfter);
				Optional<BlockStmt> finallyBlock = tryStmt.getFinallyBlock();
				finallyBlock.ifPresent(
						blockStmt -> processBlockStmt(blockStmt, insertBefore, insertAfter));
				if (!allPathsHasReturnStatement(tryStmt.getTryBlock())) {
					insertBefore.add(statement);
					insertAfter.add(statement);
				}
			} else if (statement instanceof WhileStmt) {
				WhileStmt whileStmt = (WhileStmt) statement;
				Statement body = whileStmt.getBody();
				processBody(body, insertBefore, insertAfter);

				insertBefore.add(statement);
				insertAfter.add(statement);
			} else if (statement instanceof ExplicitConstructorInvocationStmt) {
				insertAfter.add(statement);
			}
		}
	}
	private static void processBlockStmt(BlockStmt blockStatement, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		processStatements(blockStatement.getStatements(), insertBefore, insertAfter);
	}

	private static void processClassOrInterfaceDeclaration(
			ClassOrInterfaceDeclaration classOrInterfaceDeclaration, List<Statement> insertBefore,
			List<Statement> insertAfter) {
		List<MethodDeclaration> methods = classOrInterfaceDeclaration.getMethods();
		for (MethodDeclaration methodDeclaration : methods) {
			Optional<BlockStmt> blockStmt = methodDeclaration.getBody();
			blockStmt.ifPresent(stmt -> processBlockStmt(stmt, insertBefore, insertAfter));
		}
		List<ConstructorDeclaration> constructorDeclarations = classOrInterfaceDeclaration.getConstructors();
		for(ConstructorDeclaration constructorDeclaration : constructorDeclarations) {
			processBlockStmt(constructorDeclaration.getBody(), insertBefore, insertAfter);
		}
		List<FieldDeclaration> fieldDeclarations = classOrInterfaceDeclaration.getFields();
		for(FieldDeclaration fieldDeclaration : fieldDeclarations) {
			processVariables(fieldDeclaration.getVariables(), insertBefore, insertAfter);
		}
		NodeList<BodyDeclaration<?>> members = classOrInterfaceDeclaration.getMembers();
		for(BodyDeclaration<?> member : members) {
			if (member instanceof ClassOrInterfaceDeclaration) {
				processClassOrInterfaceDeclaration((ClassOrInterfaceDeclaration)member, insertBefore, insertAfter);
			}
		}
	}

	public static void insertStatementToSourceFile(Path path, String className, StatementSupplier statementSupplier)
			throws Exception {
		ParserConfiguration parserConfiguration = new ParserConfiguration();
		parserConfiguration.setCharacterEncoding(Charset.defaultCharset());

		ParseResult<CompilationUnit> result = new JavaParser(parserConfiguration)
				.parse(COMPILATION_UNIT, provider(path,
						parserConfiguration.getCharacterEncoding()));
		Optional<CompilationUnit> res = result.getResult();
		List<Statement> insertBefore = new ArrayList<>();
		List<Statement> insertAfter = new ArrayList<>();
		List<Comment> comments = new ArrayList<>();
		if (res.isPresent()) {
			CompilationUnit unit = res.get();
			comments.addAll(unit.getAllComments());
			Optional<ClassOrInterfaceDeclaration> classDeclaration = unit.getClassByName(className);
			if (classDeclaration.isPresent()) {
				ClassOrInterfaceDeclaration declaration = classDeclaration.get();
				processClassOrInterfaceDeclaration(declaration, insertBefore, insertAfter);
			}
		}
		List<Pair<Integer, Integer>> ignoreLinesRanges = new ArrayList<>();
		for(Comment comment : comments) {
			if (comment.getContent().contains("[begin_ignore_not_covered_in_tests]")) {
				Optional<Range> commentRange = comment.getRange();
				if (commentRange.isPresent()) {
					Range range = commentRange.get();
					int beginning = range.begin.line;
					int end = -1;
					for(Comment endComment : comments) {
						if (endComment.getContent().contains("[end_ignore_not_covered_in_tests]")) {
							Optional<Range> endCommentRange = endComment.getRange();
							if (endCommentRange.isPresent()) {
								Range rangeEnd = endCommentRange.get();
								int endCommentEnd = rangeEnd.end.line;
								if (endCommentEnd > beginning && (end == -1 || end > endCommentEnd)) {
									end = endCommentEnd;
								}
							}
						}
					}
					if (end != -1) {
						ignoreLinesRanges.add(new Pair<>(beginning, end));
					}
				}
			}
		}

		ignoreLinesRanges.sort((p1, p2) -> {
			if (p1.equals(p2)) {
				return 0;
			}
			if (p1.getFirst().equals(p2.getFirst())) {
				return Integer.compare(p1.getSecond(), p2.getSecond());
			} else {
				return Integer.compare(p1.getFirst(), p2.getFirst());
			}
		});
		try (Stream<String> linesStream = Files.lines(path)) {
			List<String> lines = linesStream.collect(Collectors.toList());
			List<InsertTo> insertTos = getInserts(insertBefore, insertAfter);
			String modified = insertStatement(lines, insertTos, statementSupplier, ignoreLinesRanges);
			Files.write(path, modified.getBytes());
		}
	}

	private static String insertStatement(List<String> lines, List<InsertTo> insertTos, Supplier<String> statementSupplier, List<Pair<Integer, Integer>> ignoreLinesRanges) {
		List<String> modifiedLines = new ArrayList<>();
		int j = 0;
		for (int i = 0; i < lines.size(); i++) {
			while (j < insertTos.size() && insertTos.get(j).getLine() <= i + 1) {
				if (j == 0 || insertTos.get(j - 1).getLine() != insertTos.get(j).getLine()) {
					final int insertLine = insertTos.get(j).getLine();
					if (ignoreLinesRanges.stream().noneMatch(integerIntegerPair ->
							integerIntegerPair.getFirst() <= insertLine
									&& integerIntegerPair.getSecond() >= insertLine)) {
						modifiedLines.add(
								String.join("", Collections.nCopies(insertTos.get(j).getCol(), " "))
										+ statementSupplier.get());
					}
				}
				j++;
			}
			modifiedLines.add(lines.get(i));
		}
		while (j < insertTos.size()) {
			modifiedLines.add(statementSupplier.get());
			j++;
		}
		return String.join("\n", modifiedLines);
	}
	private static List<InsertTo> getInserts(List<Statement> insertBefore,
			List<Statement> insertAfter) {
		List<InsertTo> insertTos = new ArrayList<>();
		for (Statement before : insertBefore) {
			Optional<Range> range = before.getRange();
			range.ifPresent(
					value -> insertTos.add(new InsertTo(value.begin.line, value.begin.column - 1)));
		}
		for (Statement after : insertAfter) {
			Optional<Range> range = after.getRange();
			range.ifPresent(
					value -> insertTos.add(
							new InsertTo(value.end.line + 1, value.begin.column - 1)));
		}
		Collections.sort(insertTos);
		List<InsertTo> insertTosWithoutDuplicates = new ArrayList<>();
		for(int i = 0; i < insertTos.size(); i++) {
			if (i == 0 || insertTos.get(i - 1).compareTo(insertTos.get(i)) != 0) {
				insertTosWithoutDuplicates.add(insertTos.get(i));
			}
		}
		return insertTosWithoutDuplicates;
	}
}
