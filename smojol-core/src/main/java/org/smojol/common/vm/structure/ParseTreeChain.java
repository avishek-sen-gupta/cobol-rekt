package org.smojol.common.vm.structure;

import java.util.List;
import java.util.function.Function;
import org.antlr.v4.runtime.tree.ParseTree;

public interface ParseTreeChain<T extends ParseTree> {
  T get();

  List<T> list();

  boolean exists();

  <R extends ParseTree> ParseTreeChain<R> chain(Function<T, R> access);
}
