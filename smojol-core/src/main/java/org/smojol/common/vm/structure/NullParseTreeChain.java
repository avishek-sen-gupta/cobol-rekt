package org.smojol.common.vm.structure;

import java.util.List;
import java.util.function.Function;
import org.antlr.v4.runtime.tree.ParseTree;

public class NullParseTreeChain<T extends ParseTree> implements ParseTreeChain<T> {
  @Override
  public T get() {
    return null;
  }

  @Override
  public List<T> list() {
    return List.of();
  }

  @Override
  public boolean exists() {
    return false;
  }

  @Override
  public <R extends ParseTree> ParseTreeChain<R> chain(Function<T, R> access) {
    return new NullParseTreeChain<>();
  }
}
