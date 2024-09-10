package org.smojol.common.vm.structure;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.List;
import java.util.function.Function;

public class MemberParseTreeChain<T extends ParseTree> implements ParseTreeChain<T> {
    private final T root;
    private Function<T, ? extends ParseTree> access;
    private Function<T, List<? extends ParseTree>> listAccess;
    private Function<T, List<? extends ParseTree>> access1;

    public MemberParseTreeChain(T root) {
        this.root = root;
    }

    @Override
    public T get() {
        return root;
    }

    @Override
    public List<T> list() {
        return ImmutableList.of();
    }

    @Override
    public boolean exists() {
        return true;
    }

    @Override
    public <R extends ParseTree> ParseTreeChain<R> chain(Function<T, R> access) {
        R result = access.apply(root);
        if (result == null) return new NullParseTreeChain<>();
        return new MemberParseTreeChain<>(result);
    }

    public <R extends ParseTree> ListParseTreeChain<R> listChain(Function<T, List<R>> access) {
        return new ListParseTreeChain<>(access.apply(root));
    }
}
