package com.mojo.algorithms.navigation;

import com.mojo.algorithms.id.Identifiable;

import java.util.List;

public interface GenericTreeNode<T> extends Identifiable {
    List<T> astChildren();
}
