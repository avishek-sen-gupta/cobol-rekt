package org.smojol.common.navigation;

import org.smojol.common.id.Identifiable;

import java.util.List;

public interface GenericTreeNode<T> extends Identifiable {
    List<T> astChildren();
}
