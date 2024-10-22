package org.smojol.common.navigation;

import org.smojol.common.id.Identifiable;

import java.util.Collection;

public interface TreeNode extends Identifiable {
    <T extends TreeNode> Collection<T> astChildren();
}
