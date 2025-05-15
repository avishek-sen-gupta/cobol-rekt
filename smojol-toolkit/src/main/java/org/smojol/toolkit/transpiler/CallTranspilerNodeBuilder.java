package org.smojol.toolkit.transpiler;

import com.mojo.algorithms.domain.TranspilerNode;
import com.mojo.algorithms.domain.TypedRecord;
import com.mojo.algorithms.transpiler.*;
import org.smojol.common.ast.CallTarget;
import org.smojol.common.program.StaticCallTarget;
import org.smojol.toolkit.ast.DynamicCallTarget;

public class CallTranspilerNodeBuilder {
    public static TranspilerNode build(CallTarget callTarget) {
        return switch (callTarget) {
            case StaticCallTarget sct -> new CallTranspilerNode(new PrimitiveValueTranspilerNode(TypedRecord.typedString(sct.getName())));
            case DynamicCallTarget dct -> new CallTranspilerNode(new ValueOfNode(new SymbolReferenceNode(dct.getName())));
            default -> new NullTranspilerNode();
        };
    }
}
