package org.smojol.common.pseudocode;

import org.smojol.common.id.IdProvider;
import org.smojol.common.vm.expression.CobolExpression;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.common.vm.type.TypedRecord;

public class SymbolReferenceBuilder {
    private final IdProvider idProvider;

    public SymbolReferenceBuilder(IdProvider idProvider) {
        this.idProvider = idProvider;
    }

    public SymbolReference recordReference(CobolDataStructure record) {
        return new RecordSymbolReference(record, record.name());
    }

    public SymbolReference intermediateSymbolReference(CobolExpression expression) {
        return new IntermediateSymbolReference(expression, idProvider.next());
    }

    public SymbolReference nullReference() {
        return new NullSymbolReference();
    }

    public SymbolReference staticReference(TypedRecord data) {
        return new StaticSymbolReference(data, idProvider.next());
    }
}
