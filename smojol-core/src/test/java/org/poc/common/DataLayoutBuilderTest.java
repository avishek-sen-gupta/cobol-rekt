package org.poc.common;

import org.junit.jupiter.api.Test;
import org.smojol.common.vm.memory.DataLayoutBuilder;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.type.AlphanumericDataTypeSpec;
import org.smojol.common.vm.type.ZonedDecimalSignType;
import org.smojol.common.vm.type.ZonedDecimalDataTypeSpec;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DataLayoutBuilderTest {
    @Test
    public void canBuildSimpleAlphanumericDataLayout() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("XXXX");
        assertEquals(4, layout.memory().asBytes().size());
        assertEquals(AlphanumericDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(4, layout.getTypeSpec().sizeInBytes());
    }

    @Test
    public void canBuildMoreComplexlySpecifiedAlphanumericDataLayout() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("XX(3)XX(5)");
        assertEquals(10, layout.memory().asBytes().size());
        assertEquals(AlphanumericDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(10, layout.getTypeSpec().sizeInBytes());
    }

    @Test
    public void canBuildMoreComplexlySpecifiedAlphanumericDataLayoutWhichContainsNumbers() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("XX(3)9X(5)9(3)");
        assertEquals(13, layout.memory().asBytes().size());
        assertEquals(AlphanumericDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(13, layout.getTypeSpec().sizeInBytes());
    }

    @Test
    public void canBuildSimpleNumericDataLayou() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("9999");
        assertEquals(4, layout.memory().asBytes().size());
        assertEquals(ZonedDecimalDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(ZonedDecimalSignType.UNSIGNED, ((ZonedDecimalDataTypeSpec) layout.getTypeSpec()).getSignType());
        assertEquals(4, layout.getTypeSpec().sizeInBytes());
    }

    @Test
    public void canBuildMoreComplexNumericDataLayout() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("99(3)99(5)");
        assertEquals(10, layout.memory().asBytes().size());
        assertEquals(ZonedDecimalDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(ZonedDecimalSignType.UNSIGNED, ((ZonedDecimalDataTypeSpec) layout.getTypeSpec()).getSignType());
        assertEquals(10, layout.getTypeSpec().sizeInBytes());
    }

    @Test
    public void canBuildSignedNumericDataLayout() {
        DataLayoutBuilder builder = new DataLayoutBuilder();
        MemoryLayout layout = builder.layout("S99(3)99(5)");
        assertEquals(10, layout.memory().asBytes().size());
        assertEquals(ZonedDecimalDataTypeSpec.class, layout.getTypeSpec().getClass());
        assertEquals(ZonedDecimalSignType.SIGNED, ((ZonedDecimalDataTypeSpec) layout.getTypeSpec()).getSignType());
        assertEquals(10, layout.getTypeSpec().sizeInBytes());
    }
}
