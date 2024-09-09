package org.poc.common;

import com.google.common.collect.ImmutableList;
import org.junit.jupiter.api.Test;
import org.smojol.common.vm.exception.IllegalIndexException;
import org.smojol.common.vm.memory.MemoryLayout;
import org.smojol.common.vm.memory.MemoryRegion;
import org.smojol.common.vm.expression.RightAdjuster;
import org.smojol.common.vm.type.*;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.poc.common.MemoryTestUtils.assertBytes;
import static org.poc.common.MemoryTestUtils.assertMemory;

public class DataTypesTest {
    @Test
    public void canAllocateMemoryForNumbers() {
        MemoryRegion memoryRegion = new MemoryRegion("12345");
        assertEquals("12345", memoryRegion.asString());
        List<Byte> bytes = memoryRegion.asBytes();
        assertEquals((byte) '1', bytes.get(0));
        assertEquals((byte) '2', bytes.get(1));
        assertEquals((byte) '3', bytes.get(2));
        assertEquals((byte) '4', bytes.get(3));
        assertEquals((byte) '5', bytes.get(4));
        memoryRegion.set("67890");
        assertEquals("67890", memoryRegion.asString());
    }

    @Test
    public void canConvertOverflowingZonedDecimalDigitsIntoProperNumbers() {
        // Formula is (HEXDIGIT-9)*(10^ Zero-based position of digit from the right)
        assertEquals(20, MemoryRegion.zoned("1A"));
        assertEquals(200, MemoryRegion.zoned("1A0"));
        assertEquals(340, MemoryRegion.zoned("2E0"));
    }

    @Test
    public void canConvertSignedPositiveIntegerIntoZOSInternalRepresentation() {
        List<Byte> bytes = MemoryLayout.toSignedBytes(4321);
        assertBytes(bytes, "F4:F3:F2:C1");
    }

    @Test
    public void canConvertSignedPositiveIntegerStringIntoZOSInternalRepresentation() {
        List<Byte> positiveBytes = MemoryLayout.toSignedBytes("4321", true);
        assertBytes(positiveBytes, "F4:F3:F2:C1");
        List<Byte> negativeBytes = MemoryLayout.toSignedBytes("4321", false);
        assertBytes(negativeBytes, "F4:F3:F2:D1");
    }

    @Test
    public void canConvertUnsignedPositiveIntegerIntoZOSInternalRepresentation() {
        List<Byte> bytes = MemoryLayout.toUnsignedBytes(4321);
        assertBytes(bytes, "F4:F3:F2:F1");
    }

    @Test
    public void canConvertSignedNegativeIntegerIntoZOSInternalRepresentation() {
        List<Byte> bytes = MemoryLayout.toSignedBytes(-4321);
        assertBytes(bytes, "F4:F3:F2:D1");
    }

    @Test
    public void canAlignAlphanumerics() {
        assertEquals("ABCDE", new RightAdjuster(5).filter("ABCDE"));
        assertEquals("ABCDE", new RightAdjuster(5).filter("ABCDEFGH"));
        assertEquals("ABC  ", new RightAdjuster(5).filter("ABC"));
        assertEquals("     ", new RightAdjuster(5).filter(""));
    }

    @Test
    public void canAlignAtDecimalPoint() {
        assertEquals("1234", new DecimalPointAligner(2, 2).filter("12.34"));
        assertEquals("1234", new DecimalPointAligner(2, 2).filter("512.34"));
        assertEquals("0230", new DecimalPointAligner(2, 2).filter("2.3"));
        assertEquals("2300", new DecimalPointAligner(2, 2).filter("4523"));
        assertEquals("0056", new DecimalPointAligner(2, 2).filter(".5678"));
        assertEquals("7800", new DecimalPointAligner(2, 2).filter("678."));
        assertEquals("0000", new DecimalPointAligner(2, 2).filter("."));
        assertEquals("CDEF", new DecimalPointAligner(2, 2).filter("ABCD.EFGG"));
    }

    @Test
    public void canCreateAlphanumericLayouts() {
        DataTypeSpec alphanumeric = new AlphanumericDataTypeSpec(5);
        MemoryRegion memoryRegion = new MemoryRegion(alphanumeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), alphanumeric);
        layout.set("12345");
        assertEquals("12345", layout.read());
        layout.set("ABCDEFGH");
        assertEquals("ABCDE", layout.read());
    }

    @Test
    public void canCreateIntegerNumericLayouts() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("12345");
        assertEquals(12345.0, layout.read());
        layout.set("123456");
        assertEquals(23456.0, layout.read());
        layout.set("123");
        assertEquals(123.0, layout.read());
        assertMemory(memoryRegion, "F0:F0:F1:F2:F3");
    }

    @Test
    public void canCreateFractionalNumericLayouts() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 2);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("12.345");
        assertEquals(1234.0, layout.read());
        assertMemory(memoryRegion, "F1:F2:F3:F4");
        layout.set("12345.");
        assertEquals(4500.0, layout.read());
        assertMemory(memoryRegion, "F4:F5:F0:F0");
        layout.set(".12345");
        assertEquals(12.0, layout.read());
        assertMemory(memoryRegion, "F0:F0:F1:F2");
    }

    @Test
    public void canCreateOutputFormattedFractions() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 2);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("12.345");
        assertEquals(1234.0, layout.read());
        assertMemory(memoryRegion, "F1:F2:F3:F4");
        assertEquals(12.34, layout.readFormatted());
    }

    @Test
    public void canForceConvertIntoCorrectNumbersForIntegerNumericLayouts() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(4, 0);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("1234");
        assertMemory(memoryRegion, "31:32:33:34");
        layout.refresh();
        assertMemory(memoryRegion, "F1:F2:F3:F4");
        assertEquals(1234.0, layout.read());
    }

    @Test
    public void canForceConvertIntoCorrectNumbersForFractionalNumericLayouts() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("12.34");
        assertMemory(memoryRegion, "31:32:2E:33:34");
        layout.refresh();

        assertMemory(memoryRegion, "F1:F3:F4:F3:F4");
        assertEquals(13434.0, layout.read());
    }

    @Test
    public void writingRawBytesToNumericTruncatesFromTheLeftInsteadOfLookingForDecimalPoint() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("12.345");
        assertMemory(memoryRegion, "32:2E:33:34:35");
        layout.refresh();
        assertMemory(memoryRegion, "F3:F4:F3:F4:F5");
        assertEquals(34345.0, layout.read());
    }

    @Test
    public void canHandleOverflowsIfConvertingFromRawDataExceedsDataTypeLength() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("99.99");
        layout.refresh();
        assertMemory(memoryRegion, "F0:F0:F4:F9:F9");
    }

    @Test
    public void canSetSignedIntegers() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(3, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("-123");
        assertMemory(memoryRegion, "F1:F2:D3");
        layout.set("123");
        assertMemory(memoryRegion, "F1:F2:C3");
    }

    @Test
    public void canInterpretRawDataAccordingToZOSBehaviour() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("99.99");
        layout.refresh();
        assertMemory(memoryRegion, "F0:F0:F4:F9:C9");
    }

    @Test
    public void canInterpretDecimalPointInSignedFractionalNumbers() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(5, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("99.99");
        assertMemory(memoryRegion, "F0:F0:F0:F9:C9");
        layout.set("-99.99");
        assertMemory(memoryRegion, "F0:F0:F0:F9:D9");
    }

    @Test

    public void canInterpretSignedFractionalNumbers() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("12.34");
        assertMemory(memoryRegion, "F1:F2:F3:F4:C0");
        layout.set("-12.34");
        assertMemory(memoryRegion, "F1:F2:F3:F4:D0");
        layout.set("2.34");
        assertMemory(memoryRegion, "F0:F2:F3:F4:C0");
        layout.set("-2.34");
        assertMemory(memoryRegion, "F0:F2:F3:F4:D0");
        layout.set("578.4216");
        assertMemory(memoryRegion, "F7:F8:F4:F2:C1");
        layout.set("-578.4216");
        assertMemory(memoryRegion, "F7:F8:F4:F2:D1");

    }

    @Test
    public void canHandleSignsWithZoneRecalculationWithoutTruncating() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(3, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("-6");
        assertMemory(memoryRegion, "00:2D:36");
        layout.refresh();
        assertMemory(memoryRegion, "F1:F3:C6");
    }

    @Test
    public void canHandleSignsWithZoneRecalculationWithTruncating() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("-6");
        assertMemory(memoryRegion, "2D:36");
        layout.refresh();
        assertMemory(memoryRegion, "F3:C6");
    }

    /**
     * This is essentially an invalid move, and the result is undefined.
     * This reference clearly identifies this as invalid: https://www.ibm.com/docs/en/cobol-zos/6.3?topic=moves-valid-invalid-elementary
     * However, the z/OS compiler seems to interpret this correctly as -060, +060 internally.
     * This test is here to document that Smojol does not treat this specially, and thus,
     * gives results different from z/OS.
     */

    @Test
    public void doesNotHandleZOSSpecialCaseWhenInterpreterDoesNotIgnoreSignIfImpliedDecimalPointIsPresentAndRawDataHasNegative() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 1, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("-6");
        assertMemory(memoryRegion, "00:2D:36");
        layout.refresh();
        assertMemory(memoryRegion, "F3:F6:C0");

        layout.setRaw("+6");
        assertMemory(memoryRegion, "00:2B:36");
        layout.refresh();
        assertMemory(memoryRegion, "F1:F6:C0");

        layout.setRaw("-6.3");
        assertMemory(memoryRegion, "36:2E:33");
        layout.refresh();
        assertMemory(memoryRegion, "F4:F3:C0");
    }

    @Test
    public void canIgnoreSignInRawDataIfImpliedDecimalPointIsNotPresentOrRedundant() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 0, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.setRaw("-8426");
        layout.refresh();
        assertMemory(memoryRegion, "F2:C6");
        layout.setRaw("-6");
        layout.refresh();
        assertMemory(memoryRegion, "F3:C6");
    }

    @Test
    public void canHandleZero() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("0.0");
        assertMemory(memoryRegion, "F0:F0:F0:F0:C0");

        // This diverges from ZOS implementations, ZOS allows -0
        layout.set("-0.0");
        assertMemory(memoryRegion, "F0:F0:F0:F0:C0");
    }

    @Test
    public void canSetEmptyStringOnSignedNumericInterpretedAsZero() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("");
        assertMemory(memoryRegion, "F0:F0:F0:F0:C0");
    }

    @Test
    public void canSetEmptyStringOnUnsignedNumericInterpretedAsZero() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), numeric);
        layout.set("");
        assertMemory(memoryRegion, "F0:F0:F0:F0:F0");
    }

    @Test
    public void canIndexTableMemory() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.UNSIGNED);
        TableSpec tableSpec = new TableSpec(numeric, 3);
        MemoryRegion memoryRegion = new MemoryRegion(tableSpec.sizeInBytes());
        assertEquals(15, tableSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), tableSpec);
        MemoryLayout firstElement = layout.index(0);
        MemoryLayout secondElement = layout.index(1);
        MemoryLayout thirdElement = layout.index(2);
        firstElement.set("12.46");
        assertEquals(12460.0, firstElement.read());
        assertMemory(firstElement.memory(), "F1:F2:F4:F6:F0");
        assertMemory(secondElement.memory(), "00:00:00:00:00");
        assertMemory(thirdElement.memory(), "00:00:00:00:00");

        secondElement.set("43.795");
        assertEquals(43795.0, secondElement.read());
        assertEquals(43.795, secondElement.readFormatted());
    }

    @Test
    public void cannotIndexNonTableMemory() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.UNSIGNED);
        MemoryRegion numericMemoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout numericLayout = new MemoryLayout(numericMemoryRegion.fullAccess(), numeric);
        IllegalIndexException nonTableSpecException1 = assertThrows(IllegalIndexException.class, () -> numericLayout.index(0));
        assertEquals("Illegal indexing attempt: 0. Cause: Non-TableSpec type", nonTableSpecException1.getMessage());

        DataTypeSpec alphanumeric = new AlphanumericDataTypeSpec(2);
        MemoryRegion alphaNumericMemoryRegion = new MemoryRegion(numeric.sizeInBytes());
        MemoryLayout alphaNumericLayout = new MemoryLayout(alphaNumericMemoryRegion.fullAccess(), alphanumeric);
        IllegalIndexException nonTableSpecException2 = assertThrows(IllegalIndexException.class, () -> alphaNumericLayout.index(0));
        assertEquals("Illegal indexing attempt: 0. Cause: Non-TableSpec type", nonTableSpecException2.getMessage());
    }

    @Test
    public void canErrorOnOutOfBoundsIndexAccess() {
        DataTypeSpec numeric = new ZonedDecimalDataTypeSpec(2, 3, ZonedDecimalSignType.UNSIGNED);
        TableSpec tableSpec = new TableSpec(numeric, 3);
        MemoryRegion memoryRegion = new MemoryRegion(tableSpec.sizeInBytes());
        assertEquals(15, tableSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), tableSpec);
        IllegalIndexException exceptionIndexGreaterThanMaximumSize = assertThrows(IllegalIndexException.class, () -> layout.index(6));
        assertEquals("Illegal indexing attempt: 6. Cause: Index out of bounds", exceptionIndexGreaterThanMaximumSize.getMessage());
        IllegalIndexException exceptionIndexLessThanZero = assertThrows(IllegalIndexException.class, () -> layout.index(-6));
        assertEquals("Illegal indexing attempt: -6. Cause: Index out of bounds", exceptionIndexLessThanZero.getMessage());
    }

    @Test
    public void canAllocateCorrectNumberOfBytesBasedOnNumberOfDigitsInComp3DataType() {
        assertEquals(3, new Comp3lDataTypeSpec(3, 2, Comp3SignType.UNSIGNED).sizeInBytes());
        assertEquals(3, new Comp3lDataTypeSpec(2, 2, Comp3SignType.UNSIGNED).sizeInBytes());
    }

    @Test
    public void canReadUnsignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(3, 2, Comp3SignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        memoryRegion.write(HexMapper.asBytes(ImmutableList.of(0x01, 0x23, 0x4F)));
        assertEquals(1234.0, layout.read());
    }

    @Test
    public void canWriteUnsignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(2, 2, Comp3SignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        layout.set("12.34");
        assertMemory(memoryRegion, "01:23:4F");
        assertEquals("12.34", layout.readFormatted().toString());
    }

    @Test
    public void canTruncateLongerItemsUnsignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(2, 2, Comp3SignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        layout.set("1234.56");
        assertMemory(memoryRegion, "03:45:6F");
        assertEquals("34.56", layout.readFormatted().toString());
    }

    @Test
    public void canTruncateIntegersUnsignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(2, 2, Comp3SignType.UNSIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        layout.set("1234");
        assertMemory(memoryRegion, "03:40:0F");
        assertEquals("34.0", layout.readFormatted().toString());
    }

    @Test
    public void canPerformArithmeticOperationsWithUnsignedComp3Data() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(2, 2, Comp3SignType.UNSIGNED);
        MemoryRegion memoryRegion1 = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryRegion memoryRegion2 = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryRegion resultMemoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout1 = new MemoryLayout(memoryRegion1.fullAccess(), comp3lDataTypeSpec);
        MemoryLayout layout2 = new MemoryLayout(memoryRegion2.fullAccess(), comp3lDataTypeSpec);
        MemoryLayout resultLayout = new MemoryLayout(resultMemoryRegion.fullAccess(), comp3lDataTypeSpec);
        layout1.set("12.34");
        layout2.set("12.34");
        double sum = (Double) layout1.readFormatted() + (Double) layout2.readFormatted();
        resultLayout.set(String.valueOf(sum));
        assertEquals("24.68", resultLayout.readFormatted().toString());
        assertEquals(2468.0, resultLayout.read());
    }

    @Test
    public void canReadSignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(3, 2, Comp3SignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        memoryRegion.write(HexMapper.asBytes(ImmutableList.of(0x01, 0x23, 0x4D)));
        assertEquals(-1234.0, layout.read());
    }

    @Test
    public void canWritePositiveAndNegativeSignedComp3Item() {
        Comp3lDataTypeSpec comp3lDataTypeSpec = new Comp3lDataTypeSpec(2, 2, Comp3SignType.SIGNED);
        MemoryRegion memoryRegion = new MemoryRegion(comp3lDataTypeSpec.sizeInBytes());
        MemoryLayout layout = new MemoryLayout(memoryRegion.fullAccess(), comp3lDataTypeSpec);
        layout.set("-12.34");
        assertMemory(memoryRegion, "01:23:4D");
        assertEquals(-1234.0, layout.read());
        layout.set("12.34");
        assertMemory(memoryRegion, "01:23:4C");
        assertEquals(1234.0, layout.read());
    }
}
