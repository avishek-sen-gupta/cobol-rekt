package org.smojol.api.pipeline;

import org.jetbrains.annotations.NotNull;
import org.jooq.Field;
import org.jooq.Record;
import org.jooq.Table;

import static org.jooq.impl.DSL.field;
import static org.jooq.impl.DSL.table;

public class DbConstants {
    public static final String PROJECT_ID = "PROJECT_ID";
    public static final @NotNull Field<String> PROGRAM_NAME_FIELD = field("PROGRAM_NAME", String.class);
    public static final @NotNull Field<Long> PROJECT_ID_FIELD = field(PROJECT_ID, Long.class);
    public static final @NotNull Field<Long> ID_FIELD = field("ID", Long.class);
    public static final @NotNull Field<Long> CFG_ID_FIELD = field("CFG_ID", Long.class);
    public static final @NotNull Field<String> BODY_FIELD = field("BODY", String.class);
    public static final @NotNull Table<Record> IR_AST = table("IR_AST");
    public static final @NotNull Table<Record> IR_CFG = table("IR_CFG");
    public static final @NotNull Table<Record> PROJECT = table("PROJECT");
    public static final @NotNull Table<Record> LOOP_BODY = table("LOOP_BODY");
    public static final @NotNull Table<Record> T1_T2_ANALYSIS = table("T1_T2_ANALYSIS");
    public static final @NotNull Table<Record> UNIFIED_FLOW = table("UNIFIED_FLOW");
}
