create table T1_T2_ANALYSIS (
                        ID integer primary key autoincrement,
                        CFG_ID integer,
                        IS_REDUCIBLE BOOLEAN not null CHECK (IS_REDUCIBLE IN (0, 1)),
                        LIMIT_FLOW_GRAPH TEXT not null,
                        DATE_CREATED TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (CFG_ID) REFERENCES IR_CFG(ID)
);
