create table LOOP_BODY (
                        ID integer primary key autoincrement,
                        PROGRAM_NAME varchar(100) not null,
                        CFG_ID integer,
                        BODY TEXT not null,
                        DATE_CREATED TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (CFG_ID) REFERENCES IR_CFG(ID)
);
