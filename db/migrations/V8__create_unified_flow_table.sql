create table UNIFIED_FLOW (
                        ID integer primary key autoincrement,
                        PROGRAM_NAME integer,
                        PROJECT_ID integer,
                        BODY TEXT not null,
                        DATE_CREATED TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (PROJECT_ID) REFERENCES PROJECT(ID)
);
