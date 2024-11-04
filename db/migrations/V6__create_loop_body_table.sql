create table LOOP_BODY (
                        ID integer primary key autoincrement,
                        PROGRAM_NAME varchar(100) not null,
                        PROJECT_ID integer,
                        BODY TEXT not null,
                        DATE_CREATED TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        FOREIGN KEY (PROJECT_ID) REFERENCES PROJECT(ID)
);
