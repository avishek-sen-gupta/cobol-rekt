create table PROJECT (
                        ID integer primary key autoincrement,
                        NAME varchar(100) not null,
                        DATE_CREATED TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
