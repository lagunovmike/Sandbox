library(RSQLite)
library(DBI)
options("encoding" = "UTF-8")

db <- dbConnect(SQLite(), dbname = "Ticktick/ticktickDB.db", encoding = "UTF-8")
dbListTables(db)
dbListFields(db, "ticktick")

fromDB_tick <- dbGetQuery()
a_data <- as_tibble(dbGetQuery(db, "
                     SELECT *
                     FROM Tasks"))
head(a_data)
rs <- dbSendQuery(db, 
        "INSERT INTO Tasks ('Title', 'Priority', 'Created.time', 'Completed.time')
        VALUES (?,?,?,?);
        ")
rs
dbBind(rs, list(lastmonth$title, lastmonth$priority, lastmonth$created.time, 
       lastmonth$completed.time))
dbClearResult(rs)
dbDisconnect(db)




###
names(a_data) <- names(lastmonth)
a_data <- slice(a_data, -nrow(a_data))
setdiff(lastmonth$created.time, a_data$Created.time)











###
db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db")
dbExecute(db, "CREATE TABLE tt_demo AS SELECT * FROM ticktick")
dbExecute(db, "DROP TABLE tt_demo4")

dbWriteTable(db, "ttdemo", ticktick[1:10,])


dbExecute(db, "
          CREATE TABLE ttdemo (
    id               INTEGER  PRIMARY KEY AUTOINCREMENT
                              UNIQUE
                              NOT NULL,
    [folder.name]    INTEGER,
    [list.name]      TEXT,
    title            TEXT,
    tags             TEXT,
    content          TEXT,
    [is.check.list]  TEXT,
    [start.date]     DATETIME,
    [due.date]       DATETIME,
    reminder         TEXT,
    [repeat.]        INTEGER,
    priority         REAL,
    status           REAL,
    [created.time]   DATETIME,
    [completed.time] DATETIME,
    [order]          REAL,
    timezone         TEXT,
    [is.all.day]     INTEGER,
    [is.floating]    INTEGER,
    [column.name]    TEXT,
    [column.order]   REAL,
    [view.mode]      TEXT,
    taskid           REAL,
    parentid         INTEGER
);

          ")


lastm_q <- dbSendQuery(db, "
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ttdemo
          WHERE cr >= strftime('%Y-%m-%d', ?)
          OR com >= strftime('%Y-%m-%d', ?)", 
                       params = rep(as.character(today()-months(1)),2))
lastm <- dbFetch(lastm_q)
as.tibble(lastm)
RSQLite::dbClearResult(db)
dbDisconnect(db)
