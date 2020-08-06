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
db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db", encoding = "UTF-8")
dbWriteTable(db, "tt_demo4", ticktick[1:10,])
dbExecute(db, "CREATE TABLE tt_demo AS SELECT * FROM ticktick")
dbExecute(db, "DROP TABLE tt_demo3")
dbDisconnect(db)
