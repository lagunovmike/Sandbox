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
dbBind(rs, list(lastmonth$title, lastmonth$priority, lastmonth$created.time, 
       lastmonth$completed.time))
dbClearResult(rs)
dbDisconnect(db)




###
names(a_data) <- names(lastmonth)
a_data <- slice(a_data, -nrow(a_data))
setdiff(lastmonth$created.time, a_data$Created.time)
