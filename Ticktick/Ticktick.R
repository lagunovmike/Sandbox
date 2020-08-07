library(lubridate)
library(tidyverse)
library(DBI)
library(dbplyr)

#### Reading data
fileDest <- str_sort(list.files("Ticktick/backups", full.names = TRUE), 
                     decreasing = TRUE)[1]
ticktick <- read_csv(fileDest, skip = 3, locale = locale(encoding = "UTF-8"))
id <- c(seq(1:nrow(ticktick)))
ticktick <- cbind(id,ticktick)
names(ticktick)[1] <- "ID"
names(ticktick) <- gsub(" ", "", names(ticktick))

### Connect to the db
db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db")

#################################
### Check whether csv equal to the db, otherwise append new entries
db_nrow <- dbGetQuery(db, "SELECT COUNT(ID) as count FROM ttdemo")
if(nrow(ticktick) != db_nrow$count){
    dbID <- dbGetQuery(db,"SELECT ID FROM ttdemo");
    # Look for changes
    new_short <- setdiff(ticktick$ID, dbID$ID)
    new_full <- filter(ticktick, ID %in% new_short)
    ## Append into the db
    dbAppendTable(db, name = "ttdemo", value = new_full)
} else{
    cat("Database is up to date")
}

# Get last month tasks
lastm_q <- dbSendQuery(db, "
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (DueDate, 'unixepoch') as dd,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ttdemo
          WHERE cr >= strftime('%Y-%m-%d', ?)
          OR com >= strftime('%Y-%m-%d', ?)
          OR dd >= strftime('%Y-%m-%d', ?)",
                       params = rep(as.character(today()-months(1)),3))
lastm <- dbFetch(lastm_q)
RSQLite::dbClearResult(lastm_q)
dbDisconnect(db)

# Handling DueDate issue
# Sometimes DueDate is smaller than the creation date
lastm$dd <- as_datetime(lastm$dd)
for(i in 1:nrow(lastm)){
    if(!is.na(lastm$dd[i])){
        if(date(ymd_hms(lastm$dd[i])) < date(ymd_hms(lastm$cr[i]))){
            lastm$dd[i] <- ymd_hms(lastm$dd[i]) + days(1)
        }
    }
}
lastmonth <- tibble(lastm) %>%
    rename(CreatedTime = cr, DueDate = dd, CompletedTime = com) %>%
    mutate(CreatedTime = ymd_hms(CreatedTime), 
           DueDate = ymd_hms(DueDate), 
           CompletedTime = ymd_hms(CompletedTime)) %>%
    mutate(overdue = if_else(date(CompletedTime) <= date(DueDate), "on time", 
                             if_else(date(CompletedTime) > date(DueDate), 
                                     "overdue", "not finished"))) %>%
    mutate(IsCompleted = ifelse(is.na(CompletedTime), 
                                 "not completed", "nompleted")) %>%
    mutate(Priority = factor(Priority, levels = c("0", "1", "3", "5"),
                            labels = c("None", "Low", "Medium", "High")))
    
lastmonth$Priority <- factor(lastmonth$Priority, levels = c(0,1,3,5),
                             labels = c(0 = "None", 1 = "Low", 3 = "Medium", 5 = "High"))
lastmonth
# Completion Rate
tasks <- nrow(lastmonth)
completed <- sum(!is.na(lastmonth$completed.time))
prop <- round(completed/tasks,2)
c(tasks = tasks, completed = completed, prop = prop)

tapply(lastmonth$completed.time, lastmonth$priority, function(x){sum(!is.na(x))})


