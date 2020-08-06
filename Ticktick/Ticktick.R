library(lubridate)
library(tidyverse)
library(DBI)
library(dbplyr)

#### Reading data
fileDest <- str_sort(list.files("Ticktick/backups", full.names = TRUE), 
                     decreasing = TRUE)[1]
ticktick <- read_csv(fileDest, skip = 3, locale = locale(encoding = "UTF-8"))
names(ticktick) <-make.names(tolower(names(ticktick)))

#################################
#### Backup into the DB
#################################


db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db", encoding = "UTF-8")
dbticktick <- dbGetQuery(db, "
                     SELECT title, \"created.time\"
                     FROM tt_demo4"); 
names(dbticktick)[2] <- "created.time"
sapply(ticktick_srt, class)
dbListTables(db)
dbListFields(db, "tt_demo4")
# Look for changes
ticktick_srt <- select(ticktick, title, created.time) %>%
  rename(created.time = created.time, title = title) %>%
  mutate(created.time = as.numeric(created.time))
new_short <- setdiff(ticktick_srt, dbticktick)
new_full <- filter(ticktick, created.time == as.POSIXct(new_short$created.time, origin = "1970-01-01", tz = "UTC"))
names(new_full) <- dbListFields(db,"ticktick")[-1]

## Append into the db
dbAppendTable(db, name = "tt_demo", value = new_full)
dbDisconnect(db)

# Get last month tasks
remote_(tt)
dbListFields(db, "ticktick")
tt <- tbl(db, "ticktick")

tt %>% 
  select(Title, DueDate, CreatedTime, CompletedTime) %>%
  filter(as.POSIXct(CreatedTime, origin = "1970-01-01") < "2020-01-01")

lastmonth <- ticktick %>%
  select(title, priority, created.time, completed.time) %>%
  filter(created.time >= today() - months(1) | completed.time >= today() - months(1)) %>%
  mutate(is.completed = ifelse(is.na(completed.time), 
                                          "Not completed", "Completed"))
lastmonth$priority <- factor(lastmonth$priority, levels = c(0,1,3,5),
       labels = c("None", "Low", "Medium", "High"))
# Completion Rate
tasks <- nrow(lastmonth)
completed <- sum(!is.na(lastmonth$completed.time))
prop <- round(completed/tasks,2)
c(tasks = tasks, completed = completed, prop = prop)

tapply(lastmonth$completed.time, lastmonth$priority, function(x){sum(!is.na(x))})


