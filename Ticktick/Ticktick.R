library(lubridate)
library(tidyverse)
library(DBI)
#### Reading data
fileDest <- str_sort(list.files("Ticktick/backups", full.names = TRUE), 
                     decreasing = TRUE)[1]
ticktick <- read_csv(fileDest, skip = 3, locale = locale(encoding = "UTF-8"))
names(ticktick) <-make.names(tolower(names(ticktick)))

#### Backup into the DB

db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db", encoding = "UTF-8")
dbticktick <- (dbGetQuery(db, "
                     SELECT Title, CreatedTime
                     FROM ticktick"))

# Look for changes
ticktick_srt <- select(ticktick, title, created.time) %>%
  rename(CreatedTime = created.time, Title = title) %>%
  mutate(CreatedTime = as.numeric(CreatedTime))

which(ticktick == setdiff(ticktick_srt, dbticktick))



dbDisconnect(db)

# Get last month tasks
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


