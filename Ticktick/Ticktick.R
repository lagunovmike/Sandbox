library(lubridate)
library(tidyverse)
library(DBI)
source("Ticktick/functions.R")

#### Reading data
latest_backup <- read_latest()
# Update database
updateDB()

# Get data from a period ("all" or "last month")
tt_db <- getdata()

ticktick <- tibble(tt_db) %>%
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
                            labels = c("None", "Low", "Medium", "High"))) %>%
    mutate(ComplDelay = date(CompletedTime) - date(DueDate))
    
tasks_by_y <- ticktick %>%
    select(Title, Priority, DueDate, CreatedTime, ComplDelay) %>%
    group_by(year = year(CreatedTime), month = month(CreatedTime)) %>%
    summarise(Tasks = n(), .groups = "drop") %>%
    unite(YearMon, year:month, remove = TRUE, sep = "-") %>%
    mutate(YearMon = as.yearmon(YearMon))
tasks_by_y
plot(tasks_by_y, type = "l", col = "blue")
points(tasks_by_y, col = "blue", pch = 19)
text(x = tasks_by_y$YearMon,y = tasks_by_y$Tasks, labels = tasks_by_y$Tasks, pos = 3,
     col = "magenta", cex = 0.8)
# Completion Rate
tasks <- nrow(lastmonth)
completed <- sum(!is.na(lastmonth$completed.time))
prop <- round(completed/tasks,2)
c(tasks = tasks, completed = completed, prop = prop)

tapply(lastmonth$completed.time, lastmonth$priority, function(x){sum(!is.na(x))})


