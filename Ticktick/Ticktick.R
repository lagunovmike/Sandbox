library(lubridate)
library(tidyverse)
library(DBI)
library(zoo)
source("Ticktick/functions.R")

#### Reading data
latest_backup <- read_latest()
# Update database
updateDB()

# Get data from a period ("all" or "last month")
tt_db <- getdata(period = "all")

### Manipulation
ticktick <- tibble(tt_db) %>%
    rename(CreatedTime = cr, DueDate = dd, CompletedTime = com) %>%
    mutate(CreatedTime = ymd_hms(CreatedTime), 
           DueDate = ymd_hms(DueDate), 
           CompletedTime = ymd_hms(CompletedTime)) %>%
    mutate(Overdue = if_else(date(CompletedTime) <= date(DueDate), "on time", 
                             if_else(date(CompletedTime) > date(DueDate), 
                                     "overdue", "not finished"))) %>%
    mutate(IsCompleted = ifelse(is.na(CompletedTime), 
                                 "not completed", "nompleted")) %>%
    mutate(Priority = factor(Priority, levels = c("0", "1", "3", "5"),
                            labels = c("None", "Low", "Medium", "High"))) %>%
    mutate(ComplDelay = date(CompletedTime) - date(DueDate))


### Tasks per month    
tasks_by_y <- ticktick %>%
    select(Title, Priority, DueDate, CreatedTime, ComplDelay) %>%
    group_by(year = year(CreatedTime), month = month(CreatedTime)) %>%
    summarise(Tasks = n(), .groups = "drop") %>%
    unite(YearMon, year:month, remove = TRUE, sep = "-") %>%
    mutate(YearMon = as.yearmon(YearMon))
tasks_by_y
# Plot
ggplot(tasks_by_y, aes(YearMon, Tasks)) +
    geom_col(fill = "purple") +
    theme_minimal()

### Average completed tasks per day
avg_by_day <- ticktick %>%
    select(CompletedTime) %>%
    filter(!is.na(CompletedTime)) %>%
    group_by(t_date = date(CompletedTime)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(year = year(t_date)) %>%
    summarise(avg_done_day = mean(count), .groups = "drop")
avg_by_day
# plot
ggplot(avg_by_day, aes(year, avg_done_day)) +
    geom_col(fill = "#386baf") +
    xlab("Year") +
    ylab ("Count") +
    ggtitle("Average finished tasks per day") +
    theme_minimal()
    

### Completion Rate
tasks <- nrow(ticktick)
completed <- sum(!is.na(ticktick$CompletedTime))
prop <- round(completed/tasks,2)
c(tasks = tasks, completed = completed, prop = prop)

tapply(ticktick$CompletedTime, ticktick$Priority, function(x){sum(!is.na(x))})


