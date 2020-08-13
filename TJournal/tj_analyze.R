# Libraries
library(httr) #connecting to api
library(jsonlite) #parsing json
library(lubridate) #date converting
library(DBI) #connecting to the db
library(dplyr)
library(zoo)
Sys.setlocale("LC_CTYPE", "russian") #solve encoding issues

db <- dbConnect(RSQLite::SQLite(), "news.db")
db_data <- dbGetQuery(db, "SELECT * FROM tjournal")
dbDisconnect(db)
db_data$pub_date <- as_datetime(db_data$pub_date, tz = "Europe/Moscow")
pub_date <- date(db_data$pub_date)

b_date <- tapply(pub_date, date(pub_date), length)

by_day <- data.frame(day = unique(pub_date), count = b_date, row.names = seq_along(b_date))
by_day <- arrange(by_day, day) %>%
    mutate(rollmean = round(rollmean(x = count, k = 7, fill = "extend"),2))

with(by_day, plot(day, count))
with(by_day, lines(day, rollmean, col = "red"))
abline(h = mean(by_day$count), col = "blue")

# check a peak in the middle of 2015
alot <- by_day[which.max(by_day$count),]$day
alot_row <- filter(db_data, date(pub_date) == alot)
# answer: some spam, deleted articles

summary(by_day$count)
sd(by_day$count)
hist(by_day$count)
# check whether there are days with zero entries
di <- diff(unique(pub_date[order(pub_date)]))
unique(di)[-(di==1)]

# deleted articles

deleted <- filter(db_data, title == "Статья удалена")
nrow(deleted)/nrow(db_data)

not_del <- filter(db_data, title != "Статья удалена") %>%
    group_by(date(pub_date)) %>%
    summarize(count_d = n(), .groups = "drop")

hist(not_del$count_d)

by_wday <- db_data %>%
    group_by(wday = wday(pub_date, label = TRUE, week_start = 1, abbr = FALSE)) %>%
    summarize(count_wday = n(), .groups = "drop") %>%
    mutate(wday = as.factor(wday))
barplot(by_wday$count_wday~by_wday$wday)

by_hms <- db_data %>%
    group_by(pub_time = hour(pub_date)) %>%
    summarize(count = n(), .groups = "drop")


plot(by_hms$pub_time, by_hms$count, pch = 19)
lines(by_hms$pub_time, by_hms$count)
