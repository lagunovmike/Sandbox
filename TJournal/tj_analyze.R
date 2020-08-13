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

pub_date <- date(as_datetime(db_data$pub_date))

b_date <- tapply(pub_date, date(pub_date), length)

comb <- data.frame(day = unique(pub_date), count = b_date, row.names = seq_along(b_date))
#comb <- comb[-which.max(comb$day),]
comb <- arrange(comb, day) %>%
    mutate(rollmean = rollmean(x = count, k = 7, fill = "extend"))

with(comb, plot(day, count))
with(comb, lines(day, rollmean, col = "red"))
abline(h = mean(comb$count), col = "blue")

# check a peak in the middle of 2015
alot <- comb[which.max(comb$count),]$day
alot_row <- filter(db_data, date(as_datetime(pub_date)) == alot)
# answer: some spam, deleted articles

summary(comb$count)
sd(comb$count)
hist(comb$count)
# check whether there are days with zero entries
di <- diff(unique(pub_date[order(pub_date)]))
unique(di)[-(di==1)]

# deleted articles

deleted <- filter(db_data, title == "Статья удалена")
nrow(deleted)/nrow(db_data)

not_del <- filter(db_data, title != "Статья удалена") %>%
    group_by(date(as_datetime(pub_date))) %>%
    summarize(count_d = n())

hist(not_del$count_d)    
