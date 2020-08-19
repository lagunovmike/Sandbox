# Libraries
library(httr) #connecting to api
library(jsonlite) #parsing json
library(lubridate) #date converting
library(DBI) #connecting to the db
library(dplyr)
library(ggplot2)
library(zoo)

Sys.setlocale("LC_CTYPE", "russian") #solve encoding issues

db <- dbConnect(RSQLite::SQLite(), "news.db")
db_data <- dbGetQuery(db, "SELECT * FROM tjournal")
dbDisconnect(db)
db_data$pub_date <- as_datetime(db_data$pub_date, tz = "Europe/Moscow")

##########
# pubs by day
pubs_by_day <- db_data %>%
    select(title, pub_date) %>%
    filter(title != "Статья удалена") %>%
    group_by(date = date(pub_date)) %>%
    summarise(pubs = n(), .groups = "drop") %>%
    mutate(rollmean = rollmean(pubs, 30, fill = "extend")) %>%
    arrange(date)

# plot "pubs per day"
ggplot(pubs_by_day, aes(date, pubs)) +
    geom_point() +
    geom_line(aes(date, rollmean), size = .2, color = "red") +
    geom_hline(aes(yintercept = mean(pubs)), 
               linetype = "longdash", color = "orange", size = 1) +
    ggtitle("Non-deleted publications per day") +
    labs(x = "Date", y = "Amount", color = "") +
    theme_minimal()


##########
# views per week
hits_by_week <- db_data %>%
    select(title, pub_date, hits) %>%
    filter(title != "Статья удалена", year(pub_date) >= "2019") %>%
    group_by(year_week = floor_date(pub_date, "1 week", week_start = 1)) %>%
    summarise(tot_hits = sum(hits), .groups = "drop") %>%
    arrange(year_week)
qplot(x = year_week,y =  tot_hits,data =  hits_by_week)

##########
# tot views per day
hits_by_day <- db_data %>%
    select(title, pub_date, hits) %>%
    filter(title != "Статья удалена") %>%
    group_by(date = date(pub_date)) %>%
    summarise(tot_hits = sum(hits), .groups = "drop") %>%
    arrange(date)

## plot "views per day"
ggplot(hits_by_day, aes(date, tot_hits)) +
    geom_point() +
    geom_hline(aes(yintercept = mean(tot_hits)), 
               linetype = "longdash", color = "orange", size = 1) +
    geom_smooth(method = "gam", na.rm = TRUE) +
    ggtitle("Total daily views on non-deleted publications") +
    coord_cartesian(ylim = c(0, 1000000)) +
    labs(x = "Date", y = "Amount", color = "") +
    theme_minimal()


by_month <- by_day %>%
    filter(year(day) == "2018" | year(day) == "2019") %>%
    group_by(month = month(day, label = TRUE)) %>%
    summarise(count = mean(count), .groups = "drop")
qplot(by_month$month, by_month$count)
    
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

##########
# By hour across days
by_hms <- db_data %>%
    group_by(pub_hour = hour(pub_date), pub_minute = minute(pub_date),
            ) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(pub_hour)%>%
    summarize(average = mean(count), .groups = "drop")

by_hms2 <- db_data %>%
    select(pub_date) %>%
    mutate(quarter_hour = strftime(pub_date, "%H:%M:%S", tz = "Europe/Moscow")) %>%
    mutate(comb = paste(rep("1970-01-01", length(quarter_hour)), 
                        quarter_hour)) %>%
    mutate(comb_floor = floor_date(ymd_hms(comb), "5 mins")) %>%
    group_by(comb_floor, minute(comb)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(comb_floor) %>%
    summarise(average = mean(count), .groups = "drop")
#par(mfrow = c(1,1), mar = c(2,1,2,1))
plot(by_hms2, pch = 19, type = "l", col = "blue")
points(x = by_hms2$comb_floor, y = by_hms2$average, cex = .5)
text(x = by_hms2$comb_floor, y = by_hms2$average, 
     labels = strftime(by_hms2$comb_floor, "%H:%M"), cex = .8)
# previous
plot(by_hms$pub_hour, by_hms$average, pch = 19, 
     xlab = "Час", 
     ylab = "Количество",
     main = "Количество постов по часам")
lines(by_hms$pub_hour, by_hms$average)

ggplot(by_hms, aes(pub_hour, average)) +
    geom_line(size = 2, color = "blue") +
    geom_point(size = 3) +
    #geom_smooth(se = FALSE, size = 2) +
    xlab("Час") +
    ylab("Количество") +
    ggtitle("Количество постов по часам за сутки") +
    theme_minimal()

plot(db_data$id, db_data$pub_date, type = "p", xlab = "Post ID", ylab = "Date", 
     xlim = c(126000, max(db_data$id)), ylim = c(as.POSIXct("2019-11-01"), max(db_data$pub_date)))
abline(h = as.POSIXct("2020-01-01"), lty = "dashed", col = "orange")



# who post on round hour

editoral <- db_data %>%
    select(pub_date, isEditoral) %>%
    mutate(pub_date = floor_date(pub_date, "minute"),
           isEditoral = as.logical(isEditoral)) %>%
    filter(minute(pub_date) == 0, second(pub_date) == 0) %>%
    group_by(year(pub_date)) %>%
    summarize(mean(isEditoral, na.rm = TRUE), .groups = "drop")
editoral


editoral2 <- db_data %>%
    select(pub_date, isEditoral) %>%
    mutate(pub_date = date(pub_date), entry = 1)


# Correlation between hits and comments
hitcom <- db_data %>%
    filter(hits >= quantile(hits, 0.25), comment_count >= quantile(comment_count, 0.25))
hit_countLM <- lm(comment_count ~ hits, hitcom)
summary(hit_countLM)
hitQuant <- quantile(db_data$hits, .95)
comQuant <- quantile(db_data$comment_count, .95)
cor(hitcom$hits, hitcom$comment_count)

ggplot(hitcom, aes(hits, comment_count)) +
    geom_point(alpha = 0.01,) +
    geom_smooth(method = lm, color = "blue") +
    coord_cartesian(xlim = c(0, hitQuant), ylim = c(0, comQuant))