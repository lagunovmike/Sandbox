spend <- read.csv("expences.csv")
spend <- spend[complete.cases(spend),]
spend$Date <- as.Date(spend$Date)
spend$Mood <- factor(spend$Mood, labels = c("bad", "meh", "good", "rad"))

library(ggplot2)
g <- ggplot(subset(spend, !is.na(Mood)), aes(Mood, Expences, color = Mood)) +
  geom_boxplot() +
  ylim(0,100) +
  geom_hline(yintercept =  median(spend$Expences), col = "orange") +
  theme_light() +
  labs(title = "Траты от настроения")
g


library(lattice)
plot(spend$Date, spend$Expences)
xyplot(spend$Expences ~ spend$Date | spend$Mood, layout = c(4,1), panel = function(x,y, ...){
  panel.xyplot(x,y,...)
  panel.abline(h = mean(spend$Expences), lty = 2)
})

xyplot(spend$Expences ~ spend$Mood)

qplot(Expences, data = spend, fill = Mood)

qplot(Date, Expences, data = spend, geom = c("point", "smooth"), ylim = c(0,120))
### Is there any difference on weekends

library(lubridate)
# expences by weekdays by month
qplot(wday(Date), Expences, data = spend, facets = .~ month(Date))

# same but without month
qplot(wday(Date), Expences, data = spend, ylim = c(0,100))

tapply(spend$Expences, wday(spend$Date), mean)
sub_weekend <- subset(spend, wday(spend$Date) > 5)$Expences
sub_workday <- subset(spend, wday(spend$Date) <= 5)$Expences
par(mfrow = c(1,2))
hist(sub_workday)
hist(sub_weekend)
c(mean(sub_workday), mean(sub_weekend))

