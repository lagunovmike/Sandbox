#' ---
#' title: "Habr weekly"
#' author: "Michael Lagunov"
#' ---
#' 
#+ include = "FALSE"
library(rvest)
library(tidyverse, quietly = TRUE)
library(kableExtra, quietly = TRUE)
library(lubridate)
library(knitr)
Sys.setlocale("LC_CTYPE", "russian") #solve encoding issues

#+ include = "FALSE"
habr <- read_html("https://habr.com/ru/top/weekly/")

images <- habr %>%
    html_nodes(".post__text") %>%
    map_df(~list(images = html_nodes(.x, "img") %>%
                     html_attr("src") %>%
                     {if(length(.) == 0) NA else .}))
images$images

link <- habr %>%
    html_nodes(".post__title > a") %>%
    html_attr("href")


parsed <- habr %>%
    html_nodes(".shortcuts_items") %>%
    map_df(~list(id = seq(1:20),
                 title = html_nodes(.x, ".post__title_link") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 views = html_nodes(.x, ".post-stats__views-count") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 brief = html_nodes(.x, ".post__text_v1") %>%
                     html_text() %>%
                     {if (length(.) == 0) NA else .},
                 image = images$images))
parsed$image <- sapply(parsed$image, function(x){
    if(!is.na(x)){
        paste("!","[image]","(", x, ")", sep = "")
    } else{
        ""
    }
})

for(i in 1:length(link)){
    parsed$title[i] <- paste0("[", parsed$title[i], "]", "(", link[i], ")")
}
#+ echo=FALSE
kable(parsed, col.names = c("Номер", "Заголовок", 
                            "Просмотры", "Вступление", "Картинка")) %>%
    kable_styling()

#+ eval=FALSE, include = FALSE
rmarkdown::render("report_script.R")
