#' ---
#' title: Отчет за неделю от TJournal
#' author: ""
#' output: 
#'   html_document: 
#'     df_print: kable
#' ---
#' 
## ----set-options, include = FALSE--------
Sys.setlocale("LC_CTYPE", "russian")

#' 
## ---- results='asis', include = FALSE----
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(kableExtra)



#' 
#' 
## ---- include = FALSE---------------
tj_api <- function(path){
    url <- modify_url("https://api.tjournal.ru/", path = path)
    resp <- GET(url)
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
    
    structure(
        list(
            content = parsed,
            path = path,
            response = resp
        ),
        class = "TJ_api"
    )
}


print.tj_api <- function(x, ...) {
    cat("<tj_api ", x$path, ">\n", sep = "")
    str(x$content)
    invisible(x)
}

#' 
## ---- echo = F--------------------------------------------------------------------------
get_popular <- function(period = "week"){
    url <- paste0("v1.8/timeline/mainpage/", period)
    mainpage <- tj_api(url)
    titles <- sapply(mainpage$content$result, function(x){x$title})
    hits <- sapply(mainpage$content$result, function(x){x$hitsCount})
    link <- sapply(mainpage$content$result, function(x){x$url})
    link_md <- paste("[link]","(",link,")",sep="")
    title_md <- paste0("[", titles, "]", "(", link, ")")
    img <- sapply(mainpage$content$result, function(x){x$cover$url})
    img_type <- sapply(mainpage$content$result, 
                       function(x){x$cover$additionalData$type})
    img_md <- c()
    for(i in 1:length(img)){
      if(img[i] != "NULL"){
        if(img_type[i] == "jpg" | img_type[i] == "png"){
          img_md[i] <- paste("!","[image]","(", img[i], ")", sep = "")
        } else {
          img_md[i] <- ""
        }
      } else{
         img_md[i] <- ""
      }
    }
    pub_date <- sapply(mainpage$content$result, function(x){x$date})
    brief <- sapply(mainpage$content$result, function(x){x$summarize})
    id <- seq()
    together <- tibble(title = title_md,
                       summary = brief,
                       hits = hits, 
                       image = img_md,
                       date = date(as_datetime(pub_date))) %>%
      arrange(desc(hits)) %>%
      mutate(id = seq_along(title)) %>%
      relocate(id)
    return(together)
}
together <- get_popular()
together <- slice(together, 1:10)

#' 
#' 
#' ### От `r day(min(together$date))` до `r day(max(together$date))` `r month(max(together$date),label = TRUE, locale = Sys.setlocale("LC_CTYPE", "russian"))`
#' 
## ---- echo = F--------------------------------------------------------------------------
kable(together, col.names = c("Номер",
                              "Заголовок",
                              "Краткое содержание",
                              "Просмотры",
                              "Картинка",
                              "Дата"),) %>%
  kable_styling(bootstrap_options = c("striped"), fixed_thead = T)

#+ eval=FALSE, include = FALSE
rmarkdown::render("C:/Projects/DataScience/Sandbox/TJournal/tj_report_script.r")