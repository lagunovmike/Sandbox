library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

tj_api <- function(path){
    url <- modify_url("https://api.tjournal.ru/", path = path)
    resp <- GET(url)
    parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE,
                                 flatten = TRUE)
    
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
    cat("<TJournal ", x$path, ">\n", sep = "")
    str(x$content)
    invisible(x)
}

tj_api("v1.8/timeline/mainpage?count=3")

mainpage$content$result[[1]]$

get_popular <- function(period = "month"){
    url <- paste0("v1.8/timeline/mainpage/", period)
    mainpage <- tj_api(url)
    titles <- sapply(mainpage$content$result, function(x){x$title})
    titles <- as_utf8(titles)
    hits <- sapply(mainpage$content$result, function(x){x$hitsCount})
    link <- sapply(mainpage$content$result, function(x){x$url})
    link_md <- paste("[link]","(",link,")",sep="")
    pub_date <- sapply(mainpage$content$result, function(x){x$date})
    together <- data.frame(title = titles,
                           hits = hits, 
                           date = date(as_datetime(pub_date)), 
                           link = link_md)
    return(together)
}
debug(get_popular)
get_popular()


myhtml <- read_html("https://leonardo.osnova.io/b99409d2-0dcf-a04e-44da-f05d34ff0651/")

myhtml %>% 
    html_node("video") %>%
    html_text()


con <- url("https://leonardo.osnova.io/98c43002-c14e-97f5-a770-ef292c12f2f6/")
htmlCode <- readLines(con)
htmlTreeParse("https://leonardo.osnova.io/b99409d2-0dcf-a04e-44da-f05d34ff0651/")


# get latest news ID
latest_news <- tj_api("v1.8/news/default/recent")
latest_id <- latest_news$content$result[[1]]$id

entries <- tj_api('v1.8/entry/197670')
entries$content$result$id


get_collection <- function(n = 10){
    entries <- list()
    counter <- 0
    for (i in 1:n){
        link <- paste0("v1.8/entry/", i)
        get_entry <- tj_api(link)
        if(get_entry$response$status_code == 404) next
        id <- get_entry$content$result$id
        title <- get_entry$content$result$title
        brief <- get_entry$content$result$summarize
        link <- get_entry$content$result$url
        link <- paste("[link]","(",link,")",sep="")
        pub_date <- as_datetime(get_entry$content$result$date)
        hits <- get_entry$content$result$hitsCount
        entries[[i]] <- c(id = id, title = title, brief = brief, link = link,
                          pub_date = pub_date, hits = hits)
        
        if(counter == 10){
            print(i)
            counter <- 0
        }
        counter <- counter + 1
        
    }
    collection <- do.call(dplyr::bind_rows, entries)
    return(collection)
}

start_time <- Sys.time()
my_collection <- get_collection(1000)
finish_time <- Sys.time()
round(finish_time - start_time)
