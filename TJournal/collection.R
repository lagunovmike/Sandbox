library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(DBI)
Sys.setlocale("LC_CTYPE", "russian")

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


get_collection <- function(n = 10, offset = 0){
    
    entries <- list()
    count_to_db <- 0
    count_sleep <- 1
    pb <- txtProgressBar(min = 0, max = 500, initial = 0)
    for (i in 1:n){
        setTxtProgressBar(pb,count_to_db)
        if(count_sleep == 200){
            cat("\n", "resting")
            Sys.sleep(80)
            cat("\n")
            count_sleep <- 1
        }
        
        link <- paste0("v1.8/entry/", i+offset)
        get_entry <- tj_api(link)
        if(get_entry$response$status_code == 404){
            count_sleep <- count_sleep + 1
            next  
        } 
        id <- get_entry$content$result$id
        title <- get_entry$content$result$title
        brief <- get_entry$content$result$summarize
        link <- get_entry$content$result$url
        pub_date <- as_datetime(get_entry$content$result$date)
        hits <- get_entry$content$result$hitsCount
        comment_count <- get_entry$content$result$commentsCount
        favorite <- get_entry$content$result$favoritesCount
        isEditoral <- get_entry$content$result$isEditorial
        isRepost <- get_entry$content$result$isRepost
        entries[[i]] <- c(id = id, title = title, brief = brief, link = link,
                          pub_date = pub_date, hits = hits, 
                          comment_count = comment_count, favorite = favorite, 
                          isEditoral = isEditoral, isRepost = isRepost
                          )
        
        if(count_to_db == 30){
            count_to_db <- 0
            collection <- do.call(dplyr::bind_rows, entries)
            entries <- list()
            db <- dbConnect(RSQLite::SQLite(), dbname = "TJournal/news.db")
            dbAppendTable(db, name = "tjournal", value = collection)
            dbDisconnect(db)
            #flush.console()
            cat(" Done: ",i)
        }
        count_to_db <- count_to_db + 1
        count_sleep <- count_sleep + 1
        
    }
    collection <- do.call(dplyr::bind_rows, entries)
    return(collection)
}
latest_news <- tj_api("v1.8/news/default/recent")
latest_id <- latest_news$content$result$id[1]

db <- dbConnect(RSQLite::SQLite(), dbname = "TJournal/news.db")
max_id_db <- as.vector(dbGetQuery(db, "SELECT MAX(id) as max FROM tjournal")$max)
dbDisconnect(db)

cat("Total: ", max_id_db)
cat("Started id: ", latest_id, " ; Done: ", round(max_id_db/latest_id,2))
my_collection <- get_collection(sum(latest_id,-max_id_db), offset = max_id_db)
