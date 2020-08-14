# Libraries
library(httr) #connecting to api
library(jsonlite) #parsing json
library(lubridate) #date converting
library(DBI) #connecting to the db
Sys.setlocale("LC_CTYPE", "russian") #solve encoding issues

### FUNCTIONS

# Api connecting
tj_api <- function(path){
    url <- modify_url("https://api.tjournal.ru/", path = path)
    resp <- GET(url)
    
    ### Fixing Errors
    # look for the 429 error
    if(grepl("429 Too Many", resp) == TRUE){
        cat("\n","429 error, sleeping", "\n")
        Sys.sleep(120)
        cat("\n")
        #try again
        resp <- GET(url)
    }
    # look for the error 500
    if(grepl("технические работы", resp) == TRUE){
        return(500)
        break
    }
    
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
    # set counters and a progress bar
    count_to_db <- 0
    count_sleep <- 0
    count_toomany <- 0
    pb <- txtProgressBar(min = 0, max = 200, initial = 0)
    
    for (i in 1:n){
        setTxtProgressBar(pb,count_to_db)
        
        # sleep for a while every n entries
        if(count_sleep == 500){
            cat("\n", "resting")
            Sys.sleep(18)
            cat("\n")
            count_sleep <- 1
        }
        
        link <- paste0("v1.8/entry/", i+offset)
        get_entry <- tj_api(link)
        
        if(class(get_entry) == "numeric"){
            if(get_entry == 500){
                cat("\n","error: 500, skipping ID", "\n")
                next
            } else{
                cat("\n","some new error", "\n")
                next
            }
        }
        
        # skip if 404
        if(get_entry$response$status_code == 404){
            count_sleep <- count_sleep + 1
            Sys.sleep(0.4)
            next
        } 
        
        # data fetching
        id <- get_entry$content$result$id
        title <- get_entry$content$result$title
        brief <- get_entry$content$result$summarize
        link <- get_entry$content$result$url
        pub_date <- get_entry$content$result$date
        hits <- get_entry$content$result$hitsCount
        comment_count <- get_entry$content$result$commentsCount
        favorite <- get_entry$content$result$favoritesCount
        isEditoral <- get_entry$content$result$isEditorial
        isRepost <- get_entry$content$result$isRepost
        
        # appending to a list
        entries[[i]] <- c(id = id, title = title, brief = brief, link = link,
                          pub_date = pub_date, hits = hits, 
                          comment_count = comment_count, favorite = favorite, 
                          isEditoral = isEditoral, isRepost = isRepost
                          )
        # send to the db every n entries
        if(count_to_db == 30){
            count_to_db <- 0
            collection <- do.call(dplyr::bind_rows, entries)
            entries <- list()
            db <- dbConnect(RSQLite::SQLite(), dbname = "news.db")
            dbAppendTable(db, name = "tjournal", value = collection)
            dbDisconnect(db)
            Sys.sleep(1)
            cat("   Done: ",i)
        }
        
        # raise counters
        count_to_db <- count_to_db + 1
        count_sleep <- count_sleep + 1
        Sys.sleep(0.05)
        
    }
    cat("\n","Completed!")
}
### END FUNCTION


latest_news <- tj_api("v1.8/timeline/index/recent")
latest_id <- max(latest_news$content$result$id)

# Get info before start
db <- dbConnect(RSQLite::SQLite(), dbname = "news.db")
max_id_db <- as.vector(dbGetQuery(db, "SELECT MAX(id) as max FROM tjournal")$max)
dbDisconnect(db)

# Place info
cat("Total:",  latest_id)
cat("Started id:", max_id_db, "- Finished prop:", round(max_id_db/latest_id,2))

my_collection <- get_collection(sum(latest_id,-max_id_db), offset = max_id_db)
