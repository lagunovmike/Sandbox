read_latest <- function(path = "Ticktick/backups"){
    fileDest <- str_sort(list.files(path, full.names = TRUE), 
                         decreasing = TRUE)[1]
    ticktick <- read_csv(fileDest, skip = 3, locale = locale(encoding = "UTF-8"))
    id <- c(seq(1:nrow(ticktick)))
    ticktick <- cbind(id,ticktick)
    names(ticktick)[1] <- "ID"
    names(ticktick) <- gsub(" ", "", names(ticktick))
    cat("Done")
    return(ticktick)
}


updateDB <- function(backupFile = latest_backup){
    db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db")
    db_nrow <- dbGetQuery(db, "SELECT COUNT(ID) as count FROM ttdemo")
    if(nrow(backupFile) != db_nrow$count){
        dbID <- dbGetQuery(db,"SELECT ID FROM ttdemo");
        # Look for changes
        new_short <- setdiff(backupFile$ID, dbID$ID)
        new_full <- filter(backupFile, ID %in% new_short)
        ## Append into the db
        dbAppendTable(db, name = "ttdemo", value = new_full)
        dbDisconnect(db)
        cat(paste("Updated:", length(new_short), "new enrties"))
    } else{
        cat("Database is up to date")
    }
}


getdata <- function(period = "all"){
    db <- dbConnect(RSQLite::SQLite(), dbname = "Ticktick/ticktickDB.db")
    if(period == "all"){
        getdata_q <- dbSendQuery(db,"
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (DueDate, 'unixepoch') as dd,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ttdemo;")
    } else if (period == "last month"){
        getdata_q <- dbSendQuery(db,"
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (DueDate, 'unixepoch') as dd,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ttdemo
          WHERE cr >= strftime('%Y-%m-%d', ?)
          OR com >= strftime('%Y-%m-%d', ?)
          OR dd >= strftime('%Y-%m-%d', ?);
                             ",
                                 params = rep(as.character(today()-months(1)),3))
    } else {
        stop("Wrong period!")
    }
    tt_data <- dbFetch(getdata_q)
    
    # Fixing DueDate issue
    tt_data$dd <- as_datetime(tt_data$dd)
    tt_data$cr <- as_datetime(tt_data$cr)
    tt_data$com <- as_datetime(tt_data$com)
    for(i in 1:nrow(tt_data)){
        if(!is.na(tt_data$dd[i])){
            if(date(ymd_hms(tt_data$dd[i])) < date(ymd_hms(tt_data$cr[i]))){
                tt_data$dd[i] <- ymd_hms(tt_data$dd[i]) + days(1)
            }
        }
    }
    cat(paste(period, "data retrieved"))
    return(tt_data)
    RSQLite::dbClearResult(getdata_q)
    dbDisconnect(db)
}