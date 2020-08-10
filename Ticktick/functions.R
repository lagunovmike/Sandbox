read_latest <- function(path = "Ticktick/backups"){
    fileDest <- str_sort(list.files(path, full.names = TRUE), 
                         decreasing = TRUE)[1]
    ticktick <- read_csv(fileDest, skip = 3, locale = locale(encoding = "UTF-8"))
    names(ticktick) <- gsub(" ", "", names(ticktick))
    cat("Done")
    ticktick <- select(ticktick, -taskId, -parentId)
    return(ticktick)
}


updateDB <- function(backupFile = latest_backup, db_path = "Ticktick/ticktickDB.db"){
    db <- dbConnect(RSQLite::SQLite(), dbname = db_path)
    db_nrow <- dbGetQuery(db, "SELECT COUNT(taskId) as count FROM ticktick")
    if(nrow(backupFile) > db_nrow$count){
        dbID <- dbGetQuery(db,"SELECT * FROM ticktick");
        backupFile$StartDate <- unclass(backupFile$StartDate)
        backupFile$DueDate <- unclass(backupFile$DueDate)
        backupFile$CreatedTime <- unclass(backupFile$CreatedTime)
        backupFile$CompletedTime <- unclass(backupFile$CompletedTime)
        # Look for changes
        new_full <- setdiff(backupFile, dbID[,-1])
        #new_full <- filter(backupFile, CreatedTime %in% new_short)
        ## Append into the db
        dbAppendTable(db, name = "ticktick", value = new_full)
        dbDisconnect(db)
        cat(paste("Database updated:", nrow(new_full)), "new enrties")
    } else{
        cat("Database is up to date")
    }
}


getdata <- function(period = "all", db_path = "Ticktick/ticktickDB.db"){
    db <- dbConnect(RSQLite::SQLite(), dbname = db_path)
    if(period == "all"){
        getdata_q <- dbSendQuery(db,"
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (DueDate, 'unixepoch') as dd,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ticktick;")
    } else if (period == "last month"){
        getdata_q <- dbSendQuery(db,"
          SELECT
          Title,
          Priority,
          DATETIME (CreatedTime, 'unixepoch') as cr,
          DATETIME (DueDate, 'unixepoch') as dd,
          DATETIME (CompletedTime, 'unixepoch') as com
          FROM ticktick
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