---
title: "4pda_report"
author: "Michael Lagunov"
date: "8/19/2020"
output: html_document
---
```{r}
library(rvest)
library(utf8)
Sys.setlocale("LC_ALL", "russian") #solve encoding issues
wsite <- read_html("https://4pda.ru", encoding = "latin1")
rvest::repair_encoding(wsite)
rvest::guess_encoding(wsite)
a <- wsite %>%
    html_nodes(".list-post-title") %>%
    html_text()
repair_encoding(a)
a
```

