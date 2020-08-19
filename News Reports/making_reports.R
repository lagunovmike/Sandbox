
# TJournal
knitr::purl("Tjournal/tj_report.Rmd", "Tjournal/tj_report_script2.r", 
            documentation = 2, quiet = TRUE)
rmarkdown::render("TJournal/tj_report_script2.r",
                  output_file = "../Reports/tj_report.html")
file.remove("Tjournal/tj_report_script2.r")


# VC
knitr::purl("VC/vc_report.Rmd", "VC/vc_report_script2.r", 
            documentation = 2, quiet = TRUE)
rmarkdown::render("VC/vc_report_script2.r",
                  output_file = "../Reports/vc_report.html")
file.remove("VC/vc_report_script2.r")

# Habr
knitr::purl("Habr/habr_weekly.Rmd", "Habr/habr_weekly_script2.r", 
            documentation = 2, quiet = TRUE)
rmarkdown::render("Habr/habr_weekly_script2.r",
                  output_file = "../Reports/habr_report.html")
file.remove("Habr/habr_weekly_script2.r")
