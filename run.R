# Metadata ---------------------------------------------------------------------
# Emily Markowitz' Website
# Developed by Emily Markowitz, for myself
# August 2021
#

# Libraries --------------------------------------------------------------------

PKG <- c(
  # "distill",
  "glue",
  # "kableExtra",
  # "formattable",
  # "flextable",
  "rmarkdown",
  "dplyr",
  "magrittr",
  "pagedown",
  "readr",
  "readxl",
  "xml2" # check urls
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}


# Knowns -----------------------------------------------------------------------

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Functions --------------------------------------------------------------------

secttitle <- function(txt = "", img = NULL) {

  if (!is.null(img)){
    str <- paste0("## ![](./images/",img,"){width='50px'} *",txt,"*")
  } else {
    str <- paste0("<br>

                ## *",txt,"*")
  }
  return(str)
}

group_rows <- function (dat, col) {

  groups <- data.frame()
  gr0 <- as.character(unlist(unique(dat[,col])))

  for (i in 1:length(gr0)) {
    gr <- gr0[i]
    gr_r <- which(dat[,col] == gr)
    if (length(gr_r) == 1) {
      gr_r <- c(gr_r, gr_r)
    } else {
      gr_r <- c(gr_r[1], gr_r[length(gr_r)])
    }
    groups <- rbind.data.frame(groups,
                               data.frame(group = gr,
                                          st = gr_r[1],
                                          end = gr_r[2]))
  }

  return(groups)
}


#' Check that your links work
#'
#' @param URLs A vector of strings with website URLs, local directories, and/or local files.
#' @param quiet default = FALSE. Will not return messages if = TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' # Use test page URL:
#'   URLs <- c(
#'     "https://github.com",
#'     "http://steipe.biochemistry.utoronto.ca/abc/assets/testCheckLinks.html",
#'     "./",
#'     "./ex.txt",
#'     "./aa/")
#'  checkLinks(URLs)
checkLinks <- function(URLs,
                       quiet = FALSE) {

  # https://stackoverflow.com/questions/31214361/what-is-the-r-equivalent-for-excel-iferror

  URLs <- URLs[!is.na(URLs)] # remove empty rows

  notworking <- c()

  for (i in 1:length(URLs)){

    # Fix URLs if local directories
    URL <- URLs[i]

    if (substr(x = URL, start = 1, stop = 2) == "./") {
      if (URL == "./") {
        URL <- gsub(replacement = getwd(), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      } else {
        URL <- gsub(replacement = paste0(getwd(), "/"), pattern = "./",
                    x = URL, fixed = TRUE, useBytes = TRUE)
      }

    }

    if (substr(x = URL, start = nchar(URL), stop = nchar(URL)) == "/") {
      URL <- substr(x = URL, start = 1, stop = (nchar(URL)-1))
    }

    # download page and access URLs. If does not exist, collect it in "badurl"
    myPage <- try(expr = xml2::read_html(URL), silent = T)
    if (grepl(pattern = "Error", x = myPage[1],
              fixed = TRUE, useBytes = TRUE) &
        isFALSE(file.exists(URL))) {

      notworking <- c(notworking, URL)
    }
  }

  if (quiet == FALSE){
    if (length(notworking) == 0) {
      print("All links are good!")
    } else {
      print(paste0("There ",
                   ifelse(length(notworking)>1, "are ", "is "),
                   length(notworking),
                   " bad link",
                   ifelse(length(notworking)>1, "s", ""),
                   "."))
    }
  }

  return(notworking)
}



copyedit <- function(format,
                     pattern,
                     x) {
  for (i in 1:length(pattern)){
    if (format == "italics"){
      xx <- gsub(pattern = pattern[i],
                 replacement = paste0("*", pattern[i], "*"),
                 x = x)
    }

    if (format == "bold"){
      xx <- gsub(pattern = pattern[i],
                 replacement = paste0("**", pattern[i], "**"),
                 x = x)
    }
  }
  return(xx)
}


# Sign into google drive -------------------------------------------------------

library(googledrive)
googledrive::drive_deauth()
googledrive::drive_auth()
1

# googledrive::drive_download(
#   file = as_id("1fWyNAjxYFugM44LVqqVdmrndaRDWWBmEERcNrGMKnVI"),
#   overwrite = TRUE,
#   path = "./data/data_cv"
# )

# cv0 <- read_csv(paste0(getwd(), "/data/data_cv.csv"))
# cv0 <- readxl::read_xlsx(path = "data/data_cv.xlsx",
#                          sheet = "entries",
#                          skip = 1)

# # Since you seem to be using R, here is an R-based solution. You need the {readxl}, {purrr}, and {readr} packages. I am “namespacing” all functions, so you know, where they are from. I am using the example Excel file from {readxl}.
# path_to_xlsx <- "data/data_cv.xlsx"
# # This Excel file has 4 sheets. The names of the sheets are read by excel_sheets.
# sheet_names <- readxl::excel_sheets(path_to_xlsx)
# # Now we import all excel sheets into one list.
# sheets <- purrr::map(sheet_names, ~ readxl::read_excel(path_to_xlsx, sheet = .x))
# # We get a list of 4 data.frames or tibbles. Let’s name them.
# base::names(sheets) <- sheet_names
# # Now export all tibbles from the list to separate CSVs in one go.
# purrr::iwalk(sheets, ~ readr::write_excel_csv2(x = .x,
#                                                file = paste0("data/", .y, ".csv")))

# Load Data --------------------------------------------------------------------

source("cv/functions_cv.R")
cv_data <- create_CV_object(
  data_location = "https://docs.google.com/spreadsheets/d/1RM6K0sWYGGlWkFTHQHR9Y8rGPGPscVSYHWf4fSJDz1g",
  cache_data = FALSE)

dat0 <- cv_data$entries_data

# Edit Data --------------------------------------------------------------------


# Check links
# checkLinks(cv$img)
# checkLinks(cv$url)
# checkLinks(cv$url1)

# Make formatting edits that are difficult to keep in a CSV

dat0 <- dat0 %>%
  dplyr::mutate(public_cv = as.logical(public_cv)) %>%
  dplyr::mutate(website = as.logical(website)) %>%
  dplyr::mutate(custom_cv = as.logical(custom_cv)) %>%
# Images
  dplyr::mutate(images = ifelse(is.na(img), "",
                              paste0("![*",img_txt,"*](",img,"){width='400px'}"))) %>%
  # URL links
  dplyr::mutate(Links = ifelse(is.na(url), "",
                               paste0("[", url_txt,"](",url,")"))) %>%
  dplyr::mutate(Links = ifelse(is.na(url1), Links,
                               paste0(Links, " \n\n [", url_txt1,"](",url1,")")))

dat0$description_1 <- copyedit(
  format = "bold",
  pattern = c("EH Markowitz", "Markowitz, EH"),
  x = dat0$description_1)

dat0$description_1 <- copyedit(
  format = "italics",
  pattern = c("et al."),
  x = dat0$description_1)

dat0$title <- copyedit(
  format = "italics",
  pattern = c(
    "Cum Laude",
    "R/V Oceanus",
    "R/V Seawolf",
    "NOAAS Gordon Gunter"),
  x = dat0$title)

# Clean up entries dataframe to format we need it for printing
dat0 <- dat0 %>%
  tidyr::unite(
    tidyr::starts_with('description'),
    col = "description_bullets",
    sep = "\n- ",
    remove = FALSE,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(description_bullets =
                   ifelse(description_bullets != "",
                          paste0("\n- ", description_bullets),
                          "")) %>%
  dplyr::mutate(timeline =
                  dplyr::case_when(
                    grepl(pattern = "[a-zA-Z]+", x = start) ~ start, # in review, in prep
                    is.na(start) ~ "",
                    is.na(end) ~ as.character(start),
                    start == end ~ as.character(start),
                    !is.na(start) & !is.na(end) ~
                      paste0(start, " - ", end)) ) %>%
  dplyr::arrange(desc((end))) %>%
  dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))

dat0$description_bullets <- copyedit(
  format = "italics",
  pattern = c("Paralichthys dentatus",
              "Centropristis striata",
              "Merluccius productus",
              "Disidicus gigas",
              "R/V Oceanus",
              "R/V Seawolf",
              "NOAAS Gordon Gunter"),
  x = dat0$description_bullets)

dat0 <- dat0 %>%
  dplyr::mutate(Links_inline =
                  ifelse(Links == "",
                         "",
                         paste0('Links: ', gsub(pattern = ' \n\n ',
                                     replacement = ', ', x = Links), ''))) %>%
  dplyr::arrange(desc(start))

# dat0$description_1[dat0$page %in% "papers"] <- NA
# dat0$description_2[dat0$page %in% "papers"] <- NA


cv_data$entries_data <- dat0

# Render CV --------------------------------------------------------------------

# package up data for use in CV!
source("cv/functions_cv.r")
readr::write_rds(cv_data, 'cv/cached_positions.rds')
cache_data <- TRUE

# Knit the HTML version
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = FALSE,
                                cache_data = cache_data),
                  output_file = "index.html")


# Knit the PDF version to temporary html location
tmp_html_cv_loc <- fs::file_temp(ext = ".html")
rmarkdown::render("cv/cv.rmd",
                  params = list(pdf_mode = TRUE, cache_data = cache_data),
                  output_file = tmp_html_cv_loc)

# Convert to PDF using Pagedown
pagedown::chrome_print(input = tmp_html_cv_loc,
                       output = "docs/markowitz_cv.pdf")

# Render Site ------------------------------------------------------------------

CV <- readr::write_rds(cv_data, 'cv/cached_positions.rds')
cv <-
  CV$entries_data <-
  CV$entries_data %>%
  dplyr::filter(website == TRUE)

sections <- c("about", "index", unique(cv$page))

sections <- sections[sections != "other"]

for (i in 1:length(sections)) {
  rmarkdown::render(paste0("./", sections[i], ".Rmd"),
                    output_dir = "./docs/",
                    output_file = paste0(sections[i], ".html"))
}




