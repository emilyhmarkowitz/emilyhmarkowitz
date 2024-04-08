# Metadata ---------------------------------------------------------------------
# Emily Markowitz' Website
# Developed by Emily Markowitz, for myself
# April 2024
#

# source("./run.R")

# Libraries --------------------------------------------------------------------

PKG <- c(
  "glue",
  "leaflet",
  "leafpop",
  "digest",
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

# Load Data --------------------------------------------------------------------

source("cv/functions_cv.R")
cv_data <- create_CV_object(
  data_location = "https://docs.google.com/spreadsheets/d/1Mm2yJiEy4LAdWRrXClkJeiVeP2OBxeeDLZ3Yzm7d1WI",
  cache_data = FALSE)

dat0 <- cv_data$entries_data

# Edit Data --------------------------------------------------------------------

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

# Check links
# checkLinks(cv$img)
# checkLinks(cv$url)
# checkLinks(cv$url1)

sections <- c("about", "index", unique(cv$page))

sections <- sections[sections != "other"]

for (i in 1:length(sections)) {
  rmarkdown::render(paste0("./", sections[i], ".Rmd"),
                    output_dir = "./docs/",
                    output_file = paste0(sections[i], ".html"))
}




