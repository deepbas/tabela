#' Initialize a polite scraping session for a given URL
#'
#' @param url Character. Root URL to crawl.
#' @return A polite session object.
#' @export
init_session <- function(url) {
  # bow() reads robots.txt and sets polite defaults
  polite::bow(url, user_agent = "deepbas99@gmail.com")
}

#' Extract all hyperlinks from a session
#'
#' @param session A polite session object.
#' @return Character vector of absolute URLs.
#' @export
get_all_links <- function(session) {
  page  <- polite::scrape(session)
  hrefs <- rvest::html_nodes(page, "a") %>% rvest::html_attr("href")
  xml2::url_absolute(hrefs, session$url)
}

#' Filter links by arbitrary extensions
#'
#' @param links Character vector of URLs.
#' @param exts  Character vector of file extensions (without dot), e.g. c("csv","txt").
#' @return Character vector of filtered URLs.
#' @export
filter_links_by_ext <- function(links,
  exts = c("csv","txt","xlsx","sav","por","RData")) {
links <- links[!is.na(links) & nzchar(links)]
pattern <- paste0("\\.(", paste(exts, collapse="|"), ")$")
stringr::str_subset(links, pattern)
}



#' Memoised extractor of file info for various types
#'
#' @param url Character: link to the file.
#' @return A named list: name, url, type, rows, cols or objects.
#' @export
get_file_info <- memoise::memoise(function(url) {
  name <- basename(url)
  ext  <- tolower(tools::file_ext(name))
  info <- list(name = name, url = url, type = ext)
  
  # CSV / TXT: count rows/cols
  if (ext %in% c("csv","txt")) {
    hdr  <- readr::read_delim(url, delim = ifelse(ext=="csv", ",", "\t"),
                              n_max = 0, show_col_types = FALSE)
    info$cols <- ncol(hdr)
    info$rows <- length(readr::read_lines(url)) - 1
  }
  # XLSX: read only sheet names then dims of first sheet
  else if (ext == "xlsx") {
    sheets <- readxl::excel_sheets(url)
    df0    <- readxl::read_excel(url, sheet = sheets[[1]], n_max = 0)
    info$sheets <- length(sheets)
    info$first_sheet <- sheets[[1]]
    info$cols  <- ncol(df0)
    info$rows  <- NA_integer_
  }
  # SPSS (.sav/.por)
  else if (ext %in% c("sav","por")) {
    dat <- foreign::read.spss(url, to.data.frame = TRUE)
    info$cols <- ncol(dat); info$rows <- nrow(dat)
  }
  # RData: load into temp env and list objects
  else if (ext == "RData") {
    e <- new.env()
    load(url, envir = e)
    objs <- ls(envir = e)
    info$objects <- objs
    info$n_objects <- length(objs)
  }
  # other: placeholder
  else {
    info$note <- "unsupported extension"
  }
  info
})

#' Scrape and summarize downloadable files + (optional) HTML tables
#'
#' @param root_url Character. Page to scrape.
#' @param exts Character vector of extensions (no dot). Defaults include csv, txt, xlsx, sav, por, RData.
#' @param page Integer. Batch number for pagination (1 = first page_size links).
#' @param page_size Integer. Number of file-links per page. Default 10.
#' @param list_tables Logical. If TRUE, also extract all HTML tables as data.frames.
#' @return A list with elements:
#'   * $files: tibble of file info (name,url,type,rows,cols,…)
#'   * $tables (optional): list of HTML tables
#' @export
scrape_page_data <- function(root_url,
                             exts       = c("csv","txt","xlsx","sav","por","RData"),
                             page       = 1,
                             page_size  = 10,
                             list_tables = FALSE) {
  session <- init_session(root_url)
  all_links <- get_all_links(session)
  files     <- unique(filter_links_by_ext(all_links, exts))
  files     <- sort(files)
  
  # pagination bounds
  start <- (page-1)*page_size + 1
  end   <- min(page*page_size, length(files))
  batch <- files[start:end]
  
  safe_info <- purrr::possibly(get_file_info, NULL)
  info_list <- purrr::map(batch, safe_info) %>% purrr::compact()
  files_df  <- dplyr::bind_rows(info_list) %>% tibble::as_tibble()
  
  result <- list(files = files_df)
  
  if (list_tables) {
    page_html <- polite::scrape(session)
    tbl_nodes <- rvest::html_elements(page_html, "table")
    tbls      <- purrr::map(tbl_nodes, ~ rvest::html_table(.x, fill = TRUE))
    result$tables <- tbls
    cat(sprintf("Scraped page %d (%d–%d of %d files)\n", page, start, end, length(files)))
    cat(sprintf("  → Found %d HTML tables on this page\n", length(tbls)))
  }

  invisible(result)
}