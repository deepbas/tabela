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
#' @param exts Character vector of file extensions (without dot), e.g. c("csv","txt").
#' @return Character vector of filtered URLs.
#' @export
filter_links_by_ext <- function(links, exts = c("csv","txt","xlsx","sav","por","RData")) {
  links <- links[!is.na(links) & nzchar(links)]
  pattern <- paste0("\\.(", paste(exts, collapse="|"), ")$", ignore.case = TRUE)
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

#' Recursively crawl links up to a given depth
#'
#' @param session A polite session object.
#' @param start_url Character: the seed URL.
#' @param max_depth Integer: maximum link‐following depth.
#' @return Character vector of all discovered URLs (including start_url).
#' Recursively crawl up to max_depth within the same domain, skipping non-HTML
#' @noRd
crawl_links <- function(session, start_url, max_depth = 2) {
  library(httr); library(xml2); library(rvest)
  
  # Determine root domain
  root_host <- parse_url(start_url)$hostname
  
  visited <- character()
  queue   <- list(list(url = start_url, depth = 0L))
  result  <- character()
  
  while (length(queue) > 0) {
    node       <- queue[[1]]
    queue      <- queue[-1]
    url         <- node$url
    depth       <- node$depth
    
    # Skip if already seen or beyond depth
    if (depth > max_depth || url %in% visited) next
    visited    <- c(visited, url)
    result     <- c(result, url)
    
    # HEAD to check HTML content
    head_res   <- try(HEAD(url, user_agent = "deepak@domain.com"), silent = TRUE)
    if (inherits(head_res, "try-error") || status_code(head_res) >= 400) next
    ct         <- headers(head_res)[["content-type"]]
    if (!grepl("html", ct, ignore.case = TRUE)) next
    
    # Scrape page text
    page       <- try(nod(session, url) %>% scrape(), silent = TRUE)
    if (inherits(page, "try-error") || is.null(page)) next
    
    # Extract and resolve links
    hrefs      <- page %>% html_elements("a") %>% html_attr("href")
    hrefs      <- hrefs[!is.na(hrefs)]
    abs_links  <- url_absolute(hrefs, url)
    
    # 1. drop any NA or empty hrefs
    abs_links <- abs_links[!is.na(abs_links) & nzchar(abs_links)]
    # 2. keep only http(s) URLs
    abs_links <- abs_links[grepl("^https?://", abs_links)]

    # 3. filter to same hostname—use vapply so we get a logical vector
    same_host <- vapply(abs_links, function(u) {
      h <- tryCatch(parse_url(u)$hostname, error = function(e) NULL)
      !is.null(h) && identical(h, root_host)
    }, logical(1))

    abs_links <- abs_links[same_host]

    
    # Enqueue new URLs
    new_links  <- setdiff(unique(abs_links), visited)
    queue      <- c(queue, lapply(new_links, function(u) list(url=u, depth=depth+1L)))
  }
  
  result
}


#' Scrape and summarize downloadable files + (optional) HTML tables
#'
#' @param root_url Character. Page to scrape.
#' @param exts Character vector of extensions (no dot). Defaults include csv, txt, xlsx, sav, por, RData.
#' @param page Integer. Batch number for pagination (1 = first `page_size` links).
#' @param page_size Integer. Number of file-links per page. Default `10`.
#' @param list_tables Logical. If TRUE, also extract all HTML tables as data.frames.
#' @return A list with elements:
#'   * `$files`: tibble of file info (name,url,type,rows,cols,…)
#'   * `$tables` (optional): list of HTML tables
#' @export
scrape_page_data <- function(root_url, exts = c("csv"), page = 1, page_size = 10,
                             max_depth = 1, list_tables = FALSE, download_data = FALSE) {
  session   <- init_session(root_url)
  all_pages <- crawl_links(session, root_url, max_depth)

  all_links <- unique(unlist(lapply(all_pages, function(u) {
    pg <- try(nod(session, u) %>% scrape(), silent = TRUE)
    if (inherits(pg, "data.frame") || inherits(pg, "spec_tbl_df") ||
        inherits(pg, "try-error") || is.null(pg)) {
      return(character())
    }
    pg %>%
      html_elements("a") %>%
      html_attr("href") %>%
      xml2::url_absolute(u)
  })))

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
  
  if(download_data && nrow(files_df)>0) {
    # only CSVs here; you could branch by extension
    csv_urls  <- files_df %>% dplyr::filter(type=="csv") %>% dplyr::pull(url)
    csv_names <- files_df %>% dplyr::filter(type=="csv") %>% dplyr::pull(name)

    data_list <- purrr::map(csv_urls, ~ readr::read_csv(.x, show_col_types = FALSE))
    names(data_list) <- csv_names
    result$data <- data_list
  }

  if(list_tables) {
    page_html <- polite::nod(session, root_url) %>% polite::scrape()
    tbl_nodes <- rvest::html_elements(page_html, "table")
    tbls      <- purrr::map(tbl_nodes, ~ rvest::html_table(.x, fill = TRUE))
    result$tables <- tbls
    cat(sprintf("  → Found %d HTML tables on root page\n", length(tbls)))
  }

  invisible(result)
}

