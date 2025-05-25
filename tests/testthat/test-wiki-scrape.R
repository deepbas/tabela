# tests/testthat/test-wiki-scrape.R

library(testthat)

# Verify scraping logic against the 2012 Summer Olympics Wikipedia page
test_that("scrape_page_data works on Wiki Olympics page", {
  skip_on_cran()
  url <- "https://en.wikipedia.org/wiki/2012_Summer_Olympics"

  # Expect at least one HTML table on that page
  res <- scrape_page_data(url, exts = character(), list_tables = TRUE)
  expect_type(res, "list")
  expect_true(length(res$tables) >= 1)
  expect_s3_class(res$tables[[1]], "data.frame")

  # Ensure filter_links_by_ext never errors
  links <- get_all_links(init_session(url))
  expect_silent(filter_links_by_ext(links, exts = c("csv","txt")))
})

