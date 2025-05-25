# tests/testthat/test-init_and_links.R

library(testthat)

# ensure init_session returns the correct polite session class
test_that("init_session() returns a polite session", {
  skip_on_cran()
  skip_if_not_installed("polite")
  url     <- "https://www.r-project.org"
  session <- init_session(url)
  expect_s3_class(session, "session")
  expect_equal(session$url, url)
})

# validate that get_all_links returns absolute URLs
test_that("get_all_links() returns a character vector of URLs", {
  skip_on_cran()
  skip_if_not_installed("rvest")
  url     <- "https://www.r-project.org"
  session <- init_session(url)
  links   <- get_all_links(session)
  expect_type(links, "character")
  expect_true(all(grepl("^https?://", links)))
})

# confirm filter_links_by_ext filters only the specified extensions
test_that("filter_links_by_ext() correctly filters by extension", {
  urls <- c(
    "https://foo.com/data.csv",
    "https://foo.com/readme.txt",
    "https://foo.com/image.png",
    NA, ""
  )
  out <- filter_links_by_ext(urls, exts = c("csv","txt"))
  expect_equal(sort(out), sort(c(
    "https://foo.com/data.csv",
    "https://foo.com/readme.txt"
  )))
})


