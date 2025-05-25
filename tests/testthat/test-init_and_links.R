# tests/testthat/test-init_and_links.R

library(testthat)
library(pkgname)   # replace pkgname with your package’s name

test_that("init_session() returns a polite bow_session", {
  skip_on_cran()
  skip_if_not_installed("polite")
  url <- "https://www.r-project.org"
  session <- init_session(url)
  expect_s3_class(session, "bow_session")
  expect_equal(session$url, url)
})

test_that("get_all_links() returns a character vector of URLs", {
  skip_on_cran()
  skip_if_not_installed("rvest")
  url     <- "https://www.r-project.org"
  session <- init_session(url)
  links   <- get_all_links(session)
  expect_type(links, "character")
  # should be absolute URLs
  expect_true(all(grepl("^https?://", links)))
})

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
