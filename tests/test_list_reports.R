context("Loading Run Report files")
source("../list_reports.R")

test_that("Loading of clean data", {
  dt <- listRunReports("./test_list_reports_data/good")
  
  expect_equal(nrow(dt), 2)
  expect_equal(
    sort(colnames(dt)),
    c("dir_path", "file", "name", "path")
  )
})

test_that("Name duplication is handled gracefully", {
  dt <- listRunReports("./test_list_reports_data/duplicated") 
  expect_equal(nrow(dt), 2)
  expect_false(dt$name[1] == dt$name[2])
})