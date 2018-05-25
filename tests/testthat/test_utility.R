context("Reading Run Report")

test_that("Loading of valid data", {
  expect_error(readSeqWareTSV("./test_utility_data/180406_valid.tsv"), NA)
})

test_that("Loading of invalid data is handled gracefully", {
  expect_error(readSeqWareTSV("./test_utility_data/180406_invalid.tsv"))

  # fread() cleaned up bug does not happen
  # https://github.com/Rdatatable/data.table/issues/2904
  expect_error(readSeqWareTSV("./test_utility_data/180406_valid.tsv"), NA)
})
