test_that("get_sname() works", {
  sname <- get_sname(snr = "0001")
  expect_length(sname, 1)
  expect_type(sname, "character")
})


test_that("get_rmd() works", {
  rmd_code <- get_rmd(x_seq = 1:3)
  rmd_split <- strsplit(rmd_code, "(?=```\\{r)", perl = TRUE)
  expect_length(rmd_split[[1]], 18)
})


test_that("generate_rmd() works", {
  rmdtxt <- generate_rmd(x_seq = 1:3, ubb = FALSE, export = FALSE)
  rmd_split <- strsplit(rmdtxt, "(?=```\\{r)", perl = TRUE)
  expect_length(rmd_split[[1]], 21)

  rmdtxt <- generate_rmd(x_seq = 1:3, ubb = TRUE, export = FALSE)
  not_UBB <- grepl("Unterrichtsbeobachtung", rmdtxt)
  expect_true(not_UBB)
})









