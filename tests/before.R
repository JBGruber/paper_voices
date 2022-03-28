library(testthat)

source('../misc/functions.R')

test_that("check for missing packages", {
  expect_true({
    install_missing()
  })
})

test_that("force spell check", {
  expect_true({
    custom_spell_check(c("../paper/abstract.md", "../paper/abstract.md"))
  })
})

test_that("citations correct", {
  expect_true({
    update_bib()
  })
})
