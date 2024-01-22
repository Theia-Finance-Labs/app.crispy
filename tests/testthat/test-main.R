box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / main[...],
)

test_that("main server works without error", {
  setwd(file.path("..", ".."))
  expect_error(testServer(server, {

  }), NA)
})
