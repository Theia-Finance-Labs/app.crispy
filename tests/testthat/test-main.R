box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / main[...],
)

setwd(fs::path("..", "..")) # need to change the working directory to access the data

test_that("main server works", {
  testServer(server, {
    expect_true(nrow(backend_crispy_data) > 0)
  })
})
