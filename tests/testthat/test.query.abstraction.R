context("Query abstraction")

test_that("query.flatten.results null results",{  
  # 
  
  test1 <- query.flatten.results(list())
  test1.ids <- test1$results[, test1$idfield, drop = TRUE]
  
  expect_equal(nrow(test1.ids), 0)
  expect_null(test1$idfield)
  
})

test_that("query.flatten.results non-null results", {
  sample.res <- list()
  sample.res[["x"]] <- data.frame(a=1, b=2, c=3)
  sample.res[["y"]] <- data.frame(a=11, b=12, c=13)
  sample.res[["z"]] <- data.frame(a=21, b=22, c=23)
  exp.out <- list(results = data.frame(a=c(1,11,21), b=c(2,12,22), c=c(3,13,23), test=c("x","y","z"), stringsAsFactors = FALSE), idfield = "test")
  test.val <- query.flatten.results(sample.res, idfield.proposed = "test")
  expect_equal(test.val, exp.out)
  
  #name collision
  exp.out2 <- list(results = data.frame(a=c(1,11,21), b=c(2,12,22), c=c(3,13,23), a1=c("x","y","z"), stringsAsFactors = FALSE), idfield = "a1")
  test.val2 <- query.flatten.results(sample.res, idfield.proposed = "a")
  expect_equal(test.val2, exp.out2)
  
})