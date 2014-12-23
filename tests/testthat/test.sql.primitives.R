context("SQL construction by primitives")

test_that("table scheming",{  
  # 
  expect_equal( sql.table.schemed(c("T1", "T2", "T3"), c(NA, "s1", "s2")) , 
                c("T1", "s1.T2", "s2.T3"))
  expect_equal( sql.table.schemed(c("T1", "T2", "T3"), c("", "s1", "s2")) , 
                c("T1", "s1.T2", "s2.T3"))
  expect_equal( sql.table.schemed("T1", NA), 
                "T1")
  expect_equal( sql.table.schemed("T1", ""), 
                "T1")
})

test_that("query.flatten.results", {
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