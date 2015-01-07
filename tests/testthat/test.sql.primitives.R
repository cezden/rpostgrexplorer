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

test_that("sql.fill.template", {
  test1 <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%QUERY%%"=c("bca", "bcb"), "%%DICTKEY%%"=c("abc","abd")))
  test1.exp <- structure(list(q1 = "select \n  abc as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bca) \ngroup by abc", 
                              q2 = "select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bcb) \ngroup by abd"), 
                         .Names = c("q1", "q2"))
  expect_equal(test1, test1.exp)
  test1.b <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%DICTKEY%%"=c("abc","abd"), "%%QUERY%%"=c("bca", "bcb")))
  expect_equal(test1.b, test1.exp)
  
  
  test2 <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%QUERY%%"=c("bcb"), "%%DICTKEY%%"=c("abc","abd")))
  test2.exp <- structure(list(q1 = "select \n  abc as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bcb) \ngroup by abc", 
                              q2 = "select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bcb) \ngroup by abd"),
                         .Names = c("q1", "q2"))
  expect_equal(test2, test2.exp)
  test2.b <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%DICTKEY%%"=c("abc","abd"), "%%QUERY%%"=c("bcb")))
  expect_equal(test2.b, test2.exp)
  
  
  test3 <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%QUERY%%"=c("bca","bcb"), "%%DICTKEY%%"=c("abd")))
  test3.exp <- structure(list(q1 = "select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bca) \ngroup by abd", 
                              q2 = "select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bcb) \ngroup by abd"),
                         .Names = c("q1", "q2"))
  expect_equal(test3, test3.exp)
  
  test3.c <- sql.fill.template("dictionary_values", query.names=c("q1","q2"), list("%%QUERY%%"=c("bca","bcb"), "%%DICTKEY%%"="abd"))
  expect_equal(test3.c, test3.exp)
  
  test4 <- sql.fill.template("dictionary_values", query.names="q1", list("%%QUERY%%"="bca", "%%DICTKEY%%"="abd"))
  test4.exp <- structure(list(q1 = "select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bca) \ngroup by abd"), .Names = "q1")
  expect_equal(test4, test4.exp)
  
  test4.b <- sql.fill.template("dictionary_values", query.names="q1", list("%%DICTKEY%%"="abd", "%%QUERY%%"="bca"))
  expect_equal(test4.b, test4.exp)
  
  test4.c <- sql.fill.template("dictionary_values", query.names=c("q1"), list("%%DICTKEY%%"=c("abd"), "%%QUERY%%"=c("bca")))
  expect_equal(test4.c, test4.exp)
  
  test4.d <- sql.fill.template("dictionary_values", query.names=c("q1"), list("%%DICTKEY%%"="abd", "%%QUERY%%"=c("bca")))
  expect_equal(test4.d, test4.exp)

  test5 <- sql.fill.template("dictionary_values", query.names=NULL, list("%%DICTKEY%%"="abd", "%%QUERY%%"=c("bca")))
  test5.exp <- list("select \n  abd as dictkey, \n  count(*) as dictkeycount \nfrom \n  (bca) \ngroup by abd")
  expect_equal(test5, test5.exp)
  
  
})