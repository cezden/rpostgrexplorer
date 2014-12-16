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
