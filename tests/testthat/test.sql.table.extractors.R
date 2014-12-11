context("SQL construction")

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

test_that("relationship query consistency",{  
  #
  t1 <- sql.entity.relation.simple(
      tab1.name = "T1", 
      tab2.name = "T2", 
      tab1.groupping.col = "col", 
      tab2.groupping.col = "col", 
      schemaname1 = "s1", 
      schemaname2 = "s1"
      )

  t1.tst <- sql.entity.relation.simple(
    tab1.name = "T1", 
    tab2.name = "T2", 
    tab1.groupping.col = "col", 
    schemaname1 = "s1"
  )
  
  expect_equal( t1, t1.tst)
  

  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2")
  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2", schemaname1 = "S1")
  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2", schemaname1 = "S1", schemaname2 = "S2")
  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", schemaname1 = "S1", schemaname2 = "S2")
  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", schemaname1 = "S1")
  #sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1")
  
  
})


