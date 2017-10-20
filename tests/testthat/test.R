library(TrajDataMining)
context("Test of main methods")



test_that("Test speed filter", {
  print ("teste1")
  expect_lt(length(speedFilter(A1,0.01)),length(A1))
  
  
})

test_that("Test douglasPeucker method", {
  # expect_type(A1,"S4")
  print ("teste2")
  expect_lt(length(douglasPeucker(A1,110792.3)),length(A1))
  
})

test_that ("Test owMeratniaByCollection",{
  print ("teste3")
  expect_lt(length(owMeratniaByCollection(tracksCollection,13804.84 ,0.03182201)),length(A1))
  
})

test_that ("Test OwMeratnialBy",{
  print ("teste4")
  expect_lt(length(owMeratniaBy(A1,110792.3,585.6829)), length(A1))
  
})

test_that ("Test speedCluster", {
  
  print ("teste5")
  # expect_equal(class( speedCluster(A1,1.408736,0.1,586)),class(""))
  expect_type(speedCluster(A1,1.408736,0.1,586),"list")
})
test_that("Test DirectionCluster",{
  print ("teste6")
  expect_type(directionCluster(A1,0,0,359),"list")
  
})




