test_that("WC works", {
  expect_no_condition(makePlots(space1 = Bikes$space1, settings = list(plotType = "WC", x="hum", y="temp", k=4, metric="euclidean", linkage="ward.D2", WCa=0.5, showalpha=TRUE),cov = cov(Bikes$space1), space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res,"Residual")))
})

test_that("tour works",{
  expect_no_condition(makePlots(space1 = Bikes$space1, settings = list(plotType = "tour", k=4, metric="euclidean", linkage="ward.D2", tourspace="space1", colouring="clustering", out_dim=2, tour_path="grand", display="scatter",
                                                                       radial_start=NULL, radial_var=NULL, slice_width=NULL, seed=2025),cov = cov(Bikes$space1), space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res,"Residual")))
})

test_that("obs works", {
  expect_no_condition(makePlots(space1 = Bikes$space1, settings = list(plotType = "Obs", x="hum", y="temp", obs="A1", k=4, metric="euclidean", linkage="ward.D2"),cov = cov(Bikes$space1), space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res,"Residual")))
})

test_that("dimRed works", {
  expect_no_condition(makePlots(space1 = Bikes$space1, settings = list(plotType = "dimRed", k=4, metric="euclidean", linkage="ward.D2", colouring="clustering", dimspace = "space1", dimReduction = tSNE, algorithm = "tSNE", user_group = NULL, seed=2025),cov = cov(Bikes$space1), space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res,"Residual")))
})
