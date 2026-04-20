test_that("WC works", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(plotType = "WC", x = "hum", y = "temp", k = 4, metric = "euclidean", linkage = "ward.D2", WCa = 0.5, showalpha = TRUE), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("WC works with makeResults", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(plotType = "WC", x = "hum", y = "temp", WCa = 0.5, showalpha = TRUE), linked = Bikes$space2, results = makeResults(cluster = Bikes$space1, settings = list(k = 4, metric = "euclidean", linkage = "ward.D2"), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual"))))
})

test_that("tour works", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(
    plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2", tourspace = "space1", colouring = "clustering", out_dim = 2, tour_path = "grand", display = "scatter",
    radial_start = NULL, radial_var = NULL, slice_width = NULL, seed = 2025
  ), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("obs works", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(plotType = "Obs", x = "hum", y = "temp", obs = "A1", k = 4, metric = "euclidean", linkage = "ward.D2"), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("dimRed works", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(plotType = "dimRed", k = 4, metric = "euclidean", linkage = "ward.D2", colouring = "clustering", dimspace = "space1", dimReduction = tSNE, algorithm = "tSNE", user_group = NULL, seed = 2025), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("radial tour works with random basis", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(
    plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2", tourspace = "space1", colouring = "clustering", out_dim = 2, tour_path = "radial", display = "scatter",
    radial_start = "random", radial_var = 1, slice_width = NULL, seed = 2025
  ), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("radial tour works with ppi basis", {
  expect_message(makePlots(cluster = Bikes$space1, settings = list(
    plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2", tourspace = "space1", colouring = "bins", out_dim = 2, tour_path = "radial", display = "scatter",
    radial_start = "pda", radial_var = c(1,4), slice_width = NULL, seed = 2025
  ), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")), regexp = "Converting input data to the required matrix format.")
})

test_that("tour final frame works", {
  expect_no_condition(makePlots(cluster = Bikes$space1, settings = list(
    plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2", tourspace = "space1", colouring = "clustering", out_dim = 2, tour_path = "grand", display = "scatter",
    radial_start = "random", radial_var = 1, slice_width = NULL, seed = 2025, final_frame = TRUE
  ), cov = cov(Bikes$space1), linked = Bikes$space2, getScore = outsideScore(Bikes$other$res, "Residual")))
})

test_that("stats",{
  expect_no_condition(makePlots(cluster = Bikes$space1, linked = Bikes$space2, exp = data.frame(value = colMeans(Bikes$space1)), covInv = solve(cov(Bikes$space1)), settings = list(plotType = "ch",  k = 4, metric = "euclidean", linkage = "ward.D2")))
})
