testData <- datasets::iris
names(testData) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species")
testData$Num <- 1:150

test_that("principal: summary table (eigenvalues, variance explained)", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )
    summary <- r$summaryTable$asDF
    expect_equal(unname(summary$eigenvalue), c(2.918497817, 0.9140304715, 0.1467568756, 0.02071483643), tolerance = 1e-6)
    expect_equal(unname(summary$initVarProp), c(0.7296244541, 0.2285076179, 0.03668921889, 0.005178709107), tolerance = 1e-6)
    expect_equal(unname(summary$initVarCum), c(0.7296244541, 0.958132072, 0.9948212909, 1), tolerance = 1e-6)
})

test_that("principal: loading table", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showLoadings = TRUE
    )
    loadings <- r$loadingTable$asDF
    expect_equal(unname(loadings$var), c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"))
    expect_equal(unname(loadings$`loading:1`), c(0.8901687649, -0.4601427064, 0.9915551834, 0.9649789607), tolerance = 1e-6)
    expect_equal(unname(loadings$`loading:2`), c(0.3608298881, 0.8827162692, 0.02341518838, 0.06399984704), tolerance = 1e-6)
    expect_equal(unname(loadings$QLT), c(0.9225986381, 0.9909193221, 0.9837299528, 0.935280375), tolerance = 1e-6)
})

test_that("principal: KMO and Bartlett's test", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showKMO = TRUE
    )
    kmo <- r$kmoTable$asDF
    expect_equal(unname(kmo$test), c("Bartlett's Test of Sphericity", "Kaiser-Meyer-Olkin Measure of Sampling Adequacy (MSA)"))
    expect_equal(unname(kmo$statistic), c(706.959243, 0.540076675), tolerance = 1e-6)
    expect_equal(unname(kmo$df), c(6, NA))
})

test_that("principal: observation table (first 5 observations)", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showObservations = TRUE
    )
    obs <- head(r$obsTable$asDF, 5)
    expect_equal(unname(obs$obs), c("1", "2", "3", "4", "5"))
    expect_equal(unname(as.character(obs$group)), c("setosa", "setosa", "setosa", "setosa", "setosa"))
    expect_equal(unname(obs$`1`), c(-2.257141176, -2.074013015, -2.356335112, -2.291706786, -2.381862704), tolerance = 1e-6)
    expect_equal(unname(obs$`2`), c(0.4784238321, -0.671882687, -0.3407664246, -0.5953998627, 0.6446756594), tolerance = 1e-6)
    expect_equal(unname(obs$qlt), c(0.9968578292, 0.9864649801, 0.9995167432, 0.9977577125, 0.9997491324), tolerance = 1e-6)
})

test_that("principal: observation plot, grouped by Species", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )$obsPlot
    expect_plot_snapshot("principal-obsPlot", testPlot)
})

test_that("principal: observation plot, no group", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = NULL
    )$obsPlot
    expect_plot_snapshot("principal-obsPlot-noGroup", testPlot)
})

test_that("principal: variable (loading) plot", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )$varPlot
    expect_plot_snapshot("principal-varPlot", testPlot)
})

test_that("principal: scree plot", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = NULL
    )$screePlot
    expect_plot_snapshot("principal-screePlot", testPlot)
})

test_that("principal: biplot (form)", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showBiplot = TRUE,
        biplotType = "formPlot"
    )$biPlot
    expect_plot_snapshot("principal-biplot-form", testPlot)
})

test_that("principal: biplot (covariance)", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showBiplot = TRUE,
        biplotType = "covPlot"
    )$biPlot
    expect_plot_snapshot("principal-biplot-cov", testPlot)
})

test_that("principal: Varimax rotation", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        rotation = "Varimax"
    )$varPlot
    expect_plot_snapshot("principal-varimax", testPlot)
})

test_that("principal: unstandardized variables", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        stdVariables = FALSE
    )$varPlot
    expect_plot_snapshot("principal-unstandardized", testPlot)
})

test_that("principal: custom dimensions and axes", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        dimNum = 3,
        xaxis = 1,
        yaxis = 3
    )$obsPlot
    expect_plot_snapshot("principal-customDims", testPlot)
})

test_that("principal: observation table with a factor labelVar shows level labels, not codes", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = "Species",
        groupVar = NULL,
        showObservations = TRUE
    )
    obs <- head(r$obsTable$asDF, 5)
    expect_equal(unname(obs$obs), c("1", "2", "3", "4", "5"))
    expect_true("label" %in% names(obs))
    expect_equal(which(names(obs) == "label"), which(names(obs) == "obs") + 1)
    expect_equal(unname(obs$label), rep("setosa", 5))
})

test_that("principal: observation table has no label column when labelVar is not set", {
    r <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = NULL,
        showObservations = TRUE
    )
    obs <- r$obsTable$asDF
    expect_false("label" %in% names(obs))
})

test_that("principal: point labels (labelVar)", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = "Species",
        groupVar = NULL
    )$obsPlot
    expect_plot_snapshot("principal-labelVar", testPlot)
})

test_that("principal: titles, axis and legend text options", {
    testPlot <- vijMulti::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        obsTitleText = "Iris observations",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        obsSubtitleText = "PCA on 4 flower measurements",
        subtitleFontFace = "italic",
        obsCaptionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom"
    )$obsPlot
    expect_plot_snapshot("principal-titles-axis-legend", testPlot)
})

test_that("principal: a missing group is shown as (Missing)", {
    naData <- testData
    naData$Species[2] <- NA
    r <- vijMulti::principal(
        data = naData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showObservations = TRUE
    )
    expect_equal(unname(r$obsTable$asDF$group[1:3]), c("setosa", "(Missing)", "setosa"))
    # The plot state keeps the real NA (labelled at plot time, in the current language)
    expect_true(is.na(r$obsPlot$state$obsData$group[2]))
})

test_that("principal: constant variables are detected whatever their unit", {
    run <- function(data) vijMulti::principal(data = data, vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
                                              labelVar = NULL, groupVar = NULL)
    ref <- run(testData)
    # A variable in small units (variance 6e-19 < eps) is not constant: same results
    smallData <- testData
    smallData$Petal_Width <- smallData$Petal_Width * 1e-9
    expect_equal(unname(run(smallData)$summaryTable$asDF$eigenvalue), unname(ref$summaryTable$asDF$eigenvalue))
    # A constant variable is rejected
    constData <- testData
    constData$Petal_Width <- 7
    expect_error(run(constData),
                 "The following variable(s) are constant and cannot be used in a principal component analysis: Petal_Width.",
                 fixed = TRUE)
})

test_that("principal: dimensions with null variance (rank-deficient data) can't be used", {
    x <- c(1, 3, 2, 5, 4, 6)
    y <- c(2, 1, 4, 3, 6, 5)
    rank2Data <- data.frame(a = x, b = y, c = x + y, d = x - 2 * y)
    run <- function(data, dimNum = 2) vijMulti::principal(data = data, vars = c("a", "b", "c", "d"), labelVar = NULL,
                                                          groupVar = NULL, dimNum = dimNum, stdScores = TRUE)
    # Null eigenvalues are shown as 0, the non-null dimensions can be used
    r <- run(rank2Data)
    expect_identical(unname(r$summaryTable$asDF$eigenvalue[3:4]), c(0, 0))
    expect_true(all(is.finite(r$obsPlot$state$obsData$y)))
    # but not the null ones: their standard scores were arbitrary (rounding noise / null variance)
    expect_error(run(rank2Data, dimNum = 3), "The number of dimensions cannot be greater than 2.", fixed = TRUE)
    rank1Data <- data.frame(a = x, b = 2 * x + 1, c = 10 - x, d = x / 3)
    expect_error(run(rank1Data), "PCA requires at least two dimensions with non-zero variance.", fixed = TRUE)
    # On rejection, the Summary table only shows the initial eigenvalues: no rotation of the null axes'
    # arbitrary eigenvectors (Varimax on them gave variance to the null 3rd component)
    a <- vijMulti::principalClass$new(options = vijMulti::principalOptions$new(vars = c("a", "b", "c", "d"), dimNum = 3,
                                      rotation = "Varimax", stataRotation = TRUE), data = rank2Data)
    a$init()
    expect_error(a$run(), "The number of dimensions cannot be greater than 2.", fixed = TRUE)
    summary <- a$results$summaryTable$asDF
    expect_identical(unname(summary$eigenvalue[3:4]), c(0, 0))
    expect_true(all(is.na(summary$loadings)) && all(is.na(summary$varProp)))
})

test_that("principal: a weak but real dimension is not taken for a null one", {
    x <- c(1, 3, 2, 5, 4, 6)
    y <- c(2, 1, 4, 3, 6, 5)
    weakData <- data.frame(a = x, b = x + 3.4e-6 * y, c = 2 * x + 1)   # 2nd eigenvalue 3e-12, 3rd null
    r <- vijMulti::principal(data = weakData, vars = c("a", "b", "c"), labelVar = NULL, groupVar = NULL, stdScores = TRUE)
    eigen <- unname(r$summaryTable$asDF$eigenvalue)
    expect_gt(eigen[2], 1e-12)
    expect_identical(eigen[3], 0)
    expect_true(all(is.finite(r$obsPlot$state$obsData$y)))
})
