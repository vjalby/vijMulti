testData <- data.frame(
    STAFF = factor(c("Senior Managers", "Senior Managers", "Senior Managers", "Senior Managers", "Junior Managers", "Junior Managers", "Junior Managers", "Junior Managers", "Senior Employees", "Senior Employees", "Senior Employees", "Senior Employees", "Junior Employees", "Junior Employees", "Junior Employees", "Junior Employees", "Secretaries", "Secretaries", "Secretaries", "Secretaries", "Senior Managers", "Junior Managers", "Junior Managers", "Senior Employees", "Senior Employees", "Junior Employees", "Junior Employees", "Secretaries", "Secretaries"), levels = c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries")),
    SMOKE = factor(c("None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol"), levels = c("None", "Light", "Medium", "Heavy", "No Alcohol", "Alcohol"), ordered = TRUE),
    COUNT = c(4L, 2L, 3L, 2L, 4L, 3L, 7L, 4L, 25L, 10L, 12L, 4L, 18L, 24L, 33L, 13L, 10L, 6L, 7L, 2L, 11L, 1L, 17L, 5L, 46L, 10L, 78L, 7L, 18L)
)

# Same smoking data as testData, pivoted into a ready-made row x column contingency
# table (STAFF x the 4 core smoking categories), for testing mode = "contTable"
wideData <- data.frame(
    STAFF = factor(c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries"),
                   levels = c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries")),
    None = c(4L, 4L, 25L, 18L, 10L),
    Light = c(2L, 3L, 10L, 24L, 6L),
    Medium = c(3L, 7L, 12L, 33L, 7L),
    Heavy = c(2L, 4L, 4L, 13L, 2L)
)

test_that("corresp: contingency table mode (mode = contTable)", {
    r <- vijMulti::corresp(
        data = wideData,
        mode = "contTable",
        rows = NULL,
        cols = NULL,
        columns = c("None", "Light", "Medium", "Heavy"),
        rowLabels = "STAFF",
        counts = NULL,
        showContingency = TRUE,
        showChisq = TRUE
    )
    ct <- r$contingency$asDF
    expect_equal(unname(ct$.row), c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries", "Active Margin"))
    expect_equal(unname(ct$None), c(4, 4, 25, 18, 10, 61))
    expect_equal(unname(ct$.margin), c(11, 18, 51, 88, 25, 193))

    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "3", "Total"))
    expect_equal(unname(eig$inertia), c(0.07475910589, 0.01001718051, 0.0004135740799, 0.08518986048), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.8775587314, 0.117586535, 0.004854733621, 1), tolerance = 1e-6)
    chisq <- r$chisq$asDF
    expect_equal(chisq$statistic, 16.44, tolerance = 1e-3)
    expect_equal(chisq$df, 12)
    expect_equal(chisq$p, 0.17183, tolerance = 1e-4)
    # Total inertia = chi2 / n (all dimensions, not only the displayed ones)
    expect_equal(unname(eig$inertia[nrow(eig)]), chisq$statistic / sum(wideData[, -1]), tolerance = 1e-6)
})

test_that("corresp: χ² test note when some expected counts are below 5", {
    run <- function(data) vijMulti::corresp(data = data, mode = "contTable", rows = NULL, cols = NULL,
                                            columns = c("None", "Light", "Medium", "Heavy"), rowLabels = "STAFF",
                                            counts = NULL, showChisq = TRUE)
    # Senior Managers x Heavy: expected count 11 x 25 / 193 = 1.4
    r <- run(wideData)
    expect_equal(r$chisq$notes$expected$note, "Some expected counts are less than 5. The χ² test may not be valid.")
    # Same table x 10: smallest expected count 14.2
    bigData <- wideData
    bigData[, -1] <- bigData[, -1] * 10
    r <- run(bigData)
    expect_null(r$chisq$notes$expected)
})

test_that("corresp: jamovi weights are ignored with a notice (mode = contTable)", {
    weightedData <- wideData
    attr(weightedData, "jmv-weights") <- c(1, 2, 1, 1, 1)
    attr(weightedData, "jmv-weights-name") <- "W"
    r <- vijMulti::corresp(
        data = weightedData,
        mode = "contTable",
        rows = NULL,
        cols = NULL,
        columns = c("None", "Light", "Medium", "Heavy"),
        rowLabels = "STAFF",
        counts = NULL,
        showContingency = TRUE
    )
    expect_equal(r$get(".weights")$content, "Row weights are ignored when the data is a contingency table.")
    expect_equal(unname(r$contingency$asDF$None), c(4, 4, 25, 18, 10, 61))
})

test_that("corresp: negative counts are rejected (mode = contTable)", {
    negData <- wideData
    negData$None[1] <- -4
    expect_error(
        vijMulti::corresp(
            data = negData,
            mode = "contTable",
            rows = NULL,
            cols = NULL,
            columns = c("None", "Light", "Medium", "Heavy"),
            rowLabels = "STAFF",
            counts = NULL,
            showContingency = TRUE
        ),
        "Counts may not be negative."
    )
})

test_that("corresp: duplicate row labels are rejected (mode = contTable)", {
    dupData <- wideData
    dupData$STAFF[2] <- dupData$STAFF[1]
    expect_error(
        vijMulti::corresp(
            data = dupData,
            mode = "contTable",
            rows = NULL,
            cols = NULL,
            columns = c("None", "Light", "Medium", "Heavy"),
            rowLabels = "STAFF",
            counts = NULL,
            showContingency = TRUE
        ),
        "Row labels must be unique."
    )
})

test_that("corresp: missing row labels are rejected (mode = contTable)", {
    naData <- wideData
    naData$STAFF[1] <- NA
    expect_error(
        vijMulti::corresp(
            data = naData,
            mode = "contTable",
            rows = NULL,
            cols = NULL,
            columns = c("None", "Light", "Medium", "Heavy"),
            rowLabels = "STAFF",
            counts = NULL,
            showContingency = TRUE
        ),
        "Row labels may not be missing."
    )
})

test_that("corresp: inertia (eigenvalues) table", {
    r <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showChisq = TRUE
    )
    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "3", "4", "Total"))
    expect_equal(unname(eig$inertia), c(0.03947232601, 0.02254491281, 0.002649068675, 0.0001700865699, 0.06483639407), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.6087989096, 0.3477200288, 0.04085774222, 0.002623319392, 1), tolerance = 1e-6)
    chisq <- r$chisq$asDF
    expect_equal(chisq$statistic, 25.03, tolerance = 1e-3)
    expect_equal(chisq$df, 20)
    expect_equal(chisq$p, 0.20041, tolerance = 1e-4)
    # Total inertia = chi2 / n (all dimensions, not only the displayed ones)
    expect_equal(unname(eig$inertia[nrow(eig)]), chisq$statistic / sum(testData$COUNT), tolerance = 1e-6)
})

test_that("corresp: contingency table", {
    r <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showContingency = TRUE
    )
    ct <- r$contingency$asDF
    expect_equal(unname(ct$.row), c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries", "Active Margin"))
    expect_equal(unname(ct$None), c(4, 4, 25, 18, 10, 61))
    expect_equal(unname(ct$.margin), c(22, 36, 102, 176, 50, 386))
})

# Small aggregated observation table (3 x 3), for testing the counts (weights) checks
weightData <- data.frame(
    R = factor(c("a", "a", "b", "b", "c", "c", "a", "b", "c")),
    C = factor(c("x", "y", "x", "y", "x", "y", "z", "z", "z")),
    N = c(5, 3, 2, 6, 4, 4, 3, 2, 5)
)

# do.call(): the generated wrapper quotes its variable arguments, so `rows = rows` would pass the name "rows"
corresp_weighted <- function(data, rows = "R") {
    do.call(vijMulti::corresp, list(
        data = data,
        mode = "obsTable",
        rows = rows,
        cols = "C",
        columns = NULL,
        rowLabels = NULL,
        counts = "N",
        showContingency = TRUE
    ))
}

test_that("corresp: missing counts are rejected (mode = obsTable)", {
    naData <- weightData
    naData$N[9] <- NA
    expect_error(corresp_weighted(naData), "Counts may not contain missing values.")
})

test_that("corresp: negative counts are rejected even on a row with a missing value (mode = obsTable)", {
    negData <- weightData
    negData$R[1] <- NA
    negData$N[1] <- -5
    expect_error(corresp_weighted(negData), "Counts may not be negative.")
})

test_that("corresp: zero counts mean no observation (mode = obsTable)", {
    # Category "d" only has zero counts: it is dropped, like a category without observations
    zeroData <- rbind(weightData, data.frame(R = "d", C = c("x", "y"), N = 0))
    ct <- corresp_weighted(zeroData)$contingency$asDF
    expect_equal(unname(ct$.row), c("a", "b", "c", "Active Margin"))
    expect_equal(unname(ct$.margin), c(11, 10, 13, 34))

    allZeroData <- weightData
    allZeroData$N <- 0
    expect_error(corresp_weighted(allZeroData), "Counts may not all be zero.")
})

test_that("corresp: a variable named .COUNTS is rejected (mode = obsTable)", {
    reservedData <- weightData
    names(reservedData)[1] <- ".COUNTS"
    expect_error(corresp_weighted(reservedData, rows = ".COUNTS"), "\".COUNTS\" is a reserved name.", fixed = TRUE)
})

test_that("corresp: profiles with a supplementary row and column", {
    # Only the profiles used by the analysis are shown: row profiles of every row
    # over the active columns, column profiles of every column over the active rows
    r <- vijMulti::corresp(
        data = wideData,
        mode = "contTable",
        rows = NULL,
        cols = NULL,
        columns = c("None", "Light", "Medium", "Heavy"),
        rowLabels = "STAFF",
        counts = NULL,
        supplementaryRows = "5",
        supplementaryCols = "4",
        showProfiles = TRUE
    )
    rp <- r$rowProfiles$asDF
    expect_equal(names(rp), c(".row", "None", "Light", "Medium", ".margin"))
    expect_equal(unname(rp$.row), c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries *", "Mass"))
    expect_equal(unname(rp$None), c(4/9, 4/14, 25/47, 18/75, 10/23, 51/145))
    expect_equal(unname(rp$.margin), rep(1, 6))
    expect_equal(r$rowProfiles$notes$supp$note, "* Supplementary rows")

    cp <- r$colProfiles$asDF
    expect_equal(names(cp), c(".row", "None", "Light", "Medium", "Heavy", ".mass"))
    expect_equal(unname(cp$.row), c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Active Margin"))
    expect_equal(unname(cp$None), c(4, 4, 25, 18, 51) / 51)
    expect_equal(unname(cp$Heavy), c(2, 4, 4, 13, 23) / 23)
    expect_equal(unname(cp$.mass), c(9, 14, 47, 75, 145) / 145)
    expect_equal(r$colProfiles$notes$supp$note, "* Supplementary columns")
})

test_that("corresp: categories named like the label or margin columns keep their values", {
    # Table columns are keyed by category name: a category named "row", "Active Margin"
    # or "Mass" used to replace the row-label or margin column (now keyed ".row", ".margin", ".mass")
    collideData <- data.frame(
        A = factor(rep(c("Mass", "x", "y"), each = 3)),
        B = factor(rep(c("row", "Active Margin", "Mass"), 3)),
        COUNT = c(10L, 2L, 3L, 4L, 12L, 5L, 3L, 4L, 15L)
    )
    r <- vijMulti::corresp(
        data = collideData,
        mode = "obsTable",
        rows = "A",
        cols = "B",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showContingency = TRUE,
        showProfiles = TRUE
    )
    titles <- function(table) vapply(table$columns, function(col) col$title, character(1), USE.NAMES = FALSE)

    ct <- r$contingency$asDF
    expect_equal(names(ct), c(".row", "Active Margin", "Mass", "row", ".margin"))
    expect_equal(titles(r$contingency), c("A", "Active Margin", "Mass", "row", "Active Margin"))
    expect_equal(unname(ct$.row), c("Mass", "x", "y", "Active Margin"))
    expect_equal(unname(ct$row), c(10, 4, 3, 17))
    expect_equal(unname(ct$.margin), c(15, 21, 22, 58))

    rp <- r$rowProfiles$asDF
    expect_equal(names(rp), c(".row", "Active Margin", "Mass", "row", ".margin"))
    expect_equal(titles(r$rowProfiles), c("A", "Active Margin", "Mass", "row", "Active Margin"))
    expect_equal(unname(rp$.row), c("Mass", "x", "y", "Mass"))
    expect_equal(unname(rp$.margin), c(1, 1, 1, 1))

    cp <- r$colProfiles$asDF
    expect_equal(names(cp), c(".row", "Active Margin", "Mass", "row", ".mass"))
    expect_equal(titles(r$colProfiles), c("A", "Active Margin", "Mass", "row", "Mass"))
    expect_equal(unname(cp$.row), c("Mass", "x", "y", "Active Margin"))
    expect_equal(unname(cp$Mass), c(3, 5, 15, 23) / 23)
    expect_equal(unname(cp$.mass), c(15, 21, 22, 58) / 58)
})

test_that("corresp: categories named like the reserved table keys are rejected", {
    reservedData <- data.frame(
        A = factor(rep(c(".margin", "x", "y"), each = 3)),
        B = factor(rep(c(".mass", "u", "v"), 3)),
        COUNT = c(10L, 2L, 3L, 4L, 12L, 5L, 3L, 4L, 15L)
    )
    # Rejected even when the contingency and profile tables are not shown
    expect_error(
        vijMulti::corresp(
            data = reservedData,
            mode = "obsTable",
            rows = "A",
            cols = "B",
            columns = NULL,
            rowLabels = NULL,
            counts = "COUNT"
        ),
        "The category names .margin, .mass are reserved.",
        fixed = TRUE
    )
})

test_that("corresp: decimal supplementary row/column numbers are rejected, not truncated", {
    run <- function(rows = "", cols = "") vijMulti::corresp(data = wideData, mode = "contTable", rows = NULL, cols = NULL,
                                                          columns = c("None", "Light", "Medium", "Heavy"), rowLabels = "STAFF",
                                                          counts = NULL, supplementaryRows = rows, supplementaryCols = cols,
                                                          showContingency = TRUE)
    expect_error(run(rows = "2.9"), "Supplementary row numbers must be a list of numbers, e.g. 1,2,9", fixed = TRUE)
    expect_error(run(cols = "1,3.5"), "Supplementary column numbers must be a list of numbers, e.g. 1,2,9", fixed = TRUE)
    # A whole number written as a decimal is accepted
    r <- run(rows = "2.0")
    expect_equal(unname(r$contingency$asDF$.row[2]), "Junior Managers *")
})

test_that("corresp: a supplementary column marked like an active one keeps its own column", {
    # Supplementary "a" is shown as "a *", like the active column already named so
    starData <- data.frame(lab = c("R1", "R2", "R3", "R4", "R5"),
                           `a *` = c(10, 5, 8, 3, 7), b = c(3, 9, 4, 6, 2), c = c(6, 2, 11, 5, 8),
                           d = c(4, 8, 3, 9, 5), a = c(5, 1, 2, 7, 4), check.names = FALSE)
    r <- vijMulti::corresp(data = starData, mode = "contTable", rows = NULL, cols = NULL, counts = NULL,
                           columns = c("a *", "b", "c", "d", "a"), rowLabels = "lab", supplementaryCols = "5",
                           showContingency = TRUE, showProfiles = TRUE, showSummaries = TRUE)
    titles <- function(table) vapply(table$columns, function(col) col$title, character(1), USE.NAMES = FALSE)

    ct <- r$contingency$asDF
    expect_equal(titles(r$contingency), c("lab", "a *", "b", "c", "d", "a *", "Active Margin"))
    expect_equal(unname(ct[["a *"]]), c(10, 5, 8, 3, 7, 33))
    expect_equal(unname(ct$a[1:5]), c(5, 1, 2, 7, 4))

    cp <- r$colProfiles$asDF
    expect_equal(titles(r$colProfiles), c("lab", "a *", "b", "c", "d", "a *", "Mass"))
    expect_equal(unname(cp[["a *"]]), c(c(10, 5, 8, 3, 7) / 33, 1))
    expect_equal(unname(cp$a), c(c(5, 1, 2, 7, 4) / 19, 1))

    expect_equal(unname(r$colSummary$asDF$col), c("a *", "b", "c", "d", "a *"))
})

test_that("corresp: row and column summary tables (first row)", {
    r <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showSummaries = TRUE
    )
    rowSum <- r$rowSummary$asDF[1,]
    expect_equal(unname(rowSum$row), "Senior Managers")
    expect_equal(unname(rowSum$margin), 0.05699481865, tolerance = 1e-6)
    expect_equal(unname(rowSum$score1), -0.05409754112, tolerance = 1e-6)
    expect_equal(unname(rowSum$score2), -0.2913131521, tolerance = 1e-6)
    expect_equal(unname(rowSum$qlt), 0.9637078448, tolerance = 1e-6)

    colSum <- r$colSummary$asDF[1,]
    expect_equal(unname(colSum$col), "None")
    expect_equal(unname(colSum$margin), 0.1580310881, tolerance = 1e-6)
    expect_equal(unname(colSum$score1), 0.3735976041, tolerance = 1e-6)
    expect_equal(unname(colSum$score2), -0.1257482436, tolerance = 1e-6)
    expect_equal(unname(colSum$qlt), 0.9984944007, tolerance = 1e-6)
})

test_that("corresp: row plot", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showRowPlot = TRUE
    )$rowplot
    expect_plot_snapshot("corresp-rowplot", testPlot)
})

test_that("corresp: column plot", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showColPlot = TRUE
    )$colplot
    expect_plot_snapshot("corresp-colplot", testPlot)
})

test_that("corresp: biplot", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT"
    )$biplot
    expect_plot_snapshot("corresp-biplot", testPlot)
})

test_that("corresp: biplot keeps labels when rows and columns share category names", {
    # Square table (same categories in rows and columns): the biplot labels used
    # to come from rbind()-ed rownames, made unique as "A1", "B1", ...
    squareData <- data.frame(
        FATHER = factor(c("A", "A", "B", "B", "C", "C", "D", "D", "A", "C", "B", "D")),
        SON = factor(c("A", "B", "B", "C", "C", "D", "D", "A", "C", "A", "D", "B")),
        COUNT = c(20L, 5L, 18L, 4L, 22L, 6L, 15L, 3L, 7L, 2L, 9L, 8L)
    )
    r <- vijMulti::corresp(
        data = squareData,
        mode = "obsTable",
        rows = "FATHER",
        cols = "SON",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        supplementaryCols = "2"
    )
    # $plot is a jmvcore PlotObject; its $fun() returns the ggplot object.
    # suppressMessages() as in jmvcore's Analysis$.render(): ggtheme already holds
    # a colour scale, so scale_color_manual() triggers "Scale for colour is already present"
    plot <- suppressMessages(r$biplot$plot$fun())
    labels <- ggplot2::ggplot_build(plot)$data[[2]]$label
    expect_equal(labels, c("A", "B", "C", "D", "A", "C", "D", "B *"))
})

test_that("corresp: alcohol columns as supplementary categories", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        supplementaryCols = "5,6"
    )$biplot
    expect_plot_snapshot("corresp-supplementaryCols", testPlot)
})

test_that("corresp: symmetric normalization", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        normalization = "symmetric"
    )$biplot
    expect_plot_snapshot("corresp-symmetric", testPlot)
})

test_that("corresp: sorted rows as supplementary (row-principal normalization)", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        normalization = "rowprincipal"
    )$biplot
    expect_plot_snapshot("corresp-rowprincipal", testPlot)
})

test_that("corresp: titles, axis and legend text options", {
    testPlot <- vijMulti::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        biplotTitleText = "Staff smoking habits",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        biplotSubtitleText = "Correspondence analysis",
        subtitleFontFace = "italic",
        biplotCaptionText = "Source: fictitious survey data",
        captionAlign = "1"
    )$biplot
    expect_plot_snapshot("corresp-titles-axis-legend", testPlot)
})

test_that("corresp: null-inertia axes are shown but not usable (proportional rows)", {
    # R3 and R4 are proportional to R1 and R2: only 2 non-null axes out of 3
    propData <- data.frame(
        Label = factor(c("R1", "R2", "R3", "R4", "R5"), levels = c("R1", "R2", "R3", "R4", "R5")),
        C1 = c(10L, 4L, 20L, 8L, 6L),
        C2 = c(20L, 2L, 40L, 4L, 3L),
        C3 = c(5L, 12L, 10L, 24L, 5L),
        C4 = c(8L, 9L, 16L, 18L, 9L)
    )
    args <- list(mode = "contTable", rows = NULL, cols = NULL, columns = c("C1", "C2", "C3", "C4"),
                 rowLabels = "Label", counts = NULL)
    r <- do.call(vijMulti::corresp, c(list(data = propData), args))
    # All 3 dimensions are shown, the null one as exactly 0
    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "3", "Total"))
    expect_identical(unname(eig$singular[3]), 0)
    expect_identical(unname(eig$inertia[3]), 0)
    # ... but only the 2 non-null ones are usable, the Summary table being filled anyway
    a <- correspClass$new(options = do.call(correspOptions$new, c(args, list(dimNum = 3))), data = propData)
    a$init()
    expect_error(a$run(), "less than or equal to 2")
    expect_equal(unname(a$results$eigenvalues$asDF$dim), c("1", "2", "3", "Total"))
    # Only one non-null axis (R5 left out): a 2-dimension CA is rejected
    expect_error(do.call(vijMulti::corresp, c(list(data = propData[1:4, ]), args)),
                 "at least two dimensions")
    # No non-null axis at all (quasi-independent table with a tiny but non-zero χ², passing the
    # χ² check): rejected before the inertia table is filled
    indepData <- data.frame(
        Label = factor(c("R1", "R2", "R3")),
        C1 = c(20000.001, 40000, 60000),
        C2 = c(50000, 100000, 150000),
        C3 = c(30000, 60000, 90000),
        C4 = c(40000, 80000, 120000)
    )
    a <- correspClass$new(options = do.call(correspOptions$new, args), data = indepData)
    a$init()
    expect_error(a$run(), "at least two dimensions")
    expect_equal(nrow(a$results$eigenvalues$asDF), 0)
})
