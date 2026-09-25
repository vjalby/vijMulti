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
    expect_equal(unname(eig$dim), c("1", "2", "Total"))
    expect_equal(unname(eig$inertia), c(0.07475910589, 0.01001718051, 0.0847762864), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.8775587314, 0.117586535, 1), tolerance = 1e-6)
    chisq <- r$chisq$asDF
    expect_equal(chisq$statistic, 16.44, tolerance = 1e-3)
    expect_equal(chisq$df, 12)
    expect_equal(chisq$p, 0.17183, tolerance = 1e-4)
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
    expect_equal(unname(eig$dim), c("1", "2", "Total"))
    expect_equal(unname(eig$inertia), c(0.03947232601, 0.02254491281, 0.06201723883), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.6087989096, 0.3477200288, 1), tolerance = 1e-6)
    chisq <- r$chisq$asDF
    expect_equal(chisq$statistic, 25.03, tolerance = 1e-3)
    expect_equal(chisq$df, 20)
    expect_equal(chisq$p, 0.20041, tolerance = 1e-4)
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
