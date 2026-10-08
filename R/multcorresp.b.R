multcorrespClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "multcorrespClass",
    inherit = multcorrespBase,
    private = list(
        .getVarName = function(aVar) {
            if (self$options$descAsVarName && !is.null(aVar)) {
                aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                if (!is.null(aVarName))
                    return(aVarName)
                else
                    return(aVar)
            } else {
                return(aVar)
            }
        },
        .init = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 3) {
                private$.showHelpMessage()
            }

            # Init tables (adding columns)
            nDim <- self$options$dimNum
            private$.initInertiaTable(self$results$eigenvalues, self$options$method)
            if (self$options$showDiscriminations)
                private$.initDiscriminationTable(self$results$discrim, nDim)
            if (self$options$showCategories)
                private$.initCategoryTable(self$results$categories, nDim)
            if (self$options$showObservations)
                private$.initObservationTable(self$results$observations, nDim)
        },
        .run = function() {
            # Long by design (reviewed 2026-09-30): a linear validate -> compute (.mca) ->
            # fill-tables sequence; splitting it further would just scatter it.
            if (is.null(self$options$vars) || length(self$options$vars) < 3  || nrow(self$data) == 0)
                return(FALSE)

            activeVars <- self$options$vars
            supplVars <- self$options$supplVars
            allVars <- c(activeVars, supplVars)
            if (is.null(supplVars))
                supplIdx <- NULL
            else
                supplIdx <- (length(activeVars)+1):(length(allVars))

            # Display names (real variable name -> description, "*"-suffixed for
            # supplementary variables), disambiguated with make.unique() so two
            # variables never end up with the identical display string. Stored on
            # res once it exists (see below).
            varDisplayNameRaw <- vapply(allVars, function(aVar) {
                aVarName <- private$.getVarName(aVar)
                if (aVar %in% self$options$supplVars) paste(aVarName, "*") else aVarName
            }, character(1), USE.NAMES = FALSE)
            varDisplayName <- stats::setNames(make.unique(varDisplayNameRaw), allVars)

            # remove cases with with NA in vars
            data <- self$data[stats::complete.cases(self$data[, allVars, drop = FALSE]), , drop = FALSE]
            data <- droplevels(data)

            if (nrow(data) == 0) {
                vijErrorMessage(self, .("Unable to compute MCA because of too many missing values."))
            }

            # list of ordered factors (used to draw path) - real variable names, not
            # positions, so this stays correct even if two variables' display names
            # later collide and merge in the category plot (see .mainplot)
            ordVars <- c()
            for (aVar in allVars) {
                if (is.ordered(data[[aVar]]))
                    ordVars <- c(ordVars, aVar)
            }

            if (!is.null(self$options$labelVar))
                rowLabels <- as.character(data[[self$options$labelVar]])
            else
                rowLabels <- NULL

            method <- self$options$method
            methodStr <- switch(method,
                                "Indicator" = .("Indicator matrix"),
                                "Burt" = .("Burt matrix"))

            #### Compute MCA ####

            nDim <- self$options$dimNum

            res <- tryCatch(
                    private$.mca(data[,allVars], method = method, nd = nDim, supcol = supplIdx,
                                 rowlabels = rowLabels, rownames = rownames(data)),
                    error = function (e) NULL
                )

            if (is.null(res)) {
                vijErrorMessage(self, .("Unable to compute MCA for the selected variables."))
            }

            if (nDim > res$nd.max) {
                errorMessage <- jmvcore::format(.("The number of dimensions cannot be greater than {max}."), max = res$nd.max)
                vijErrorMessage(self, errorMessage)
            }

            res$varDisplayName <- varDisplayName

            #### Inertia Table ####

            private$.fillInertiaTable(self$results$eigenvalues, res, method, methodStr)

            #### Discrimination Table ####

            if (self$options$showDiscriminations)
                private$.fillDiscriminationTable(self$results$discrim, res, nDim, supplIdx)

            #### Category Table ####

            if (self$options$showCategories)
                private$.fillCategoryTable(self$results$categories, res, nDim, supplIdx)

            #### Observation Table ####

            if (self$options$showObservations)
                private$.fillObservationTable(self$results$observations, res, nDim)

            #### Plots  ####

            # Check axis values
            if (self$options$xaxis > nDim || self$options$yaxis > nDim) {
                errorMessage <- .("X-Axis and Y-Axis cannot be greater than the number of dimensions.")
                vijErrorMessage(self, errorMessage)
            } else if (self$options$xaxis == self$options$yaxis) {
                errorMessage <- .("X-Axis and Y-Axis cannot be equal.")
                vijErrorMessage(self, errorMessage)
            }

            res$ordVars <- ordVars # List of ordered factors

            # Which of cat$coord/cat$stdcoord and ind$coord/ind$stdcoord .mainplot()
            # will actually use for this normalization - decided once here so only
            # that one needs to travel into each plot's state below.
            catCoordField <- if (self$options$normalization %in% c("principal", "catprincipal")) "coord" else "stdcoord"
            indCoordField <- if (self$options$normalization %in% c("principal", "obsprincipal")) "coord" else "stdcoord"

            # Each state below carries only the res fields that plot type actually
            # reads (see .discrimplot()/.mainplot()), with coordinate matrices cut
            # down from res$nd.max columns to the nDim actually ever displayed
            # (dim1/dim2 <= nDim is enforced above). res itself stays untouched/full
            # for the table-fill functions and .saveCoordinates() above, which don't
            # need this trimming.
            if (self$options$showDiscriminationPlot) {
                self$results$discrimplot$setState(list(
                    eig = res$eig[seq_len(nDim), , drop = FALSE],
                    allvar = list(eta2 = res$allvar$eta2[, seq_len(nDim), drop = FALSE]),
                    varDisplayName = res$varDisplayName
                ))
            }

            if (self$options$showCategoryPlot) {
                self$results$categoryplot$setState(list(
                    eig = res$eig[seq_len(nDim), , drop = FALSE],
                    cat = list(coord = res$cat[[catCoordField]][, seq_len(nDim), drop = FALSE], factors = res$cat$factors, level = res$cat$level),
                    varDisplayName = res$varDisplayName,
                    varActive = res$varActive,
                    ordVars = res$ordVars
                ))
            }
            if (self$options$showObservationPlot) {
                self$results$obsplot$setState(list(
                    eig = res$eig[seq_len(nDim), , drop = FALSE],
                    ind = list(coord = res$ind[[indCoordField]][, seq_len(nDim), drop = FALSE]),
                    rowlabels = res$rowlabels,
                    varActive = res$varActive
                ))
            }
            if (self$options$showBiPlot) {
                self$results$biplot$setState(list(
                    eig = res$eig[seq_len(nDim), , drop = FALSE],
                    cat = list(coord = res$cat[[catCoordField]][, seq_len(nDim), drop = FALSE], factors = res$cat$factors, level = res$cat$level),
                    ind = list(coord = res$ind[[indCoordField]][, seq_len(nDim), drop = FALSE]),
                    rowlabels = res$rowlabels,
                    varDisplayName = res$varDisplayName,
                    varActive = res$varActive,
                    ordVars = res$ordVars
                ))
            }

            #### Saving coordinates  ####

            if (self$options$normalization %in% c("principal", "obsprincipal"))
                private$.saveCoordinates(res$ind$coord, "principal")
            else
                private$.saveCoordinates(res$ind$stdcoord, "standard")
        },
        .mca = function(data, method, nd, supcol, rowlabels = NULL, rownames = NULL) {
            # MCA based on ca::mjca.

            #### Variables ####
            Q <- ncol(data) - length(supcol)   # Nb of active variables
            n <- nrow(data)                    # Nb of observations

            #### MCA ####
            mca <- ca::mjca(data, nd = NA,
                            lambda = if (method == "Burt") "Burt" else "indicator",
                            supcol = if (is.null(supcol)) NA else supcol)
            sv <- mca$sv                                   # Indicator: sqrt(lambda) ; Burt: lambda
            eig <- sv^2                                    # Eigenvalues of the analysed matrix
            lambdaI <- if (method == "Burt") sv else eig   # Eigenvalues of the indicator matrix
            K <- length(eig)

            #### Axis orientation ####
            # Arbitrary in the computation: on each axis, the active category farthest from the origin is positive
            actCat <- setdiff(seq_len(nrow(mca$colcoord)), mca$colsup)
            flip <- apply(mca$colcoord[actCat, seq_len(K), drop = FALSE], 2, function(x) sign(x[which.max(abs(x))]))
            mca$colcoord <- sweep(mca$colcoord[, seq_len(K), drop = FALSE], 2, flip, FUN = "*")
            mca$rowpcoord <- sweep(mca$rowpcoord[, seq_len(K), drop = FALSE], 2, flip, FUN = "*")

            #### Eigenvalues ####
            res <- list()
            res$eig <- cbind("eigenvalue" = eig,
                             "percentage of variance" = eig / sum(eig),
                             "cumulative percentage of variance" = cumsum(eig) / sum(eig))
            res$nd.max <- K
            if (nd > res$nd.max)
                return(res)

            #### Labels ####
            res$rowlabels <- if (!is.null(rowlabels)) rowlabels else as.character(rownames)

            #### Observations ####
            # Indicator approach for both methods, as in Stata (predict after mca, method(burt)).
            # mjca's rowpcoord = s * sqrt(lambdaI) in both modes, where s = indicator standard coordinates.
            ind <- list()
            ind$stdcoord <- sweep(mca$rowpcoord[, seq_len(K), drop = FALSE], 2, sqrt(lambdaI), FUN = "/")
            rownames(ind$stdcoord) <- rownames             # used by the Observations table and saved coordinates
            ind$coord <- sweep(ind$stdcoord, 2, sv, FUN = "*")
            ind$mass <- rep(1 / n, n)
            ind$contrib <- ind$stdcoord^2 / n
            ind$cos2 <- ind$coord^2 / rowSums(ind$coord^2)
            ind$qlt <- rowSums(ind$cos2[, 1:nd, drop = FALSE])
            ind$inertia <- (ind$contrib %*% eig) / sum(ind$contrib %*% eig)
            res$ind <- ind

            #### Categories (active and supplementary together) ####
            # In mjca's column order, i.e. data order: .run() puts supplementary variables last
            # (relied on by res$varActive for point shapes and fonts in the plots).
            supCat <- mca$colsup[!is.na(mca$colsup)]       # mjca returns NA when there is no supplementary category
            count <- unlist(lapply(data, function(x) as.vector(table(x))), use.names = FALSE)   # n_c
            categ <- list()
            categ$factors <- mca$factors[, "factor"]
            categ$level <- mca$factors[, "level"]           # displayed label
            # Not mca$colpcoord: for supplementary categories in indicator mode, mjca computes it from Burt profiles
            categ$stdcoord <- mca$colcoord[, seq_len(K), drop = FALSE]
            categ$coord <- sweep(categ$stdcoord, 2, sv, FUN = "*")
            if (method == "Burt")
                categ$cos2 <- mca$colcor[, seq_len(K), drop = FALSE]
            else  # squared chi2 distance in the indicator space = n/n_c - 1
                categ$cos2 <- categ$coord^2 / (n / count - 1)
            categ$qlt <- rowSums(categ$cos2[, 1:nd, drop = FALSE])
            # Mass, contributions and inertia are not defined for supplementary categories
            categ$mass <- count / (n * Q)
            categ$mass[supCat] <- NA
            categ$contrib <- categ$mass * categ$stdcoord^2
            categ$inertia <- (categ$contrib %*% eig) / sum(categ$contrib %*% eig, na.rm = TRUE)
            res$cat <- categ

            #### Variables ####
            # Discrimination (eta2) is scale-independent: computed from indicator principal coordinates
            res$allvar$eta2 <- rowsum((count / n) * sweep(categ$stdcoord^2, 2, lambdaI, FUN = "*"), categ$factors, reorder = FALSE)
            res$varActive <- c(Q, length(supcol))  # (nb of active var, nb of supp var)

            #### Benzecri / Greenacre Adjusment ####
            if (method == "Burt") {
                p <- Q                     # Nb of variables
                m <- length(categ$factors) - length(supCat)   # Nb of active categories
                res$totalInertia <- sum(eig)
                res$adjEig <- matrix(nrow = K, ncol = 5)
                res$adjEig[,1] <- (p/(p-1))**2 * (sqrt(eig) - 1/p)**2
                res$adjEig[sqrt(eig) <= 1/p,1] <- 0
                res$totalInrB <- sum(res$adjEig[,1])
                res$totalInrG <- (p/(p-1)) * (res$totalInertia - (m-p)/p**2)
                res$adjEig[,2] <- res$adjEig[,1] / res$totalInrB
                res$adjEig[,3] <- cumsum(res$adjEig[,2])
                res$adjEig[,4] <- res$adjEig[,1] / res$totalInrG
                res$adjEig[,5] <- cumsum(res$adjEig[,4])
                res$adjEig[sqrt(eig) <= 1/p,] <- rep(NA,5)
            }
            return(res)
        },
        .dimN = function(n) jmvcore::format(.("Dim {n}"), n = n),
        .nullOrValue = function(x) if(is.na(x)) NULL else x,
        .initInertiaTable = function(table, method) {
            if (self$options$showSummary) {
                if (method == "Burt" && self$options$BenzecriAdj) {
                    table$addColumn("adjB", title = .("Inertia"), type = "number", format = "zto", superTitle = .("Benzécri Correction"))
                    table$addColumn("%B", title = .("% of Inertia"), type = "number", format = "pc", superTitle = .("Benzécri Correction"))
                    table$addColumn("C%B", title = .("Cumulative %"), type = "number", format = "pc", superTitle = .("Benzécri Correction"))

                }
                if (method == "Burt" && self$options$GreenacreAdj) {
                    table$addColumn("adjG", title = .("Inertia"), type = "number", format = "zto", superTitle = .("Greenacre Correction"))
                    table$addColumn("%G", title = .("% of Inertia"), type = "number", format = "pc", superTitle = .("Greenacre Correction"))
                    table$addColumn("C%G", title = .("Cumulative %"), type = "number", format = "pc", superTitle = .("Greenacre Correction"))

                }
            }
        },
        .fillInertiaTable = function(table, res, method, methodStr) {
            if (self$options$showSummary) {
                for (i in seq_len(res$nd.max)) {
                    values = list(
                        dim = i,
                        inertia = res$eig[i,1],
                        proportion = res$eig[i,2],
                        cumulative = res$eig[i,3]
                    )
                    if (method == "Burt" && self$options$BenzecriAdj) {
                        values[["adjB"]] <- private$.nullOrValue(res$adjEig[i,1])
                        values[["%B"]] <- private$.nullOrValue(res$adjEig[i,2])
                        values[["C%B"]] <- private$.nullOrValue(res$adjEig[i,3])
                    }
                    if (method == "Burt" && self$options$GreenacreAdj) {
                        values[["adjG"]] <- private$.nullOrValue(res$adjEig[i,1])
                        values[["%G"]] <- private$.nullOrValue(res$adjEig[i,4])
                        values[["C%G"]] <- private$.nullOrValue(res$adjEig[i,5])
                    }
                    table$addRow(rowKey = as.character(i), values = values)
                }
                # Add total row
                values = list(
                    dim = .("Total"),
                    inertia = sum(res$eig[,1]),
                    proportion = sum(res$eig[,2]),
                    cumulative = NA
                )
                if (method == "Burt" && self$options$BenzecriAdj) {
                    values[["adjB"]] <- res$totalInrB
                    values[["%B"]] <- 1
                    values[["C%B"]] <- NULL
                }
                if (method == "Burt" && self$options$GreenacreAdj) {
                    values[["adjG"]] <- res$totalInrB
                    values[["%G"]] <- sum(res$adjEig[,4], na.rm=TRUE)
                    values[["C%G"]] <- NULL
                }
                table$addRow(rowKey = "Total", values = values)
                table$addFormat(rowKey = "Total", 1, jmvcore::Cell.BEGIN_END_GROUP)
            }
            table$setNote("method", jmvcore::format(.("Method: {method}"), method = methodStr))
            if (method == "Burt" && self$options$GreenacreAdj)
                table$setNote("adjusted", jmvcore::format(.("Greenacre's corrected inertia = {inertia}"), inertia = round(res$totalInrG,4)))
        },
        .initDiscriminationTable = function(table, nDim) {
            for (j in seq_len(nDim))
                table$addColumn(paste0("dim",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = .("Discrimination"))
        },
        .fillDiscriminationTable = function(table, res, nDim, supplIdx) {
            for (i in seq_len(nrow(res$allvar$eta2))) {
                values = list()
                values[["var"]] <- res$varDisplayName[[rownames(res$allvar$eta2)[i]]]
                for (j in seq_len(nDim))
                    values[[paste0("dim",j)]] <- res$allvar$eta2[i,j]
                table$setRow(rowNo = i, values = values)
            }
            if (!is.null(supplIdx))
                table$setNote("sup", paste("*", .("Supplementary variables")))
        },
        .initCategoryTable = function(table, nDim) {
            for (j in seq_len(nDim))
                table$addColumn(paste0("coord",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = paste(.("Coordinates"),"†"))
            for (j in seq_len(nDim))
                table$addColumn(paste0("ctr",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = .("Contributions"))
            for (j in seq_len(nDim))
                table$addColumn(paste0("co2",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = .("Cos²"))
            # Because of missing values/incomplete cases, the row number is unknown at init stage.
        },
        .fillCategoryTable = function(table, res, nDim, supplIdx) {
            previousfactor <- res$cat$factors[1]
            for (i in seq_len(nrow(res$cat$coord))) {
                values = list(
                    factor = res$varDisplayName[[res$cat$factors[i]]],
                    level = res$cat$level[i],
                    mass = private$.nullOrValue(res$cat$mass[i]),
                    qlt = res$cat$qlt[i],
                    inertia = private$.nullOrValue(res$cat$inertia[i])
                )
                for (j in seq_len(nDim)) {
                    if (self$options$normalization %in% c("principal", "catprincipal"))
                        values[[paste0("coord",j)]] <- res$cat$coord[i,j]
                    else
                        values[[paste0("coord",j)]] <- res$cat$stdcoord[i,j]
                    values[[paste0("co2",j)]] <- res$cat$cos2[i,j]
                    values[[paste0("ctr",j)]] <- private$.nullOrValue(res$cat$contrib[i,j])
                }
                table$addRow(rowKey = as.character(i), values = values)
                if( res$cat$factors[i] != previousfactor) {
                    table$addFormat(rowKey = as.character(i), 1, jmvcore::Cell.BEGIN_END_GROUP)
                    previousfactor <- res$cat$factors[i]
                }
            }
            if (self$options$normalization %in% c("principal", "catprincipal"))
                table$setNote("normalization", paste("†",.("Principal coordinates")))
            else
                table$setNote("normalization", paste("†",.("Standard coordinates")))
            if (!is.null(supplIdx))
                table$setNote("sup", paste("*", .("Supplementary variables")))
        },
        .initObservationTable = function(table, nDim) {
            if (!is.null(self$options$labelVar))
                table$addColumn("label", index = 2, title = private$.getVarName(self$options$labelVar), type = "text")
            for (j in seq_len(nDim))
                table$addColumn(paste0("coord",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = paste(.("Coordinates"),"†"))
            for (j in seq_len(nDim))
                table$addColumn(paste0("ctr",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = .("Contributions"))
            for (j in seq_len(nDim))
                table$addColumn(paste0("co2",j), title = private$.dimN(j), type = "number", format = "zto", superTitle = .("Cos²"))
        },
        .fillObservationTable = function(table, res, nDim) {
            nrows <- length(res$rowlabels)
            if (nrows > 100) {
                table$setNote("100", .("Limited to the first 100 observations"))
                nrows <- 100
            }
            for (i in seq_len(nrows)) {
                values = list(
                    obs = rownames(res$ind$coord)[i],
                    mass = res$ind$mass[i],
                    qlt = res$ind$qlt[i],
                    inertia = res$ind$inertia[i]
                )
                if (!is.null(self$options$labelVar))
                    values$label <- as.character(res$rowlabels[i])
                for (j in seq_len(nDim)) {
                    if (self$options$normalization %in% c("principal", "obsprincipal"))
                        values[[paste0("coord",j)]] <- res$ind$coord[i,j]
                    else
                        values[[paste0("coord",j)]] <- res$ind$stdcoord[i,j]
                    values[[paste0("co2",j)]] <- res$ind$cos2[i,j]
                    values[[paste0("ctr",j)]] <- res$ind$contrib[i,j]
                }
                table$addRow(rowKey = as.character(i), values = values)
            }
            if (self$options$normalization %in% c("principal", "obsprincipal"))
                table$setNote("normalization",paste("†", .("Principal coordinates")))
            else
                table$setNote("normalization",paste("†", .("Standard coordinates")))
        },
        .discrimplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            dim1name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = dim1, perc = round(res$eig[dim1,2]*100,1))
            dim2name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = dim2, perc = round(res$eig[dim2,2]*100,1))

            data <- res$allvar$eta2[,c(dim1, dim2)]
            colnames(data) <- c("x","y")
            labels <- unname(res$varDisplayName[rownames(data)])

            plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, label = labels))
            plot <- plot + ggplot2::geom_point()
            plot <- plot + ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = 0))
            plot <- plot + ggrepel::geom_text_repel(show.legend = FALSE, nudge_y = 0.03/ggplot2::.pt, min.segment.length = 2,
                                                    size = self$options$labelSize/ggplot2::.pt, seed = 123)
            plot <- plot + ggtheme

            # Axes
            plot <- plot + ggplot2::coord_fixed(clip = "off")

            # Titles & Labels
            defaults <- list(title = .("Discrimination Plot"), y = dim2name, x = dim1name)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = "discrim", plot = plot) + vijTitleAndLabelFormat(self$options)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .mainplot = function(plotType, image, ggtheme, theme) {
            # Long by design (reviewed 2026-09-30): a linear `plot <- plot + ...` build; splitting
            # it would scatter the plot's construction without simplifying it.
            res <- image$state
            if (is.null(res))
                return(FALSE)

            #### Define the dimensions ####
            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            dim1name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = dim1, perc = round(res$eig[dim1,2]*100,1))
            dim2name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = dim2, perc = round(res$eig[dim2,2]*100,1))

            #### Prepare data ####
            # catdata/obsdata are only built (hence only need res$cat/res$ind) for
            # the plot types that actually render them below - "obs" never reads
            # catdata, "cat" never reads obsdata. res$cat$coord/res$ind$coord are
            # already whichever of coord/stdcoord self$options$normalization calls
            # for - picked once in .run(), before building each plot's state.
            if (plotType != "obs") {
                catdata <- as.data.frame(res$cat$coord[,c(dim1,dim2)])
                colnames(catdata) <- c("x","y")
            }
            if (plotType != "cat") {
                obsdata <- as.data.frame(round(res$ind$coord[,c(dim1,dim2)],4))
                colnames(obsdata) <- c("x","y")

                # Height of the obs/bi plot (to nudge geom_text)
                ggheight <- if (plotType == "biplot")
                    max(obsdata$y,catdata$y) - min(obsdata$y,catdata$y)
                else
                    max(obsdata$y) - min(obsdata$y)
            }

            # Start the plot
            plot <- ggplot2::ggplot()

            #### Observation Plot ####

            if (plotType != "cat") {
                # plot the points
                if (self$options$propPoint)
                    plot <- plot + ggplot2::geom_count(data = obsdata, ggplot2::aes(x = x, y = y), shape = 15, color = self$options$obsColor, show.legend = FALSE)
                else
                    plot <- plot + ggplot2::geom_point(data = obsdata, ggplot2::aes(x = x, y = y), shape = 15, color = self$options$obsColor)
                # plot the labels
                if (!is.null(self$options$labelVar)) {
                    obsdata$label <- res$rowlabels
                    if (self$options$ggrepel) {
                        plot <- plot + ggrepel::geom_text_repel(data = unique(obsdata),
                                                                ggplot2::aes(x = x, y = y, label = label),
                                                                size = self$options$labelSize/ggplot2::.pt,
                                                                color = self$options$obsColor, seed = 123)
                    } else {
                        plot <- plot + ggplot2::geom_text(data = unique(obsdata),
                                                          ggplot2::aes(x = x, y = y, label = label),
                                                    size = self$options$labelSize/ggplot2::.pt,
                                                    color = self$options$obsColor,
                                                    nudge_y = ggheight*0.03, hjust = 0.5, check_overlap = TRUE)
                    }
                }
            }

            #### Category Plot ####
            if (plotType != "obs") {
                catdata$level <- res$cat$level
                # Order color levels
                catDisplayFactors <- unname(res$varDisplayName[res$cat$factors])
                catdata$factors <- factor(catDisplayFactors, levels = unique(catDisplayFactors), ordered = TRUE)
                if (self$options$boldCat) {
                    catFace <- "bold"
                    supCatFace <- "bold.italic"
                    catPtSize <- 3
                } else {
                    catFace <- "plain"
                    supCatFace <- "italic"
                    catPtSize <- 2
                }
                # Plotting
                plot <- plot + ggplot2::geom_point(data = catdata, ggplot2::aes(x = x, y = y, color = factors, shape = factors), size = catPtSize)
                plot <- plot + ggrepel::geom_text_repel(data = catdata,
                                                        ggplot2::aes(x = x, y = y, label = level, color = factors, fontface = factors),
                                                        size = self$options$labelSize/ggplot2::.pt, show.legend = FALSE, seed = 123)
                if (self$options$connectOrdinalCat) {
                    ordDisplayFactors <- unique(catDisplayFactors[res$cat$factors %in% res$ordVars])
                    plot <- plot + ggplot2::geom_path(data = catdata[catdata$factors %in% ordDisplayFactors,],
                                                      ggplot2::aes(x = x, y = y, color = factors), show.legend = FALSE)
                }
            }

            plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::geom_vline(xintercept = 0, linetype = 2)

            #### Theme and colors ####
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "color", theme)

            # Shape
            varShapes <- c(rep(16, res$varActive[1]), rep(17, res$varActive[2]))
            if (plotType != "obs")
                plot <- plot + ggplot2::scale_shape_manual(values = varShapes)
            # FontFace
            if (plotType != "obs") {
                varFontface <- c(rep(catFace, res$varActive[1]), rep(supCatFace, res$varActive[2]))
                plot <- plot + ggplot2::scale_discrete_manual("fontface", values = varFontface )
            }

            # Plot frame & coord
            plot <- plot + ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0),
                                          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1))
            plot <- plot + ggplot2::coord_fixed()

            #### Plot title ####
            title <- switch(plotType,
                                   cat = .("Category Plot"),
                                   obs = .("Observation Plot"),
                                   biplot = .("Biplot")
            )
            if (plotType == "obs") {
                if (self$options$normalization %in% c("principal", "obsprincipal"))
                    subtitle <- .("Principal coordinates")
                else
                    subtitle <- .("Standard coordinates")
            } else if (plotType == "cat") {
                if (self$options$normalization %in% c("principal", "catprincipal"))
                    subtitle <- .("Principal coordinates")
                else
                    subtitle <- .("Standard coordinates")
            } else { # biplot
                subtitle <- switch(self$options$normalization,
                                           principal = .("Principal coordinates"),
                                           obsprincipal = .("Observation principal coordinates"),
                                           catprincipal = .("Category principal coordinates"),
                                           standard = .("Standard coordinates")
                        )
            }

            # Titles & Labels
            defaults <- list(title = title, subtitle = subtitle, y = dim2name, x = dim1name, legend = .("Variables"))
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType, plot = plot) + vijTitleAndLabelFormat(self$options)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .categoryplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'cat', image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'biplot', image, ggtheme, theme))
        },
        .obsplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'obs', image, ggtheme, theme))
        },
        .saveCoordinates = function(coord, type) {
            if (self$options$obsCoordOV && self$results$obsCoordOV$isNotFilled()) {
                nDim <- self$options$dimNum
                keys <- seq_len(nDim)
                measureTypes <- rep("continuous", nDim)

                titles <- vapply(keys, function(k) jmvcore::format(.("Dim {n}"), n = k), character(1))

                methodStr <- switch(self$options$method,
                                    "Indicator" = .("Indicator matrix"),
                                    "Burt" = .("Burt matrix"))

                if (type == "principal")
                    descriptionString <- jmvcore::format(.("MCA Principal Coordinates ({method})"), method = methodStr)
                else
                    descriptionString <- jmvcore::format(.("MCA Standard Coordinates ({method})"), method = methodStr)


                descriptions <- character(length(keys))
                for (i in keys) {
                    descriptions[i] = descriptionString
                }

                self$results$obsCoordOV$set(
                    keys=keys,
                    titles=titles,
                    descriptions=descriptions,
                    measureTypes=measureTypes
                )

                self$results$obsCoordOV$setRowNums(rownames(coord))

                for (i in seq_len(nDim))
                    self$results$obsCoordOV$setValues(index=i, coord[, i])
            }

        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes <strong>Multiple Correspondence Analysis (MCA)</strong> for several categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=ca" target="_blank">ca package</a> by M. Greenacre, O. Nenadic, M. Friendly.</p>
<p>Both classic methods are available:</p>
<ul>
<li><strong>Indicator matrix:</strong> CA of the indicator matrix</li>
<li><strong>Burt matrix:</strong> CA of the Burt matrix. The eigenvalues are the squares of those of the indicator matrix method. </li>
</ul>
<p>Both methods give the same <em>standard</em> coordinates and discriminations, but different <em>principal</em> coordinates.</p>
<p>With the <strong>Burt method</strong>, since observations are not part of the Burt matrix, their standard coordinates come from the indicator method
and their principal coordinates are scaled like those of the categories. Cos² and QLT are then computed in the Burt space.</p>
<p>When selected, <strong>Benzécri and Greenacre corrections</strong> are applied to eigenvalues only (<strong>Summary</strong> table). Principal coordinates (and inertia) of categories and observations are computed from the original eigenvalues of the Burt matrix.</p>
<p>The <strong>Normalization</strong> options specify how the coordinates are scaled (by the square roots of the eigenvalues):</p>
<ul>
<li><strong>Principal:</strong> Both category and observation coordinates are scaled.</li>
<li><strong>Category principal:</strong> Only category coordinates are scaled.</li>
<li><strong>Observation principal:</strong> Only observation coordinates are scaled.</li>
<li><strong>Standard:</strong> Both category and observation coordinates are standard.</li>
</ul>
<p>A sample file is included at Open > Data Library > vijMulti > Cars</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
