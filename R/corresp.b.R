correspClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "correspClass",
    inherit = correspBase,
    #### Active bindings ---- from jmv/conttables.b.R
    active = list(
        countsName = function() {
            if ( ! is.null(self$options$counts)) {
                return(self$options$counts)
            } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
                return (attr(self$data, "jmv-weights-name"))
            }
            NULL
        }
    ),
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
        .getData = function() {
            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            if (is.null(rowVarName) || is.null(colVarName))
                return(NULL)

            data <- jmvcore::select(self$data, c(rowVarName, colVarName))

            # Weight data
            countsName <- self$options$counts
            if (!is.null(countsName)) {
                # vijPlots/Mosaic weights
                data[['.COUNTS']] <- jmvcore::toNumeric(self$data[[countsName]])
            } else if (!is.null(attr(self$data, "jmv-weights"))) {
                # jamovi built-in weights
                data[['.COUNTS']] <- jmvcore::toNumeric(attr(self$data, "jmv-weights"))
            } else {
                # no weights
                data[['.COUNTS']] <- as.integer(rep(1, nrow(data)))
            }

            data <- droplevels(jmvcore::naOmit(data))   # categories left without observations are dropped
            return(data)
        },
        # Contingency table cross-tabulated from an observation table (NULL if no data)
        .contTableFromObs = function() {
            data <- private$.getData()
            if (is.null(data))
                return(NULL)
            if (nrow(data) == 0) {
                vijErrorMessage(self, .("Not enough data to compute CA."))
            }

            if (any(data$.COUNTS < 0)) {
                vijErrorMessage(self, .('Counts may not be negative.'))
            }
            if (any(is.infinite(data$.COUNTS))) {
                vijErrorMessage(self, .('Counts may not be infinite.'))
            }

            formula <- jmvcore::composeFormula('.COUNTS', c(self$options$rows, self$options$cols))
            return(stats::xtabs(formula, data))
        },
        # Contingency table read as-is from the Columns cells (NULL if no data)
        .contTableFromCells = function() {
            if (is.null(self$options$rowLabels) || length(self$options$columns) < 3 || nrow(self$data) == 0)
                return(NULL)

            contingencyTable <- jmvcore::select(self$data, self$options$columns)
            for (colName in self$options$columns)
                contingencyTable[[colName]] <- jmvcore::toNumeric(contingencyTable[[colName]])

            if (anyNA(contingencyTable)) {
                vijErrorMessage(self, .("Some values are missing from the contingency table."))
            }
            if (any(contingencyTable < 0)) {
                vijErrorMessage(self, .('Counts may not be negative.'))
            }
            rowLabels <- self$data[[self$options$rowLabels]]
            if (anyNA(rowLabels))
                vijErrorMessage(self, .("Row labels may not be missing."))
            if (anyDuplicated(rowLabels))
                vijErrorMessage(self, .("Row labels must be unique."))
            row.names(contingencyTable) <- rowLabels
            return(as.matrix(contingencyTable))
        },
        .getProfile = function(contingencyTable, supplementaryRows, supplementaryCols) {
            # Row profiles of every row (active and supplementary) over the active columns only,
            # plus a ".mass" row (active column masses). Supplementary columns are left out:
            # their profiles (the ones projected) are in the column profiles.
            activeCols <- setdiff(seq_len(ncol(contingencyTable)), supplementaryCols)
            activeRows <- setdiff(seq_len(nrow(contingencyTable)), supplementaryRows)
            rowProfiles <- contingencyTable[, activeCols, drop = FALSE]
            rowProfiles <- rbind(rowProfiles, colSums(rowProfiles[activeRows, , drop = FALSE]))
            rowProfiles <- proportions(rowProfiles, margin = 1)
            rowProfiles <- stats::addmargins(rowProfiles, margin = 2)
            # Margin keys, translated by .displayName() when filling the table
            rownames(rowProfiles)[nrow(rowProfiles)] <- ".mass"
            colnames(rowProfiles)[ncol(rowProfiles)] <- ".margin"
            return(rowProfiles)
        },
        .getContingencyTable = function(contingencyTable, supplementaryRows, supplementaryCols) {
            savedSupplementaryRows <- contingencyTable[supplementaryRows,, drop = FALSE]
            savedSupplementaryCols <- contingencyTable[,supplementaryCols, drop = FALSE]
            contingencyTable[supplementaryRows,] <- 0                # set supplementary rows to 0
            contingencyTable[,supplementaryCols] <- 0                # Empty supplementary columns
            contingencyTable <- stats::addmargins(contingencyTable, margin=c(1,2))    # Add margin row (sum)
            # Set the supplementary rows and columns back
            contingencyTable[supplementaryRows,1:(ncol(contingencyTable)-1)] <- savedSupplementaryRows
            contingencyTable[1:(nrow(contingencyTable)-1),supplementaryCols] <- savedSupplementaryCols
            # Delete values and margins for supplementary rows/columns
            for (i in supplementaryRows) {
                for (j in supplementaryCols) {
                    contingencyTable[i,j] <- NA
                }
            }
            contingencyTable[supplementaryRows,ncol(contingencyTable)] <- NA
            contingencyTable[nrow(contingencyTable), supplementaryCols] <- NA
            return(contingencyTable)
        },
        .displayName = function(key) {
            # Table column keys and row labels: the margins use reserved keys (".margin", ".mass")
            # rather than their translated names, so a category can't collide with them
            switch(key, ".margin" = .("Active Margin"), ".mass" = .("Mass"), key)
        },
        .fillProfileTable = function(profileTable, profiles, supplementary, suppText, rowName, colName) {
            # Built in .run(), not .init(), like the contingency table (see .fillContingencyTable):
            # rows/columns and "*" marks depend on the data and the validated supplementary options.
            profileTable$addColumn(".row", type = "text", title = rowName)
            for (col in colnames(profiles)) {
                profileTable$addColumn(col, title = private$.displayName(col), type = "number", format = "zto", superTitle = colName)
            }
            for (i in seq(nrow(profiles))) {
                profileTable$addRow(i, values = profiles[i,])
                profileTable$setCell(rowNo = i, ".row", private$.displayName(rownames(profiles)[i]))
            }
            profileTable$addFormat(rowNo = nrow(profiles), 1, jmvcore::Cell.BEGIN_END_GROUP)
            if (!is.null(supplementary))
                profileTable$setNote("supp", paste("*", suppText))
        },
        .fillContingencyTable = function(table, contingencyTable, supplementaryRows, supplementaryCols,
                                          rowVarNameString, colVarNameString) {
            # Built in .run(), not .init(), on purpose: the column type (integer vs number) depends on
            # the actual count values (a "number" column shows 12 as 12.00), in contTable mode the rows
            # come from the Row Labels values, and the "*" marks need the validated supplementary options.
            fullTable <- private$.getContingencyTable(contingencyTable, supplementaryRows, supplementaryCols)
            # with decimal weights/counts, Contingency Table uses type = "number"
            isDecimal <- any(abs(fullTable - round(fullTable)) > sqrt(.Machine$double.eps), na.rm = TRUE)
            # Margin keys, translated by .displayName() when filling the table
            rownames(fullTable)[nrow(fullTable)] <- ".margin"
            colnames(fullTable)[ncol(fullTable)] <- ".margin"
            table$addColumn(".row", type="text", title = rowVarNameString)
            for (col in colnames(fullTable)) {
                if (col != ".margin")
                    table$addColumn(col, type = ifelse(isDecimal,"number","integer"), superTitle = colVarNameString)
                else
                    table$addColumn(col, title = private$.displayName(col), type = ifelse(isDecimal,"number","integer"))
            }
            for (i in seq(nrow(fullTable))) {
                table$addRow(i, values = fullTable[i,])
                table$setCell(rowNo = i, ".row", private$.displayName(rownames(fullTable)[i]))
            }
            table$addFormat(rowNo = nrow(fullTable), 1, jmvcore::Cell.BEGIN_END_GROUP)
            # Change NaN/NA to NULL. Is there another way to have empty cells ?
            for (i in seq(nrow(fullTable))) {
                for (j in seq(ncol(fullTable))) {
                    if (is.na(fullTable[i,j]))
                        table$setCell(rowNo = i, colnames(fullTable)[j], NULL)
                }
            }
            if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                table$setNote("supp", paste("*", .("Supplementary rows/columns")))
        },
        # Position of a warning raised in .run(): below the weights notice added by .init(), if any
        .warningPos = function() {
            weighted <- if (self$options$mode == "obsTable") !is.null(self$countsName)
                        else !is.null(attr(self$data, "jmv-weights-name"))
            if (weighted) 2 else 1
        },
        .getVarNameStrings = function() {
            # row/col display names, as shown in the summary table column headers and the contingency table
            if (self$options$mode == "obsTable") {
                list(row = private$.getVarName(self$options$rows), col = private$.getVarName(self$options$cols))
            } else { # contTable
                list(row = self$options$rowLabels, col = self$options$columnTitle)
            }
        },
        .normalizationString = function() {
            # summary tables note and plot subtitle
            switch(self$options$normalization,
                   principal = .("Principal normalization"),
                   symmetric = .("Symmetric normalization"),
                   rowprincipal = .("Row principal normalization"),
                   colprincipal = .("Column principal normalization"),
                   standard = .("Standard normalization"))
        },
        .initSummaryTable = function(table, labelCol, labelTitle, nDim) {
            # labelCol = "row" or "col"
            # labelTitle = col/rowVarNameString
            dimN <- function(n) jmvcore::format(.("Dim {n}"), n = n)
            table$addColumn(name = "id", title = "#", type = "integer")
            table$addColumn(name = labelCol, title = labelTitle, type = "text")
            table$addColumn(name = "margin", title = .("Mass"), type = "number", format = "zto")
            table$addColumn(name = "inertia", title = .("Rel. Inertia"), type = "number", format = "zto")
            table$addColumn(name = "qlt", title = .("QLT"), type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("score",i), title = dimN(i), superTitle = paste(.("Coordinates"),"†"), type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("contrib",i), title = dimN(i), superTitle = .("Contributions"), type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("cos",i), title = dimN(i), superTitle = .("Cos²"), type = "number", format = "zto")
        },
        .fillSummaryTable = function(table, points, labelCol, supplementary, suppText, nDim, normalizationString) {
            # table = table to fill
            # points = res$row or res$col (all rows/cols, table order; NA for undefined values of supplementary points)
            # labelCol = "row" or "col"
            blank <- function(x) if (is.na(x)) "" else x
            for (i in seq_len(nrow(points$coord))) {
                theValues <- list(id = i, margin = blank(points$mass[i]), inertia = blank(points$inertia[i]),
                                  qlt = sum(points$cos2[i, 1:nDim], na.rm = TRUE))
                theValues[[labelCol]] <- rownames(points$coord)[i]
                for (j in seq(nDim)) {
                    theValues[[paste0("score",j)]] <- points$coord[i,j]
                    theValues[[paste0("contrib",j)]] <- blank(points$contrib[i,j])
                    theValues[[paste0("cos",j)]] <- points$cos2[i,j]
                }
                table$addRow(i, values = theValues)
            }
            if (!is.null(supplementary))
                table$setNote("supp", paste("*", suppText))
            table$setNote("norm", paste("†", normalizationString))
        },
        .parseSupplementary = function(optionValue, nmax, parseErrorMsg, rangeErrorMsg) {
            if (is.null(optionValue) || optionValue == "0" || optionValue == "")
                return(NULL)
            supp <- suppressWarnings(as.integer(unlist(strsplit(optionValue, ","))))   # non-numbers become NA, rejected below
            if (any(is.na(supp))) {
                vijErrorMessage(self, parseErrorMsg)
            } else {
                supp <- sort(unique(supp))
                if (!all(supp %in% 1:nmax))
                    vijErrorMessage(self, jmvcore::format(rangeErrorMsg, nmax = nmax))
            }
            supp
        },
        # Category names: reject the reserved keys, warn about a trailing "*" (the supplementary mark),
        # then mark the supplementary rows and columns with " *"
        .checkCategoryNames = function(contingencyTable, supplementaryRows, supplementaryCols) {
            catNames <- c(rownames(contingencyTable), colnames(contingencyTable))
            # ".row", ".margin" and ".mass" are used as row/column keys
            reservedNames <- intersect(catNames, c(".row", ".margin", ".mass"))
            if (length(reservedNames) > 0) {
                vijErrorMessage(self, jmvcore::format(.("The category names {names} are reserved. Please rename these categories."),
                                                      names = paste(reservedNames, collapse = ", ")))
            }
            if (length(supplementaryRows) + length(supplementaryCols) > 0 && any(grepl("\\*\\s*$", catNames))) {
                vijWarningMessage(self, .('Some row or column names end with "*", which is also used to mark supplementary rows and columns. Consider renaming them to avoid confusion.'),
                                  pos = private$.warningPos())
            }
            for (i in supplementaryRows)
                rownames(contingencyTable)[i] <- paste(rownames(contingencyTable)[i], "*")
            for (j in supplementaryCols)
                colnames(contingencyTable)[j] <- paste(colnames(contingencyTable)[j], "*")
            return(contingencyTable)
        },
        .computeChiSquared = function(contingencyTable, supplementaryRows, supplementaryCols) {
            activeContingencyTable <- contingencyTable
            if (!is.null(supplementaryRows))
                activeContingencyTable <- activeContingencyTable[-supplementaryRows,, drop = FALSE]
            if (!is.null(supplementaryCols))
                activeContingencyTable <- activeContingencyTable[,-supplementaryCols, drop = FALSE]

            # Every row (column), active or supplementary, needs counts in the active columns (rows):
            # otherwise its profile, hence its coordinates, are undefined
            activeRows <- setdiff(seq_len(nrow(contingencyTable)), supplementaryRows)
            activeCols <- setdiff(seq_len(ncol(contingencyTable)), supplementaryCols)
            if (any(rowSums(contingencyTable[, activeCols, drop = FALSE]) == 0) ||
                any(colSums(contingencyTable[activeRows, , drop = FALSE]) == 0)) {
                vijErrorMessage(self, .("Some categories have zero counts and must be removed."))
            }

            chisqres <- tryCatch(
                            suppressWarnings(stats::chisq.test(activeContingencyTable)),
                            error = function (e) NULL
                        )

            if (is.null(chisqres) || !is.finite(chisqres$statistic) ) {
                vijErrorMessage(self, .("Unable to compute the χ² statistic."))
            }
            if (chisqres$statistic <= .Machine$double.eps) {
                vijErrorMessage(self, .("The χ² statistic is equal to zero."))
            }

            return(chisqres)
        },
        .fillChisqTable = function(table, chisqres) {
            table$setRow(rowNo = 1, values = list(
                statistic = chisqres$statistic,
                df = chisqres$parameter,
                p = chisqres$p.value
            ))
            observed <- chisqres$observed
            if (any(abs(observed - round(observed)) > sqrt(.Machine$double.eps)))
                table$setNote("nonint", .("Counts are not integers. The χ² test may not be valid."))
            else
                table$setNote("nonint", NULL)
        },
        .fillInertiaTable = function(table, res) {
            # Populate the inertia table
            for (i in seq_along(res$sv)) {
                table$addRow(i, values = list(
                    dim = i,
                    singular = res$sv[i],
                    inertia = res$eig[i,1],
                    proportion = res$eig [i,2],
                    cumulative = res$eig [i,3]
                ))
            }
            # Add total row
            table$addRow(rowKey="Total", values = list(
                dim = .("Total"),
                singular = "",
                inertia = sum(res$eig[,1]),
                proportion = 1,
                cumulative = NA
            ))
            table$addFormat(rowKey="Total", 1, jmvcore::Cell.BEGIN_END_GROUP)
        },
        .init = function() {
            if (self$options$mode == "obsTable")
                hasVars <- !is.null(self$options$rows) && !is.null(self$options$cols)
            else # contTable
                hasVars <- !is.null(self$options$rowLabels) && length(self$options$columns) >= 3

            if (!hasVars) {
                private$.showHelpMessage()
            } else if (self$options$mode == "obsTable") {
                # Weight message
                countsName <- self$countsName
                if (!is.null(countsName)) {
                    warningMessage <- ..('The data is weighted by the variable {}.', countsName)
                    vijWarningMessage(self, warningMessage, '.weights')
                }
            } else if (!is.null(attr(self$data, "jmv-weights-name"))) { # contTable
                # jamovi weights are not applied to a contingency table
                vijWarningMessage(self, .('Row weights are ignored when the data is a contingency table.'), '.weights')
            }

            # Init row/col Summary Tables (adding columns)
            if (hasVars && self$options$showSummaries) {
                varNames <- private$.getVarNameStrings()
                nDim <- self$options$dimNum
                private$.initSummaryTable(self$results$rowSummary, "row", varNames$row, nDim)
                private$.initSummaryTable(self$results$colSummary, "col", varNames$col, nDim)
            }
        },
        .run = function() {
            # Long by design (reviewed 2026-09-30): after building the contingency table, it is a
            # linear validate -> compute -> fill-tables sequence. Only the two input modes' data
            # preparation was worth extracting; further splitting would just scatter it.

            # Clear χ² test note (from 1.0.0 version).
            self$results$eigenvalues$setNote("chisq", NULL)

            contingencyTable <- if (self$options$mode == "obsTable")
                private$.contTableFromObs()
            else # contTable
                private$.contTableFromCells()
            if (is.null(contingencyTable))
                return(FALSE)

            # Set variable names
            varNames <- private$.getVarNameStrings()
            rowVarNameString <- varNames$row
            colVarNameString <- varNames$col

            #### Supplementary Rows & Column ####

            supplementaryRows <- private$.parseSupplementary(
                self$options$supplementaryRows, nrow(contingencyTable),
                .("Supplementary row numbers must be a list of numbers, e.g. 1,2,9"),
                .("Supplementary row numbers must be between 1 and {nmax}.")
            )
            supplementaryCols <- private$.parseSupplementary(
                self$options$supplementaryCols, ncol(contingencyTable),
                .("Supplementary column numbers must be a list of numbers, e.g. 1,2,9"),
                .("Supplementary column numbers must be between 1 and {nmax}.")
            )

            contingencyTable <- private$.checkCategoryNames(contingencyTable, supplementaryRows, supplementaryCols)

            #### Normalisation ####

            normalizationString <- private$.normalizationString()

            #### Contingency Table (with supplementary rows/columns ####

            private$.fillContingencyTable(self$results$contingency, contingencyTable, supplementaryRows, supplementaryCols,
                                           rowVarNameString, colVarNameString)

            #### Row and Column Profile Tables ####

            if(self$options$showProfiles) {
                # Row Profiles (supplementary columns left out)
                rowProfiles <- private$.getProfile(contingencyTable, supplementaryRows, supplementaryCols)
                private$.fillProfileTable(self$results$rowProfiles, rowProfiles, supplementaryRows,
                                          .("Supplementary rows"), rowVarNameString, colVarNameString)
                # Column Profiles (supplementary rows left out)
                colProfiles <- t(private$.getProfile(t(contingencyTable),supplementaryCols, supplementaryRows))
                private$.fillProfileTable(self$results$colProfiles, colProfiles, supplementaryCols,
                                          .("Supplementary columns"), rowVarNameString, colVarNameString)
            }

            # Check minimum dimension
            maxDim <- min(nrow(contingencyTable)-length(supplementaryRows), ncol(contingencyTable)-length(supplementaryCols)) - 1
            if (maxDim < 2) {
                vijErrorMessage(self, .("Not enough data to compute CA."))
            }

            #### Chi-Squared test ####

            chisqres <- private$.computeChiSquared(contingencyTable, supplementaryRows, supplementaryCols)

            if (self$options$showChisq)
                private$.fillChisqTable(self$results$chisq, chisqres)

            # Check solution dimension
            nDim <- self$options$dimNum
            if (nDim > maxDim) {
                errorMessage <- jmvcore::format(.("Number of dimensions must be less than or equal to {maxDim}."), maxDim = maxDim)
                vijErrorMessage(self,errorMessage)
            }

            #### Compute CA ####
            res <- tryCatch(
                    private$.ca(contingencyTable, row.sup = supplementaryRows, col.sup = supplementaryCols, ncp = nDim, norm = self$options$normalization),
                    error = function (e) NULL
                )

            if (is.null(res) ) {
                vijErrorMessage(self, .("Unable to compute correspondence analysis for the selected data."))
            }

            #### Inertia Table ####

            private$.fillInertiaTable(self$results$eigenvalues, res)

            #### Summary Tables ####

            if(self$options$showSummaries) {
                private$.fillSummaryTable(self$results$rowSummary, res$row, "row", supplementaryRows,
                                           .("Supplementary rows"), nDim, normalizationString)
                private$.fillSummaryTable(self$results$colSummary, res$col, "col", supplementaryCols,
                                           .("Supplementary columns"), nDim, normalizationString)
            }

            # Check axis values
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            if (xaxis > nDim || yaxis > nDim) {
                errorMessage <- jmvcore::format(.("Axis numbers must be less than or equal to the number of dimensions ({nDim})."), nDim = nDim)
                vijErrorMessage(self, errorMessage)
            }
            if (xaxis == yaxis) {
                vijErrorMessage(self, .("Axis numbers cannot be equal."))
            }
            if (res$sv[max(xaxis, yaxis)] < .Machine$double.eps) {
                message <- jmvcore::format(.("The singular value for dimension {n} is close to zero. The plots may not be accurate."), n = max(xaxis, yaxis))
                pos <- private$.warningPos()
                vijWarningMessage(self, message, pos = pos)
            }

            #### Plots ####

            private$.preparePlots(res, rowVarNameString, colVarNameString)
        },
        # Builds every plot's image$state from the CA result
        .preparePlots = function(res, rowVarNameString, colVarNameString) {
            if (self$options$showRowPlot) {
                self$results$rowplot$setState(list(
                    eig = res$eig,
                    row = res$row[c("coord", "sup")],
                    rowVarNameString = rowVarNameString
                ))
            }
            if (self$options$showColPlot) {
                self$results$colplot$setState(list(
                    eig = res$eig,
                    col = res$col[c("coord", "sup")],
                    colVarNameString = colVarNameString
                ))
            }
            if (self$options$showBiPlot) {
                self$results$biplot$setState(list(
                    eig = res$eig,
                    row = res$row[c("coord", "sup")],
                    col = res$col[c("coord", "sup")],
                    rowVarNameString = rowVarNameString,
                    colVarNameString = colVarNameString
                ))
            }
        },
        .ca = function(contingencyTable, ncp = 2, row.sup = NULL, col.sup = NULL, norm = "principal") {
            # CA based on ca::ca. Changed from factoMineR::CA with vijMulti 1.1.0
            #### CA ####
            ca <- ca::ca(contingencyTable, nd = NA,
                         suprow = if (is.null(row.sup)) NA else row.sup,
                         supcol = if (is.null(col.sup)) NA else col.sup)
            sv <- ca$sv                                    # singular values
            eig <- sv^2                                    # eigenvalues (principal inertias)
            K <- length(sv)
            dims <- seq_len(ncp)
            actCol <- setdiff(seq_along(ca$colnames), col.sup)

            #### Axis orientation ####
            # Arbitrary in the computation: on each axis, the active column farthest from the origin is positive
            flip <- apply(ca$colcoord[actCol, , drop = FALSE], 2, function(x) sign(x[which.max(abs(x))]))
            rowStd <- sweep(ca$rowcoord, 2, flip, FUN = "*")
            colStd <- sweep(ca$colcoord, 2, flip, FUN = "*")
            dimnames(rowStd) <- list(ca$rownames, paste("Dim", seq_len(K)))   # "Dim k": used by the plots
            dimnames(colStd) <- list(ca$colnames, paste("Dim", seq_len(K)))

            #### Eigenvalues ####
            res <- list()
            res$sv <- sv
            res$eig <- cbind("eigenvalue" = eig,
                             "percentage of variance" = eig / sum(eig),
                             "cumulative percentage of variance" = cumsum(eig) / sum(eig))

            #### Rows and columns ####
            # Coordinates scaled according to the normalization; cos2 and contributions use principal coordinates
            rowScale <- switch(norm, principal = sv, symmetric = sqrt(sv), rowprincipal = sv, colprincipal = 1, standard = 1)
            colScale <- switch(norm, principal = sv, symmetric = sqrt(sv), rowprincipal = 1, colprincipal = sv, standard = 1)
            # All rows (columns) in table order; mass, contributions and inertia are NA for supplementary points
            points <- function(std, mass, dist, inertia, sup, scale) {
                mass[sup] <- NA
                list(coord = sweep(std, 2, scale, FUN = "*")[, dims, drop = FALSE],
                     contrib = (mass * std^2)[, dims, drop = FALSE],
                     cos2 = (sweep(std, 2, sv, FUN = "*")^2 / dist^2)[, dims, drop = FALSE],
                     inertia = ifelse(is.na(mass), NA, inertia / sum(eig)),
                     mass = mass,
                     sup = seq_len(nrow(std)) %in% sup)
            }
            res$row <- points(rowStd, ca$rowmass, ca$rowdist, ca$rowinertia, row.sup, rowScale)
            res$col <- points(colStd, ca$colmass, ca$coldist, ca$colinertia, col.sup, colScale)
            return(res)
        },
        .caplot = function(plotType, image, ggtheme, theme) {
            if (is.null(image$state))
                return(FALSE)

            # Plot data
            res <- image$state
            # sup: 1 = row, 2 = rowsup, 3 = column, 4 = colsup
            toDF <- function(points, type) {
                df <- data.frame(points$coord, sup = type + points$sup, label = rownames(points$coord), check.names = FALSE)
                df[order(points$sup), ]   # active points first, then supplementary ones
            }
            ptcoord <- rbind(
                if (plotType != 'column') toDF(res$row, 1),
                if (plotType != 'row') toDF(res$col, 3)
            )
            ptcoord$sup <- factor(ptcoord$sup, levels = c(1,2,3,4))
            # ptcoord dataframe containt the row and column coordinates
            # ptcoord$sup is the type of point (1 = row, 2 = rowsup, 3 = column, 4 = colsup)

            # Plot inertia
            percentInertia <- round(100*res$eig[,2], 1)
            # Plot axis
            xaxis <- self$options$xaxis
            xaxisdim <- paste("Dim", xaxis)
            dim1name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = xaxis, perc = percentInertia[xaxis])
            yaxis <- self$options$yaxis
            yaxisdim <- paste("Dim", yaxis)
            dim2name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = yaxis, perc = percentInertia[yaxis])

            # Building the plot
            plot <-  ggplot2::ggplot(ptcoord, ggplot2::aes(x = .data[[xaxisdim]], y = .data[[yaxisdim]], color = .data[["sup"]], shape = .data[["sup"]]))
            plot <- plot + ggplot2::geom_point()
            plot <- plot + ggrepel::geom_text_repel(ggplot2::aes(label = .data[["label"]]), show.legend = FALSE, size = self$options$labelSize/ggplot2::.pt, seed = 123)
            plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::geom_vline(xintercept = 0, linetype = 2)

            # Apply jmv theme
            plot <- plot + ggtheme

            # Set point colors
            plot <- plot +
                ggplot2::scale_color_manual(values = c("1" = self$options$rowColor, "2" = self$options$supColor,
                                                       "3" = self$options$colColor, "4" = self$options$supColor)) +
                ggplot2::scale_shape_manual(values = c("1" = 19, "2" = 19, "3" = 17, "4" = 17)) +
                ggplot2::guides(color = "none", shape = "none")

            # Plot frame
            plot <- plot + ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0), panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1))

            # Plot title
            title <- switch(plotType,
                            row = jmvcore::format(.("Row Points for {rows}"), rows = res$rowVarNameString),
                            column = jmvcore::format(.("Column Points for {cols}"), cols = res$colVarNameString),
                            biplot = jmvcore::format(.("Row and Column Points for {rows} and {cols}"),
                                                     rows = res$rowVarNameString,
                                                     cols = res$colVarNameString)
            )
            # Plot subtitle
            subtitle <- private$.normalizationString()

            # Titles & Labels
            defaults <- list(title = title, subtitle = subtitle, y = dim2name, x = dim1name)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .rowplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'row', image, ggtheme, theme))
        },
        .colplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'column', image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'biplot', image, ggtheme, theme))
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes <strong>Correspondence Analysis (CA)</strong> for two categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=ca" target="_blank">ca package</a> by M. Greenacre, O. Nenadic, M. Friendly.</p>
<p>The data can be</p>
<ul>
<li>an <strong>Observation table</strong> (raw data), possibly weighted using <em>jamovi</em> built-in weight system or using the "Counts" variable</li>
<li>or a <strong>Contingency table</strong></li>
</ul>
<p><strong>Supplementary row or column</strong> numbers may be entered as integer lists: 1,3,6</p>
<p>Five normalizations (scaling of row and column scores before plotting) are available:</p>
<ul>
<li><strong>Principal:</strong> Row and column scores are in principal coordinates, i.e. standard coordinates scaled by the singular values.</li>
<li><strong>Symmetric:</strong> Row and column scores are standard coordinates scaled by the square root of the singular values.</li>
<li><strong>Row principal:</strong> Row scores are in principal coordinates, column scores in standard coordinates.</li>
<li><strong>Column principal:</strong> Column scores are in principal coordinates, row scores in standard coordinates.</li>
<li><strong>Standard:</strong> Row and column scores are in standard coordinates (unit weighted variance on each dimension).</li>
</ul>
<p>A sample file is included at Open > Data Library > vijMulti > Smoking</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
