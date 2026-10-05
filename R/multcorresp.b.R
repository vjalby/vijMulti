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
                if ("ordered" %in% class(data[[aVar]]))
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
                    cat = list(coord = res$cat[[catCoordField]][, seq_len(nDim), drop = FALSE], factors = res$cat$factors),
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
                    cat = list(coord = res$cat[[catCoordField]][, seq_len(nDim), drop = FALSE], factors = res$cat$factors),
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
            # Long by design (reviewed 2026-09-30): one post-processing pass over FactoMineR's result.
            res <- FactoMineR::MCA(data, method = method, ncp = 999, quali.sup = supcol, graph = FALSE)
            res$nd.max <- nrow(res$eig)
            if (nd > res$nd.max)
                return(res)
            # res$rowlabels is used for Observation table and plot
            if (!is.null(rowlabels))
                res$rowlabels <- rowlabels
            else
                res$rowlabels <- as.character(rownames)
            # rownames = rownames(self$data-without-NA) is used for saving coordinates
            rownames(res$ind$coord) <- rownames
            # Build the list of variable names for levels (hmmm, i'm sure there's a better way to do that)
            varNames <- names(data)     # variable list
            if (!is.null(supcol))
                varNames <- varNames[-supcol]   # remove supplementary variables
            varFactors <- c()
            for (aVar in varNames) {
                varFactors <- c(varFactors, rep(aVar, nlevels(data[[aVar]]))) # unused levels are now dropped from the begining
            }
            # Build the list of supplementary variable names for levels
            supFactors <- c()
            if (!is.null(supcol)) {
                varNames <- names(data)
                varNames <- varNames[supcol]
                for (aVar in varNames)
                    supFactors <- c(supFactors, rep(aVar, nlevels(factor(data[[aVar]]))))
            }
            res$cat$factors <- c(varFactors, supFactors)
            # convert % to decimal
            res$eig[,2:3] <- res$eig[,2:3] / 100
            res$var$contrib <- res$var$contrib / 100
            res$ind$contrib <- res$ind$contrib / 100
            # var QLT
            res$var$qlt <- rowSums(res$var$cos2[,1:nd, drop = FALSE])
            # var Inertia
            varinertia <- res$var$contrib %*% res$eig[,1]
            res$var$inertia <- varinertia / sum(varinertia)
            # ind QLT
            res$ind$qlt <- rowSums(res$ind$cos2[,1:nd, drop = FALSE])
            # ind Inertia
            indinertia <- res$ind$contrib %*% res$eig[,1]
            res$ind$inertia <- indinertia / sum(indinertia)
            # Cat Std coordinates
            res$var$stdcoord <- sweep(res$var$coord, 2, sqrt(res$eig [,1]), FUN = "/")
            # Observation Std coordinates
            res$ind$stdcoord <- sweep(res$ind$coord, 2, sqrt(res$eig [,1]), FUN = "/")
            # Observation masses
            res$ind$mass <- res$call$marge.row

            #### Burt fixes ####

            ## Burt: observations are not rows of the Burt table, so their coordinates are a convention.
            ## FactoMineR (and ca::mjca) project them as supplementary rows: pcoord = s*sqrt(lambda),
            ## stdcoord = s/sqrt(lambda), where s = indicator std coord and lambda = indicator eigenvalue.
            ## We follow Stata (predict, rowscores after mca, method(burt)): stdcoord = s (unit variance,
            ## same as Indicator) and pcoord = s*lambda (variance = Burt eigenvalue, same scaling as categories).
            ## Checked against Stata 17 (issp93): identical. cos2 & qlt recomputed; contrib unchanged.

            if (method == "Burt" && TRUE) {
                ## MCA compute Pal Coordinates for Indicator Inertia only. So we have to rebuild std coordinates from
                ## indicator-principal coordinates and redo the computation of co2 & qlt. contrib are unchanged. Inertia computed above is ok.
                # Obs coordinates
                res$ind$stdcoord <- sweep(res$ind$coord, 2, res$eig [,1]**(1/4), FUN = "/")
                res$ind$coord <- sweep(res$ind$stdcoord, 2, sqrt(res$eig [,1]), FUN = "*")
                # Obs CO2
                res$ind$cos2 <- sweep(res$ind$coord**2, 1, rowSums(res$ind$coord**2), FUN = "/")
                res$ind$qlt <- rowSums(res$ind$cos2[,1:nd, drop = FALSE])
            }

            #### Supp Categories ####
            if (!is.null(supcol)) {
                res$quali.sup$qlt <- rowSums(res$quali.sup$cos2[,1:nd, drop = FALSE])
                res$quali.sup$stdcoord <- sweep(res$quali.sup$coord, 2, sqrt(res$eig [,1]), FUN = "/")
            }

            #### All categories ####
            if (!is.null(supcol)) {
                res$cat$coord <- rbind(res$var$coord, res$quali.sup$coord)
                res$cat$stdcoord <- rbind(res$var$stdcoord, res$quali.sup$stdcoord)
                res$cat$cos2 <- rbind(res$var$cos2, res$quali.sup$cos2)
                null <- matrix(NA, nrow(res$quali.sup$coord), ncol(res$quali.sup$coord))
                res$cat$contrib <- rbind(res$var$contrib, null)
                null <- matrix(NA, nrow(res$quali.sup$coord), ncol = 1)
                res$cat$inertia <- rbind(res$var$inertia, null)
                res$cat$qlt <- c(res$var$qlt, res$quali.sup$qlt)
                null <- rep(NA, nrow(res$quali.sup$coord))
                res$cat$mass <- c(res$call$marge.col, null)
            } else {
                res$cat$coord <- res$var$coord
                res$cat$stdcoord <- res$var$stdcoord
                res$cat$cos2 <- res$var$cos2
                res$cat$contrib <- res$var$contrib
                res$cat$inertia <- res$var$inertia
                res$cat$qlt <- res$var$qlt
                res$cat$mass <- res$call$marge.col
            }
            res$varActive <- c(ncol(data) - length(supcol), length(supcol))  # (nb of active var, nb of supp var)
            # All var
            if (!is.null(supcol)) {
                res$allvar$eta2 <- rbind(res$var$eta2,res$quali.sup$eta2)
            } else {
                res$allvar$eta2 <- res$var$eta2
            }

            #### Benzecri / Greenacre Adjusment ####
            if (method == "Burt") {
                p <- length(res$call$quali) # Nb of variables
                m <- length(res$call$marge.col) # Nb of categories
                res$totalInertia <- sum(res$eig[,1])
                res$adjEig <- matrix(nrow = length(res$eig[,1]), ncol = 5)
                res$adjEig[,1] <- (p/(p-1))**2 * (sqrt(res$eig[,1]) - 1/p)**2
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,1] <- 0
                res$totalInrB <- sum(res$adjEig[,1])
                res$totalInrG <- (p/(p-1)) * (res$totalInertia - (m-p)/p**2)
                res$adjEig[,2] <- res$adjEig[,1] / res$totalInrB
                res$adjEig[,3] <- cumsum(res$adjEig[,2])
                res$adjEig[,4] <- res$adjEig[,1] / res$totalInrG
                res$adjEig[,5] <- cumsum(res$adjEig[,4])
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,] <- rep(NA,5)
            }

            # Delete unused large tables (to save memory ?)
            res$var <- NULL
            res$svd <- NULL
            res$call <- NULL
            return(res)
        },
        .mca2 = function(data, method, nd, supcol, rowlabels = NULL, rownames = NULL) {
            # TEST: alternative to .mca() based on ca::mjca instead of FactoMineR::MCA.
            # Returns the same structure as .mca(). Not called anywhere yet; ca is not in DESCRIPTION.
            # Axis signs may differ from .mca() (orientation is arbitrary).
            m <- ca::mjca(data, lambda = if (method == "Burt") "Burt" else "indicator", nd = NA,
                          supcol = if (is.null(supcol)) NA else supcol)
            sv <- m$sv                                   # Indicator: sqrt(lambda) ; Burt: lambda
            ev <- sv^2                                   # eigenvalues of the analysed table
            lambdaI <- if (method == "Burt") sv else ev  # indicator eigenvalues
            K <- length(ev)
            dimNames <- paste("Dim", seq_len(K))
            res <- list()
            res$eig <- cbind("eigenvalue" = ev,
                             "percentage of variance" = ev / sum(ev),
                             "cumulative percentage of variance" = cumsum(ev) / sum(ev))
            rownames(res$eig) <- paste("dim", seq_len(K))
            res$nd.max <- K
            if (nd > res$nd.max)
                return(res)
            if (!is.null(rowlabels))
                res$rowlabels <- rowlabels
            else
                res$rowlabels <- as.character(rownames)

            #### Variable and level names (FactoMineR style) ####
            supIdx <- if (is.null(supcol)) integer(0) else supcol
            actIdx <- setdiff(seq_along(data), supIdx)
            levList <- lapply(data, levels)
            allLevels <- unlist(levList)
            levOwner <- rep(seq_along(levList), lengths(levList))
            for (j in seq_along(levList)) {
                # prefix "var_" only for variables whose levels collide with another variable's levels
                if (any(levList[[j]] %in% allLevels[levOwner != j]))
                    levList[[j]] <- paste(names(data)[j], levList[[j]], sep = "_")
            }
            nLev <- lengths(levList)
            actCat <- unlist(lapply(actIdx, function(j) sum(nLev[seq_len(j - 1)]) + seq_len(nLev[j])))
            supCat <- unlist(lapply(supIdx, function(j) sum(nLev[seq_len(j - 1)]) + seq_len(nLev[j])))
            res$cat$factors <- c(rep(names(data)[actIdx], nLev[actIdx]), rep(names(data)[supIdx], nLev[supIdx]))
            Q <- length(actIdx)
            n <- nrow(data)

            #### Active categories ####
            catStd <- m$colcoord[actCat, seq_len(K), drop = FALSE]
            catCoord <- sweep(catStd, 2, sv, FUN = "*")
            catMass <- stats::setNames(m$colmass[actCat], unlist(levList[actIdx], use.names = FALSE))
            dimnames(catStd) <- dimnames(catCoord) <- list(unlist(levList[actIdx], use.names = FALSE), dimNames)
            catContrib <- catMass * catStd^2
            catCos2 <- catCoord^2 / rowSums(catCoord^2)
            catInertia <- catContrib %*% ev
            catInertia <- catInertia / sum(catInertia)
            # eta2 (discrimination) is scale-independent: computed from indicator principal coordinates
            fac <- factor(rep(names(data)[actIdx], nLev[actIdx]), levels = names(data)[actIdx])
            eta2 <- rowsum(Q * catMass * sweep(catStd^2, 2, lambdaI, FUN = "*"), fac)
            colnames(eta2) <- dimNames

            #### Observations (indicator approach, as in Stata, for both methods) ####
            # mjca's rowpcoord = s * sqrt(lambdaI) in both modes, where s = indicator standard coordinates
            indStd <- sweep(m$rowpcoord[, seq_len(K), drop = FALSE], 2, sqrt(lambdaI), FUN = "/")
            indCoord <- sweep(indStd, 2, sv, FUN = "*")
            dimnames(indStd) <- dimnames(indCoord) <- list(rownames, dimNames)
            res$ind$coord <- indCoord
            res$ind$stdcoord <- indStd
            res$ind$mass <- stats::setNames(rep(1 / n, n), rownames)
            res$ind$contrib <- indStd^2 / n
            res$ind$cos2 <- indCoord^2 / rowSums(indCoord^2)
            res$ind$qlt <- rowSums(res$ind$cos2[, 1:nd, drop = FALSE])
            indinertia <- res$ind$contrib %*% ev
            res$ind$inertia <- indinertia / sum(indinertia)

            #### Supplementary categories ####
            if (length(supIdx) > 0) {
                # Not m$colpcoord: in indicator mode, mjca computes it from Burt profiles
                supStd <- m$colcoord[supCat, seq_len(K), drop = FALSE]
                supCoord <- sweep(supStd, 2, sv, FUN = "*")
                dimnames(supCoord) <- dimnames(supStd) <- list(unlist(levList[supIdx], use.names = FALSE), dimNames)
                supCount <- unlist(lapply(data[supIdx], function(x) as.vector(table(x))))
                if (method == "Burt") {
                    supCos2 <- m$colcor[supCat, seq_len(K), drop = FALSE]
                } else {
                    # cos2 in the indicator space (as FactoMineR): squared chi2 distance = n/n_c - 1
                    supCos2 <- supCoord^2 / (n / supCount - 1)
                }
                dimnames(supCos2) <- dimnames(supCoord)
                # eta2 of supplementary variables, from indicator principal coordinates
                supFac <- factor(rep(names(data)[supIdx], nLev[supIdx]), levels = names(data)[supIdx])
                supEta2 <- rowsum((supCount / n) * sweep(supStd^2, 2, lambdaI, FUN = "*"), supFac)
                colnames(supEta2) <- dimNames
                res$quali.sup$coord <- supCoord
                res$quali.sup$stdcoord <- supStd
                res$quali.sup$cos2 <- supCos2
                res$quali.sup$qlt <- rowSums(supCos2[, 1:nd, drop = FALSE])
                res$quali.sup$eta2 <- supEta2
                nSup <- nrow(supCoord)
                res$cat$coord <- rbind(catCoord, supCoord)
                res$cat$stdcoord <- rbind(catStd, supStd)
                res$cat$cos2 <- rbind(catCos2, supCos2)
                res$cat$contrib <- rbind(catContrib, matrix(NA, nSup, K))
                res$cat$inertia <- rbind(catInertia, matrix(NA, nSup, 1))
                res$cat$qlt <- c(rowSums(catCos2[, 1:nd, drop = FALSE]), res$quali.sup$qlt)
                res$cat$mass <- c(catMass, rep(NA, nSup))
                res$allvar$eta2 <- rbind(eta2, supEta2)
            } else {
                res$cat$coord <- catCoord
                res$cat$stdcoord <- catStd
                res$cat$cos2 <- catCos2
                res$cat$contrib <- catContrib
                res$cat$inertia <- catInertia
                res$cat$qlt <- rowSums(catCos2[, 1:nd, drop = FALSE])
                res$cat$mass <- catMass
                res$allvar$eta2 <- eta2
            }
            res$varActive <- c(Q, length(supIdx))  # (nb of active var, nb of supp var)

            #### Benzecri / Greenacre Adjusment ####
            if (method == "Burt") {
                p <- Q                     # Nb of variables
                mc <- length(actCat)       # Nb of categories
                res$totalInertia <- sum(res$eig[,1])
                res$adjEig <- matrix(nrow = K, ncol = 5)
                res$adjEig[,1] <- (p/(p-1))**2 * (sqrt(res$eig[,1]) - 1/p)**2
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,1] <- 0
                res$totalInrB <- sum(res$adjEig[,1])
                res$totalInrG <- (p/(p-1)) * (res$totalInertia - (mc-p)/p**2)
                res$adjEig[,2] <- res$adjEig[,1] / res$totalInrB
                res$adjEig[,3] <- cumsum(res$adjEig[,2])
                res$adjEig[,4] <- res$adjEig[,1] / res$totalInrG
                res$adjEig[,5] <- cumsum(res$adjEig[,4])
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,] <- rep(NA,5)
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
                    level = rownames(res$cat$coord)[i],
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
                catdata$level <- rownames(catdata)
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
            helpMsg <- .('<p>This module computes <strong>Multiple Correspondence Analysis (MCA)</strong> for several categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=FactoMineR" target="_blank">FactoMineR</a> package by F. Husson, J. Josse, S. Le, J. Mazet.</p>
<p>Both classic methods are available:</p>
<ul>
<li><strong>Indicator matrix:</strong> CA of the indicator matrix</li>
<li><strong>Burt matrix:</strong> CA of the Burt matrix. The eigenvalues are the squares of those of the indicator matrix method. </li>
</ul>
<p>Both methods give the same <em>standard</em> coordinates and discriminations (but different <em>principal</em> coordinates).
With the Burt method, observations, which are not part of the Burt matrix, are positioned using the indicator approach (as in Stata):
their standard coordinates are identical to those of the indicator method, and their principal coordinates are rescaled by the square roots
of the eigenvalues of the Burt matrix, like the categories.</p>
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
