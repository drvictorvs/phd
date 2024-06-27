if(require(semPlot))
{
  assignInNamespace('semPaths', value = function (object, what = "paths", whatLabels, style, layout = "tree",
                                                  intercepts = TRUE, residuals = TRUE, thresholds = TRUE,
                                                  intStyle = "multi", rotation = 1, curve, curvature = 1,
                                                  nCharNodes = 3, nCharEdges = 3, sizeMan = 5, sizeLat = 8,
                                                  sizeInt = 2, sizeMan2, sizeLat2, sizeInt2, shapeMan, shapeLat,
                                                  shapeInt = "triangle", ask, mar, title, title.color = "black",
                                                  title.adj = 0.1, title.line = -1, title.cex = 0.8, include,
                                                  combineGroups = FALSE, manifests, latents, groups, color,
                                                  residScale, gui = FALSE, allVars = FALSE, edge.color, reorder = TRUE,
                                                  structural = FALSE, ThreshAtSide = FALSE, thresholdColor,
                                                  thresholdSize = 0.5, fixedStyle = 2, freeStyle = 1, as.expression = character(0),
                                                  optimizeLatRes = FALSE, inheritColor = TRUE, levels, nodeLabels,
                                                  edgeLabels, pastel = FALSE, rainbowStart = 0, intAtSide,
                                                  springLevels = FALSE, nDigits = 2, exoVar, exoCov = TRUE,
                                                  centerLevels = TRUE, panelGroups = FALSE, layoutSplit = FALSE,
                                                  measurementLayout = "tree", subScale, subScale2, subRes = 4,
                                                  subLinks, modelOpts = list(mplusStd = "std"), curveAdjacent = "<->",
                                                  edge.label.cex = 0.6, cardinal = "none", equalizeManifests = FALSE,
                                                  covAtResiduals = TRUE, bifactor, optimPoints = 1:8 * (pi/4),
                                                  ...)
  {
    call <- paste(deparse(substitute(object)), collapse = "")
    if (grepl("\\+", call)) {
      args <- unlist(strsplit(call, split = "\\+"))
      obs <- lapply(args, function(x) semPlotModel(eval(parse(text = x))))
      object <- obs[[1]]
      for (i in 2:length(obs)) object <- object + obs[[i]]
    }
    if (!"semPlotModel" %in% class(object))
      object <- do.call(semPlotModel, c(list(object), modelOpts))
    stopifnot("semPlotModel" %in% class(object))
    ECP <- NULL
    if (missing(sizeMan2))
      sizeMan2 <- sizeMan
    if (missing(sizeLat2))
      sizeLat2 <- sizeLat
    if (missing(sizeInt2))
      sizeInt2 <- sizeInt
    if (missing(shapeMan)) {
      if (sizeMan == sizeMan2)
        shapeMan <- "square"
      else shapeMan <- "rectangle"
    }
    if (missing(shapeLat)) {
      if (sizeLat == sizeLat2)
        shapeLat <- "circle"
      else shapeLat <- "ellipse"
    }
    if (missing(intAtSide))
      intAtSide <- !residuals
    if (!rotation %in% 1:4) {
      stop("Rotation must be 1, 2 3 or 4.")
    }
    if (any(object@Pars$edge == "int")) {
      object@Vars$name[object@Vars$name == "1"] <- "_1"
      object@Pars$lhs[object@Pars$lhs == "1"] <- "_1"
      object@Pars$rhs[object@Pars$rhs == "1"] <- "_1"
    }
    if (!is.character(layout) || length(layout) > 1) {
      layoutFun <- layout
      layout <- "spring"
    }
    else layoutFun <- NULL
    if (!missing(bifactor) & !layout %in% c("tree2", "tree3",
                                            "circle2", "circle3")) {
      warning("'bifactor' argument only supported in layouts 'tree2', 'tree3', 'circle2' and 'circle3'")
    }
    if (missing(curve)) {
      if (layout %in% c("tree", "tree2", "tree3")) {
        curve <- 1
      }
      else {
        curve <- 0
      }
    }
    curveDefault <- curve
    if (missing(whatLabels)) {
      edge.labels <- TRUE
    }
    else {
      edge.labels <- FALSE
    }
    if (missing(as.expression)) {
      if ("lisrel" %in% unlist(sapply(object@Original, class))) {
        as.expression <- "edges"
      }
      else as.expression <- ""
    }
    if (missing(style)) {
      if ("lisrel" %in% unlist(sapply(object@Original, class))) {
        style <- "lisrel"
      }
      else style <- "OpenMx"
    }
    if (grepl("ram", style, ignore.case = TRUE))
      style <- "OpenMx"
    if (!grepl("mx|lisrel", style, ignore.case = TRUE))
      stop("Only OpenMx (ram) or LISREL style is currently supported.")
    if (missing(residScale))
      residScale <- sizeMan
    if (missing(exoVar))
      exoVar <- !grepl("lis", style, ignore.case = TRUE)
    if (intercepts == FALSE) {
      object@Pars <- object@Pars[object@Pars$edge != "int",
      ]
    }
    object@Vars$trueExo <- !object@Vars$name %in% object@Pars$rhs[object@Pars$edge %in%
                                                                    c("->", "~>")]
    if (!exoVar) {
      object@Pars <- object@Pars[!((object@Pars$lhs %in% object@Vars$name[object@Vars$trueExo] |
                                      object@Pars$rhs %in% object@Vars$name[object@Vars$trueExo]) &
                                     object@Pars$edge == "<->" & (object@Pars$rhs ==
                                                                    object@Pars$lhs)), ]
    }
    if (!exoCov) {
      object@Pars <- object@Pars[!((object@Pars$lhs %in% object@Vars$name[object@Vars$trueExo] |
                                      object@Pars$rhs %in% object@Vars$name[object@Vars$trueExo]) &
                                     object@Pars$edge == "<->" & (object@Pars$rhs !=
                                                                    object@Pars$lhs)), ]
    }
    if (residuals == FALSE) {
      object@Pars <- object@Pars[!(object@Pars$edge == "<->" &
                                     object@Pars$lhs == object@Pars$rhs), ]
    }
    if (combineGroups) {
      object@Pars$group <- ""
    }
    if (is.null(object@Pars$BetweenWithin)) {
      object@Pars$BetweenWithin <- ""
      if (nrow(object@Thresholds) > 0) {
        object@Thresholds$BetweenWithin <- ""
      }
    }
    if ((length(unique(object@Pars$BetweenWithin)) > 1 && !all(unique(object@Pars$BetweenWithin) %in%
                                                               c("Within", "Between"))) | length(unique(object@Pars$BetweenWithin)) >
        2)
      stop("BetweenWithin must be labeled 'Between' and 'Within' only")
    if (length(unique(object@Pars$BetweenWithin)) == 2) {
      object@Pars$group <- paste(object@Pars$group, "-", object@Pars$BetweenWithin)
      object@Pars$group <- gsub("\\s+\\-\\s+(?=Within$)",
                                "", object@Pars$group, perl = TRUE)
      object@Pars$group <- gsub("\\s+\\-\\s+(?=Between$)",
                                "", object@Pars$group, perl = TRUE)
      if (nrow(object@Thresholds) > 0) {
        object@Thresholds$group <- paste(object@Thresholds$group,
                                         "-", object@Thresholds$BetweenWithin)
        object@Thresholds$group <- gsub("\\s+\\-\\s+(?=Within$)",
                                        "", object@Thresholds$BetweenWithin, perl = TRUE)
        object@Thresholds$group <- gsub("\\s+\\-\\s+(?=Between$)",
                                        "", object@Thresholds$BetweenWithin, perl = TRUE)
      }
    }
    if (missing(title)) {
      title <- length(unique(object@Pars$group)) > 1
    }
    if (structural) {
      object@Pars <- object@Pars[!(object@Pars$lhs %in% object@Vars$name[object@Vars$manifest] |
                                     object@Pars$rhs %in% object@Vars$name[object@Vars$manifest]),
      ]
      object@Vars <- object@Vars[!object@Vars$manifest, ]
      object@Thresholds <- data.frame()
    }
    if (any(object@Pars$edge == "<->" & object@Pars$lhs != object@Pars$rhs)) {
      bidirs <- object@Pars[object@Pars$edge == "<->" & object@Pars$lhs !=
                              object@Pars$rhs, ]
      bidirs[c("lhs", "rhs")] <- bidirs[c("rhs", "lhs")]
      bidirs$par <- -1
      object@Pars <- rbind(object@Pars, bidirs)
    }
    object@Pars <- object@Pars[!duplicated(object@Pars), ]
    manNames <- object@Vars$name[object@Vars$manifest]
    if (!missing(manifests)) {
      if (!(all(manNames %in% manifests) & length(manifests) ==
            length(manNames))) {
        stop(paste("Argument 'manifests' should be a vector containing reordered elements of the vector",
                   dput(manNames)))
      }
      manNames <- manifests
    }
    latNames <- object@Vars$name[!object@Vars$manifest]
    if (!missing(latents)) {
      if (!(all(latNames %in% latents) & length(latents) ==
            length(latNames))) {
        stop(paste("Argument 'latents' should be a vector containing reordered elements of the vector",
                   dput(latNames)))
      }
      latNames <- latents
    }
    Labels <- c(manNames, latNames)
    nM <- length(manNames)
    nL <- length(latNames)
    nN <- length(Labels)
    object@Vars <- object@Vars[match(Labels, object@Vars$name),
    ]
    DefaultColor <- FALSE
    if (!missing(groups)) {
      if (is.character(groups)) {
        if (any(grepl("man", groups, ignore.case = TRUE)) &
            any(grepl("lat", groups, ignore.case = TRUE))) {
          groups <- as.list(c(manNames, latNames))
        }
        else if (any(grepl("man", groups, ignore.case = TRUE)) &
                 !any(grepl("lat", groups, ignore.case = TRUE))) {
          groups <- as.list(manNames)
        }
        else if (!any(grepl("man", groups, ignore.case = TRUE)) &
                 any(grepl("lat", groups, ignore.case = TRUE))) {
          groups <- as.list(latNames)
        }
        else stop("Character specification of 'groups' must contain 'man','lat' or both")
      }
      if (is.factor(groups) | is.character(groups))
        groups <- tapply(1:length(groups), groups, identity)
      if (!is.list(groups))
        stop("'groups' argument is not a factor or list")
      if (missing(color)) {
        if (pastel) {
          if (length(groups) == 1)
            color <- "white"
          else color <- rainbow_hcl(length(groups), start = rainbowStart *
                                      360, end = (360 * rainbowStart + 360 * (length(groups) -
                                                                                1)/length(groups)))
        }
        else {
          if (length(groups) == 1)
            color <- "white"
          else color <- rainbow(length(groups), start = rainbowStart,
                                end = (rainbowStart + (max(1, length(groups) -
                                                             1))/length(groups))%%1)
        }
      }
    }
    else {
      if (missing(color)) {
        color <- "background"
      }
    }
    object <- defExo(object, layout)
    Groups <- unique(object@Pars$group)
    qgraphRes <- list()
    if (missing(ask)) {
      if (length(Groups) > 1)
        ask <- TRUE
      else ask <- FALSE
    }
    askOrig <- par("ask")
    if (missing(include))
      include <- 1:length(Groups)
    if (panelGroups) {
      layout(t(1:length(include)))
    }
    AllLabs <- Labels
    AllMan <- manNames
    AllLat <- latNames
    par(ask = ask)
    if (is.null(object@Pars$sub)) {
      if (!layoutSplit) {
        object@Pars$sub <- 0
      }
      else {
        object@Pars$sub <- 1
        for (i in seq_along(latNames)) {
          object@Pars$sub[object@Pars$lhs == latNames[i] &
                            object@Pars$rhs %in% manNames & object@Pars$edge %in%
                            c("->", "~>")] <- i + 1
          object@Pars$sub[object@Pars$rhs %in% object@Pars$rhs[object@Pars$rhs %in%
                                                                 manNames & object@Pars$sub == (i + 1)] & object@Pars$edge ==
                            "int"] <- i + 1
          object@Pars$sub[object@Pars$rhs %in% object@Pars$rhs[object@Pars$rhs %in%
                                                                 manNames & object@Pars$sub == (i + 1)] & object@Pars$lhs %in%
                            object@Pars$rhs[object@Pars$rhs %in% manNames &
                                              object@Pars$sub == (i + 1)] & object@Pars$edge ==
                            "<->"] <- i + 1
        }
        ManInSub <- manNames[manNames %in% object@Pars$lhs[object@Pars$sub >
                                                             1] | manNames %in% object@Pars$rhs[object@Pars$sub >
                                                                                                  1]]
        object@Pars$sub[(object@Pars$lhs %in% ManInSub |
                           object@Pars$rhs %in% ManInSub) & object@Pars$sub ==
                          1] <- 0
      }
    }
    if (missing(subLinks)) {
      subLinks <- latNames
    }
    if (missing(subScale)) {
      subScale <- 0.1 + 0.3 * (1/max(1, max(object@Pars$sub)))
    }
    if (missing(subScale2)) {
      subScale2 <- subScale * 1.5
    }
    layoutMain <- layout
    rotationMain <- rotation
    for (gr in Groups[(1:length(Groups)) %in% include]) {
      grSub <- object@Pars$sub[object@Pars$group == gr]
      if (length(unique(grSub)) == 1)
        grSub[] <- 0
      subModList <- list()
      for (Sub in (max(grSub):0)[max(grSub):0 %in% c(grSub,
                                                     0)]) {
        if (Sub > 0) {
          GroupPars <- object@Pars[object@Pars$group ==
                                     gr & object@Pars$sub == Sub, ]
          GroupVars <- object@Vars
          GroupThresh <- object@Thresholds[object@Thresholds$group ==
                                             gr & object@Pars$sub == Sub, ]
        }
        else {
          GroupPars <- object@Pars[object@Pars$group ==
                                     gr, ]
          GroupVars <- object@Vars
          GroupThresh <- object@Thresholds[object@Thresholds$group ==
                                             gr, ]
        }
        if (Sub > 1) {
          GroupVars$exogenous <- FALSE
          rotation <- 1
          layout <- measurementLayout
        }
        else {
          rotation <- rotationMain
          layout <- layoutMain
        }
        Labels <- AllLabs
        manNames <- AllMan
        latNames <- AllLat
        nM <- length(AllMan)
        nL <- length(AllLat)
        if (reorder) {
          ConOrd <- function(nodes) {
            E <- GroupPars[c("lhs", "rhs")]
            subE <- rbind(as.matrix(E[E[, 1] %in% nodes,
                                      1:2]), as.matrix(E[E[, 2] %in% nodes, 2:1]))
            subE <- subE[subE[, 2] %in% GroupVars$name[!GroupVars$manifest],
                         , drop = FALSE]
            ranks <- rank(match(subE[, 2], GroupVars$name))
            avgCon <- sapply(nodes, function(x) mean(ranks[subE[,
                                                                1] == x]))
            return(order(avgCon))
          }
          EnM <- which(GroupVars$manifest & !GroupVars$exogenous)
          if (length(EnM) > 0) {
            GroupVars[EnM, ] <- GroupVars[EnM, ][ConOrd(GroupVars$name[EnM]),
            ]
          }
          rm(EnM)
          ExM <- which(GroupVars$manifest & GroupVars$exogenous)
          if (length(ExM) > 0) {
            GroupVars[ExM, ] <- GroupVars[ExM, ][ConOrd(GroupVars$name[ExM]),
            ]
          }
          rm(ExM)
          manNames <- GroupVars$name[GroupVars$manifest]
          Labels <- c(manNames, latNames)
        }
        Ni <- sum(GroupPars$edge == "int")
        if (any(object@Pars$edge == "int")) {
          Labels[Labels == "1"] <- "_1"
          if (intStyle == "single") {
            Labels <- c(Labels, "1")
          }
          else if (intStyle == "multi") {
            Labels <- c(Labels, rep("1", Ni))
          }
        }
        nN <- length(Labels)
        Edgelist <- GroupPars[c("lhs", "rhs")]
        Edgelist$lhs <- match(Edgelist$lhs, Labels)
        Edgelist$lhs[GroupPars$edge == "int"] <- (nM + nL +
                                                    1):nN
        Edgelist$rhs <- match(Edgelist$rhs, Labels)
        Edgelist$lhs <- as.numeric(Edgelist$lhs)
        Edgelist$rhs <- as.numeric(Edgelist$rhs)
        Edgelist <- as.matrix(Edgelist)
        if (!allVars) {
          NodesInGroup <- sort(unique(c(Edgelist[, 1],
                                        Edgelist[, 2])))
          incl <- 1:nN %in% NodesInGroup
          Edgelist[, 1] <- match(Edgelist[, 1], NodesInGroup)
          Edgelist[, 2] <- match(Edgelist[, 2], NodesInGroup)
        }
        else incl <- 1:nN
        Labels <- Labels[incl]
        nN <- length(Labels)
        nM <- sum(manNames %in% Labels)
        nL <- sum(latNames %in% Labels)
        GroupVars <- GroupVars[GroupVars$name %in% Labels,
        ]
        manInts <- Edgelist[GroupPars$edge == "int" & GroupPars$rhs %in%
                              manNames, , drop = FALSE]
        latInts <- Edgelist[GroupPars$edge == "int" & GroupPars$rhs %in%
                              latNames, , drop = FALSE]
        manIntsEndo <- manInts[!GroupVars$exogenous[manInts[,
                                                            2]], , drop = FALSE]
        manIntsExo <- manInts[GroupVars$exogenous[manInts[,
                                                          2]], , drop = FALSE]
        latIntsEndo <- latInts[!GroupVars$exogenous[latInts[,
                                                            2]], , drop = FALSE]
        latIntsExo <- latInts[GroupVars$exogenous[latInts[,
                                                          2]], , drop = FALSE]
        endoMan <- which(Labels %in% manNames & Labels %in%
                           GroupVars$name[!GroupVars$exogenous])
        exoMan <- which(Labels %in% manNames & Labels %in%
                          GroupVars$name[GroupVars$exogenous])
        endoLat <- which(Labels %in% latNames & Labels %in%
                           GroupVars$name[!GroupVars$exogenous])
        exoLat <- which(Labels %in% latNames & Labels %in%
                          GroupVars$name[GroupVars$exogenous])
        Bidir <- GroupPars$edge == "<->"
        if (!grepl("mx", style, ignore.case = TRUE)) {
          Bidir[GroupPars$lhs == GroupPars$rhs] <- FALSE
        }
        Shape <- c(rep(shapeMan, nM), rep(shapeLat, nL))
        if (any(GroupPars$edge == "int"))
          Shape <- c(Shape, rep(shapeInt, Ni))
        Curve <- curve
        if (layout == "tree" | layout == "circle" | layout ==
            "circular") {
          if (intStyle == "single") {
            Curve <- ifelse(GroupPars$lhs != GroupPars$rhs &
                              ((GroupPars$lhs %in% manNames & GroupPars$rhs %in%
                                  manNames) | (GroupPars$lhs %in% latNames &
                                                 GroupPars$rhs %in% latNames)), curve,
                            NA)
            Curve <- ifelse(GroupPars$lhs %in% manNames,
                            Curve, -1 * Curve)
            Curve <- ifelse(GroupPars$edge == "int" &
                              GroupPars$rhs %in% latNames, curve, -1 *
                              Curve)
            Layout <- matrix(, length(Labels), 2)
            Layout[, 2] <- ifelse(Labels %in% manNames,
                                  1, 2)
            Layout[Labels %in% manNames, 1] <- seq(-1, 1, length.out = nM)
            if (any(GroupPars$edge == "int")) {
              sq <- seq(-1, 1, length.out = nL + 1)
              cent <- floor(median(1:(nL + 1)))
              Layout[!Labels %in% manNames, 1] <- sq[c(which(1:(nL +
                                                                  1) < cent), which(1:(nL + 1) > cent),
                                                       cent)]
            }
            else {
              Layout[Labels %in% latNames, 1] <- seq(-1,
                                                     1, length.out = nL)
            }
          }
          else if (intStyle == "multi") {
            Layout <- matrix(, length(Labels), 2)
            Layout[endoMan, 2] <- 1
            Layout[endoLat, 2] <- 2
            Layout[exoLat, 2] <- 3
            Layout[exoMan, 2] <- 4
            Layout[latIntsEndo[, 1], 2] <- 2
            Layout[latIntsExo[, 1], 2] <- 3
            if (intAtSide) {
              Layout[manIntsExo[, 1], 2] <- 4
              Layout[manIntsEndo[, 1], 2] <- 1
            }
            else {
              Layout[manIntsExo[, 1], 2] <- 5
              Layout[manIntsEndo[, 1], 2] <- 0
            }
            if (nrow(manIntsEndo) > 0) {
              Layout <- mixInts(endoMan, manIntsEndo,
                                Layout, intAtSide = intAtSide)
            }
            else {
              if (length(endoMan) == 1)
                Layout[endoMan, 1] <- 0
              else Layout[endoMan, 1] <- seq(-1, 1, length.out = length(endoMan))
            }
            if (nrow(manIntsExo) > 0) {
              Layout <- mixInts(exoMan, manIntsExo, Layout,
                                intAtSide = intAtSide)
            }
            else {
              if (length(exoMan) == 1)
                Layout[exoMan, 1] <- 0
              else Layout[exoMan, 1] <- seq(-1, 1, length.out = length(exoMan))
            }
            if (nrow(latIntsEndo) > 0) {
              Layout <- mixInts(endoLat, latIntsEndo,
                                Layout, trim = TRUE)
            }
            else {
              Layout[endoLat, 1] <- seq(-1, 1, length.out = length(endoLat) +
                                          2)[-c(1, length(endoLat) + 2)]
            }
            if (nrow(latIntsExo) > 0) {
              Layout <- mixInts(exoLat, latIntsExo, Layout,
                                trim = TRUE)
            }
            else {
              Layout[exoLat, 1] <- seq(-1, 1, length.out = length(exoLat) +
                                         2)[-c(1, length(exoLat) + 2)]
            }
            if (equalizeManifests) {
              EndoHorRange <- max(sapply(c(0, 1, 4, 5),
                                         function(x) sum(Layout[, 2] == x)))
              for (lvl in c(0, 1, 4, 5)) Layout[Layout[,
                                                       2] == lvl, 1] <- sum(Layout[, 2] == lvl) *
                  Layout[Layout[, 2] == lvl, 1]/EndoHorRange
            }
          }
          else stop("MeanStyle not supported")
        }
        else if (layout %in% c("tree2", "circle2")) {
          if (!missing(bifactor) && any(bifactor %in%
                                        Labels)) {
            roots <- which(Labels %in% bifactor)
          }
          else {
            if (nrow(manIntsExo) > 0) {
              roots <- manIntsExo[, 1]
            }
            else if (length(exoMan) > 0) {
              roots <- exoMan
            }
            else if (nrow(latIntsExo) > 0) {
              roots <- latIntsExo[, 1]
            }
            else if (length(exoLat) > 0) {
              roots <- exoLat
            }
            else if (nrow(latIntsEndo) > 0) {
              roots <- latIntsEndo[, 1]
            }
            else if (length(endoLat) > 0) {
              roots <- endoLat
            }
            else if (nrow(rbind(manIntsExo, manIntsEndo)) >
                     0) {
              roots <- rbind(manIntsExo, manIntsEndo)[,
                                                      1]
            }
            else if (any(GroupPars$edge %in% c("->", "~>"))) {
              roots <- Mode(Edgelist[, 1][GroupPars$edge %in%
                                            c("->", "~>")])
            }
            else {
              roots <- Mode(c(Edgelist[, 1], Edgelist[,
                                                      2]))
            }
          }
          Layout <- rtLayout(roots, GroupPars, Edgelist,
                             layout, exoMan)
          if (centerLevels)
            Layout[, 1] <- ave(Layout[, 1], Layout[, 2],
                               FUN = function(x) scale(x, TRUE, FALSE))
        }
        else if (layout %in% c("tree3", "circle3")) {
          Edgelist2 <- Edgelist[GroupPars$edge %in% c("->",
                                                      "~>"), ]
          Edgelist2[Edgelist2[, 2] %in% which(GroupVars$manifest &
                                                GroupVars$exogenous), ] <- Edgelist2[Edgelist2[,
                                                                                               2] %in% which(GroupVars$manifest & GroupVars$exogenous),
                                                                                     2:1]
          if (!missing(bifactor) && any(bifactor %in%
                                        Labels)) {
            Edgelist2[!Labels[Edgelist2[, 1]] %in% bifactor &
                        !Labels[Edgelist2[, 2]] %in% bifactor, 1:2] <- Edgelist2[!Labels[Edgelist2[,
                                                                                                   1]] %in% bifactor & !Labels[Edgelist2[,
                                                                                                                                         2]] %in% bifactor, 2:1]
          }
          iG <- graph.edgelist(Edgelist2)
          sp <- shortest.paths(iG, mode = "out")
          sp[!is.finite(sp)] <- 0
          maxPaths <- apply(sp, 1, max)
          if (any(GroupPars$edge == "int")) {
            maxPathsInts <- maxPaths[Edgelist[GroupPars$edge ==
                                                "int", 2]]
            if (!intAtSide) {
              maxPathsInts[maxPathsInts == min(maxPaths)] <- min(maxPaths) -
                1
              maxPathsInts[maxPathsInts == max(maxPaths)] <- max(maxPaths) +
                1
            }
            maxPaths <- c(maxPaths, maxPathsInts)
          }
          if (springLevels) {
            Cons <- cbind(NA, maxPaths)
            Layout <- qgraph.layout.fruchtermanreingold(Edgelist,
                                                        vcount = length(maxPaths), constraints = Cons *
                                                          sqrt(length(maxPaths)))
          }
          else {
            Layout <- cbind(NA, maxPaths)
            Layout[, 1] <- ave(Layout[, 2], Layout[, 2],
                               FUN = function(x) seq(-1, 1, length.out = length(x) +
                                                       2)[-c(1, length(x) + 2)])
            if (any(GroupPars$edge == "int")) {
              intMap <- rbind(manInts, latInts)
              for (i in sort(unique(Layout[, 2]))) {
                if (any(which(Layout[, 2] == i) %in% intMap[,
                                                            1])) {
                  conInts <- which(Layout[, 2] == i)
                  conInts <- conInts[conInts %in% intMap[,
                                                         1]]
                  Layout <- mixInts(intMap[intMap[, 1] %in%
                                             conInts, 2], intMap, Layout, trim = TRUE,
                                    intAtSide = intAtSide)
                }
              }
            }
          }
        }
        else Layout <- layout
        if (layout %in% c("tree", "tree2", "tree3")) {
          loopRotation <- rep(0, nN)
          loopRotation[endoMan] <- pi
          loopRotation[exoMan] <- 0
          loopRotation[endoLat] <- 0
          noCons <- sapply(endoLat, function(x) nrow(Edgelist[(Edgelist[,
                                                                        1] == x | Edgelist[, 2] == x) & (Edgelist[,
                                                                                                                  1] %in% endoMan | Edgelist[, 2] %in% endoMan),
                                                              , drop = FALSE]) == 0)
          if (length(noCons) == 0)
            noCons <- logical(0)
          loopRotation[endoLat][noCons] <- pi
          if (length(endoLat) > 1 & !(length(exoLat) ==
                                      0 & length(exoMan) == 0)) {
            if (length(exoLat) > 0 | any(endoLat %in%
                                         latIntsEndo[, 2])) {
              loopRotation[endoLat[which.min(Layout[endoLat,
                                                    1])]] <- ifelse(noCons[which.min(Layout[endoLat,
                                                                                            1])], 5/4 * pi, 7/4 * pi)
              loopRotation[endoLat[which.max(Layout[endoLat,
                                                    1])]] <- ifelse(noCons[which.min(Layout[endoLat,
                                                                                            1])], 3/4 * pi, 1/4 * pi)
            }
            else {
              loopRotation[endoLat[which.min(Layout[endoLat,
                                                    1])]] <- 6/4 * pi
              loopRotation[endoLat[which.max(Layout[endoLat,
                                                    1])]] <- 2/4 * pi
            }
          }
          else if (length(endoLat) == 1 && endoLat %in%
                   latIntsEndo[, 2]) {
            loopRotation[endoLat] <- 6/4 * pi
          }
          loopRotation[exoLat] <- pi
          noCons <- sapply(exoLat, function(x) nrow(Edgelist[(Edgelist[,
                                                                       1] == x | Edgelist[, 2] == x) & (Edgelist[,
                                                                                                                 1] %in% exoMan | Edgelist[, 2] %in% exoMan),
                                                             , drop = FALSE]) == 0)
          if (length(noCons) == 0)
            noCons <- logical(0)
          loopRotation[exoLat][noCons] <- 0
          if (length(exoLat) > 1 & length(exoMan) > 0) {
            if (length(endoLat) > 0 | any(exoLat %in%
                                          latIntsExo[, 2])) {
              loopRotation[exoLat[which.min(Layout[exoLat,
                                                   1])]] <- ifelse(noCons[which.min(Layout[exoLat,
                                                                                           1])], 7/4 * pi, 5/4 * pi)
              loopRotation[exoLat[which.max(Layout[exoLat,
                                                   1])]] <- ifelse(noCons[which.min(Layout[exoLat,
                                                                                           1])], 1/4 * pi, 3/4 * pi)
            }
            else {
              loopRotation[exoLat[which.min(Layout[exoLat,
                                                   1])]] <- 6/4 * pi
              loopRotation[exoLat[which.max(Layout[exoLat,
                                                   1])]] <- 2/4 * pi
            }
          }
          else if (length(exoLat) == 1 && exoLat %in%
                   latIntsExo[, 2]) {
            loopRotation[exoLat] <- 6/4 * pi
          }
          if (any(GroupVars$exogenous) & optimizeLatRes) {
            for (i in which(Labels %in% latNames & Labels %in%
                            GroupPars$lhs[GroupPars$lhs == GroupPars$rhs])) {
              subEdgelist <- Edgelist[(Edgelist[, 1] ==
                                         i | Edgelist[, 2] == i) & (Edgelist[,
                                                                             1] != Edgelist[, 2]), , drop = FALSE]
              conNodes <- c(subEdgelist[subEdgelist[,
                                                    1] == i, 2], subEdgelist[subEdgelist[,
                                                                                         2] == i, 1])
              if (nrow(subEdgelist) == 0)
                conNodes <- sort(unique(c(Edgelist[, 1:2])))
              subLayout <- Layout[conNodes, , drop = FALSE]
              lower <- which(Layout[, 2] < Layout[i, 2])
              higher <- which(Layout[, 2] > Layout[i,
                                                   2])
              passNode <- which((Edgelist[, 1] %in% lower &
                                   Edgelist[, 2] %in% higher) | (Edgelist[,
                                                                          2] %in% lower & Edgelist[, 1] %in% higher))
              if (length(passNode) > 0) {
                passLayout <- do.call(rbind, lapply(passNode,
                                                    function(ii) c(mean(Layout[Edgelist[ii,
                                                    ], 1]), mean(Layout[Edgelist[ii, ],
                                                                        2]))))
                subLayout <- rbind(subLayout, passLayout)
              }
              Degrees <- apply(subLayout, 1, function(x) atan2(x[1] -
                                                                 Layout[i, 1], x[2] - Layout[i, 2]))
              loopRotation[i] <- optimPoints[which.max(sapply(optimPoints,
                                                              loopOptim, Degrees = Degrees))]
            }
          }
        }
        else loopRotation <- rep(NA, length(Labels))
        if (layout == "tree") {
          Layout[Layout[, 2] > 0 & Layout[, 2] < 5, 2] <- as.numeric(as.factor(Layout[Layout[,
                                                                                             2] > 0 & Layout[, 2] < 5, 2]))
          Layout[Layout[, 2] == 0, 2] <- (1 * !residuals) *
            0.25
          Layout[Layout[, 2] == 5, 2] <- max(Layout[Layout[,
                                                           2] < 5, 2]) + (1 - (1 * !residuals) * 0.25)
        }
        if (!missing(levels) & layout %in% c("tree", "tree2",
                                             "tree3", "circle", "circle2", "circle3")) {
          if (length(levels) < length(unique(Layout[,
                                                    2])))
            stop(paste("'levels' argument must have at least",
                       length(unique(Layout[, 2])), "elements"))
          Layout[, 2] <- levels[as.numeric(as.factor(Layout[,
                                                            2]))]
        }
        ECP <- matrix(NA, nrow(Edgelist), 2)
        if (layout %in% c("tree", "tree2", "tree3")) {
          inBetween <- function(x) {
            if (Layout[x[1], 2] != Layout[x[2], 2])
              return(0)
            else return(sum(Layout[Layout[, 2] == Layout[x[1],
                                                         2], 1] > min(Layout[x, 1]) & Layout[Layout[,
                                                                                                    2] == Layout[x[1], 2], 1] < max(Layout[x,
                                                                                                                                           1])))
          }
          inBet <- apply(Edgelist, 1, inBetween)
          inBet[inBet > 0] <- as.numeric(as.factor(inBet[inBet >
                                                           0]))
          if (isTRUE(curveAdjacent)) {
            percurveAdjacent <- rep(TRUE, nrow(Edgelist))
          }
          else {
            percurveAdjacent <- rep(FALSE, nrow(Edgelist))
            curveAdjacent <- gsub("<->", "cov", curveAdjacent)
            curveAdjacent <- gsub("(->)|(~>)", "reg",
                                  curveAdjacent)
            if (is.character(curveAdjacent)) {
              percurveAdjacent[(any(grepl("reg", curveAdjacent,
                                          ignore.case = TRUE)) & GroupPars$edge %in%
                                  c("->", "~>", ignore.case = TRUE)) | (any(grepl("cov",
                                                                                  curveAdjacent)) & GroupPars$edge %in%
                                                                          c("<->"))] <- TRUE
            }
          }
          Curve <- ifelse(Layout[Edgelist[, 1], 2] ==
                            Layout[Edgelist[, 2], 2] & Edgelist[, 1] !=
                            Edgelist[, 2] & GroupPars$edge != "int", ifelse(inBet <
                                                                              (1 - percurveAdjacent), 0, curve + curvature *
                                                                              (inBet)/max(1, max(inBet)) * curve), NA)
          Curve[Layout[Edgelist[, 1], 1] > Layout[Edgelist[,
                                                           2], 1]] <- -1 * Curve[Layout[Edgelist[, 1],
                                                                                        1] > Layout[Edgelist[, 2], 1]]
          Curve <- ifelse(Edgelist[, 1] %in% endoMan |
                            Edgelist[, 2] %in% endoMan, -1 * Curve, Curve)
          if (any(grepl("all", cardinal)) || isTRUE(cardinal))
            cardinal <- "exo endo man lat cov reg load source dest"
          if (length(cardinal) > 0 && !identical(cardinal,
                                                 FALSE) && !all(grepl("none", cardinal))) {
            if (packageDescription("qgraph")$Version ==
                "1.2.3")
              warning("'cardinal' argument requires qgraph version 1.2.4")
            ECP <- matrix(NA, nrow(Edgelist), 2)
            lvlDiff <- Layout[Edgelist[, 1], 2] - Layout[Edgelist[,
                                                                  2], 2]
            ECP[lvlDiff > 0, 1] <- pi
            ECP[lvlDiff > 0, 2] <- 0
            ECP[lvlDiff < 0, 1] <- 0
            ECP[lvlDiff < 0, 2] <- pi
            ECP[lvlDiff == 0, 1] <- ifelse(Curve != 0,
                                           ifelse((loopRotation[Edgelist[, 1]] + 0.5 *
                                                     pi)%%2 * pi <= pi, pi, 0), NA)[lvlDiff ==
                                                                                      0]
            ECP[lvlDiff == 0, 2] <- ifelse(Curve != 0,
                                           ifelse((loopRotation[Edgelist[, 1]] + 0.5 *
                                                     pi)%%2 * pi <= pi, pi, 0), NA)[lvlDiff ==
                                                                                      0]
            ECP <- (ECP - 0.5 * (rotation - 1) * pi)%%(2 *
                                                         pi)
            allSelect <- matrix(FALSE, nrow(ECP), 2)
            for (cardGroup in cardinal) {
              select <- matrix(grepl("(exo)|(endo)|(man)|(lat)|(cov)|(reg)|(load)|(src)|(source)|(dest)",
                                     cardGroup), nrow(ECP), 2)
              if (grepl("(endo)|(exo)", cardGroup)) {
                select <- select & ((grepl("endo", cardGroup,
                                           ignore.case = TRUE) & !GroupVars$trueExo[Edgelist[,
                                                                                             1]]) | (grepl("exo", cardGroup, ignore.case = TRUE) &
                                                                                                       GroupVars$trueExo[Edgelist[, 1]]))
              }
              if (grepl("(lat)|(man)", cardGroup)) {
                select <- select & ((grepl("lat", cardGroup,
                                           ignore.case = TRUE) & (!GroupVars$manifest[Edgelist[,
                                                                                               1]] | !GroupVars$manifest[Edgelist[,
                                                                                                                                  2]])) | (grepl("man", cardGroup, ignore.case = TRUE) &
                                                                                                                                             (GroupVars$manifest[Edgelist[, 1]] |
                                                                                                                                                GroupVars$manifest[Edgelist[, 2]])))
              }
              if (grepl("(cov)|(reg)|(load)", cardGroup)) {
                select <- select & ((grepl("cov", cardGroup,
                                           ignore.case = TRUE) & (GroupPars$edge ==
                                                                    "<->")) | (grepl("reg", cardGroup, ignore.case = TRUE) &
                                                                                 (GroupPars$edge %in% c("->", "~>") &
                                                                                    !(!GroupVars$manifest[Edgelist[, 1]] &
                                                                                        GroupVars$manifest[Edgelist[, 2]]))) |
                                      (grepl("load", cardGroup, ignore.case = TRUE) &
                                         (GroupPars$edge %in% c("->", "~>") &
                                            (!GroupVars$manifest[Edgelist[,
                                                                          1]] & GroupVars$manifest[Edgelist[,
                                                                                                            2]]))))
              }
              if (grepl("(src)|(source)|(dest)", cardGroup)) {
                select[, 1] <- select[, 1] & grepl("(src)|(source)",
                                                   cardGroup, ignore.case = TRUE)
                select[, 2] <- select[, 2] & grepl("dest",
                                                   cardGroup, ignore.case = TRUE)
              }
              allSelect[select] <- TRUE
            }
            ECP[!allSelect] <- NA
          }
          if (!missing(bifactor) && any(bifactor %in%
                                        Labels) && layout %in% c("tree2", "tree3",
                                                                 "circle2", "circle3")) {
            loopRotation[!Labels %in% bifactor] <- loopRotation[!Labels %in%
                                                                  bifactor] + pi
            ECP[!GroupPars$lhs %in% bifactor, 1] <- ECP[!GroupPars$lhs %in%
                                                          bifactor, 1] + pi
            ECP[!GroupPars$lhs %in% bifactor, 2] <- ECP[!GroupPars$lhs %in%
                                                          bifactor, 2] + pi
            Curve[!GroupPars$lhs %in% bifactor & !GroupPars$lhs %in%
                    bifactor] <- -1 * Curve[!GroupPars$lhs %in%
                                              bifactor & !GroupPars$lhs %in% bifactor]
          }
          loopRotation <- loopRotation - 0.5 * (rotation -
                                                  1) * pi
          if (rotation == 2) {
            Layout <- Layout[, 2:1]
            Layout[, 1] <- -1 * Layout[, 1]
          }
          if (rotation == 3) {
            Layout[, 1] <- -1 * Layout[, 1]
            Layout[, 2] <- -1 * Layout[, 2]
          }
          if (rotation == 4) {
            Layout <- Layout[, 2:1]
            Layout[, 2] <- -1 * Layout[, 2]
          }
          Layout[, 2] <- Layout[, 2] - max(Layout[, 2]) +
            0.5
        }
        if (layout %in% c("circle", "circle2", "circle3")) {
          if (rotation %in% c(2, 4))
            stop("Circle layout only supported if rotation is 1 or 3")
          underMean <- Layout[, 2] < mean(Layout[, 2])
          Layout[, 2] <- -1 * Layout[, 2] + max(Layout[,
                                                       2]) + 0.5
          Ltemp <- Layout
          unVert <- sort(unique(Layout[, 2]))
          for (i in unVert) {
            l <- sum(Layout[, 2] == i)
            sq <- seq(0, 2 * pi, length.out = l + 1)[-(l +
                                                         1)] + pi/l
            c <- 1
            for (j in order(Layout[Layout[, 2] == i, 1])) {
              Ltemp[which(Layout[, 2] == i)[j], ] <- c(RotMat(sq[c]) %*%
                                                         c(0, i))
              c <- c + 1
            }
          }
          Layout <- Ltemp
          loopRotation <- apply(Layout, 1, function(x) atan2(x[1],
                                                             x[2]))
          loopRotation <- ifelse(underMean, loopRotation,
                                 (loopRotation + pi)%%(2 * pi))
        }
        if (layout == "spring")
          loopRotation <- rep(NA, length(Labels))
        if (layoutSplit & length(unique(grSub)) > 1 & Sub >
            0) {
          if (is.character(Layout)) {
            Layout <- qgraph(Edgelist, layout = Layout,
                             DoNotPlot = TRUE, edgelist = TRUE)$layout
          }
          subModList[[Sub]] <- list(Layout = Layout, loopRotation = loopRotation,
                                    Curve = Curve, Labels = Labels, ECP = ECP)
        }
      }
      if (layoutSplit & length(unique(grSub)) > 1 & length(subModList) >
          1) {
        din <- par("din")
        diamet <- sqrt(sum(din^2))
        subDim <- diamet * c(subScale, subScale2)
        if (is.character(Layout)) {
          Layout <- qgraph(Edgelist, layout = Layout,
                           DoNotPlot = TRUE)$layout
        }
        Layout <- LayoutScaler(Layout, din[1]/2, din[2]/2)
        subModList[[1]]$Layout <- LayoutScaler(subModList[[1]]$Layout)
        centAngles <- atan2(subModList[[1]]$Layout[, 1],
                            subModList[[1]]$Layout[, 2]) + pi
        subModList[[1]]$Layout <- LayoutScaler(subModList[[1]]$Layout,
                                               din[1]/2, din[2]/2)
        centAngles[subModList[[1]]$Layout[, 1] == 0 & subModList[[1]]$Layout[,
                                                                             2] == 0] <- mean(centAngles[!(subModList[[1]]$Layout[,
                                                                                                                                  1] == 0 & subModList[[1]]$Layout[, 2] == 0)]) +
          pi
        if (layout %in% c("tree", "tree2", "tree3")) {
          err <- 1.1
        }
        else {
          err <- 1.1
        }
        srot <- ifelse(rotation %in% c(1, 3), 1/err, err)
        centAngles <- atan2(srot * sin(centAngles), cos(centAngles))
        if (subRes != 0) {
          centAngles <- round_any(centAngles%%(2 * pi),
                                  (2 * pi)/subRes)
        }
        for (g in rev(seq_along(subModList))) {
          if (g > 1 && !is.null(subModList[[g]])) {
            link <- c(which(subModList[[1]]$Labels ==
                              subLinks[g - 1]), which(subModList[[g]]$Labels ==
                                                        subLinks[g - 1]))
            subModList[[g]]$Layout <- LayoutScaler(subModList[[g]]$Layout,
                                                   subDim[1]/2, subDim[2]/2)
            subModList[[g]]$Layout[, 1] <- subModList[[g]]$Layout[,
                                                                  1] - subModList[[g]]$Layout[link[2], 1]
            subModList[[g]]$Layout[, 2] <- subModList[[g]]$Layout[,
                                                                  2] - subModList[[g]]$Layout[link[2], 2]
            subModList[[g]]$Layout <- t(RotMat(centAngles[link[1]]) %*%
                                          t(subModList[[g]]$Layout))
            subModList[[g]]$Layout[, 1] <- subModList[[g]]$Layout[,
                                                                  1] + subModList[[1]]$Layout[link[1], 1]
            subModList[[g]]$Layout[, 2] <- subModList[[g]]$Layout[,
                                                                  2] + subModList[[1]]$Layout[link[1], 2]
          }
          subLabnums <- match(subModList[[g]]$Labels[subModList[[g]]$Labels !=
                                                       "1"], Labels)
          subLabnums <- c(subLabnums, manInts[match(match(GroupPars$rhs[GroupPars$sub ==
                                                                          g & GroupPars$edge == "int" & GroupPars$rhs %in%
                                                                          manNames], Labels), manInts[, 2]), 1])
          subLabnums <- c(subLabnums, latInts[match(match(GroupPars$rhs[GroupPars$sub ==
                                                                          g & GroupPars$edge == "int" & GroupPars$rhs %in%
                                                                          latNames], Labels), latInts[, 2]), 1])
          Layout[subLabnums, ] <- subModList[[g]]$Layout
          Curve[GroupPars$sub == g] <- subModList[[g]]$Curve
          if (g == 1) {
            loopRotation[subLabnums] <- subModList[[g]]$loopRotation
            ECP[object@Pars$sub == g & object@Pars$group ==
                  gr, ] <- subModList[[g]]$ECP
            for (g2 in length(subModList):2) {
              if (!is.null(subModList[[g2]])) {
                loopRotation[Labels == latNames[g2 - 1]] <- centAngles[g2 -
                                                                         1]
              }
            }
          }
          else {
            loopRotation[subLabnums] <- (subModList[[g]]$loopRotation +
                                           centAngles[link[1]])%%(2 * pi)
            ECP[object@Pars$sub == g & object@Pars$group ==
                  gr, ] <- (subModList[[g]]$ECP + centAngles[link[1]])%%(2 *
                                                                           pi)
          }
        }
      }
      if (edge.labels) {
        eLabels <- GroupPars$label
      }
      else eLabels <- rep("", nrow(Edgelist))
      vSize <- numeric(nN)
      vSize[Labels %in% manNames] <- sizeMan
      vSize[Labels %in% latNames] <- sizeLat
      vSize[Labels == "1"] <- sizeInt
      vSize2 <- numeric(nN)
      vSize2[Labels %in% manNames] <- sizeMan2
      vSize2[Labels %in% latNames] <- sizeLat2
      vSize2[Labels == "1"] <- sizeInt2
      eColor <- rep(NA, nrow(Edgelist))
      if (missing(thresholdColor)) {
        tColor <- rep("border", nN)
      }
      else {
        tColor <- rep(thresholdColor, nN)
      }
      if (grepl("path|diagram|mod", what, ignore.case = TRUE)) {
      }
      else if (grepl("stand|std", what, ignore.case = TRUE)) {
        Edgelist <- cbind(Edgelist, GroupPars$std)
        if (edge.labels)
          eLabels <- GroupPars$std
      }
      else if (grepl("est|par", what, ignore.case = TRUE)) {
        Edgelist <- cbind(Edgelist, GroupPars$est)
        if (edge.labels)
          eLabels <- GroupPars$est
      }
      else if (grepl("eq|cons", what, ignore.case = TRUE)) {
        unPar <- unique(object@Pars$par[object@Pars$par >
                                          0 & duplicated(object@Pars$par)])
        if (pastel) {
          cols <- rainbow_hcl(max(c(object@Pars$par, GroupThresh$par)),
                              c = 35, l = 85)
        }
        else {
          cols <- rainbow(max(c(object@Pars$par, GroupThresh$par)))
        }
        for (i in unPar) {
          eColor[GroupPars$par == i] <- cols[i]
        }
        if (nrow(GroupThresh) > 0) {
          warning("Equality constraints of Thresholds currently not supported")
        }
      }
      else if (!grepl("col", what, ignore.case = TRUE))
        stop("Could not detect use of 'what' argument")
      if (is.list(color)) {
        colList <- color
        color <- rep("", nN)
        if (!is.null(colList$man)) {
          color[Labels %in% manNames] <- colList$man
        }
        if (!is.null(colList$lat)) {
          color[Labels %in% latNames] <- colList$lat
        }
        if (!is.null(colList$int)) {
          color[Labels == "1"] <- colList$int
        }
      }
      if (!missing(groups)) {
        NodeGroups <- groups
        Ng <- length(NodeGroups)
        if (length(color) == 1) {
          Vcolors <- rep(color, nN)
        }
        else if (length(color) == nM) {
          Vcolors <- c(color, rep("", nN - nM))
        }
        else if (length(color) == nN) {
          Vcolors <- color
        }
        else if (length(color) != Ng) {
          stop("'color' vector not of appropriate length")
        }
        if (missing(manifests) & any(sapply(NodeGroups,
                                            mode) != "character"))
          warning("Groups specified numerically and 'manifests' not supplied. Results might be unexpected.")
        if (length(color) == Ng) {
          Vcolors <- rep("", nN)
          for (g in 1:Ng) {
            if (mode(NodeGroups[[g]]) == "character") {
              NodeGroups[[g]] <- matchVar(NodeGroups[[g]],
                                          GroupVars, manIntsExo, manIntsEndo, latIntsExo,
                                          latIntsEndo)
            }
            Vcolors[NodeGroups[[g]]] <- color[g]
          }
        }
      }
      else {
        NodeGroups <- NULL
        if (length(color) == 1) {
          Vcolors <- rep(color, nN)
        }
        else if (length(color) == nM) {
          Vcolors <- c(color, rep(NA, nN - nM))
        }
        else if (length(color) == nN) {
          Vcolors <- color
        }
        else stop("'color' vector not of appropriate length")
      }
      if (any(Vcolors == "")) {
        if (inheritColor) {
          VcolorsBU <- Vcolors
          W <- 1
          for (i in 1:nN) {
            if (Vcolors[i] == "") {
              cons <- c(Edgelist[Edgelist[, 1] == i, 2],
                        Edgelist[Edgelist[, 2] == i, 1])
              if (ncol(Edgelist) == 3) {
                W <- abs(c(Edgelist[Edgelist[, 1] == i,
                                    3], Edgelist[Edgelist[, 2] == i, 3]))
                W <- W[VcolorsBU[cons] != ""]
              }
              cons <- cons[VcolorsBU[cons] != ""]
              if (length(cons) > 0) {
                Vcolors[i] <- mixColfun(VcolorsBU[cons],
                                        W)
              }
              else Vcolors[i] <- NA
            }
          }
        }
        Vcolors[Vcolors == ""] <- NA
      }
      if (grepl("col", what, ignore.case = TRUE)) {
        for (i in 1:nrow(Edgelist)) {
          cols <- Vcolors[Edgelist[i, ]]
          if (!all(cols == "background"))
            eColor[i] <- mixColfun(cols[cols != "background"])
        }
      }
      if (!missing(whatLabels)) {
        if (grepl("path|diagram|model|name|label", whatLabels,
                  ignore.case = TRUE)) {
          eLabels <- GroupPars$label
        }
        else if (grepl("stand|std", whatLabels, ignore.case = TRUE)) {
          eLabels <- GroupPars$std
        }
        else if (grepl("est|par", whatLabels, ignore.case = TRUE)) {
          eLabels <- GroupPars$est
        }
        else if (grepl("eq|cons", whatLabels, ignore.case = TRUE)) {
          eLabels <- GroupPars$par
        }
        else if (grepl("no|omit|hide|invisible", whatLabels,
                       ignore.case = TRUE)) {
          eLabels <- rep("", nrow(Edgelist))
        }
        else stop("Could not detect use of 'whatLabels' argument")
      }
      if (!"edges" %in% as.expression) {
        if (is.numeric(eLabels)) {
          eLabels <- ifelse(is.na(eLabels), "", formatC(eLabels,
                                                        format = ifelse(all(eLabels%%1 == 0), "d",
                                                                        "f"), digits = nDigits))
        }
        else {
          if (nCharEdges > 0)
            eLabels <- abbreviate(eLabels, nCharEdges)
        }
      }
      if (!"nodes" %in% as.expression) {
        if (is.numeric(Labels)) {
          Labels <- ifelse(is.na(Labels), "", formatC(Labels,
                                                      format = ifelse(all(Labels%%1 == 0), "d",
                                                                      "f"), digits = nDigits))
        }
        else {
          if (nCharNodes > 0)
            Labels <- abbreviate(Labels, nCharNodes)
        }
      }
      if (grepl("lisrel", style, ignore.case = TRUE) & residuals &
          covAtResiduals) {
        isResid <- GroupPars$edge == "<->" & GroupPars$lhs !=
          GroupPars$rhs & (GroupPars$lhs %in% GroupPars$lhs[GroupPars$edge ==
                                                              "<->" & GroupPars$lhs == GroupPars$rhs] & GroupPars$rhs %in%
                             GroupPars$rhs[GroupPars$edge == "<->" & GroupPars$lhs ==
                                             GroupPars$rhs])
      }
      else isResid <- rep(FALSE, nrow(Edgelist))
      if (grepl("mx", style, ignore.case = TRUE))
        LoopAsResid <- FALSE
      else LoopAsResid <- TRUE
      if (!allVars) {
        NodeGroups2 <- NodeGroups
        if (!is.null(NodeGroups2)) {
          newNodes <- match(1:length(AllLabs), (1:length(AllLabs))[incl])
          for (g in 1:length(NodeGroups2)) {
            NodeGroups2[[g]] <- newNodes[NodeGroups2[[g]]]
            NodeGroups2[[g]] <- NodeGroups2[[g]][!is.na(NodeGroups2[[g]])]
          }
        }
      }
      if (missing(mar)) {
        if (!layoutSplit) {
          Mar <- c(5, 5, 5, 5)
          if (grepl("lisrel", style, ignore.case = TRUE))
            Mar[c(1, 3)] <- Mar[c(1, 3)] + 3
          Mar <- Mar[(0:3 + (rotation - 1))%%4 + 1]
          if (title)
            Mar[3] <- Mar[3] + 2
        }
        else {
          Mar <- c(3, 3, 3, 3)
        }
      }
      else Mar <- mar
      if (!missing(edge.color)) {
        eColor <- edge.colors
        if (thresholds & missing(thresholdColor)) {
          tColor <- rep(edge.color, length(tColor))
        }
      }
      if (length(freeStyle) > 2 | length(fixedStyle) > 2)
        warning("'freeStyle' and 'fixedStyle' are assumed to be vectors of at most length 2. Unexpected results will probably occur.")
      lty <- rep(1, nrow(GroupPars))
      if (any(is.numeric(fixedStyle) | grepl("^\\d+$", fixedStyle)))
        lty <- ifelse(GroupPars$fixed, as.numeric(fixedStyle[is.numeric(fixedStyle) |
                                                               grepl("^\\d+$", fixedStyle)]), lty)
      if (any(isColor(fixedStyle) & !(is.numeric(fixedStyle) |
                                      grepl("^\\d+$", fixedStyle))))
        eColor[GroupPars$fixed] <- fixedStyle[isColor(fixedStyle) &
                                                !(is.numeric(fixedStyle) | grepl("^\\d+$", fixedStyle))]
      if (any(is.numeric(freeStyle) | grepl("\\d+", freeStyle)))
        lty <- ifelse(GroupPars$fixed, lty, as.numeric(freeStyle[is.numeric(freeStyle) |
                                                                   grepl("\\d+", freeStyle)]))
      if (any(isColor(freeStyle) & !(is.numeric(freeStyle) |
                                     grepl("\\d+", freeStyle))))
        eColor[!GroupPars$fixed] <- freeStyle[isColor(freeStyle) &
                                                !(is.numeric(freeStyle) | grepl("\\d+", freeStyle))]
      Directed <- GroupPars$edge != "--"
      if ("edges" %in% as.expression) {
        eLabels <- lapply(eLabels, function(x) if (x ==
                                                   "")
          x
          else as.expression(parse(text = x)))
      }
      if ("nodes" %in% as.expression) {
        Labels <- lapply(Labels, function(x) if (x == "")
          x
          else as.expression(parse(text = x)))
      }
      if (!is.null(layoutFun))
        Layout <- layoutFun
      if (!missing(nodeLabels)) {
        nLab <- nodeLabels[object@Vars$name %in% GroupVars$name]
      }
      else nLab <- Labels
      if (!missing(edgeLabels)) {
        eLab <- edgeLabels[object@Pars$group == gr]
      }
      else eLab <- eLabels
      CircleEdgeEnd <- rep(FALSE, nrow(Edgelist))
      if (any(c("Between", "Within") %in% GroupPars$BetweenWithin)) {
        if (all(GroupPars$BetweenWithin == "Within")) {
          BetweenPars <- object@Pars[object@Pars$group ==
                                       gsub("Within$", "Between", gr), ]
          BetweenVars <- unique(c(BetweenPars$lhs, BetweenPars$rhs))
          CircleEdgeEnd[GroupPars$rhs %in% BetweenVars &
                          GroupPars$edge %in% c("->", "~>")] <- TRUE
        }
        else if (all(GroupPars$BetweenWithin == "Between")) {
          WithinPars <- object@Pars[object@Pars$group ==
                                      gsub("Between$", "Within", gr), ]
          WithinVars <- unique(c(WithinPars$lhs, WithinPars$rhs))
          Shape[Labels %in% WithinVars] <- shapeLat
        }
        else stop("BetweenWithin not only 'Between' or 'Within'.")
      }
      bars <- list()
      length(bars) <- nN
      barSide <- rep(1, nN)
      if (thresholds) {
        if (missing(thresholdColor) & missing(edge.color)) {
          tColor <- rep("border", nN)
        }
        if (nrow(GroupThresh) > 0) {
          for (node in unique(match(GroupThresh$lhs, GroupVars$name))) {
            IntSide <- 1
            if (layout == "tree") {
              if (rotation %in% c(1, 3)) {
                barSide[node] <- ifelse(Layout[node, 2] >
                                          mean(Layout[, 2]), 3, 1)
              }
              else {
                barSide[node] <- ifelse(Layout[node, 1] >
                                          mean(Layout[, 1]), 4, 2)
              }
            }
            else {
              barSide[node] <- sum((atan2(scale(Layout[, 1])[node], scale(Layout[, 2])[node]) +
                                      pi)%%(2 * pi) > c(0, pi/2, pi, 1.5 * pi))
            }
            bars[[node]] <- pnorm(GroupThresh$est[GroupThresh$lhs ==
                                                    GroupVars$name[node]])
          }
        }
      }
      curveScale <- !layout %in% c("tree", "tree2", "tree3")
      qgraphRes[[which(Groups == gr)]] <- qgraph::qgraph(Edgelist,
                                                         labels = nLab, bidirectional = Bidir, directed = Directed,
                                                         shape = Shape, layout = Layout, lty = lty, loopRotation = loopRotation,
                                                         curve = Curve, edge.labels = eLab, mar = Mar, vsize = vSize,
                                                         vsize2 = vSize2, edge.color = eColor, groups = NodeGroups2,
                                                         color = Vcolors, residuals = LoopAsResid, residScale = residScale,
                                                         residEdge = isResid, edgelist = TRUE, curveDefault = curveDefault,
                                                         knots = GroupPars$knot, aspect = layoutSplit, CircleEdgeEnd = CircleEdgeEnd,
                                                         curveScale = curveScale, bars = bars, barSide = barSide,
                                                         barColor = tColor, barlength.out = thresholdSize, barsAtSide = ThreshAtSide,
                                                         edge.label.cex = edge.label.cex, edgeConnectPoints = ECP,
                                                         ...)
      if (title) {
        title(gr, col.main = title.color, adj = title.adj,
              outer = TRUE, cex.main = title.cex, line = title.line)
      }
    }
    par(ask = askOrig)
    if (length(qgraphRes) == 1)
      qgraphRes <- qgraphRes[[1]]
    invisible(qgraphRes)
  },
  ns = asNamespace('semPlot'))
}
