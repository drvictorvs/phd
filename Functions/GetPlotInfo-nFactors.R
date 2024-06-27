
if(require(nFactors)){

 setGeneric(name = 'GetPlotInfo',
            def = function(x, y, ...)
             standardGeneric('GetPlotInfo'),
            where = VVMisc)

 setMethod(f = 'GetPlotInfo',
           definition = function(x, y, ...) UseMethod('GetPlotInfo'),
           where = VVMisc)

 setMethod(f = 'GetPlotInfo', signature = "SingleGroupClass",
           definition = function(x, y, ...) {
            .local <- function(x, y, type = "score", npts = 200, drop2 = TRUE,
                               degrees = 45, theta_lim = c(-6, 6),
                               which.items = 1:extract.mirt(x, "nitems"),
                               MI = 0, CI = 0.95, rot = list(xaxis = -70,
                                                             yaxis = 30,
                                                             zaxis = 10),
                               facet_items = TRUE, main = NULL, drape = TRUE,
                               colorkey = TRUE, ehist.cut = 1e-10, add.ylab2 = TRUE,
                               par.strip.text = list(cex = 0.7),
                               par.settings = list(strip.background = list(col = "#9ECAE1"),
                                                   strip.border = list(col = "black")),
                               auto.key = list(space = "right",
                                               points = FALSE,
                                               lines = TRUE),
                               profile = FALSE,
                               ...)
            {
             dots <- list(...)
             if (!(type %in% c("info", "SE", "infoSE", "rxx", "trace",
                               "score", "itemscore", "infocontour", "infotrace",
                               "scorecontour", "empiricalhist", "Davidian", "EAPsum")))
              cli::cli_abort("type supplied is not supported")
             if (any(degrees > 90 | degrees < 0))
              cli::cli_abort("Improper angle specified. Must be between 0 and 90.",
                             call. = FALSE)
             rot <- list(x = rot[[1]], y = rot[[2]], z = rot[[3]])
             nfact <- x@Model$nfact
             if (nfact > 3)
              cli::cli_abort("Can't plot high dimensional solutions.", call. = FALSE)
             J <- x@Data$nitems
             theta <- seq(theta_lim[1L], theta_lim[2L], length.out = npts/(nfact^2))
             ThetaFull <- Theta <- mirt:::thetaComb(theta, nfact)
             prodlist <- attr(x@ParObjects$pars, "prodlist")
             if (all(x@Data$K[which.items] == 2L) && facet_items)
              auto.key <- FALSE
             if (length(prodlist))
              ThetaFull <- mirt:::prodterms(Theta, prodlist)
             if (length(degrees) > ncol(ThetaFull))
              type <- "infoangle"
             if (length(degrees) == 1L)
              degrees <- rep(degrees, ncol(ThetaFull))
             info <- numeric(nrow(ThetaFull))
             if (type %in% c("info", "infocontour", "rxx", "SE", "infoSE"))
              info <- mirt:::testinfo(x, ThetaFull, degrees = degrees,
                                      which.items = which.items)
             if (type == "infoangle") {
              for (i in seq_len(length(degrees))) info <- info +
                mirt:::testinfo(x, ThetaFull,
                                degrees = rep(degrees[i], ncol(ThetaFull)),
                                which.items = which.items)
             }
             mins <- x@Data$mins
             maxs <- mirt:::extract.mirt(x, "K") + mins - 1
             rotate <- if (is.null(dots$rotate))
              "none"
             else dots$rotate
             if (x@Options$exploratory) {
              if (!is.null(dots$rotate)) {
               so <- summary(x, verbose = FALSE, digits = 5,
                             ...)
               a <- mirt:::rotateLambdas(so) * 1.702
               for (i in 1:J) x@ParObjects$pars[[i]]@par[1:nfact] <- a[i,
               ]
              }
             }
             itemtrace <- mirt:::computeItemtrace(x@ParObjects$pars, ThetaFull,
                                                  x@Model$itemloc, CUSTOM.IND = x@Internals$CUSTOM.IND)
             score <- c()
             for (i in 1:J) score <- c(score, (0:(x@Data$K[i] - 1) +
                                                mins[i]) * (i %in% which.items))
             score <- matrix(score, nrow(itemtrace), ncol(itemtrace),
                             byrow = TRUE)
             plt <- data.frame(cbind(info, score = rowSums(score *
                                                            itemtrace), Theta = Theta))
             bundle <- length(which.items) != J
             gp <- mirt:::ExtractGroupPars(x@ParObjects$pars[[J + 1]])
             if (MI > 0L && nfact == 1L) {
              tmpx <- x
              if (!x@Options$SE)
               cli::cli_abort("Must compute an information matrix")
              covB <- x@vcov
              pre.ev <- eigen(covB)
              names <- colnames(covB)
              tmp <- lapply(names, function(x, split) {
               as.numeric(strsplit(x, split = split)[[1L]][-1L])
              }, split = "\\.")
              imputenums <- do.call(c, tmp)
              CIscore <- CIinfo <- CIrxx <- matrix(0, MI, length(plt$score))
              for (i in seq_len(MI)) {
               while (TRUE) {
                tmp <- try(imputePars(pars = x@ParObjects$pars,
                                      pre.ev = pre.ev, imputenums = imputenums,
                                      constrain = x@Model$constrain), silent = TRUE)
                if (!is(tmp, "try-error"))
                 break
               }
               tmpx@ParObjects$pars <- tmp
               gp2 <- mirt:::ExtractGroupPars(tmp[[J + 1]])
               itemtrace <- mirt:::computeItemtrace(tmpx@ParObjects$pars,
                                                    ThetaFull, x@Model$itemloc, CUSTOM.IND = x@Internals$CUSTOM.IND)
               tmpscore <- rowSums(score * itemtrace)
               CIscore[i, ] <- tmpscore
               CIinfo[i, ] <- testinfo(tmpx, ThetaFull)
               CIrxx[i, ] <- CIinfo[i, ]/(CIinfo[i, ] + 1/gp2$gcov[1L,
                                                                   1L])
              }
             }
             mins <- mins[which.items]
             maxs <- maxs[which.items]
             ybump <- (max(maxs) - min(mins))/15
             ybump_full <- (sum(maxs) - sum(mins))/15
             #### EAPSum ####
             if (type == "EAPsum") {
              if (is.null(main))
               main <- "Expected vs Observed Sum-Scores"
              fs <- fscores(x, method = "EAPsum", full.scores = FALSE,
                            verbose = FALSE, ...)
              plt <- with(fs, data.frame(Scores = Sum.Scores, y = c(observed, expected),
                                         group = rep(c("observed", "expected"),
                                                     each = nrow(fs))))
              ##### Return Plot N = 1 #####
              return(plt)
              # return(xyplot(y ~ Scores, plt, type = "l", main = main,
              #               group = plt$group, auto.key = auto.key,
              #               xlab = expression(Sum - Score),
              #               ylab = expression(n),
              #               par.strip.text = par.strip.text,
              #               par.settings = par.settings, ...))
             }
             if (nfact == 3) {
              colnames(plt) <- c("info", "score", "Theta1", "Theta2",
                                 "Theta3")
              plt$SE <- 1/sqrt(plt$info)
              #### Infocontour ####
              if (type == "infocontour") {
               if (is.null(main)) {
                main <- paste("Test Information Contour")
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(contourplot(info ~ Theta1 * Theta2 | Theta3,
               #                    data = plt, main = main, xlab = expression(theta[1]),
               #                    ylab = expression(theta[2]), par.strip.text = par.strip.text,
               #                    par.settings = par.settings, ...))
              }
              #### Scorecontour ####
              else if (type == "scorecontour") {
               if (is.null(main)) {
                main <- paste("Expected Score Contour")
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(contourplot(score ~ Theta1 * Theta2 |
               #                     Theta3, data = plt, ylim = c(sum(mins) - ybump_full,
               #                                                  sum(maxs) + ybump_full), main = main,
               #                    xlab = expression(theta[1]),
               #                    ylab = expression(theta[2]),
               #                    ylim = c(sum(mins) - 0.1, sum(maxs) + 0.1),
               #                    par.strip.text = par.strip.text,
               #                    par.settings = par.settings, ...))
              }
              #### Info ####
              else if (type == "info") {
               if (is.null(main)) {
                main <- "Test Information"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(wireframe(info ~ Theta1 + Theta2 | Theta3,
               #                  data = plt, main = main, zlab = expression(I(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              #### SEcontour ####
              else if (type == "SEcontour") {
               if (is.null(main)) {
                main <- "Test Standard Errors"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(contourplot(score ~ Theta1 * Theta2 |
               #                     Theta3, data = plt, main = main, xlab = expression(theta[1]),
               #                    ylab = expression(theta[2]), par.strip.text = par.strip.text,
               #                    par.settings = par.settings, ...))
              }
              #### Score ####
              else if (type == "score") {
               if (is.null(main)) {
                main <- if (bundle)
                 "Expected Bundle Score"
                else "Expected Total Score"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(wireframe(score ~ Theta1 + Theta2 | Theta3,
               #                  data = plt,
               #                  main = main,
               #                  ylim = c(sum(mins) - ybump_full, sum(maxs) + ybump_full),
               #                  zlab = expression(Total(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ylim = c(sum(mins) -
               #                                                         0.1, sum(maxs) + 0.1), ...))
              }
              #### SE ####
              else if (type == "SE") {
               if (is.null(main)) {
                main <- "Test Standard Errors"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 3 #####
               return(plt)
               # return(wireframe(SE ~ Theta1 + Theta2 | Theta3,
               #                  data = plt, main = main, zlab = expression(SE(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              else {
               cli::cli_abort("plot type not supported for three dimensional model",
                              call. = FALSE)
              }
             }
             else if (nfact == 2) {
              colnames(plt) <- c("info", "score", "Theta1", "Theta2")
              plt$SE <- 1/sqrt(plt$info)
              #### Infocontour ####
              if (type == "infocontour") {
               if (is.null(main)) {
                main <- paste("Test Information Contour")
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(plt)
               # return(contourplot(info ~ Theta1 * Theta2, data = plt,
               #                    main = main, xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                    par.strip.text = par.strip.text, par.settings = par.settings,
               #                    ...))
              }
              #### Scorecontour ####
              else if (type == "scorecontour") {
               if (is.null(main)) {
                main <- paste("Expected Score Contour")
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(plt)
               # return(contourplot(score ~ Theta1 * Theta2, data = plt,
               #                    ylim = c(sum(mins) - ybump_full, sum(maxs) +
               #                              ybump_full), main = main, xlab = expression(theta[1]),
               #                    ylab = expression(theta[2]), par.strip.text = par.strip.text,
               #                    par.settings = par.settings, ...))
              }
              #### Info ####
              else if (type == "info") {
               if (is.null(main)) {
                main <- "Test Information"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ###### Return Plot N = 2 ######
               return(plt)
               # return(wireframe(info ~ Theta1 + Theta2, data = plt,
               #                  main = main, zlab = expression(I(theta)), xlab = expression(theta[1]),
               #                  ylab = expression(theta[2]), scales = list(arrows = FALSE),
               #                  screen = rot, colorkey = colorkey, drape = drape,
               #                  par.strip.text = par.strip.text, par.settings = par.settings,
               #                  ...))
              }
              #### Trace ####
              else if (type == "trace") {
               if (is.null(main))
                main <- "Item Probability Functions"
               P <- vector("list", length(which.items))
               names(P) <- colnames(x@Data$data)[which.items]
               ind <- 1L
               for (i in which.items) {
                tmp <- probtrace(extract.item(x, i), ThetaFull)
                if (ncol(tmp) == 2L && drop2)
                 tmp <- tmp[, 2, drop = FALSE]
                tmp2 <- data.frame(P = as.numeric(tmp),
                                   cat = gl(ncol(tmp),
                                            k = nrow(ThetaFull),
                                            labels = paste0("P",
                                                            seq_len(ncol(tmp)))))
                P[[ind]] <- tmp2
                ind <- ind + 1L
               }
               nrs <- sapply(P, nrow)
               Pstack <- do.call(rbind, P)
               names <- c()
               for (i in seq_len(length(nrs))) names <- c(names,
                                                          rep(names(P)[i], nrs[i]))
               plotobj <- data.frame(Pstack, item = names, Theta = ThetaFull)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               ##### Return Plot N = 2 #####
               return(list(plotobj = plotobj, rot = rot, colorkey = colorkey, drape = drape, par.settings = par.settings,
                           par.strip.text = par.strip.text))
               # return(wireframe(P ~ Theta.1 * Theta.2 | item,
               #                  plotobj, zlim = c(-0.1, 1.1), group = plotobj$cat,
               #                  zlab = expression(P(theta)), xlab = expression(theta[1]),
               #                  ylab = expression(theta[2]), scales = list(arrows = FALSE),
               #                  screen = rot, colorkey = colorkey, drape = drape,
               #                  par.strip.text = par.strip.text, par.settings = par.settings,
               #                  ...))
              }
              #### Infotrace ####
              else if (type == "infotrace") {
               if (is.null(main))
                main <- "Item Information"
               I <- matrix(NA, nrow(Theta), J)
               for (i in which.items) I[, i] <- iteminfo(extract.item(x,
                                                                      i), ThetaFull, degrees = degrees)
               I <- t(na.omit(t(I)))
               items <- rep(colnames(x@Data$data)[which.items],
                            each = nrow(Theta))
               plotobj <- data.frame(I = as.numeric(I), Theta = ThetaFull,
                                     item = items)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               ##### Return Plot N = 2 #####
               return(list(plotobj, I, Theta.1, Theta.2, items, main, plt))
               # return(wireframe(I ~ Theta.1 * Theta.2 | item,
               #                  plotobj, group = plotobj$cat, zlab = expression(P(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              #### Itemscore ####
              else if (type == "itemscore") {
               if (is.null(main))
                main <- "Expected Item Score"
               S <- vector("list", length(which.items))
               names(S) <- colnames(x@Data$data)[which.items]
               ind <- 1L
               for (i in which.items) {
                S[[ind]] <- expected.item(extract.item(x, i),
                                          ThetaFull, mins[i])
                ind <- ind + 1L
               }
               Sstack <- do.call(c, S)
               names <- rep(names(S), each = nrow(ThetaFull))
               plotobj <- data.frame(S = Sstack, item = names,
                                     Theta = ThetaFull)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               ##### Return Plot N = 2 #####
               return(plt)
               # return(wireframe(S ~ Theta.1 * Theta.2 | item,
               #                  plotobj, group = plotobj$cat, zlab = expression(P(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              #### SEContour ####
              else if (type == "SEcontour") {
               if (is.null(main)) {
                main <- "Test Standard Errors"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(contourplot(score ~ Theta1 * Theta2, data = plt,
                                  main = main, xlab = expression(theta[1]), ylab = expression(theta[2]),
                                  par.strip.text = par.strip.text, par.settings = par.settings,
                                  ...))
              }
              #### Score ####
              else if (type == "score") {
               if (is.null(main)) {
                main <- if (bundle)
                 "Expected Bundle Score"
                else "Expected Total Score"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(plot)
               # return(wireframe(score ~ Theta1 + Theta2, data = plt,
               #                  main = main, zlim = c(sum(mins) - ybump_full,
               #                                        sum(maxs) + ybump_full), zlab = expression(Total(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              #### Infoangle ####
              else if (type == "infoangle") {
               if (is.null(main)) {
                main <- "Information across different angles"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(plt)
               # graphics::symbols(plt$Theta1, plt$Theta2, circles = sqrt(plt$info/pi),
               #                   inches = 0.35, fg = "white", bg = "blue", xlab = expression(theta[1]),
               #                   ylab = expression(theta[2]), main = main)
              }
              #### SE ####
              else if (type == "SE") {
               if (is.null(main)) {
                main <- "Test Standard Errors"
                if (x@Options$exploratory)
                 main <- paste0(main, " (rotate = '", rotate,
                                "')")
               }
               ##### Return Plot N = 2 #####
               return(plt)
               # return(wireframe(SE ~ Theta1 + Theta2, data = plt,
               #                  main = main, zlab = expression(SE(theta)),
               #                  xlab = expression(theta[1]), ylab = expression(theta[2]),
               #                  scales = list(arrows = FALSE), screen = rot,
               #                  colorkey = colorkey, drape = drape, par.strip.text = par.strip.text,
               #                  par.settings = par.settings, ...))
              }
              else {
               cli::cli_abort("plot type not supported for two dimensional model",
                              call. = FALSE)
              }
             }
             else {
              colnames(plt) <- c("info", "score", "Theta")
              plt$SE <- 1/sqrt(plt$info)
              plt$rxx <- plt$info/(plt$info + 1/gp$gcov[1L, 1L])
              if (MI > 0) {
               bs_range <- function(x, CI) {
                ss <- sort(x)
                N <- length(ss)
                ret <- c(upper = ss[ceiling(N * (1 - (1 - CI)/2))],
                         middle = median(x),
                         lower = ss[floor(N * (1 - CI)/2)])
                ret
               }
               tmp <- apply(CIscore, 2, bs_range, CI = CI)
               plt$CIscoreupper <- tmp["upper", ]
               plt$CIscorelower <- tmp["lower", ]
               tmp <- apply(CIinfo, 2, bs_range, CI = CI)
               plt$CIinfoupper <- tmp["upper", ]
               plt$CIinfolower <- tmp["lower", ]
               plt$CISElower <- 1/sqrt(tmp["upper", ])
               plt$CISEupper <- 1/sqrt(tmp["lower", ])
               tmp <- apply(CIrxx, 2, bs_range, CI = CI)
               plt$CIrxxupper <- tmp["upper", ]
               plt$CIrxxlower <- tmp["lower", ]
              }
              #### Info ####
              if (type == "info") {
               if (is.null(main))
                main <- "Test Information"
               if (MI > 0) {
                #### Return Plot N = 1 c/ MI ####
                out <- list()
                out$main <- main
                out$info <- info
                out$Theta <- plt$Theta
                out$xlab <- expression(theta)
                out$ylab <- expression(I(theta))
                out$ylims <- c(min(plt$CIinfolower), max(plt$CIinfoupper))
                return(out)
                # return(data.frame(info = info, Theta = plt$Theta) |>
                #         ggplot(aes(y = info, x = Theta)) +
                #         geom_path(color = "#E6E6E6") +
                #         labs(title = main,
                #              xlab = expression(theta),
                #              ylab = expression(I(theta))) +
                #         scale_x_continuous() +
                #         scale_y_continuous(limits =  c(min(plt$CIinfolower),
                #                                        max(plt$CIinfoupper)))
                #         )
                # return(xyplot(info ~ Theta, data = plt, upper = plt$CIinfoupper,
                #               lower = plt$CIinfolower, panel = function(x,
                #                                                         y, lower, upper, ...) {
                #                panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                #                              col = "#E6E6E6", border = FALSE, ...)
                #                panel.xyplot(x, y, type = "l", lty = 1,
                #                             ...)
                #               }, main = main, ylim = c(min(plt$CIinfolower),
                #                                        max(plt$CIinfoupper)), ylab = expression(I(theta)),
                #               xlab = expression(theta), par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                #### Return Plot N = 1 s/ MI ####
                out <- list()
                out$info <- info
                out$Theta <- plt$Theta
                return(out)
                # return(xyplot(info ~ Theta, plt, type = "l",
                #               main = main, xlab = expression(theta), ylab = expression(I(theta)),
                #               par.strip.text = par.strip.text, par.settings = par.settings,
                #               ...))
               }
              }
              #### Reliability (rxx) ####
              else if (type == "rxx") {
               if (is.null(main))
                main <- "Reliability"
               if (MI > 0) {
                ##### Return Plot N = 1 c/ MI #####
                return(plt)
                # return(xyplot(rxx ~ Theta, data = plt, upper = plt$CIrxxupper,
                #               lower = plt$CIrxxlower, panel = function(x,
                #                                                        y, lower, upper, ...) {
                #                panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                #                              col = "#E6E6E6", border = FALSE, ...)
                #                panel.xyplot(x, y, type = "l", lty = 1,
                #                             ...)
                #               }, main = main, ylim = c(-0.1, 1.1), ylab = expression(r[xx](theta)),
                #               xlab = expression(theta), par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot N = 1 s/ MI #####
                return(plt)
                # return(xyplot(rxx ~ Theta, plt, type = "l",
                #               main = main, ylim = c(-0.1, 1.1), xlab = expression(theta),
                #               ylab = expression(r[xx](theta)), par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
              }
              #### SE ####
              else if (type == "SE") {
               if (is.null(main))
                main <- "Test Standard Errors"
               if (MI > 0) {
                ##### Return Plot N = 1 c/ MI #####
                return(plt)
                # return(xyplot(SE ~ Theta, data = plt, upper = plt$CISEupper,
                #               lower = plt$CISElower, panel = function(x,
                #                                                       y, lower, upper, ...) {
                #                panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                #                              col = "#E6E6E6", border = FALSE, ...)
                #                panel.xyplot(x, y, type = "l", lty = 1,
                #                             ...)
                #               }, main = main, ylim = c(min(plt$CISElower),
                #                                        max(plt$CISEupper)), ylab = expression(I(theta)),
                #               xlab = expression(theta), par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot s/ MI #####
                return(plt)
                # return(xyplot(SE ~ Theta, plt, type = "l",
                #               main = main, xlab = expression(theta), ylab = expression(SE(theta)),
                #               par.strip.text = par.strip.text, par.settings = par.settings,
                #               ...))
               }
              }
              #### InfoSE ####
              else if (type == "infoSE") {
               if (is.null(main))
                main <- "Test Information and Standard Errors"
               par.settings <- c(par.settings,
                                 lattice::simpleTheme(col = c("#0080ff","red"), lty = 1:2))
               obj1 <- xyplot(info ~ Theta, plt, type = "l",
                              main = main, xlab = expression(theta), ylab = expression(I(theta)),
                              par.strip.text = par.strip.text, par.settings = par.settings, ...)
               obj2 <- xyplot(SE ~ Theta, plt, type = "l", ylab = expression(SE(theta)),
                              par.strip.text = par.strip.text, par.settings = par.settings, ...)
               if (requireNamespace("latticeExtra", quietly = TRUE)) {
                ##### Return Plot N = 1 #####
                return(plt)
                # return(latticeExtra::doubleYScale(obj1, obj2,
                #                                   add.ylab2 = add.ylab2, ...))
               }
               else {
                cli::cli_abort("latticeExtra package is not available. Please install.",
                               call. = FALSE)
               }
              }
              #### Trace ####
              else if (type == "trace") {
               if (is.null(main))
                main <- "Item Probability Functions"
               P <- vector("list", length(which.items))
               names(P) <- colnames(x@Data$data)[which.items]
               ind <- 1L
               alltwocats <- all(extract.mirt(x, "K")[which.items] ==
                                  2L)
               for (i in which.items) {
                tmp <- probtrace(extract.item(x, i), ThetaFull)
                if (ncol(tmp) == 2L && (facet_items || (!facet_items &&
                                                        alltwocats)) && drop2)
                 tmp <- tmp[, 2, drop = FALSE]
                tmp2 <- data.frame(P = as.numeric(tmp),
                                   cat = gl(ncol(tmp),
                                            k = nrow(Theta),
                                            labels = paste0("P", seq_len(ncol(tmp)))))
                P[[ind]] <- tmp2
                ind <- ind + 1L
               }
               nrs <- sapply(P, nrow)
               Pstack <- do.call(rbind, P)
               names <- c()
               for (i in seq_len(length(nrs))) names <- c(names,
                                                          rep(names(P)[i], nrs[i]))
               plotobj <- data.frame(Pstack, item = names, Theta = Theta)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               if (facet_items) {
                ##### Return Plot N = 1 Per Item #####
                return(list(plotobj = plotobj, cat = cat, auto.key = auto.key,
                            par.settings = par.settings, par.strip.text = par.strip.text))
                # return(xyplot(P ~ Theta | item, plotobj, ylim = c(-0.1,1.1),
                #               groups = cat, xlab = expression(theta),
                #               ylab = expression(P(theta)), auto.key = auto.key,
                #               type = "l", main = main, par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot N = 1 Per Test #####
                return(list(plotobj = plotobj, cat = cat, auto.key = auto.key,
                            par.settings = par.settings, par.strip.text = par.strip.text))
                # return(xyplot(P ~ Theta | cat, plotobj, groups = plotobj$item,
                #               ylim = c(-0.1, 1.1), xlab = expression(theta),
                #               ylab = expression(P(theta)), auto.key = auto.key,
                #               type = "l", main = main, par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
              }
              #### ItemScore ####
              else if (type == "itemscore") {
               if (is.null(main))
                main <- "Expected Item Score"
               S <- vector("list", length(which.items))
               names(S) <- colnames(x@Data$data)[which.items]
               ind <- 1L
               for (i in which.items) {
                S[[ind]] <- expected.item(extract.item(x, i),
                                          ThetaFull, mins[i])
                ind <- ind + 1L
               }
               Sstack <- do.call(c, S)
               names <- rep(names(S), each = nrow(ThetaFull))
               plotobj <- data.frame(S = Sstack, item = names,
                                     Theta = Theta)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               if (facet_items) {
                ##### Return Plot N = 1 Per Item #####
                return(plt)
                # return(xyplot(S ~ Theta | item, plotobj,
                #               ylim = c(min(mins) - ybump, max(maxs) + ybump),
                #               xlab = expression(theta),
                #               ylab = expression(S(theta)), auto.key = auto.key,
                #               type = "l", main = main, par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot N = 1 Per Test#####
                return(plt)
                # return(xyplot(S ~ Theta, plotobj, groups = plotobj$item,
                #               ylim = c(min(mins) - 0.1, max(maxs) + 0.1),
                #               xlab = expression(theta), ylab = expression(S(theta)),
                #               auto.key = auto.key, type = "l", main = main,
                #               par.strip.text = par.strip.text, par.settings = par.settings,
                #               ...))
               }
              }
              #### Infotrace ####
              else if (type == "infotrace") {
               if (is.null(main))
                main <- "Item Information"
               I <- matrix(NA, nrow(Theta), J)
               for (i in which.items) I[, i] <- iteminfo(extract.item(x,
                                                                      i), ThetaFull)
               I <- t(na.omit(t(I)))
               items <- rep(colnames(x@Data$data)[which.items],
                            each = nrow(Theta))
               plotobj <- data.frame(I = as.numeric(I), Theta = Theta,
                                     item = items)
               plotobj$item <- factor(plotobj$item, levels = colnames(x@Data$data)[which.items])
               if (facet_items) {
                ##### Return Plot N = 1 Per Item #####
                return(list(main = main, I = I, items = items, plotobj = plotobj, plt = plt, main = main, par.strip.text = par.strip.text, par.settings = par.settings,
                            auto.key = auto.key))
                # return(xyplot(I ~ Theta | item, plotobj, xlab = expression(theta),
                #               ylab = expression(I(theta)), auto.key = auto.key,
                #               type = "l", main = main, par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot N = 1 Per Test #####
                return(list(main = main, I = I, items = items, plotobj = plotobj, plt = plt, main = main, par.strip.text = par.strip.text, par.settings = par.settings,
                            auto.key = auto.key))
                # return(xyplot(I ~ Theta, plotobj, groups = plotobj$item,
                #               xlab = expression(theta), ylab = expression(I(theta)),
                #               auto.key = auto.key, type = "l", main = main,
                #               par.strip.text = par.strip.text, par.settings = par.settings,
                #               ...))
               }
              }
              #### Score ####
              else if (type == "score") {
               if (is.null(main))
                main <- if (bundle)
                 "Expected Bundle Score"
               else "Expected Total Score"
               if (MI > 0) {
                ##### Return Plot N = 1 c/ MI #####
                return(plt)
                # return(xyplot(score ~ Theta, data = plt,
                #               ylim = c(sum(mins) - ybump_full, sum(maxs) + ybump_full),
                #               upper = plt$CIscoreupper,
                #               lower = plt$CIscorelower,
                #               panel = function(x,y, lower, upper, ...) {
                #                panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                #                              col = "#E6E6E6", border = FALSE, ...)
                #                panel.xyplot(x, y, type = "l", lty = 1,
                #                             ...)
                #               },
                #               main = main,
                #               ylab = expression(T(theta)),
                #               xlab = expression(theta), par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
               else {
                ##### Return Plot N = 1 s/ MI#####
                return(plt)
                # return(xyplot(score ~ Theta, plt,
                #               ylim = c(sum(mins) - ybump_full, sum(maxs) + ybump_full),
                #               xlab = expression(theta),
                #               ylab = expression(T(theta)), type = "l",
                #               main = main, par.strip.text = par.strip.text,
                #               par.settings = par.settings, ...))
               }
              }
              #### EmpiricalHist ####
              else if (type == "empiricalhist") {
               if (!(x@Options$dentype %in% c("EH", "EHW")))
                cli::cli_abort("Empirical histogram was not estimated for this object",
                               call. = FALSE)
               if (is.null(main))
                main <- "Empirical Histogram"
               Prior <- x@Internals$Prior[[1L]]
               Theta <- x@Model$Theta
               cuts <- cut(Theta, floor(npts/2))
               Prior <- do.call(c, lapply(split(Prior, cuts),
                                          mean))
               Theta <- do.call(c, lapply(split(Theta, cuts),
                                          mean))
               keep1 <- min(which(Prior > ehist.cut))
               keep2 <- max(which(Prior > ehist.cut))
               plt <- data.frame(Theta = Theta, Prior = Prior)
               plt <- plt[keep1:keep2, , drop = FALSE]
               ##### Return Plot N = 1 #####
               out <- list()
               out <- list(Prior, Theta, plt)
               return(out)

               # return(xyplot(Prior ~ Theta, plt, xlab = expression(theta),
               #               ylab = "Density", type = "b", main = main,
               #               par.strip.text = par.strip.text, par.settings = par.settings,
               #               ...))
              }
              #### Davidian ####
              else if (type == "Davidian") {
               if (x@Options$dentype != "Davidian")
                cli::cli_abort("Davidian curve was not estimated for this object",
                               call. = FALSE)
               if (is.null(main))
                main <- "Davidian Curve"
               Prior <- x@Internals$Prior[[1L]]
               Theta <- x@Model$Theta
               plt <- data.frame(Theta = Theta, Prior = Prior)
               ##### Return Plot #####
               return(plt)
               # return(xyplot(Prior ~ Theta, plt, xlab = expression(theta),
               #               ylab = "Density", type = "b", main = main,
               #               par.strip.text = par.strip.text, par.settings = par.settings,
               #               ...))
              }
              else {
               cli::cli_abort("plot not supported for unidimensional models",
                              call. = FALSE)
              }
             }
            }
            .local(x, y = y, ...)
           }, where = VVMisc)
}
