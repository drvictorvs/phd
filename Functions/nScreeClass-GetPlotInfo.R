setMethod(f = 'GetPlotInfo', signature = 'nScree',
       definition = function (x, y, nScree = x, legend = TRUE, ylab = "Eigenvalues", xlab = "Components",
                         main = "Non Graphical Solutions to Scree Test")
       {
         if (!inherits(nScree, "nScree"))
           stop("Method is only for nScree objects")
         if (nScree$Model == "components")
           nkaiser = "Eigenvalues (>mean  = "
         else nkaiser = "Eigenvalues (>0 = "
         if (nScree$Model == "factors")
           xlab = "Factors"
         eig <- nScree$Analysis$Eigenvalues
         k <- 1:length(eig)
         nk <- length(eig)
         noc <- nScree$Components$noc
         vp.p <- lm(eig[c(noc + 1, nk)] ~ k[c(noc + 1, nk)])
         x <- sum(c(1, 1) * coef(vp.p))
         y <- sum(c(1, nk) * coef(vp.p))
         if (legend == TRUE) {
           leg.txt <- c(
             paste(nkaiser, nScree$Components$nkaiser,
                   ")"), 
             c(paste("Parallel Analysis (n = ", nScree$Components$nparallel,
                     ")")), 
             c(paste("Optimal Coordinates (n = ", nScree$Components$noc,
                     ")")), 
             c(paste("Acceleration Factor (n = ", nScree$Components$naf,
                     ")")))
         }
         naf <- nScree$Components$naf
         return(list(eig = eig, k = k, nk = nk, noc = noc, vp.p = vp.p, x = x, y = y,
                     leg.txt = leg.txt, xlab = xlab, main = main, ylab = ylab, naf= naf,
                     nkaiser = nkaiser, par = nScree$Analysis$Par.Analysis))
       }, where = VVMisc)
