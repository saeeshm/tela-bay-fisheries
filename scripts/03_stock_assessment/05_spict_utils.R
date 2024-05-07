my_annual <- function (intime, vec, type = "mean") 
{
  fun <- match.fun(type)
  anntime <- unique(floor(intime))
  nanntime <- length(anntime)
  nstepvec <- rep(0, nanntime)
  floortime <- floor(intime)
  for (i in seq(nanntime)) {
    nstepvec[i] <- sum(anntime[i] == floortime)
  }
  nsteps <- max(nstepvec)
  # anntime <- anntime[which(nstepvec == max(nstepvec))]
  nanntime <- length(anntime)
  annvec <- rep(0, nanntime)
  for (i in seq(nanntime)) {
    inds <- which(anntime[i] == floortime)
    annvec[i] <- fun(vec[inds])
  }
  return(list(anntime = anntime, annvec = annvec))
}


my_plotspict.catch <- function (rep, main = "Catch", ylim = NULL, qlegend = TRUE, 
                                lcol = "blue", xlab = "Time", ylab = NULL, stamp = get.version(), 
                                verbose = TRUE, CI = 0.95) 
{
  check.rep(rep)
  if (!"sderr" %in% names(rep)) {
    manflag <- any(names(rep) == "man")
    ylabflag <- is.null(ylab)
    inp <- rep$inp
    ylimflag <- !is.null(ylim)
    Cscal <- 1
    cicol <- "lightgray"
    if (manflag) {
      repmax <- get.manmax(rep)
    }
    else {
      repmax <- rep
    }
    tvgflag <- rep$inp$timevaryinggrowth || rep$inp$logmcovflag
    if (tvgflag) {
      MSY <- get.par("logMSYvec", repmax, exp = TRUE, 
                     CI = CI)
      MSYvec <- as.data.frame(MSY)
      MSYvec$msy <- MSYvec$est
    }
    else {
      MSY <- get.par("logMSY", repmax, exp = TRUE, CI = CI)
      MSYvec <- get.msyvec(repmax$inp, MSY)
    }
    Cpredest <- get.par("logCpred", rep, exp = TRUE, CI = CI)
    Cpredest[Cpredest < 0] <- 0
    rep$Cp[rep$Cp < 0] <- 0
    indest <- which(inp$timeCpred <= tail(inp$timeC, 1))
    indpred <- which(inp$timeCpred >= tail(inp$timeC, 1))
    dtc <- inp$dtcp
    if (min(inp$dtc) < 1) {
      alo <- my_annual(inp$timeC, inp$obsC/inp$dtc)
      timeo <- alo$anntime
      obs <- alo$annvec
      al1 <- my_annual(inp$timeCpred[indest], Cpredest[indest, 
                                                    1]/dtc[indest])
      al2 <- my_annual(inp$timeCpred[indest], Cpredest[indest, 
                                                    2]/dtc[indest])
      al3 <- my_annual(inp$timeCpred[indest], Cpredest[indest, 
                                                    3]/dtc[indest])
      inds <- which(!is.na(al2$annvec))
      time <- al2$anntime[inds]
      c <- al2$annvec[inds]
      cl <- al1$annvec[inds]
      cu <- al3$annvec[inds]
      al1p <- my_annual(inp$timeCpred[indpred], Cpredest[indpred, 
                                                      1]/dtc[indpred])
      al2p <- my_annual(inp$timeCpred[indpred], Cpredest[indpred, 
                                                      2]/dtc[indpred])
      al3p <- my_annual(inp$timeCpred[indpred], Cpredest[indpred, 
                                                      3]/dtc[indpred])
      inds <- which(!is.na(al2p$annvec))
      timep <- al2p$anntime[inds]
      cp <- al2p$annvec[inds]
      clp <- al1p$annvec[inds]
      cup <- al3p$annvec[inds]
      al1f <- my_annual(inp$timeCpred, Cpredest[, 1]/dtc)
      al2f <- my_annual(inp$timeCpred, Cpredest[, 2]/dtc)
      al3f <- my_annual(inp$timeCpred, Cpredest[, 3]/dtc)
      inds <- which(!is.na(al2f$annvec))
      timef <- al2f$anntime[inds]
      clf <- al1f$annvec[inds]
      cf <- al2f$annvec[inds]
      cuf <- al3f$annvec[inds]
      if (any(inp$dtc == 1)) {
        inds <- which(inp$dtc == 1)
        timeo <- c(timeo, inp$timeC[inds])
        obs <- c(obs, inp$obsC[inds])
        timeunsort <- c(time, inp$timeCpred[inds])
        timesort <- sort(timeunsort, index = TRUE)
        time <- timesort$x
        c <- c(c, Cpredest[inds, 2])[timesort$ix]
        cl <- c(cl, Cpredest[inds, 1])[timesort$ix]
        cu <- c(cu, Cpredest[inds, 3])[timesort$ix]
      }
    }
    else {
      timeo <- inp$timeC
      obs <- inp$obsC/inp$dtc
      time <- inp$timeCpred[indest]
      c <- Cpredest[indest, 2]/dtc[indest]
      cl <- Cpredest[indest, 1]
      cu <- Cpredest[indest, 3]
      timep <- inp$timeCpred[indpred]
      cp <- Cpredest[indpred, 2]/dtc[indpred]
      clp <- Cpredest[indpred, 1]
      cup <- Cpredest[indpred, 3]
      timef <- inp$timeCpred
      clf <- Cpredest[, 1]
      cf <- Cpredest[, 2]/dtc
      cuf <- Cpredest[, 3]
    }
    fininds <- which(apply(cbind(clf, cuf), 1, function(x) all(is.finite(x))))
    if (!ylimflag) {
      if (length(ylim) != 2) {
        ylim <- range(c(obs, clf[fininds], cuf[fininds], 
                        0.9 * min(MSY[, 1]), 1.07 * max(MSY[, 3])), 
                      na.rm = TRUE)/Cscal
        if (inp$dtpredc > 0) {
          ylim <- range(ylim, clf[fininds], cuf[fininds])
        }
      }
      if (manflag) 
        ylim <- range(ylim, get.manlimits(rep, "logCpred"))
      ylim[2] <- min(c(ylim[2], 3 * max(obs)))
    }
    xlim <- range(c(inp$time, tail(inp$time, 1)))
    if (manflag) 
      xlim <- get.manlimits(rep, "time")
    if (ylabflag) {
      ylab <- "Catch"
      ylab <- add.catchunit(ylab, inp$catchunit)
    }
    plot(time, c, typ = "n", main = main, xlab = xlab, ylab = ylab, 
         xlim = xlim, ylim = ylim)
    polygon(c(repmax$inp$time, rev(repmax$inp$time)), c(MSYvec$ll, 
                                                        rev(MSYvec$ul)), col = cicol, border = cicol)
    cicol2 <- rgb(0, 0, 1, 0.1)
    lines(time, cl, col = lcol, lwd = 1.5, lty = 2)
    lines(time, cu, col = lcol, lwd = 1.5, lty = 2)
    if (!manflag) 
      abline(v = inp$timeC[tail(which(inp$timeC%%1 == 
                                        0), 1)], col = "gray")
    plot.col(timeo, obs/Cscal, cex = 0.7, do.line = FALSE, 
             add = TRUE, add.legend = qlegend)
    if ("infl" %in% names(rep) & min(inp$dtc) == 1) {
      infl <- rep$infl$infl[1:inp$nobsC, ]
      cols <- apply(!is.na(infl), 1, sum)
      ncols <- length(unique(cols))
      inds <- which(cols > 0)
      points(inp$timeC[inds], inp$obsC[inds]/Cscal, pch = 21, 
             cex = 0.9, bg = cols[inds])
    }
    if ("true" %in% names(inp)) {
      abline(h = inp$true$MSY, col = true.col(), lty = 1)
      abline(h = inp$true$MSY, col = "black", lty = 3)
    }
    lines(repmax$inp$time, MSYvec$msy)
    lines(time, c, col = lcol, lwd = 1.5)
    if (manflag) {
      manint <- rep$man[[1]]$inp$maninterval
      mandiff <- diff(manint)
      if (mandiff%%1 != 0) {
        if (verbose) 
          cat("At least part of the catch during the management period cannot be aggregated into my_annual/seasonal catches.\n")
      }
      if (check.man(rep, verbose = FALSE)$mantime) {
        timecpred <- rep$man[[1]]$inp$timeCpred
        dtcp <- rep$man[[1]]$inp$dtcp
        ind <- which(timecpred < manint[1] & dtcp <= 
                       1)
        est <- get.par("logCpred", rep$man[[1]], exp = TRUE, 
                       CI = CI)[, 2]
        alo <- my_annual(timecpred[ind], est[ind]/dtcp[ind], 
                      mean)
        timecpred <- alo$anntime
        indmax <- which.max(timecpred)
        dtcp <- diff(c(timecpred, manint[1]))
        ind <- which(timecpred < manint[1])
        ind2 <- unique(c(which(dtcp[ind] == 1), indmax))
        preind <- tail(ind2, 1)
        preman <- timecpred[preind]
        endman <- preman + manint[1] - timecpred[tail(ind, 
                                                      1)] + max(0, mandiff - 1)
        if (any(dtcp < 1 | dtcp > 1)) 
          preman <- NULL
        if (mandiff%%1 != 0) 
          endman <- NULL
        abline(v = c(preman, endman), col = "grey", 
               lty = 1, lwd = 1)
      }
      add.manlines(rep, "logCpred", index.shift = 1, plot.legend = qlegend, 
                   verbose = verbose)
    }
    else {
      if (inp$dtpredc > 0) {
        lines(timep, cp, col = lcol, lty = 3)
        lines(timep, clp, col = lcol, lwd = 1, lty = 2)
        lines(timep, cup, col = lcol, lwd = 1, lty = 2)
      }
    }
    if (rep$opt$convergence != 0) {
      warning.stamp()
    }
    box(lwd = 1.5)
    txt.stamp(stamp)
  }
}

# Passing the custom function to the tidyhydat namespace to allow it use all of
# the namespace dependencies when being called
environment(my_plotspict.catch) <- asNamespace('spict')
# assignInNamespace("plotspict.catch", my_plotspict.catch, ns = "spict")
