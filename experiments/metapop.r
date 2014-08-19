# Copyright 2014 Adam Waite
#
# This file is part of metapop.
#
# metapop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.  
#
# metapop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with metapop.  If not, see <http://www.gnu.org/licenses/>.


source("~/Documents/Code/R/utilities/default_plot.r")

INFO_FILE <- "world_info.txt"
square.plot.dim <- 12
long.plot.dim <- 18
mtext.cex <- 2.5
xlab.dist <- 2.7
ylab.dist <- 3.6
axis.cex <- 1.2
pch.cex <- 1.5
fit.col <- "black"

metapop.process <- function(path, nrows=-1) {
    if (is.na(file.info(path)$size)) {
        cat("\nFile", path, "is not a file.\n")
        return()
    }
    if (file.info(path)$size == 0) {
        cat("\nFile", path, "had 0 size.\n")
        return()
    }

    dat <- read.delim(path, stringsAsFactors=FALSE, nrows=nrows)
    dat[is.na(dat)] <- 0
    dat <- within(dat, coops <- anc_coop + evo_coop)
    dat <- within(dat, cheats <- anc_cheat + evo_cheat)
    coop.cols  <- grep("_coop", names(dat))
    cheat.cols <- grep("_cheat", names(dat))
    coop.sum.col  <- grep("coop", names(dat))
    cheat.sum.col <- grep("cheat", names(dat))
    resource.col <- grep("resource", names(dat))
    dat.summary <- aggregate(dat[c(coop.cols, cheat.cols, coop.sum.col,
                                   cheat.sum.col, resource.col)],
                             list(timestep=dat$timestep),
                             function(x) sum(as.numeric(x)))
    dat.summary <- within(dat.summary, coop.ratio <- coops/cheats)
    dat.summary <- within(dat.summary, coop.freq <- coops/(coops+cheats))

    list(data=dat, summary=dat.summary, coop.cols=coop.cols,
         cheat.cols=cheat.cols)
}

plot.folder <- function(folder, save.path, run.pattern, device="png", 
                        skip.completed=FALSE, ...)
{
    if (missing(save.path)) {
        save.folder <- ifelse(grepl("^/", folder), substring(folder,2), folder)
        save.path <- file.path("fig", save.folder)
    }
    if (!file.exists(save.path)) dir.create(save.path, recursive=TRUE)

    folders <- list.files(folder, full.names=TRUE)
    folders <- subset(folders, grepl("global|local", folders))
    
    runs <- if (missing(run.pattern)) {
        # only taking first run if multiple reps are present.
        lapply(folders, function (x) list.files(x, full.names=TRUE))
    } else {
        list.files(folders, full.names=TRUE, pattern=run.pattern)
    }
    n.runs <- length(do.call("rbind",runs))

    res <- data.frame()
    cnt <- 1

    plotted <- list.files(save.path)
    for (cond in runs) {
        for (run in cond) {
            cat(cnt, "of", n.runs, "\n")

            if (skip.completed) {
                if (any(grepl(basename(run), plotted))) {
                    cat("plot exists, skipping\n")
                    cnt <- cnt+1
                    next
                }
            }
            
            formals(plot.timepoints) <- c(list(folder=run, device=device,
                                               save.path=save.path,
                                               save.movie=FALSE,
                                               row.col="all",
                                               show.resource=FALSE,
                                               plot.type="cell"),
                                               list(...))
            res <- rbind(res, plot.timepoints())
            cnt <- cnt+1
        }
    }
    res
}

plot.timepoints <- function(folder, data.ext="tab", device="x11",
                            save.path, row.col="all", plot.type="cell",
                            save.movie=FALSE, write.data=FALSE, 
                            show.resource=FALSE, rsample, xlim,
                            ylim=c(1e-2,1e6))
{
    graphics.off()
    if (is.na(folder)) stop("Folder is NA!")

    save.path <- ifelse(device != "x11" || save.movie,
                        save.to(data.folder=folder, save.path), "")
    movie.folder <- file.path(save.path,"movie")
    if (save.movie) {
        if (!file.exists(movie.folder)) dir.create(movie.folder)
    }

    run.id <- basename(folder)
    info <- parse.infofile(file.path(folder, INFO_FILE))
    files <- get.files(folder, "tab")
    gc()

    timepoints <- files$timepoints/info$ts.scale
    world.size <- info$rows * info$cols


    passed.xlim <- eval(match.call()$xlim)
    hrs <- if (is.null(passed.xlim)) {
        passed.xlim <- range(timepoints)
        timepoints
    } else { 
        timepoints[timepoints>=passed.xlim[1] & timepoints<=passed.xlim[2]]
    }

    plot.files <- files$files[which(timepoints %in% hrs)]
    n.plot.files <- length(plot.files)

    passed.ylim <- eval(match.call()$ylim)
    y.range <- ylim
    if (is.null(passed.ylim)) {
        if (identical(row.col, "total")) {
            y.range <- c(1,1e9)
        }
    } else {
        y.range <- c(passed.ylim[1], passed.ylim[2])
        if (y.range[2] == 0) {
            cat("y min = 0, setting to 1...\n")
            y.range[2] <- 1
        }
    }

    log.hrs <- hrs+1

    setup.plot <- function(y.lab, plot.name, log.x) {
        short <- max(hrs)<5000
        pret <- pretty(hrs)
        pret.lab <- if (short) pret else pret/1000

        x.lab <- if (short) "Time (hrs)" else "Time (10^3 x hrs)"
        log.x.lab <- "Time (hrs)"
        title(main=plot.name,cex.main=0.3)
        mtext(side=2, text=y.lab, line=ylab.dist, cex=mtext.cex)

        if (log.x) {
            mtext(side=1, text=log.x.lab, line=xlab.dist, cex=mtext.cex)
            log.axis(1,lwd=par()$lwd, cex.axis=axis.cex)
        } else {
            mtext(side=1, text=x.lab, line=xlab.dist, cex=mtext.cex)
            axis(1,lwd=par()$lwd, at=pret, labels=pret.lab, cex.axis=axis.cex)
        }

        if (y.lab==total.cell.lab) {
            log.axis(2,lwd=par()$lwd, las=2, cex.axis=axis.cex)
        } else {
            axis(2,lwd=par()$lwd, las=2, cex.axis=axis.cex)
        }
    }

    total.cell.lab <- if (identical(row.col,"total")) {
        "Total individuals"
    } else if (show.resource) {
        "Individuals and resource (units)"
    } else {
        "Individuals"
    }

    # cell plot
    title.name <- run.id
    plot.name <- paste0(title.name, "_", row.col, "_",
                        passed.xlim[1], "-", passed.xlim[2])
    if (!identical(row.col, "none")) {
        default.plot(w=square.plot.dim, h=square.plot.dim, device,
                     file.path(save.path,plot.name))
        if (plot.type=="cell") {
            plot(range(hrs), range(y.range), type="n", log="y",
                 axes=FALSE, ann=FALSE)
            setup.plot(total.cell.lab, title.name, FALSE)
        }
        box(lwd=par()$lwd)
    }

    color <- list(anc_coop = "red", anc_cheat = "blue", evo_coop = "magenta",
                  evo_cheat = "cyan", coops="red", cheats="blue")

    row.sample <- NULL
    linew <- ifelse(identical(row.col, "all"), 1, 3)
    concat <- data.frame()
    res <- data.frame()
    rows <- 0
    extinct <- 0
    idx <- 1
    for (i in 2:(n.plot.files)) {
        f.current <- plot.files[i]
        f.prev    <- plot.files[i-1]
        timepoint.ss <- hrs[(i-1):i]
        current <- NULL
        prev <- NULL

        current.all <- metapop.process(f.current, info$size)
        prev.all    <- metapop.process(f.prev, info$size)
        if (is.null(current.all) || is.null(prev.all)) { next }

        data.cols <- c(prev.all$coop.cols, prev.all$cheat.cols)

        if (i==2) {
            if (!is.null(eval(match.call()$rsample))) {
                row.sample <- sample.int(n=nrow(prev.all$dat), size=rsample)
            }
            if (write.data) {
                concat <- data.frame(matrix(0,nrow=400*n.plot.files,
                                            ncol=ncol(prev.all$dat)))
                names(concat) <- names(prev.all$dat)
            }
        }

        if (write.data) {
            last <- idx + nrow(prev.all$dat) - 1
            concat[idx:last,] <- prev.all$dat
            idx <- last+1

            if (i==n.plot.files) {
                last <- idx + nrow(current.all$dat) - 1
                concat[idx:last,] <- current.all$dat
                idx <- last+1
            }
        }

        first <- TRUE
        usr.lim <- 10^par()$usr[3]
        do.plot <- function(pair) {
            extinct.loc <- 0
            resource <- pair$resource
            if (plot.type=="cell") {
                if (show.resource && resource[1] > y.range[1]) {
                    if (resource[2] < y.range[1]) {
                        lines(timepoint.ss, c(resource[1], usr.lim),
                              lwd=linew)
                        points(timepoint.ss[2], usr.lim, pch=4, lwd=linew)
                    } else if (length(timepoint.ss) != length(resource)) {
                        points(timepoint.ss[2], resource, pch=6, lwd=linew)
                    } else {
                        lines(timepoint.ss, resource,  lwd=linew)
                    }
                }
                for (type in data.cols) {
                    type.name <- names(pair)[type]

                    if (length(timepoint.ss) != length(pair[[type]]) ||
                        any(pair[[type]][1]==0))
                    {
                        points(timepoint.ss[2], pair[[type]][2], pch=6,
                               lwd=linew, col=color[[type.name]])
                    } else {
                        lines(timepoint.ss, pair[[type]],
                              col=color[[type.name]], lwd=linew)

                        # a type in a location just went extinct
                        if (any(pair[type][1,] != 0 & pair[type][2,] == 0))
                        {
                            lines(timepoint.ss, c(pair[[type]][1], 1),
                                  col=color[[type.name]], lwd=linew)
                            points(timepoint.ss[2], 1, pch=4, lwd=linew,
                                   col=color[[type.name]])
                            extinct.loc <- extinct.loc + 1
                        }
                    }
                }
            } else {
                if (length(timepoint.ss) != nrow(pair)) next
                lines(timepoint.ss, resource,  lwd=linew)
            }
            extinct.loc
        }

        prev <- NULL
        current <- NULL
        if (identical(row.col, "total")) {
            data.cols <- c(prev.all$coop.sum.col, prev.all$cheat.sum.col)
            prev <- prev.all$summary
            current <- current.all$summary
            paired <- rbind(prev, current)
            extinct <- extinct + do.plot(paired)
        } else {
            if (identical(row.col, "all")) {
                #if (show.resource) cat("not showing resource: row.col='all'")
                #show.resource <- FALSE
                prev <- prev.all$dat
                current <- current.all$dat
                if (!is.null(row.sample)) {
                    prev <- prev[row.sample,]
                    current <- current[row.sample,]
                }
                paired <- rbind(prev, current)
            } else {
                prev <- prev.all$dat[prev.all$dat$row.col %in% row.col,]
                current <-
                    current.all$dat[current.all$dat$row.col %in% row.col,]
                paired <- rbind(prev, current)
                if (nrow(paired)==0) next
            }
            for (pair in split(paired, paired$row.col)) {
                extinct <- extinct + do.plot(pair)
            }
        }

        if (save.movie) {
            savePlot(file.path(movie.folder, paste0(i-1, ".png")))
        }
    }
    if (device!="x11") graphics.off()
    concat <- concat[1:(idx-1),]
    gc()
    if (write.data) {
        folder.name <- basename(dirname(folder))
        write.table(concat, file=paste(folder.name,".tab",sep=""), sep="\t",
                    row.names=FALSE,quote=FALSE)
    }
    cat(extinct, "extinctions.\n\n")
    data.frame(run.id=run.id, release.rate=info$release.rate*info$ts.scale,
               extinct=extinct, total=info$size)
}

release.test <- function(folder, max.release=1e6, A=7, max.r=log(2), lag=100,
                         ts.scale=200) 
{
    res <- data.frame()
    for (dat.file in list.files(folder, full.names=TRUE, include.dirs=FALSE))
    {
        release.rate <- as.numeric(sub(".*release=(.*?)_.*",
                                       "\\1", dat.file, perl=TRUE))
        cat("release rate =", release.rate,"\n")
        if (release.rate > max.release) next
        #if (release.rate!=1.9) next

        size <- as.numeric(sub(".*size=(.*)_occ.*", "\\1", dat.file,
                               perl=TRUE))
        size <- size^2

        dat <- read.delim(dat.file)
        dat <- within(dat, hrs <- timestep/ts.scale)

        extinct <- nrow(dat[dat$timestep==max(dat$timestep) & dat$coops==0,])
        analysis.range <- dat[dat$coops>1e3 & dat$coops<5e4,]

        growth.rate <- NA
        lower <- NA
        upper <- NA
        res.mean <- NA
        res.per.cell <- NA
        if (nrow(analysis.range)>0) {
            # find the 5 locations that started growing earliest.
            strains <- droplevels(head(unique(analysis.range$row.col), 5))
            r.pick <- NULL
            for (strain in strains) {
                ss <- analysis.range[analysis.range$row.col==strain,]
                res.mean <- mean(ss$resource)
                res.per.cell <- mean(ss$resource/ss$coops)
                if (is.null(r.pick)) r.pick <- 1000:1
                for (r.try in r.pick) {
                    r <- max.r/r.try
                    fit <- try(nls(coops ~ A*exp(r*hrs), data=ss,
                                   start=list(A=A,r=r)),
                               silent=TRUE)
                    if (class(fit) != 'try-error') {
                        if (is.null(r.pick)) {
                            cat("picked r.try =", r.try, "\n")
                        }
                        growth.rate <- coef(fit)['r']

                        suppressMessages(ci <- confint(fit)['r',])
                        lower <- ci[1]
                        upper <- ci[2]
                        r.pick <- r.try
                        break
                    }
                }
                if (class(fit) == 'try-error') {
                    cat("couldn't find a good growth rate, skipping...\n")
                    plot(coops~timestep, data=analysis.range, 
                         subset=row.col==strain, log="y")
                    next 
                }
                res <- rbind(res, data.frame(release.rate=release.rate,
                                             extinct=extinct,
                                             total=size,
                                             res.mean = res.mean,
                                             res.per.cell=res.per.cell,
                                             growth.rate=growth.rate,
                                             lower=lower,
                                             upper=upper))
            }
        } else {
            res <- rbind(res, data.frame(release.rate=release.rate,
                                         extinct=extinct,
                                         total=size,
                                         res.mean = res.mean,
                                         res.per.cell=res.per.cell,
                                         growth.rate=growth.rate,
                                         lower=lower,
                                         upper=upper))
        }

    }
    res <- res[order(res$release.rate),]
    res <- within(res, survival.prob <- 1-(extinct/total))
    res
}

plot.growth.vs.release <- function(dat, max.release, device="x11",
                                   name="growth_rate_vs_release", ...)
{
    graphics.off()
    agg.dat <- data.frame(aggregate(growth.rate~release.rate, dat=dat, mean),
                  sd=aggregate(growth.rate~release.rate, dat=dat, sd)[[2]],
                  n=aggregate(growth.rate~release.rate, dat=dat, length)[[2]])
    agg.dat <- within(agg.dat,
                      {
                          sem <- sd/sqrt(n)
                          upper <- growth.rate+2*sem
                          lower <- growth.rate-2*sem
                      })
    default.plot(square.plot.dim,square.plot.dim,device=device,name=name)
    plot(growth.rate~release.rate, data=agg.dat, ann=FALSE, axes=FALSE,
         cex=pch.cex, ...)
    #arrows(agg.dat$release.rate, agg.dat$lower, agg.dat$release.rate,
    #       agg.dat$upper, code=3, angle=90, length=0.1,...)
    axis(1,lwd=par()$lwd, cex.axis=axis.cex)
    axis(2,lwd=par()$lwd, las=2, cex.axis=axis.cex)
    mtext(side=1, text="Release rate (units/hr)", line=3.0, cex=mtext.cex)
    mtext(side=2, text="Growth rate of surviving populations (/hr)", 
          line=5.0, cex=mtext.cex)

    fit <- lm(growth.rate~release.rate, data=dat,
              subset=release.rate <= max.release)
    abline(fit, col=fit.col, lty=2, lwd=5)
    box()
    if (device != "x11") dev.off()
    print(summary(fit))
    print(confint(fit))
}

plot.survival.freqs <- function(dat, device="x11", name) {
    graphics.off()
    min.val <- 0
    max.val <- 100
    spacing <- 0
    survival.cutoff <- 0.1

    h <- hist(dat$coop.freq.mean, plot=FALSE)
    str(h)
    freqs <- 100 * h$counts / sum(h$counts)
    error.counts <- sqrt(h$counts)
    error.freqs <- (freqs/h$counts) * error.counts
    error.freqs[freqs<min.val] <- NA
    freqs.upper <- freqs + 2*error.freqs
    freqs.lower <- freqs - 2*error.freqs
    freqs.lower[freqs.lower<=0] <- min.val

    freqs[freqs<min.val] <- min.val
    default.plot(h=10,w=20,device=device,name=name)
    bp <- barplot(freqs, log="", names.arg=h$mids, axes=FALSE, ann=FALSE,
                  axisnames=FALSE, space=spacing, ylim=c(min.val,max.val))
    arrows(bp, freqs.lower, bp, freqs.upper, code=3, length=0.1, angle=90)
    axis(1, lwd=par()$lwd, cex.axis=1.5, at=bp, labels=h$mids)
    axis(2, lwd=par()$lwd, las=2, cex.axis=1.5)
    box()
    mtext(side=1, text="Cooperator frequency (midpoint of bin)",
          line=3.0, cex=3.5)
    mtext(side=2, text="Occurrence (%)", line=4, cex=3.5)
    abline(v=survival.cutoff*10+(spacing*1.5), lty=2, col="red", lwd=5)
    title(name)

    if (device != "x11") dev.off()
}

plot.survival.prob <- function(dat,device="x11", p, k, x,
                               name="survival_prob_vs_release", ...)
{
    agg.dat <- aggregate(survival.prob~release.rate, dat=dat, mean)
    graphics.off()
    default.plot(w=square.plot.dim,h=square.plot.dim,device=device,name=name)
    plot(survival.prob~release.rate, data=agg.dat, ann=FALSE, axes=FALSE,
         cex=pch.cex, subset=survival.prob>0, type="n", log="x",  ...)
    #axis(1,lwd=par()$lwd)
    log.axis(1,lwd=par()$lwd, cex.axis=axis.cex)
    axis(2,lwd=par()$lwd, las=2, cex.axis=axis.cex)
    mtext(side=1, text="Release rate (units/hr)", line=3.0, cex=mtext.cex)
    mtext(side=2, text="Probability of survival", line=4.5, cex=mtext.cex)
    points(survival.prob~release.rate, data=agg.dat, cex=pch.cex, ...)
    box()

    fit <- nls(survival.prob~(p*(release.rate-x)) / (k+(release.rate-x)),
               data=dat, start=list(p=p,k=k,x=x),subset=survival.prob>0)

    p.max <- coef(fit)['p']
    k.survive <- coef(fit)['k']
    x.shift <- coef(fit)['x']
    if (x.shift <= 0) x.start <- .01 else x.start <- x.shift
    x.end <- match.call()$xlim
    if (!is.null(x.end)) {
        x.end <- as.list(x.end)[[3]]
    } else {
        x.end <- max(agg.dat$release.rate)
    }
    
    curve((p.max*(x-x.shift))/(k.survive+(x-x.shift)), add=TRUE, from=x.start, to=x.end, col=fit.col, lty=2, lwd=5)

    print(fit)
    print(confint(fit))
    if (device != "x11") dev.off()
}

survive2release <- function(p.max, k, shift, p) {
    r <- unname((p*(k-shift)+p.max*shift)/(p.max-p))
    list(release.rate=r, growth.rate=release2growthrate(r))
}
release2survive <- function(p.max, k, shift, r) {
    p <- unname(((p.max*(r-shift))/(k+(r-shift))))
    list(p.survive=p, growth.rate=release2growthrate(r))
}

release2growthrate <- function(r, slope=0.181, intercept=0) {
    slope * r - intercept
}

summarize.runs <- function(folder, current.df=NULL, data.ext="tab", last=5,
                           max.runs=30, pattern=NULL, min.hr=2e4, ...)
{
    graphics.off()

    name.match <- paste("^.*/(.*)\\.",data.ext,sep="")

    conditions <- list.files(folder, full.names=TRUE, pattern=pattern)
    conditions <- subset(conditions, grepl("(global|local)", conditions) &
                         file.info(conditions)$isdir)
    n.conditions <- length(conditions)
    if (length(conditions)==0) stop("no folders match.")
    var.list <- make.var.list(conditions)

    res.names <- c(names(var.list), "condition", "run.id", "coop.freq.mean",
                   "coop.freq.sd", "timepoints.used", "last", "complete")

    res <- data.frame(matrix(0, nrow=n.conditions*max.runs,
                             ncol=length(res.names)))
    names(res) <- res.names

    cond.idx <- 1
    row.idx <- 1
    n.cols <- 0
    col.names <- ""
    for (condition in conditions) {
        #if (cond.idx!=158) { cond.idx <- cond.idx+1; next }
        runs <- list.files(condition, full.names=TRUE)
        runs <- subset(runs, file.info(runs)$isdir)
        max.hrs <- var.list[["hrs"]][cond.idx]
        n.init <- var.list[["n"]][cond.idx]
        if (var.list[["u"]][cond.idx] > 0) {
            cond.idx <- cond.idx + 1
            next
        }

        cat("\nProcessing", condition, "\n")
        cat(cond.idx, "of", n.conditions, "conditions\n")
        cat(length(runs), "runs in folder\n")

        for (j in seq_along(runs)) {
            run.id <- basename(runs[j])
            if (run.id %in% current.df$run.id) {
                if (current.df[current.df$run.id==run.id,]$complete) {
                    cat(run.id, "exists, skipping.\n")
                    next
                } else {
                    cat("updating", run.id, ".\n")
                }
            } else {
                cat("adding", run.id, "\n")
            }

            res[row.idx, names(var.list)] <- lapply(var.list,
                                                    function(x) x[cond.idx])

            timepoints  <- list.files(runs[j], full.names=TRUE)

            info.file <- subset(timepoints, grepl(INFO_FILE, timepoints))
            if (length(info.file)==0) {
                cat("\n\tno info file found in run", run.id, "\n")
                next
            }
            info <- parse.infofile(info.file)

            timepoints <- subset(timepoints, !grepl(INFO_FILE, timepoints))
            n.timepoints <- length(timepoints)
            if (n.timepoints == 1) {
                cat("\n\tonly one file found in run", run.id, "\n")
                next
            }

            all.timepoints <- as.numeric(sub(name.match,"\\1", timepoints,
                                             perl=TRUE))
            all.hrs <- sort(all.timepoints/info$ts.scale)
            final.hr <- all.hrs[length(all.hrs)]
            timepoints.ordered <- timepoints[order(all.timepoints)]

            if (j==1) {
                a.run <-metapop.process(timepoints[1], info$size)$summary

                n.cols <- ncol(a.run)
                col.names <- names(a.run)
            }

            used <- 0
            coop.freqs <- vector("numeric", last)
            lasts <- tail(timepoints.ordered, last)
            for (i in rev(seq_along(lasts))) {
                tryCatch(coop.freqs[i] <- 
                    metapop.process(lasts[i], info$size)$summary$coop.freq,
                    error=function(e) { cat("skipping\n") })

                used <- used + 1

                # NaN indicates both coops and cheats are extinct.
                if (coop.freqs[i] == 0 || is.nan(coop.freqs[i])) {
                    coop.freqs <- 0
                    break
                } else if (coop.freqs[i] == 1) {
                    coop.freqs <- 1
                    break
                }

            }

            complete <- FALSE
            if (length(coop.freqs) == 1 || final.hr >= min.hr) {
                cat(run.id, "appears complete\n")
                complete <- TRUE
            } else {
                cat(run.id, "appears incomplete\n")
            }

            mean.freq <- mean(coop.freqs)
            sd.freq   <- ifelse(length(coop.freqs)==1, 0, sd(coop.freqs))

            res[row.idx,]$condition       <- cond.idx
            res[row.idx,]$run.id          <- run.id
            res[row.idx,]$coop.freq.mean  <- mean.freq
            res[row.idx,]$coop.freq.sd    <- sd.freq
            res[row.idx,]$timepoints.used <- used
            res[row.idx,]$last            <- last
            res[row.idx,]$hrs             <- final.hr
            res[row.idx,]$complete        <- complete
            row.idx <- row.idx+1
        }
        n.cols <- 0
        col.names <- ""
        cond.idx <- cond.idx + 1
    }
    cat("\n")
    res <- res[1:(row.idx-1),]
    current.df <- rbind(current.df, res)
    current.df
}

plot.summary <- function(dat, device="x11", save.to, jit=NULL, gap=2, 
                         leg.x=1, leg.y=0.8, min.hr=2e4, ...)
{
    graphics.off()

    initial.pop.size <- 1e5
    y.pad <- 0.1

    dat <- dat[order(dat$mig),]
    x.label <- c(0, log10(unique(dat[dat$mig>0,]$mig)))

    last.split <- NULL
    leg.title <- ""
    if(length(unique(dat$`mutant-freq`))>1) {
        leg.title <- "   initial mutants per population   "
        last.split <- "mutant-freq"
    } else {
        leg.title <- "   initial size   "
        last.split <- "n"
    }

    n.migs <- length(unique(dat$mig))
    n.conds <- length(unique(dat[[last.split]]))+gap
    spacing <- seq(1, n.conds*n.migs, by=n.conds)

    x.at <- seq(1, n.migs*n.conds)
    labels.at <- ((n.conds/2) + (n.conds*0:(n.migs-1)))

    #x.at <- 1:n.migs
    x.range <- range(x.at)
    y.range <- c(0-y.pad, 1+y.pad)
    for (mig.range in split(dat, dat$range)) {
        for (mig.range.occ in split(mig.range, mig.range$occ)) {
            plot.name <- paste(unique(mig.range.occ$range), ", ",
                               "occ=", unique(mig.range.occ$occ), sep="")
            plot.path <- file.path(save.to, plot.name)
            if (!file.exists(save.to)) dir.create(save.to, recursive=TRUE)
            default.plot(w=long.plot.dim,h=square.plot.dim, dev=device, name=plot.path)
            plot(x.range, y.range, type="n", ann=FALSE, axes=FALSE, log="",
                 xaxs='i')
            title(main=plot.name,cex.main=1)

            color.start <- ifelse(last.split=="n", 2, 1)
            color <- color.start
            by.last <- split(mig.range.occ, mig.range.occ[[last.split]])
            cond <- unique(names(by.last))
            if (last.split=="mutant-freq") { 
                cond <- as.numeric(cond)*initial.pop.size
            }
            x.idx <-1.1 
            for (i in seq_along(by.last)) {
                mut <- by.last[[i]]
                entries <- lapply(split(mut, mut$mig), nrow)
                x.spot <- c()
                miss <- NULL
                for (j in seq_along(x.label)) {
                    v <- if (x.label[j]==0 || x.label[j]==1) {
                        x.label[j]
                    } else {
                        10^x.label[j]
                    }
                    ent <- which(as.character(v)==names(entries))
                    if (length(ent)>0) {
                        x.spot <- c(x.spot, rep(j,entries[[ent]]))
                    } else {
                        x.spot <- c(x.spot, rep(j,1))
                        miss <- c(miss, length(x.spot))
                    }
                }
                x.jit <- jitter(spacing[x.spot]+x.idx, amount=jit)
                y.jit <- jitter(mut$coop.freq.mean, amount=jit/10)
                mut <- within(mut, {lower <- ifelse(coop.freq.sd>1e-3,
                                                    y.jit-coop.freq.sd, NA)
                                    upper <- ifelse(coop.freq.sd>1e-3,
                                                    y.jit+coop.freq.sd, NA)})
                for (m in miss) {
                    y.jit <- c(y.jit[1:(m-1)],NA,
                               y.jit[m:length(y.jit)])
                }
                if (length(x.jit)!=length(y.jit)) browser()
                points(x.jit, y.jit, col=color, pch=color, cex=1.2)
                x.jit <- if (is.null(miss)) x.jit else x.jit[-miss]
                arrows(x.jit, ifelse(mut$lower<0, 0, mut$lower),
                       x.jit, ifelse(mut$upper>1, 1, mut$upper),
                       code=3, angle=90, length=0.1, col=color)
                color <- color+1
                x.idx <- x.idx + 1
            }
            axis(1, at=labels.at, labels=x.label, lwd.ticks=par()$lwd)
                 
            axis(2, las=2, lwd.ticks=par()$lwd)
            box()
            mtext(side=1, text="log[Migration rate (/hr)]", line=3.0,
                  cex=mtext.cex)
            mtext(side=2, text="Coop. frequency", line=4.2, cex=mtext.cex)
            legend(x=leg.x,
                   y=leg.y,
                   legend=cond,
                   title=leg.title,
                   col=color.start:(color-1), pch=color.start:(color-1),
                   pt.cex=1.5) 
            if (device=="x11") {
                browser()
            } else {
                dev.off()
            }
        }
    }
    
    #print(surf)
}

rerun <- function(set, run.id, program, hours, every) {
    java <- paste('time java -Xmx1000m -server',
                  '-cp .:$CLASSPATH:/home/ajwaite/Documents/Code/Java/metapop2/lib/commons-math.jar:/home/ajwaite/Documents/Code/Java/metapop2/build/classes/framework',
                 'org.fhcrc.honeycomb.metapop.experiment.')

    java <- paste0(java, program)
    print(java)
    run <- generate.run(set[set$run.id==run.id,])
    #system(paste('echo', java), wait=FALSE)
}

plot.survival <- function(dat, split2="occ", device="x11", 
                          save.to=".", cutoff=0.1, min.hr=2e4, ...)
{
    graphics.off()
    library(binom)

    y.pad <- 0.02

    dat <- dat[order(dat$mig),]
    x.label <- c(0, 1, log10(unique(dat[dat$mig>0,]$mig)))
    n.migs <- length(unique(dat$mig))
    x.range <- range(1:n.migs)
    y.range <- c(0-y.pad, 1+y.pad)

    last.split.name <- NULL
    leg.title <- ""
    if(length(unique(dat$`mutant-freq`))>1) {
        leg.title <- "   initial mutants   "
        last.split.name <- "mutant-freq"
    } else {
        leg.title <- "   initial size   "
        last.split.name <- "n"
    }

    draw.arrows <- function(est, color) {
        for (r in 1:nrow(est)) {
            x.at <- est[r,]$x.at
            y <- est[r,]$mean
            lower <- est[r,]$lower
            upper <- est[r,]$upper
            if (y==0 || is.na(y)) {
                arrows(x.at, y, x.at, upper,
                       code=2, angle=90, length=0.1, col=color)
            } else if (y==1) {
                arrows(x.at, y, x.at, lower,
                       code=2, angle=90, length=0.1, col=color)
            } else {
                arrows(x.at, lower, x.at, upper,
                       code=3, angle=90, length=0.1, col=color)
            }
        }
    }

    n.conds <- length(unique(dat[[last.split.name]]))
    all.res <- data.frame()
    for (mig.range in split(dat, dat$range)) {
        rang <- unique(mig.range$range)
        for (s2 in split(mig.range, mig.range[[split2]])) {
            occ <- sprintf("%.2f", unique(s2$occ))
            split2.val <- sprintf("%.2f", unique(s2[[split2]]))
            plot.name <- paste0(rang, ", ", split2, "=", split2.val,
                                "_survive_freq")
            plot.path <- file.path(save.to, plot.name)
            if (!file.exists(save.to)) dir.create(save.to, recursive=TRUE)
            default.plot(w=long.plot.dim, h=square.plot.dim,
                         dev=device, name=plot.path)
            plot(x.range, y.range, type="n", ann=FALSE, axes=FALSE, log="",
                 ...)
            title(main=plot.name,cex.main=1)

            color.start <- ifelse(last.split.name=="n", 2, 1)
            color <- color.start
            by.last <- split(s2, s2[[last.split.name]])
            cond <- unique(names(by.last))
            if (last.split.name=="mutant-freq") { 
                cond <- as.numeric(cond)*unique(s2$n)
            }
            for (i in seq_along(by.last)) {
                m <- 1
                est <- c()
                last <- by.last[[i]]
                for (mig.rate in x.label) {
                    if (mig.rate=="") browser()
                    if (mig.rate != 0 && mig.rate != 1) {
                        mig.rate <- 10^mig.rate
                    }
                    if (mig.rate %in% unique(last$mig)) {
                        last.ss <- last[last$mig==mig.rate,]
                        last.s <- unique(last.ss[last.split.name])
                        survived <- length(last.ss$coop.freq.mean[
                                           last.ss$coop.freq.mean>cutoff])
                        tot <- length(last.ss$coop.freq.mean)
                        bnm <- binom.wilson(x=survived, n=tot)
                        code <- if (bnm$mean==0) 1
                            else if (bnm$mean==1) 2
                            else 3

                        est <- rbind(est, data.frame(mig.range=rang, 
                                                     last.split=last.s,
                                                     occ=occ,
                                                     bnm, mig=mig.rate,
                                                     x.at=m, code=code))
                    } else if (mig.rate==1) {
                        est <- rbind(est, rep(NA, ncol(est)))
                        est[nrow(est),"x.at"] <- m-0.5
                        m <- m-1
                    } else {
                        est <- rbind(est, data.frame(mig.range=rang, 
                                                     last.split=last.s,
                                                     occ=occ, method=NA,
                                                     x=NA,n=NA,mean=NA,
                                                     lower=NA,upper=NA,
                                                     mig=mig.rate,
                                                     x.at=m, code=NA))
                    }
                    m <- m+1
                }
                all.res <- rbind(all.res, est)
                lines(est$x.at, est$mean, col=color, pch=color,
                      type="o", cex=2)
                draw.arrows(est, color)
                color <- color+1
            }
            if (length(est$x.at) != length(x.label)) browser()
            x.label[2] <- NA
            axis(1, at=est$x.at, labels=x.label, lwd.ticks=par()$lwd)
            x.label[2] <- 1
            axis(2, las=2, lwd.ticks=par()$lwd)
            box()
            mtext(side=1, text="log[Migration rate (/hr)]", line=3.0,
                  cex=mtext.cex)
            mtext(side=2, text="Coop. survival frequency", line=4.2,
                  cex=mtext.cex)
            legend(x=1, y=0.8,
                   legend=cond,
                   title=leg.title,
                   col=color.start:(color-1), pch=color.start:(color-1),
                   pt.cex=1.5) 
            if (device=="x11") {
                browser()
            } else {
                dev.off()
            }
        }
    }
    na.exclude(all.res)
}

make.var.list <- function(filenames) {
    vars <- c("range", "type", "n", "mutant-freq", "coop-release", "gamma",
              "coop-freq", "km", "cheat-adv", "evo-adv", "evo-trade",
              "resource", "size", "occ", "mig", "u", "hrs")
    var.list <- vector("list", length(vars))
    names(var.list) <- vars

    for (v in vars) {
        match.string <- if (v=="range") {
            "^(local|global).*"
        } else if (v=="type") {
            ".*(indv|prop).*"
        } else if (v==tail(vars,1)) {
            paste0(".*", v, "=(.*?)$")
        } else {
            paste0(".*", v, "=(.*?)_.*")
        }

        var.list[[v]] <- sub(match.string, "\\1", basename(filenames))
        if (v!="type" && v!="range") {
            var.list[[v]] <- as.numeric(var.list[[v]])
        }
    }
    var.list
}

parse.infofile <- function(info) {
    if (length(info)==0) cat("\ninfo file name is empty.\n")

    tag <- ".*<.*?>(.*)<.*?>"
    info.lines <- readLines(info)
    ts.scale <- as.numeric(sub(tag, "\\1", info.lines[2], perl=TRUE))
    rows <- as.numeric(sub(tag, "\\1", info.lines[3], perl=TRUE))
    cols <- as.numeric(sub(tag, "\\1", info.lines[4], perl=TRUE))
    size <- rows*cols
    release.rate <- as.numeric(sub(".*release_rate=(.*)\\. .*", "\\1",
                                   info.lines[8], perl=TRUE))

    return(list(ts.scale=ts.scale, rows=rows, cols=cols, size=size,
                release.rate=release.rate))
}

get.files <- function(folder, data.ext) {

    name.match <- paste("^.*/(.*)\\.",data.ext,sep="")

    all.f <- list.files(folder, pattern=data.ext, full.names=TRUE)
    if (length(all.f) == 0) 
        stop("No files have extension '", data.ext, "'")

    cat("Processing folder", folder, "\n")
    all.timepoints <- as.numeric(sub(name.match,"\\1", all.f, perl=TRUE))
    ord <- order(all.timepoints)
    all.f <- all.f[ord]
    all.timepoints <- all.timepoints[ord]
    gc()

    return(list(files=all.f, timepoints=all.timepoints))
}

save.to <- function(data.folder, save.path) {
    if (missing(save.path)) {
        save.path <- file.path("fig", sub("^/?(\\.\\/)?", "", data.folder))
        if (!file.exists(save.path)) dir.create(save.path, recursive=TRUE)
    }
    return(save.path)
}


metapop.makeheatmaps <- function(folder, first=0, last, data.ext="tab",
                                 device="jpeg", pop.size, ...) {
    graphics.off()
    save.path <- save.to(data.folder=folder, ...)
    save.path <- file.path(save.path, "movie")
    file.copy(file.path(folder, INFO_FILE), save.path)
    files <- get.files(folder, data.ext)$files
    total <- length(files)
    if (missing(last)) last <- total
    for (i in (first+1):last) {
        cat("\rtimepoint ", i, "/", total, sep="")
        metapop.heatmap(dat=metapop.process(files[i]), max.pop=1e7,
                        save.path=save.path, pop.size=pop.size, device=device)
    }
    cat("\n")
}

metapop.heatmap <- function(dat, max.pop, save.path, pop.size=FALSE, 
                            device="jpeg", ...)
{
    if (missing(save.path)) save.path <- "."
    if (missing(pop.size)) pop.size <- FALSE
    #info <- parse.infofile(file.path(dat, INFO_FILE))
    coop.cols <- dat$coop.cols
    cheat.cols <- dat$cheat.cols
    dat <- dat$dat
    rows <- max(dat$row)
    cols <- max(dat$col)
    ext <- if (device != "x11") paste(".",device,sep="")
    to.screen <- if (device=="x11") TRUE else FALSE
    #device <- get(device)

    shift <- 0.5
    small.shift <- shift/10

    if (missing(max.pop)) max.pop <- max(dat[c(coop.cols, cheat.cols)])
    max.shades <- round(log10(max.pop),1)*10
    max.alphas <- max.shades*10

    all.colors <- colorRampPalette(c("red","purple","blue"))(max.shades)

    background.grid <- function(r,c,s,color) {
        for (i in 1:r) {
            for (j in 1:c) {
                rect(i-shift,j-shift,i+shift,j+shift,col=color, border="gray")
                mtext(side=1, text=j, at=j, line=0, cex=1.5)
                mtext(side=2, text=i, at=i, line=0, las=2, cex=1.5)
            }
        }
    }

    pick.color <- function(cheat.per, dens) {
        color <- rgb2hsv(col2rgb(all.colors[cheat.per]))
        return(hsv(h=color["h",], v=color["v",], s=dens))
    }

    scale.to <- function(mi, ma, val) {
        ((ma-mi)*(val)) + mi
    }

    scale.pop <- function(pop) {
        if (pop==0)
            return(0)
        else if (pop==1)
            return(1)
        else if (pop>1)
            return(ceiling(log10(pop))*10)
    }

    format.print <- function(pop.size) {
        if (pop.size < 1000) {
            return(sprintf("%d", pop.size))
        } else {
            return(sprintf("%d", pop.size))
        }
    }

    if (to.screen) {
        get(device)
    } else {
        #cat(info, file=file.path(save.path,"info.txt"))
    }

    timestep  <- unique(dat$timestep)
    coop.mat  <- matrix(0,nrow=rows,ncol=cols)
    cheat.mat <- matrix(0,nrow=rows,ncol=cols)

    if (!to.screen) {
        default.plot(w=square.plot.dim,h=square.plot.dim,device=device, 
                     file.path(save.path, formatC(timestep, format='d')))
    }
    par(oma=c(1,1,1,1))
    par(lwd=2, font=1, family="sans")
    nf <- layout(matrix(c(1,2),2,1,byrow=TRUE),
                 widths=c(par()$cin[2],par()$cin[2]),
                 heights=c(par()$cin[1]*0.1,par()$cin[1]*0.9))

    opar <- par(mar=c(1,1,1,1))

    # plots color gradient at top
    plot(range(0,max.shades),c(0,1),type="n", ann=FALSE, axes=FALSE)
    for (i in 1:max.shades) {
        rect(i-shift, 0, i+shift, 1, col=all.colors[i],border=NA)
    }

    par(mar=c(1,1,1,1))
    plot(c(1,rows),c(1,cols),type="n",axes=FALSE,ann=FALSE,
         ylim=c(shift,rows+shift), xlim=c(shift,cols+shift))
    title(paste("Timestep: ", timestep, sep=""))
    left <- 1; right <- cols
    middle <- mean(c(left,right))
    grad.key.color <- "black"
    grad.key.color2 <- "black"
    grad.key.size  <- 1.0 
    grad.key.line1 <- 5.7
    grad.key.line2 <- 4.7
    mtext(side=3,at=left,  line=grad.key.line2, text="0",
          font=2, col=grad.key.color, cex=grad.key.size)
    mtext(side=3,at=middle,line=grad.key.line2, text="50",
          font=2, col=grad.key.color, cex=grad.key.size)
    mtext(side=3,at=right,line=grad.key.line2, text="100",
          adj=0, font=2, col=grad.key.color, cex=grad.key.size)
    mtext(side=3,at=middle,line=grad.key.line1, text="Percent cheater",
          font=2, col=grad.key.color2, cex=grad.key.size)
    background.grid(rows,cols,shift,col="white")

    real.max.pop <- max(dat$coops + dat$cheats)
    if (real.max.pop > max.pop) max.pop <- real.max.pop

    min.saturation <- 0.1
    min.shade <- 1
    for (r in 1:nrow(dat)) {
        dat.row <- dat[r,]
        raw.coops <- dat.row$coops
        raw.cheats <- dat.row$cheats

        n.coops  <- scale.pop(raw.coops)
        n.cheats <- scale.pop(raw.cheats)
        total.pop <- n.coops + n.cheats
        dens      <- scale.to(mi=min.saturation, ma=1,
                              mean(c(n.coops,n.cheats))/max.shades)
        cheat.per <- scale.to(mi=min.shade, ma=max.shades,
                              (n.cheats/(total.pop)))

        this.color <- ifelse(total.pop==0, "#FFFFFF",
                             pick.color(cheat.per, dens))

        tryCatch(rect(dat.row$col-shift,dat.row$row-shift,
                      dat.row$col+shift,dat.row$row+shift,
                      col=this.color),
                 error=function(e) { cat("Error: ", e,"\n"); browser() } )

        if (pop.size) {
            text(x=dat.row$col, y=dat.row$row,
                 labels=paste(format.print(raw.coops),"\n",
                              format.print(raw.cheats), sep=""), cex=0.8)
        }

    }
    if (!to.screen) dev.off() else browser()
}

logistic.anaysis <- function(dat) {
    dat <- within(dat, coop.not.extinct <- ifelse(coop.freq.mean>0.1, 1, 0))

    dat.names <- "coop.not.extinct"
    dat.cols <- which(names(dat) %in% dat.names)

    group.names <- c("range", "type", "n", "mutant-freq", "occ", "mig")
    group.cols <- which(names(dat) %in%  group.names)

    res <- data.frame()
    agg <- data.frame(aggregate(dat[dat.cols], dat[group.cols], sum),
                      total=aggregate(dat[dat.cols], dat[group.cols],
                                      length)[[dat.names]])
    agg <- within(agg, coop.extinct <- total-coop.not.extinct)

    full.fit <- glm(cbind(coop.not.extinct, coop.extinct)~
                    range +
                    occ*as.factor(mutant.freq) +
                    log(mig)*as.factor(mutant.freq), dat=agg,
                    subset=mig>0, family="binomial")
    global.full.fit <- glm(cbind(coop.not.extinct, coop.extinct)~
                           occ*as.factor(mutant.freq) +
                           log(mig)*as.factor(mutant.freq), dat=agg,
                           subset=range=="global" & mig>0, family="binomial")
    local.full.fit <- glm(cbind(coop.not.extinct, coop.extinct)~
                           occ*as.factor(mutant.freq) +
                           log(mig)*as.factor(mutant.freq), dat=agg,
                           subset=range=="local" & mig>0, family="binomial")
    print(summary(full.fit))
    print(summary(global.full.fit))
    print(summary(local.full.fit))

    for (oc in unique(dat$occ)) {
        local.full.fit.occ <- glm(cbind(coop.not.extinct, coop.extinct)~
                              occ*as.factor(mutant.freq) +
                              log(mig)*as.factor(mutant.freq), dat=agg,
                              subset=range=="local" & mig>0 & occ==oc,
                              family="binomial")
    }
}

fish <- function(dat) {
    ss.75 <- dat[dat$`mutant-freq`==0 & 
                 dat$mig==1e-8 & dat$occ==0.75,]$coop.freq.mean
    ss.5 <- dat[dat$`mutant-freq`==0 & dat$mig==1e-8 &
                dat$occ==0.5,]$coop.freq.mean
    ss.25 <- dat[dat$`mutant-freq`==0 & dat$mig==1e-8 &
                 dat$occ==0.25,]$coop.freq.mean

    ss.75.sum <- sum(ss.75)
    ss.5.sum <- sum(ss.5)
    ss.25.sum <- sum(ss.25)
    ss.75.length <- length(ss.75)
    ss.5.length <- length(ss.5)
    ss.25.length <- length(ss.25)

    mat.vs.75 <- matrix(c(ss.75.sum,ss.5.sum,
                          ss.75.length-ss.75.sum,
                          ss.5.length-ss.5.sum), nrow=2)
    mat.vs.25 <- matrix(c(ss.25.sum,ss.5.sum,
                          ss.25.length-ss.75.sum,
                          ss.5.length-ss.5.sum), nrow=2)
    print(mat.vs.75)
    print(fisher.test(mat.vs.75))

    print(mat.vs.25)
    print(fisher.test(mat.vs.25))
}
