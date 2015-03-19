
require(tikzDevice)
require(diagram)
# load(file="/home/duke/Dropbox/serotonin/paper/results/sections/overall/overall_results.RData")
# 
# included <- paste(k.studies, "studies included in analyses")
# coded <- paste(k.studies + 27, "studies coded")
# reject <- paste(386 - k.studies - 27, "studies did not")
# 
# # Results
# l.1 <- c("1738 search results", "\\& unpublished reports")
# r.2 <- "976 duplicates"
# l.2 <- c("762 potentially relevant", "reports screened")
# r.3 <- c("376 identified", "as not relevant")
# l.3 <- c("386 underwent", "full-text review")
# r.4 <- c(reject, "meet inclusion criteria")
# l.4 <- coded
# r.5 <- c("27 reports excluded", "due to duplicate data")
# l.5 <- included

load(file="~/Dropbox/serotonin/analyses/combined/combined.RData")

included      <- nrow(df)
searchResults <- 1738
duplicates    <- 976
screened      <- searchResults - duplicates
notRelevant   <- 376
fullText      <- screened - notRelevant
duplicates    <- 27
coded         <- duplicates + included
rejected      <- fullText - coded

# RESULTS
l.1 <- c(paste(searchResults, "search results"), "\\& unpublished reports")
r.2 <- paste(duplicates, "duplicates")
l.2 <- c(paste(screened, "potentially relevant"), "reports screened")
r.3 <- c(paste(notRelevant, "identified"), "as not relevant")
l.3 <- c(paste(fullText, "underwent"), "full-text review")
r.4 <- c(paste(rejected, "did not"), "meet inclusion criteria")
l.4 <- paste(coded, "studies coded")
r.5 <- c(paste(duplicates, "reports excluded"), "due to duplicate data")
l.5 <- paste(included, "studies included in analyses")


#Labels
lab <- list(l.1, "", "", r.2, l.2, "", "", r.3, l.3, "", "", r.4, l.4,"","", r.5, l.5, "")
#SAVE DESTINATION
tikz(file='./paper/results/sections/flow/flow_source.tex', width=3.5, height=5)
op <- par(mar=c(1,1,1,1))
openplotmat(main="", cex.main=1)
bpos <- rep(2, 9)
bpos <- coordinates(bpos, mx=.05)
posadj <- .12
bpos[4,1] <- bpos[4,1] -posadj
bpos[8,1] <- bpos[8,1] -posadj
bpos[12,1] <- bpos[12,1] -posadj
bpos[16,1] <- bpos[16,1] -posadj
down.pos <- .75
over.pos <- .14
arrow <- "triangle"
len <- .20
wid <- .16
fs <- .85
straightarrow(from=bpos[1,],to=bpos[5,], lwd=2, arr.pos=down.pos, endhead=TRUE,arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[5,],to=bpos[9,], lwd=2, arr.pos=down.pos, endhead=TRUE,arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[9,],to=bpos[13,], lwd=2, arr.pos=down.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[13,],to=bpos[17,], lwd=2, arr.pos=down.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[3,],to=bpos[4,], lwd=2, arr.pos=over.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[7,],to=bpos[8,], lwd=2, arr.pos=over.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[11,],to=bpos[12,], lwd=2, arr.pos=over.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
straightarrow(from=bpos[15,],to=bpos[16,], lwd=2, arr.pos=over.pos, endhead=TRUE, arr.type=arrow, arr.length=.3, arr.width=wid)
pin <- par("pin")
xx <- 0.30
yy <- xx*pin[1]/pin[2]*.18
for (i in 1:nrow(bpos)){
   if (lab[i][1]!="")
       textrect(bpos[i,], radx=xx, rady=yy, lab=lab[[i]], cex=fs)
}
par(op)
dev.off()



