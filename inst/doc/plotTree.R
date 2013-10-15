
## ----setup---------------------------------------------------------------
library(RevoScaleR)
library(RevoEnhancements)
library(ggplot2)
rxOptions(
  computeContext=RxLocalParallel(),
  reportProgress=0
)


## ----rxDTree-------------------------------------------------------------
### Create and plot an rxDTree object

frm <- formulaExpand(price ~ ., data=diamonds)
fit <- rxDTree(frm, diamonds, maxDepth=3)


## ----rxDTree-plot-1, fig.width=6, fig.height=4---------------------------
par(mar=c(0.2, 0.2, 1.5, 0.2))
par(mai=rep(0.1, 4))
plot(fit)


## ----rxDTree-plot-2, fig.width=6, fig.height=4---------------------------
plot(fit, 
     textArgs=list(col="blue", cex=0.7))


## ----rxDTree-plot-3, fig.width=6, fig.height=4---------------------------
plot(fit, 
     textArgs=list(col="blue", cex=0.7), 
     plotArgs=list(main="Decision tree"))


## ----rxDForest-----------------------------------------------------------
### Create and plot an rxDForest object

fit <- rxDForest(frm, diamonds, maxDepth=3)
forest <- fit$forest


## ----rxDForest-plot-1, fig.width=6, fig.height=4-------------------------
par(mar=c(0.2, 0.2, 1.5, 0.2))
par(mai=rep(0.1, 4))
plot(forest[[1]], textArgs=list(col="blue", cex=0.7))


## ----rxDForest-plot-2, fig.width=6, fig.height=4-------------------------
plot(forest[[1]], 
     textArgs=list(col="blue", cex=0.7), 
     plotArgs=list(main="Decision forest: Tree 1"))
plot(forest[[2]], 
     textArgs=list(col="blue", cex=0.7), 
     plotArgs=list(main="Decision forest: Tree 2"))



