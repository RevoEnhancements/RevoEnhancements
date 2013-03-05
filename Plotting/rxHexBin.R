rxHexBin <- function (formula, data, shape = 1, xbins = 30, ...) {
  if (length(formula) != 3) stop("Two Variables Are Required in the Formula")
  call <- match.call()
	## Function for creating/updating hexbin objects.
  hexBinTransform <- function(dataList) {
    hex1 <- .rxGet("hex1")
    hex2 <- hexbin(dataList[[xvar]], dataList[[yvar]], 
                    xbnds=myXbnds, ybnds=myYbnds,
                    xbins = hex1@xbins, shape = hex1@shape,)
    myDf <- data.frame(xcm = c(hex1@xcm, hex2@xcm) * c(hex1@count, hex2@count), 
                        ycm = c(hex1@ycm, hex2@ycm) * c(hex1@count, hex2@count),
                        count = c(hex1@count, hex2@count))
    res <- aggregate(myDf, 
                    list(cell = c(hex1@cell, hex2@cell)), 
                    sum, na.rm = TRUE)
	  hex1@count <- res[,"count"]
	  hex1@cell <- res[,"cell"]
    hex1@xcm <- res[,"xcm"] / res[,"count"]
    hex1@ycm <- res[,"ycm"] / res[,"count"]
    .rxSet("hex1", hex1)
    return(NULL)
	}
	## Create metadata
	dataInfo <- rxGetInfoXdf(data, getVarInfo = TRUE)
	plotVars <- all.vars(formula)
	x <- plotVars[-1][1] # what if longer than 1
	y <- plotVars[1]
	xbnds <- c(dataInfo$varInfo[[x]][["low"]], dataInfo$varInfo[[x]][["high"]])
  ybnds <- c(dataInfo$varInfo[[y]][["low"]], dataInfo$varInfo[[y]][["high"]])
  startHex <- hexbin(x = 1, y = 1, xbins = xbins, shape = shape,
                      xbnds = xbnds, ybnds = ybnds, xlab = x, ylab = y)
  startHex@count <- NA_integer_
  startHex@cell <- NA_integer_
  startHex@xcm <- NA_real_
  startHex@ycm <- NA_real_
	resHexBin <- rxDataStep(inData = data, varsToKeep = plotVars, 
                          transformFunc = hexBinTransform, 
                          transformObjects = list(hex1 = startHex, 
                            xvar = x, yvar = y, 
                            myXbnds = xbnds, myYbnds = ybnds), 
                          transformPackages = c("hexbin"),
                          returnTransformObjects = TRUE, ...)$hex1
  resHexBin@ncells <- length(resHexBin@count)
  resHexBin@n <- sum(resHexBin@count, na.rm = TRUE)
  resHexBin@call <- call
	return(resHexBin)
}






# Load the Airlines Data
BigAir.Dir <- "C:/Users/Derek Norton/Documents/Revolution/Revo Demos"
working.file <- file.path(rxGetOption("sampleDataDir"),"AirlineDemoSmall.xdf")
working.file <- file.path(BigAir.Dir,"AirlineData87to08.xdf")


require(RevoScaleR)
require(hexbin)
bin1 <- rxHexBin(ArrDelay ~ CRSDepTime, working.file, xbins = 50)
plot(bin1, lcex=.5, border = TRUE, colramp = BTC)
plot(bin1, style="colorscale")
plot(bin1, style="centroids")
plot(bin1, style="lattice")

require(ggplot2)
ggbin <- data.frame(
	hcell2xy(bin1), 
	count = bin1@count, 
	density = bin1@count / sum(bin1@count, na.rm=TRUE)
	)
p2 <- ggplot(data = ggbin, aes(x, y, fill = count))
p2 + geom_hex(stat="identity")

a <- rxCor( ~ Year + Month + DayofMonth + DayOfWeek + 
  DepTime + CRSDepTime + ArrTime + CRSArrTime + 
  ActualElapsedTime + CRSElapsedTime + DepDelay + 
  Distance + TaxiIn + TaxiOut + CancellationCode + 
  Diverted_Pred + Late, working.file, blocksPerRead = 20)

bin1 <- rxHexBin(ArrDelay ~ CRSDepTime, working.file, xbins = 50, blocksPerRead = 20)
plot(bin1, lcex=.5, border = TRUE, colramp = BTC, legend = 1, colorcut = seq(0, 1, length = 10))

require(ggplot2)
ggbin <- data.frame(
	hcell2xy(bin1), 
	count = bin1@count, 
	density = bin1@count / sum(bin1@count, na.rm=TRUE)
	)
p2 <- ggplot(data = ggbin, aes(x, y, fill = count))
p2 + geom_hex(stat="identity", nbins = 100) + guides(fill = guide_colorbar(ticks = FALSE, label.theme = element_text(size = 8, angle = 0))) + labs(x = "CRSDepTime", y = "ArrDelay")

m <- ggplot(movies, aes(x=rating))
m+ geom_density(size = 1)
m + geom_histogram(colour = "black", fill = NA, binwidth = 0.5, size = 1)
