RevoEnhancements is an R package that adds functionality and utility functions to RevoScaleR. All functions require, and are enhancements to, [Revolution R Enterprise](http://www.revolutionanalytics.com/products/revolution-enterprise.php).

Additional functionality
------------------------

Base R Compatability
* Functions to expand model formulas, e.g. expanding `y~.` into all dependent variables in the XDF file

Summarising Big Data for easy graphics visualization
* `rxHexBin` - hexagonal binning
* `rxBoxPlot`


Big Data Mining
* Discretization and tree discretization
* Sampling from an XDF into a data frame
* Calculating test statistics, e.g. accuracy, recall and F1-score

Statistical testing
* KS statistic
* t-test

Visualisation
* Decision trees (`plot.rxDTree`)
* Decision forests (`plot.rxDTree`)

Installation
------------

You can use the `devtools` package to install `RevoEnhancements` directly from github.

    library(devtools)
    install_github("RevoEnhancements", "RevoEnhancements")

`RevoEnhancements` imports some packages from CRAN, e.g. package `hexbin`. To install dependencies from CRAN, use:

    install.packages("hexbin")
    
To demonstrate how to use the code, we also use some examples with `ggplot2` and `mlbench`.  To install these suggested packages, use:

    install.packages(c("ggplot2", "mlbench"))


