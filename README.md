RevoEnhancements
================
This repository is for adding functionality to RevoScaleR. All functions require, and are enhancements to, [Revolution R Enterprise](http://www.revolutionanalytics.com/products/revolution-enterprise.php).

Each file is its own function, but some functions do rely on each other... I am working to document this.

It is also a goal to put this into a `RevoEnhancements` package at some point so the dependencies are taken care of, however for now it is available as use what you want.

This can be installed through Hadley Wickham's [`devtools`](https://github.com/hadley/devtools) as follows:

```{r}
library(devtools)
install_github(RevoEnhancements, username = "dermcnor")
```

Area of enhancement are:
------------------------------
* CRAN R Compatability
* Big Data Graphics
* Big Data Mining
* Discretization
* Variable Importance / Selection
* Models
* Scoring
* ...
