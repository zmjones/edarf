![](https://travis-ci.org/zmjones/edarf.svg)

Functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Exploratory Data Analysis Using Random Forests](https://github.com/zmjones/rfss/)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the outcome variable given a fitted random forest from the following packages (outcome variable types supported in parenthesis): [party](http://cran.r-project.org/web/packages/party/index.html) (multivariate, regression, and classification), [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) (regression, classification, and survival), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) (regression and classification).

`partial_dependence` can be run in parallel by registering the appropriate parallel backend using [doParallel](http://cran.r-project.org/web/packages/doParallel/index.html). Beyond the random forest and the set of variables for which to calculate the partial dependence, there are additional arguments which control the dimension of the prediction grid used (naturally, the more points used the longer execution will take) and whether or not points that were not observed in the data can be used in said grid (interpolation).

The `partial_dependence` method can also either return interactions (the partial dependence on unique combinations of a subset of the predictor space) or a list of bivariate partial dependence estimates.

For regression we provide (by default) confidence intervals using the bias-corrected infinitesimal jackknife ([Wager, Hastie, and Tibsharani, 2014](http://jmlr.org/papers/v15/wager14a.html)) using code adapted from [randomForestCI](https://github.com/swager/randomForestCI).

We also provide methods to extract and plot variable importance.

We are planning on adding methods to make interpreting proximity and maximal subtree matrices easier.

Pull requests, bug reports, feature requests, etc. are welcome!
