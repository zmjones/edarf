Functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Random Forests for the Social Sciences](https://github.com/zmjones/rfss/)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the outcome variable given a fitted random forest from the following packages (outcome variable types supported in parenthesis): [party](http://cran.r-project.org/web/packages/party/index.html) (multivariate, regression, and classification), [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) (regression, classification, and survival), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) (regression and classification).

Partial dependence can be run in parallel by registering the appropriate parallel backend, as shown below. Beyond the random forest and the set of variables for which to calculate the partial dependence, there are additional arguments which control the dimension of the prediction grid used (naturally, the more points used the longer execution will take) and whether or not points that were not observed in the data can be used in said grid (interpolation).

Pull requests, bug reports, feature requests, etc. are welcome!

### Installation

It is not yet on CRAN, but you can install it from Github using [devtools](http://cran.r-project.org/web/packages/devtools/index.html). 

```{r}
library(devtools)
install_github("zmjones/edarf")
```

### Usage

```{r}
library(edarf)

## classification
data(iris)
library(randomForest)
fit <- randomForest(Species ~ ., iris)
pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
plot(pd, geom = "line")
plot(pd, geom = "area")
pd <- partial_dependence(fit, iris, "Petal.Width")
plot(pd, geom = "bar")
pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"), type = "prob")
plot(pd_int, geom = "line")
pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"))
plot(pd_int, geom = "bar")

## regression
data(swiss)
fit <- randomForest(Fertility ~ ., swiss)
pd <- partial_dependence(fit, swiss, "Education")
plot(pd)

pd_int <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"))
plot(pd_int)

## survival
data(veteran)
library(randomForestSRC)
fit <- rfsrc(Surv(time, status) ~ ., veteran)
pd <- partial_dependence(fit, "age")
pd_int <- partial_dependence(fit, c("age", "diagtime"))
plot(pd_int)

## multivariate
data(mtcars)
library(party)
fit <- cforest(hp + qsec ~ ., mtcars, controls = cforest_control(mtry = 2))
pd <- partial_dependence(fit, "mpg")
plot(pd)
pd_int <- partial_dependence(fit, c("mpg", "cyl"))
plot(pd_int)
```
