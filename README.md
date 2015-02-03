Functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) in support of "[Random Forests for the Social Sciences](https://github.com/zmjones/rfss/)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the outcome variable given a fitted random forest from the following packages (outcome variable types supported in parenthesis): [party](http://cran.r-project.org/web/packages/party/index.html) (multivariate, regression, and classification), [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) (regression, classification, and survival), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) (regression and classification).

Partial dependence can be run in parallel by registering the appropriate parallel backend, as shown below. Beyond the random forest and the set of variables for which to calculate the partial dependence, there are additional arguments which control the dimension of the prediction grid used (naturally, the more points used the longer execution will take) and whether or not points that were not observed in the data can be used in said grid (interpolation).

Pull requests, bug reports, feature requests, etc. are welcome!

### Installation

It is not yet on CRAN, but you can install it from Github using [devtools](http://cran.r-project.org/web/packages/devtools/index.html). 

```{r}
library(devtools)
install_github("zmjones/edarf")
```

### Classification

```{r}
library(randomForest)
library(party)
library(randomForestSRC)
library(edarf)
data(iris)

library(doParallel)
registerDoParallel(makeCluster(detectCores()))

fit_rf <- randomForest(Species ~ ., iris)
fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
fit_rfsrc <- rfsrc(Species ~ ., iris)

pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width")
pd_pt <- partial_dependence(fit_pt, "Petal.Width")
pd_rfsrc <- partial_dependence(fit_rfsrc, "Petal.Width")

pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"))
pd_int_pt <- partial_dependence(fit_pt, c("Petal.Width", "Sepal.Length"))
pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("Petal.Width", "Sepal.Length"))
```

### Regression

```{r}
data(swiss)

fit_rf <- randomForest(Fertility ~ ., swiss)
fit_pt <- cforest(Fertility ~ ., swiss, controls = cforest_control(mtry = 2))
fit_rfsrc <- rfsrc(Fertility ~ ., swiss)

pd_rf <- partial_dependence(fit_rf, swiss, "Education")
pd_pt <- partial_dependence(fit_pt, "Education")
pd_rfsrc <- partial_dependence(fit_rfsrc, "Education")

pd_int_rf <- partial_dependence(fit_rf, swiss, c("Education", "Catholic"))
pd_int_pt <- partial_dependence(fit_pt, c("Education", "Catholic"))
pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("Education", "Catholic"))
```

### Survival

```{r}
data(veteran)

fit_rfsrc <- rfsrc(Surv(time, status) ~ ., veteran)
pd_rfsrc <- partial_dependence(fit_rfsrc, "age")
pd_int_rfsrc <- partial_dependence(fit_rfsrc, c("age", "diagtime"))
```

### Multivariate

```{r}
data(mtcars)

fit <- cforest(hp + qsec ~ ., mtcars, controls = cforest_control(mtry = 2))
pd <- partial_dependence(fit, "mpg")
pd_int <- partial_dependence(fit, c("mpg", "cyl"))

```

### Plotting

```{r}
fit_reg <- randomForest(Fertility ~ ., swiss)

imp <- fit_reg$importance[, 1]
plot_imp(names(imp), imp)

pd_reg <- partial_dependence(fit_reg, swiss, "Education", 50)

plot_twoway_partial(pd_reg$Education, pd_reg$Fertility, smooth = TRUE)
```
