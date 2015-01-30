Functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) in support of "[Random Forests for the Social Sciences](https://github.com/zmjones/rfss/)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the response given a fitted random forest from [party](http://cran.r-project.org/web/packages/party/index.html), [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html).

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

### Plotting

```{r}
fit_reg <- randomForest(Fertility ~ ., swiss)

imp <- fit_reg$importance[, 1]
plot_imp(names(imp), imp)

pd_reg <- partial_dependence(fit_reg, swiss, "Education", 50)

plot_twoway_partial(pd_reg$Education, pd_reg$Fertility, smooth = TRUE)
```
