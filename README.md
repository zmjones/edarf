Convenience functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Data Mining as Exploratory Data Analysis](https://github.com/zmjones/datamining)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the response given a fitted random forest from [party](http://cran.r-project.org/web/packages/party/index.html), [randomSurvivalForest](http://cran.r-project.org/web/packages/randomSurvivalForest/index.html), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html).

### Classification

```{r}
require(randomForest)
require(party)
require(randomForestSRC)
require(parallel)
require(edarf)
data(iris)

fit_rf <- randomForest(Species ~ ., iris)
fit_pt <- cforest(Species ~ ., iris, controls = cforest_control(mtry = 2))
fit_rfsrc <- rfsrc(Species ~ ., iris)

pd_rf <- partial_dependence(fit_rf, iris, "Petal.Width", detectCores())
pd_pt <- partial_dependence(fit_pt, iris, "Petal.Width", detectCores())
pd_rfsrc <- partial_dependence(fit_rfsrc, iris, "Petal.Width", detectCores())

pd_int_rf <- partial_dependence(fit_rf, iris, c("Petal.Width", "Sepal.Length"), detectCores())
pd_int_pt <- partial_dependence(fit_pt, iris, c("Petal.Width", "Sepal.Length"), detectCores())
pd_int_rfsrc <- partial_dependence(fit_rfsrc, iris, c("Petal.Width", "Sepal.Length"), detectCores())
```
