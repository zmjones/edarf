Convenience functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Data Mining as Exploratory Data Analysis](https://github.com/zmjones/datamining)"

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the response given a fitted random forest from [party](http://cran.r-project.org/web/packages/party/index.html), [randomSurvivalForest](http://cran.r-project.org/web/packages/randomSurvivalForest/index.html), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html).

```{r}
data(iris)
fit <- party::cforest(Species ~ ., data = iris)
pd <- party_partial_dependence(fit, "Petal.Width", parallel::detectCores())
pd_int <- party_partial_dependence(fit, c("Petal.Width", "Sepal.Length"), parallel::detectCores())
```
