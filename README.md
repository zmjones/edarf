Functions useful for exploratory data analysis using random forests. Developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Random Forests for the Social Sciences](https://github.com/zmjones/rfss/)."

This package allows you to easily calculate the partial dependence of an arbitrarily large set of explanatory variables on the outcome variable given a fitted random forest from the following packages (outcome variable types supported in parenthesis): [party](http://cran.r-project.org/web/packages/party/index.html) (multivariate, regression, and classification), [randomForestSRC](http://cran.r-project.org/web/packages/randomForestSRC/index.html) (regression, classification, and survival), and [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html) (regression and classification).

For regression we provide (by default) confidence intervals using the bias-corrected infinitesimal jackknife ([Wager, Hastie, and Tibsharani, 2014](http://jmlr.org/papers/v15/wager14a.html)) using code adapted from [randomForestCI](https://github.com/swager/randomForestCI).

The `partial_dependence` method can also either return interactions (the partial dependence on unique combinations of a subset of the predictor space) or a list of bivariate partial dependence estimates.

Partial dependence can be run in parallel by registering the appropriate parallel backend, as shown below. Beyond the random forest and the set of variables for which to calculate the partial dependence, there are additional arguments which control the dimension of the prediction grid used (naturally, the more points used the longer execution will take) and whether or not points that were not observed in the data can be used in said grid (interpolation).

We are planning on adding methods to make interpreting proximity and maximal subtree matrices easier.

Pull requests, bug reports, feature requests, etc. are welcome!

### Installation

It is not yet on CRAN, but you can install it from Github using [devtools](http://cran.r-project.org/web/packages/devtools/index.html). 

```{r}
library(devtools)
install_github("zmjones/edarf")
```

### Partial Dependence

```{r}
library(edarf)

data(iris)
library(randomForest)
fit <- randomForest(Species ~ ., iris)
pd <- partial_dependence(fit, iris, "Petal.Width", type = "prob")
plot_pd(pd, geom = "line")
```
![](http://zmjones.com/static/images/iris_pd_line.png)

```{r}
plot_pd(pd, geom = "area")
```
![](http://zmjones.com/static/images/iris_pd_area.png)

```{r}
pd <- partial_dependence(fit, iris, "Petal.Width")
plot_pd(pd, geom = "bar")
```
![](http://zmjones.com/static/images/iris_pd_bar.png)

```{r}
pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"), interaction = TRUE, type = "prob")
plot_pd(pd_int, geom = "line")
```
![](http://zmjones.com/static/images/iris_pd_int_line.png)

```{r}
plot_pd(pd_int, geom = "area")
```
![](http://zmjones.com/static/images/iris_pd_int_area.png)

```{r}
pd_int <- partial_dependence(fit, iris, c("Petal.Width", "Sepal.Length"), interaction = TRUE)
plot_pd(pd_int, geom = "bar")
```
![](http://zmjones.com/static/images/iris_pd_int_bar.png)

### regression

```{r}
data(swiss)
fit <- randomForest(Fertility ~ ., swiss, keep.inbag = TRUE)
pd <- partial_dependence(fit, swiss, "Education")
plot_pd(pd)
```
![](http://zmjones.com/static/images/swiss_pd_line.png)

```{r}
pd_int <- partial_dependence(fit, swiss, c("Education", "Catholic"), interaction = TRUE)
plot_pd(pd_int)
```
![](http://zmjones.com/static/images/swiss_pd_int_line.png)

### survival

```{r}
library(randomForestSRC)
data(veteran)
fit <- rfsrc(Surv(time, status) ~ ., veteran)
pd <- partial_dependence(fit, "age")
plot_pd(pd)
```
![](http://zmjones.com/static/images/veteran_pd_line.png)

```{r}
pd_int <- partial_dependence(fit, c("age", "diagtime"))
plot_pd(pd_int)
```
![](http://zmjones.com/static/images/veteran_pd_int_line.png)

### multivariate

```{r}
data(mtcars)
library(party)
fit <- cforest(hp + qsec ~ ., mtcars, controls = cforest_control(mtry = 2))
pd <- partial_dependence(fit, "mpg")
plot_pd(pd)
```
![](http://zmjones.com/static/images/mtcars_pd_line.png)

```{r}
pd_int <- partial_dependence(fit, c("mpg", "cyl"), interaction = TRUE)
plot_pd(pd_int)
```

Multivariate two-way interaction plots not yet implemented.

### variance estimation

```{r}
fit <- cforest(Fertility ~ ., swiss)
out <- var_est(fit, swiss)

cl <- qnorm(.05 / 2, lower.tail = FALSE)
se <- sqrt(out$variance)
out$low <- out$Fertility - cl * se
out$high <- out$Fertility + cl * se
out$actual_fertility <- swiss$Fertility

library(ggplot2)
ggplot(out, aes(actual_fertility, Fertility)) +
    geom_point() +
        geom_errorbar(aes(ymax = high, ymin = low), size = .5, width = .5) +
            geom_abline(aes(intercept = 0, slope = 1), colour = "blue") +
                labs(x = "Observed Fertility", y = "Predicted Fertility") +
                    coord_cartesian(c(33, 95), c(33, 95)) +
                        theme_bw()
ggsave("~/Sites/zmjones.com/static/images/swiss_pred.png", width = 8, height = 4)
```
