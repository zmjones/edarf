#' Find interaction_importances by comparing marginal/joint variable importance
#'
#' Computes the individual importance of a set of variables and compares it to the importance of the variables together
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @importFrom stats predict
#' @importFrom party initVariableFrame
#' @importFrom iterators icount
#' 
#' @param fit object of class 'RandomForest', 'randomForest', or 'rfsrc'
#' @param var a character vector of the predictors of interest, which must match the input matrix
#' @param nperm integer, number of permutations to run, default is 100
#' @param parallel logical, whether to use a registered parallel backend
#' @param ... additional arguments to be passed
#'
#' @return a named vector
#'
#' @examples
#' \dontrun{
#' library(randomForest)
#' data(iris)
#'
#' fit <- randomForest(Species ~ ., iris)
#' interaction_importance(fit, c("Petal.Width", "Petal.Length"), 10, data = iris)
#'
#' library(party)
#' fit <- cforest(Species ~ ., iris, controls = cforest_unbiased(mtry = 2))
#' interaction_importance(fit, c("Petal.Width", "Petal.Length"), 10, OOB = TRUE)
#' }
#' @export
interaction_importance <- function(fit, var, nperm = 100, parallel = FALSE, ...)
    UseMethod("interaction_importance")
#' @export
interaction_importance.RandomForest <- function(fit, var, nperm = 100, parallel = FALSE,
                                                OOB = TRUE, mincriterion = 0, ...) {
    response <- fit@responses
    y <- response@variables[[1]]
    inp <- party::initVariableFrame(fit@data@get("input"), trafo = NULL)

    if (all(response@is_nominal)) {
        loss <- function(x, oob) mean((levels(y)[sapply(x, which.max)] != y)[oob])
    } else if (all(response@is_ordinal)) {
        loss <- function(x, oob) mean((sapply(x, which.max) != y)[oob])
    } else {
        loss <- function(x, oob) mean((unlist(x) - y)[oob]^2)
    }

    perm_loss <- matrix(0, nrow = nperm * length(fit@ensemble), ncol = length(var) + 2)
    colnames(perm_loss) <- c(var, "joint", "difference")

    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')

    i <- NULL
    out <- foreach::foreach(i = 1:length(fit@ensemble), .combine = "rbind", .packages = "party") %op% {
        perm_loss <- matrix(NA, nrow = nperm, ncol = length(var) + 3)
        colnames(perm_loss) <- c(var, "additive", "joint", "difference")
        
        tree <- fit@ensemble[[i]]
        if (OOB) {
            oob <- fit@weights[[i]] == 0
        } else {
            oob <- rep(TRUE, length(y))
        }
        
        tree_loss <- loss(predict_tree(tree, inp, mincriterion), oob)
        perm_loss[, var] <- do.call("rbind",
                                    lapply(1:nperm, function(j)
                                        sapply(var, function(x)
                                            loss(predict_tree(tree, inp, mincriterion, x), oob) - tree_loss)))
        perm_loss[, "additive"] <- rowSums(perm_loss[, var])
        perm_loss[, "joint"] <- sapply(1:nperm, function(j)
            loss(predict_tree(tree, inp, mincriterion, var), oob) - tree_loss)
        perm_loss[, "difference"] <- perm_loss[, "additive"] - perm_loss[, "joint"]
        perm_loss
    }
    out <- colMeans(out)
    attr(out, "class") <- c("interaction_importance", "numeric")
    attr(out, "target") <- var
    out
}
#' @export
interaction_importance.rfsrc <- function(fit, var, nperm = 100, parallel = FALSE, data, ...) {
    capture.output(out <- randomForestSRC::find.interaction(fit, var, importance = "permute",
                                                            method = "vimp", nrep = nperm, ...))
    out <- as.numeric(unlist(out))
    names(out) <- c(var, "additive", "joint", "difference")
    attr(out, "class") <- c("interaction_importance", "numeric")
    attr(out, "target") <- var
    out
}
#' @export
interaction_importance.randomForest <- function(fit, var, nperm = 100, parallel = FALSE,
                                                data = NULL, ...) {
    y <- fit$y
    ntree <- fit$ntree
    n <- nrow(data)
    
    if (is.factor(y)) loss <- function(x, y) mean(x != y)
    else loss <- function(x, y) mean((x - y)^2)
    
    ensemble_pred <- predict(fit, newdata = data, type = "response", predict.all = TRUE)$individual
    ensemble_weights <- fit$inbag

    inner_loop <- function(fit, var, data) {
        perm_loss <- matrix(0, nrow = ntree, ncol = length(var) + 3)
        colnames(perm_loss) <- c(var, "additive", "joint", "difference")
        for (v in var) {
            pidx <- sample(1:n, n, FALSE)
            pdata <- data
            pdata[, v] <- pdata[pidx, v]
            p <- predict(fit, newdata = pdata, type = "response", predict.all = TRUE)$individual
            perm_loss[, v] <- apply(ensemble_pred, 2, function(x) loss(p, x))
        }
        pidx <- sample(1:n, n, FALSE)
        pdata <- data
        pdata[, var] <- pdata[pidx, var]
        p <- predict(fit, newdata = pdata, type = "response", predict.all = TRUE)$individual
        perm_loss[, "additive"] <- rowSums(perm_loss[, var])
        perm_loss[, "joint"] <- apply(ensemble_pred, 2, function(x) loss(p, x))
        perm_loss[, "difference"] <- perm_loss[, "additive"] - perm_loss[, "joint"]
        perm_loss
    }

    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    out <- foreach::foreach(iterators::icount(ntree), .combine = "rbind", .packages = "randomForest") %op% {
        inner_loop(fit, var, data)
    }
    out <- colMeans(out)
    names(out) <- c(var, "additive", "joint", "difference")
    attr(out, "class") <- c("interaction_importance", "numeric")
    attr(out, "target") <- var
    out
}
