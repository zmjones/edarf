#' Find interaction_importances by comparing marginal/joint variable importance
#'
#' Computes the individual importance of a set of variables and compares it to the importance of the variables together
#' @importFrom foreach foreach %dopar% %do% %:% getDoParWorkers
#' @importFrom stats predict
#' @importFrom party initVariableFrame
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

    w <- fit@initweights
    perm_loss <- matrix(0, nrow = nperm * length(fit@ensemble), ncol = length(var) + 2)
    colnames(perm_loss) <- c(var, "joint", "difference")

    for (i in 1:length(fit@ensemble)) {
        tree <- fit@ensemble[[i]]
        if (OOB) {
            oob <- fit@weights[[i]] == 0
        } else {
            oob <- rep(TRUE, length(y))
        }
        
        p <- predict_tree(tree, inp, mincriterion)
        tree_loss <- loss(p, oob)
        var_idx <- which(var %in% colnames(inp@variables))

        for (k in 1:nperm) {
            row_idx <- k + (i - 1) * nperm
            for (j in 1:length(var)) {
                p <- predict_tree(tree, inp, mincriterion, var[j])
                perm_loss[row_idx, j] <- loss(p, oob) - tree_loss
            }

            p_int <- predict_tree(tree, inp, mincriterion, var)
            perm_loss[row_idx, length(var) + 1] <- loss(p_int, oob) - tree_loss
        }
        perm_loss[row_idx, ncol(perm_loss)] <- sum(perm_loss[i, var]) - perm_loss[i, length(var) + 1]
    }
    colMeans(perm_loss)
}

#' @export
interaction_importance.rfsrc <- function(fit, var, nperm = 100, parallel = FALSE, data, ...) {
    capture.output(out <- randomForestSRC::find.interaction(fit, var, importance = "permute",
                                                            method = "vimp", nrep = nperm, ...))
    out <- out[, c(1:(length(var) + 1), length(out))]
    names(out) <- c(var, "joint", "difference")
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
        perm_loss <- matrix(0, nrow = ntree, ncol = length(var) + 2)
        for (j in 1:length(var)) {
            pidx <- sample(1:n, n, FALSE)
            pdata <- data
            pdata[, var[j]] <- pdata[pidx, var[j]]
            p <- predict(fit, newdata = pdata, type = "response", predict.all = TRUE)$individual
            perm_loss[, j] <- apply(ensemble_pred, 2, function(x) loss(p, x))
        }
        pidx <- sample(1:n, n, FALSE)
        pdata <- data
        pdata[, var] <- pdata[pidx, var]
        p <- predict(fit, newdata = pdata, type = "response", predict.all = TRUE)$individual
        perm_loss[, length(var) + 1] <- apply(ensemble_pred, 2, function(x) loss(p, x))
        perm_loss[, ncol(perm_loss)] <- rowSums(perm_loss[, 1:length(var)]) - perm_loss[, length(var) + 1]
        perm_loss
    }

    '%op%' <- ifelse(foreach::getDoParWorkers() > 1 & parallel, foreach::'%dopar%', foreach::'%do%')
    out <- foreach::foreach(i = 1:nperm, .combine = "rbind", .packages = "randomForest") %op% {
        inner_loop(fit, var, data)
    }
    out <- colMeans(out)
    names(out) <- c(var, "joint", "difference")
    out
}
