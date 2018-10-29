#' ---
#' title: "Build a RF model with CV, estimate the error and find local patterns with LIME"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#' ---
#/* Note: compile with rmarkdown::render() since knitr::spin() cannot deal with the metadata */
options(stringsAsFactors = FALSE)
library(lime)
library(caret)
library(e1071)
library(ranger)
library(vip)
library(pdp)
library(gridExtra)
library(ggplot2)
library(parallel)

CORES <- 4
setwd("S:\\src\\tox_prediction\\src\\networks\\data\\tmp")

#' ## Build model
#' read input data
train.set <- read.csv("dili_labeled_smiles_genescores.csv", sep=';')
network.dists <- read.csv("../../../../data/networks/drugtargets_refmaps_dists.csv", head=TRUE, check.names=FALSE, row.names=1)

noinfo <- apply(network.dists, 1, function(x) all(is.infinite(x)))
network.dists <- network.dists[!noinfo, ]

#' focus only on the most extreme DILI classes
train.set <- train.set[train.set$vDILIConcern %in% c("vLess-DILI-Concern", "vMost-DILI-Concern"), ]
common <- intersect(train.set$SMILES, colnames(network.dists))
train.set <- train.set[train.set$SMILES %in% common,]
network.dists <- network.dists[, common]

#' make the training set
train.set.feat <- do.call(rbind, lapply(train.set$SMILES, function(x) network.dists[, x]))
train.set.feat <- as.data.frame(train.set.feat)
colnames(train.set.feat) <- rownames(network.dists)
rownames(train.set.feat) <- train.set$SMILES
train.set.feat$tox <- as.factor(make.names(train.set$vDILIConcern[match(rownames(train.set.feat), train.set$SMILES)]))
colnames(train.set.feat) <- make.names(colnames(train.set.feat))
rownames(train.set.feat) <- make.names(rownames(train.set.feat))

#' build a RF model with CV using the pathway distances as a fingerprint
fit.caret <- train(tox ~ .,
                   data=train.set.feat,
                   method='ranger',
                   trControl=trainControl(method="cv", number=10, classProbs=TRUE),
                   tuneLength=1,
                   importance='impurity')

#' ## Global explanation
#' Variable importance
vip(fit.caret) + ggtitle("RF with 10 rounds of CV")

#' Partial dependence plots
vimp <- varImp(fit.caret, scale=FALSE)
marrangeGrob(
  mclapply(1:4, function(i) {
    fit.caret %>%
    partial(pred.var=rownames(vimp$importance)[rev(order(vimp$importance))[i]],
            grid.resolution=25, ice=TRUE) %>%
    autoplot(rug=TRUE, train=train.set.feat, alpha=0.1, center=TRUE)
  }, mc.cores=CORES), nrow=2, ncol=2)

#' ## Local explanation
explainer_caret <- lime(train.set.feat, fit.caret)
explanation_caret <- explain(
  x=train.set.feat[sample(1:nrow(train.set.feat), 8), -ncol(train.set.feat)], 
  explaine=explainer_caret, 
  n_features=10,   # number of features to best describe predicted outcomes
  labels="vMost.DILI.Concern" # label of the feature to predict (toxic)
)

plot_features(explanation_caret)
plot_explanations(explanation_caret)
