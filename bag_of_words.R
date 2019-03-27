#####################################
##
## Follow the idea of Bag-of-words model in computer vision
## https://www.mathworks.com/help/vision/examples/image-category-classification-using-bag-of-features.html
## https://en.wikipedia.org/wiki/Bag-of-words_model_in_computer_vision
##
## Describe every image with a finger print and calculate the probabibility of
## belonging to class C having a certain fingerprint.
##
## To get the fingerprint:
##   * split every image into several patches
##   * describe every patch with informative features (area colagen + area void)
##   * cluster patches with k-means
##   * image fingerprint is a vectorial representation of the histogram (of frequencies) of patches per cluster
##
## To calculate the probability:
##   * naive bayes: c* = arg max p(c|w) = arg max p(c)p(w|c) = arg max p(c)prod(n=1, N, p(w_n|c))
##
## Shortcommings: many
## Improvements: many
##   * Optimize the number of clusters for the fingerprint on the model's error function
##   * Stepwise add variables to the model (also optimizing for the error function)
##
#####################################
set.seed(666)
library(e1071)
library(ggplot2)
library(cluster)
library(RColorBrewer)
library(parallel)
library(png)
library(randomForest)
source("pairs.R")

# Load data and metadata
dataset <- readRDS("clean_data.rds")
dataset$features <- dataset$features[!is.na(dataset$features$Collagen.Area.per.Tissue....) &
                                     !is.na(dataset$features$Void.Area....), ]
dataset$samplesheet <- dataset$samplesheet[dataset$samplesheet$sample.ID %in% dataset$features$Slidename, ]

# equalize number of images per patient, to the median of the dataset
npatches <- median(as.numeric(table(dataset$features$Slidename)))
dataset$features <- do.call(rbind, by(dataset$features, dataset$features$Slidename, function(x) {
  x$patch <- paste(x$Slidename, 1:nrow(x))
  x[sample(1:nrow(x), min(npatches, nrow(x))), ]
}))

# explore the colinearity of the features
x <- as.matrix(dataset$features[, 2:9])
mode(x) <- "numeric"
png(f <- paste0(tempfile(), ".png"), width=1024, height=1024)
pairs(x, diag.panel=panel.hist, pch=16, col="#00000050")
dev.off()
img <- readPNG(f)
opar <- par(mar=c(0, 0, 0, 0))
plot(0, xlim=c(0, ncol(img)), ylim=c(0, nrow(img)), type="n", bty="n", axes=F, xlab="", ylab="")
rasterImage(img, 0, 0, ncol(img), nrow(img))
par <- opar

RELEVANT_FEATURES <- c("Compactness.Tissue", "Collagen.Area.per.Tissue....", "Void.Area....")

##
## prepare codebook
##

##   * split every image into several patches: done already
##   * describe every patch with informative features (area colagen + area void): done already

##   * cluster patches with k-means
clusters <- c(4, 8, 16, 32, 64, 128, 256, 512)
k <- lapply(clusters, function(k) kmeans(dataset$features[, RELEVANT_FEATURES], k, iter.max=100))
pca   <- prcomp(dataset$features[, RELEVANT_FEATURES], center=FALSE, scale.=TRUE)
plot(pca$x[, 1], pca$x[, 2], col=paste0(RColorBrewer::brewer.pal(8, "Set1")[as.factor(k[[2]]$cluster)], 80), pch=16)

# calculate silhouettes
dissE <- daisy(dataset$features[, RELEVANT_FEATURES])
sk <- lapply(k, function(k) summary(silhouette(k$cluster, dissE))$avg.width)
plot(unlist(sk), type="l", col="red", xlab="number of clusters", ylab="average silhouette width", xaxt="n")
axis(1, at=1:length(sk), labels=clusters)
points(unlist(sk), col="red")

##   * image fingerprint is a vectorial representation of the histogram (of frequencies) of patches per cluster
fingerprints <- lapply(seq_along(clusters), function(i) {
  x <- dataset$features
  x$cluster <- k[[i]]$cluster
  f <- do.call(rbind, by(x, x$Slidename, function(x) {
    sapply(1:clusters[i], function(i) sum(x$cluster == i) / nrow(x))
  }))
  write.csv(f, file=paste0("fingerprint", clusters[i], ".csv"))
  f
})

# choose number of clusters based on the elbow rule in the above silhouettes
BEST <- 5
dataset$features$cluster <- k[[BEST]]$cluster
dataset$fingerprint <- fingerprints[[BEST]]

# let's see how fingerprints correlate with metadata
ann <- dataset$samplesheet[match(rownames(dataset$fingerprint), rownames(dataset$samplesheet)), c("SiteCode", "fibrosis_stage", "race", "sex", "Cirrhosis", "diabetes", "biopsy.type", "scoredby_blindedpath.y")]
ann$fibrosis_stage <- as.character(ann$fibrosis_stage)
ann$race           <- as.character(ann$race)
pheatmap(cor(t(dataset$fingerprint), method="spearman"), annotation_col=ann)

x <- prcomp(dataset$fingerprint)
plot(x$x[, 1], x$x[, 2], pch=16, col=brewer.pal(9, "Set1")[as.factor(ann$fibrosis_stage)])

##
##apcluster --> automatically choose the best number of clusters
#library(apcluster)
#x <- dataset$features[, RELEVANT_FEATURES]
#s <- negDistMat(x, r=2)
#h <- apcluster(s, details=TRUE, convits=10, maxits=100, seed=666)
#a <- aggExCluster(s, h)
#plot(h, s)
###

# is there a batch effect in the signature, based on the first letters of the sample? "CM" "KCN" "LDR" "STM" "WCA"
x <- prcomp(dataset$fingerprint)
plot(x$x[, 1], x$x[, 2], pch=16,
     col=brewer.pal(9, "Set1")[factor(gsub("\\d+$", "", rownames(x$x)))])

##
## Probability of class C when we observe a certain fingerprint
##
x <- as.data.frame(dataset$fingerprint)
x$Class <- factor(dataset$samplesheet$fibrosis_stage[match(rownames(x), dataset$samplesheet$sample.ID)])
x <- x[!is.na(x$Class), ]

train <- sample(1:nrow(x), round(.7 * nrow(x)))
test  <- which(!(1:nrow(x) %in% train))

plot_perf <- function(predicted, expected) {
  plot(jitter(as.numeric(predicted)) ~ jitter(as.numeric(expected)), xaxt="n", yaxt="n", xlab="expected", ylab="observed",
       col=c("red", "black")[as.numeric(as.numeric(predicted) == as.numeric(expected)) + 1])
  axis(1, 1:length(levels(expected)), levels(expected))
  axis(2, 1:length(levels(predicted)), levels(predicted))
}

# NB: the best algorithms are the simplest
nb <- naiveBayes(Class ~ ., data=x[train, ]) # build model on training data
nb_pred <- predict(nb, x[test, ])            # predict test data
table(nb_pred, x$Class[test])                # confusion matrix
plot_perf(nb_pred, x$Class[test])

# RF
rf <- randomForest(Class ~ ., data=x[train, ]) # build model on training data
rf_pred <- predict(rf, x[test, ])            # predict test data
table(rf_pred, x$Class[test])                # confusion matrix
plot_perf(rf_pred, x$Class[test])

# RF regression
x$Class2 <- as.numeric(as.character(x$Class))
rf <- randomForest(Class2 ~ ., data=x[train, !grepl("^Class$", colnames(x))]) # build model on training data
rf_pred <- predict(rf, x[test, !grepl("^Class$", colnames(x))])            # predict test data
boxplot(rf_pred ~ x$Class[test], xlab="true class", ylab="predicted", xaxt="n", ylim=c(-1, 4))
axis(1, 1:length(levels(x$Class)), levels(x$Class))

