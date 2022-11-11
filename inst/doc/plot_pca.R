## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=3, fig.path='figures/pca-', warning=FALSE)
hasDep <- require("cluster", quietly = TRUE) && require("lfda", quietly = TRUE) && require("MASS", quietly = TRUE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(ggfortify)
df <- iris[1:4]
pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species')

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', label = TRUE, label.size = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', shape = FALSE, label.size = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species', loadings = TRUE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(pca_res, scale = 0)

## ---- message = FALSE, eval=hasDep--------------------------------------------
d.factanal <- factanal(state.x77, factors = 3, scores = 'regression')
autoplot(d.factanal, data = state.x77, colour = 'Income')
autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
set.seed(1)
autoplot(kmeans(USArrests, 3), data = USArrests)

autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(cluster)
autoplot(clara(iris[-5], 3))

## ---- eval=hasDep-------------------------------------------------------------
autoplot(fanny(iris[-5], 3), frame = TRUE)

## ---- eval=hasDep-------------------------------------------------------------
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

## ---- eval=hasDep-------------------------------------------------------------
autoplot(silhouette(pam(iris[-5], 3L)))

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(lfda)

# Local Fisher Discriminant Analysis (LFDA)
model <- lfda(iris[-5], iris[, 5], r = 3, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

## ---- message = FALSE, eval=hasDep--------------------------------------------
# Semi-supervised Local Fisher Discriminant Analysis (SELF)
model <- self(iris[-5], iris[, 5], beta = 0.1, r = 3, metric="plain")
autoplot(model, data = iris, frame = TRUE, frame.colour = 'Species')

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(eurodist)

## ---- eval=hasDep-------------------------------------------------------------
autoplot(cmdscale(eurodist, eig = TRUE))

## ---- eval=hasDep-------------------------------------------------------------
autoplot(cmdscale(eurodist, eig = TRUE), label = TRUE, label.size = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(MASS)
autoplot(isoMDS(eurodist), colour = 'orange', size = 4, shape = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(sammon(eurodist), shape = FALSE, label.colour = 'blue', label.size = 3)

