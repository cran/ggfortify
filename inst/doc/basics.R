## ----global_options, include=FALSE---------------------------------------
library(knitr)
opts_chunk$set(fig.width=4, fig.height=3, fig.path='figures/basics-', warning=FALSE)

## ---- message = FALSE----------------------------------------------------
library(ggfortify)

## ------------------------------------------------------------------------
autoplot(AirPassengers)

## ------------------------------------------------------------------------
autoplot(AirPassengers, ts.colour = 'blue')

## ------------------------------------------------------------------------
p <- autoplot(AirPassengers)
class(p)
# plot as it is
p

# add title and labels
p + ggtitle('AirPassengers') + xlab('Year') + ylab('Passengers')


## ------------------------------------------------------------------------
set.seed(1)
p <- autoplot(kmeans(iris[-5], 3), data = iris)
# plot as it is
p

# change colour mapping
p + scale_colour_brewer()

## ------------------------------------------------------------------------
df <- fortify(kmeans(iris[-5], 3), data = iris)
head(df)

ggplot(df, aes(x= cluster, fill = cluster)) + geom_bar()

