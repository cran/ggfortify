## ----global_options, include=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=3, fig.path='figures/ts-', warning=FALSE)
hasDep <- require("zoo", quietly = TRUE) && require("forecast", quietly = TRUE) && require("timeSeries", quietly = TRUE) && require("KFAS", quietly = TRUE) && require("changepoint", quietly = TRUE) && require("forecast", quietly = TRUE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(ggfortify)
autoplot(AirPassengers)

## ---- eval=hasDep-------------------------------------------------------------
autoplot(AirPassengers, ts.colour = 'red', ts.linetype = 'dashed')

## ----message = FALSE, eval=hasDep---------------------------------------------
library(vars)
data(Canada)
autoplot(Canada)

## ---- eval=hasDep-------------------------------------------------------------
autoplot(Canada, facets = FALSE)

## ---- message = FALSE, eval = FALSE-------------------------------------------
#  library(xts)
#  autoplot(as.xts(AirPassengers), ts.colour = 'green')
#  
#  library(timeSeries)
#  autoplot(as.timeSeries(AirPassengers), ts.colour = ('dodgerblue3'))

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(AirPassengers, ts.geom = 'bar', fill = 'blue')

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(AirPassengers, ts.geom = 'ribbon', fill = 'green')

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(AirPassengers, ts.geom = 'point', shape = 3)

## ---- message = FALSE, eval=hasDep--------------------------------------------
mts <- ts(data.frame(a = c(1, 2, 3, 4, 4, 3), b = c(3, 2, 3, 2, 2, 1)), start = 2010)
autoplot(mts, ts.geom = 'bar', facets = FALSE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(mts, ts.geom = 'bar', facets = FALSE, stacked = TRUE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(mts, ts.geom = 'ribbon', facets = FALSE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(mts, ts.geom = 'ribbon', facets = FALSE, stacked = TRUE)

## ---- message = FALSE, eval=hasDep--------------------------------------------
library(forecast)
d.arima <- auto.arima(AirPassengers)
d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast)

## ---- eval=hasDep-------------------------------------------------------------
autoplot(d.forecast, ts.colour = 'firebrick1', predict.colour = 'red',
         predict.linetype = 'dashed', conf.int = FALSE)

## ---- eval=hasDep-------------------------------------------------------------
library(vars)
d.vselect <- VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
d.var <- VAR(Canada, p = d.vselect, type = 'const')

## ---- eval=hasDep-------------------------------------------------------------
autoplot(predict(d.var, n.ahead = 50), ts.colour = 'dodgerblue4',
         predict.colour = 'blue', predict.linetype = 'dashed')

## ----message = FALSE, eval=hasDep---------------------------------------------
library(changepoint)
autoplot(cpt.meanvar(AirPassengers))

## ---- eval=hasDep-------------------------------------------------------------
autoplot(cpt.meanvar(AirPassengers), cpt.colour = 'blue', cpt.linetype = 'solid')

## ----message = FALSE, eval=hasDep---------------------------------------------
library(strucchange)
autoplot(breakpoints(Nile ~ 1), ts.colour = 'blue', ts.linetype = 'dashed',
         cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')

## ---- message=FALSE, warning=FALSE, eval=hasDep-------------------------------
library(KFAS)
model <- SSModel(
  Nile ~ SSMtrend(degree=1, Q=matrix(NA)), H=matrix(NA)
)
 
fit <- fitSSM(model=model, inits=c(log(var(Nile)),log(var(Nile))), method="BFGS")
smoothed <- KFS(fit$model)
autoplot(smoothed)

## ---- eval=hasDep-------------------------------------------------------------
filtered <- KFS(fit$model, filtering="mean", smoothing='none')
autoplot(filtered)

## ---- eval=hasDep-------------------------------------------------------------
trend <- signal(smoothed, states="trend")
class(trend)

## ---- eval=hasDep-------------------------------------------------------------
p <- autoplot(filtered)
autoplot(trend, ts.colour = 'blue', p = p)

## ---- message = FALSE, eval=hasDep--------------------------------------------
autoplot(stl(AirPassengers, s.window = 'periodic'), ts.colour = 'blue')

## ---- fig.width = 4, fig.height = 2, eval=hasDep------------------------------
autoplot(acf(AirPassengers, plot = FALSE))

## ---- fig.width = 4, fig.height = 2, eval=hasDep------------------------------
autoplot(acf(AirPassengers, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma')

## ---- fig.width = 4, fig.height = 2, eval=hasDep------------------------------
autoplot(spec.ar(AirPassengers, plot = FALSE))

## ---- message = FALSE, fig.width=3, fig.height=3, eval=hasDep-----------------
ggcpgram(arima.sim(list(ar = c(0.7, -0.5)), n = 50))

## ---- message = FALSE, fig.width = 5, fig.height = 5, eval=hasDep-------------
library(forecast)
ggtsdiag(auto.arima(AirPassengers))

## ---- message = FALSE, fig.width = 4, fig.height = 4, eval=hasDep-------------
ggfreqplot(AirPassengers)
ggfreqplot(AirPassengers, freq = 4)

