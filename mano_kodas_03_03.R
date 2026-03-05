# Libraries
library(data.table)
library(readxl)
library(fda)

# Upload data
gold <- read_excel("Yfinance_close_prices.xlsx",
                   sheet = "Gold",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
silver <- read_excel("Yfinance_close_prices.xlsx",
                     sheet = "Silver",
                     col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
cooper <- read_excel("Yfinance_close_prices.xlsx",
                     sheet = "Cooper",
                     col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
crude_oil <- read_excel("Yfinance_close_prices.xlsx",
                        sheet = "Crude_oil",
                        col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
natural_gas <- read_excel("Yfinance_close_prices.xlsx",
                          sheet = "Natural gas",
                          col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
corn <- read_excel("Yfinance_close_prices.xlsx",
                   sheet = "Corn",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
coffee <- read_excel("Yfinance_close_prices.xlsx",
                     sheet = "coffee",
                     col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
soybeans <- read_excel("Yfinance_close_prices.xlsx",
                       sheet = "Soybeans",
                       col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
wheat <- read_excel("Yfinance_close_prices.xlsx",
                    sheet = "Wheat",
                    col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
close_prices <- read_excel("Yfinance_close_prices.xlsx",
                           sheet = "Close_prices",
                           col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric", "numeric", "numeric"))

# Ploted data (open)
plot(gold$Open)
plot(silver$Open)
plot(cooper$Open)
plot(crude_oil$Open)
plot(crude_oil$Open)
plot(natural_gas$Open)


# B SPLINE
# B spline GOLD
n = 63
argvals = seq(0,1,len=n)
x = gold$Open
sigerr = 0.2
y = x + rnorm(x)*sigerr
nbasis = 13
basisobj = create.bspline.basis(c(0,1),nbasis)
plot(basisobj)

ys = smooth.basis(argvals=argvals, y=y,
fdParobj=basisobj)
xfd = ys$fd
plotfit.fd(y, argvals, xfd)


# B spline SILVER
n = 63
argvals = seq(0,1,len=n)
x = silver$Open
sigerr = 0.2
y = x + rnorm(x)*sigerr
nbasis = 13
basisobj = create.bspline.basis(c(0,1),nbasis)
plot(basisobj)

ys = smooth.basis(argvals=argvals, y=y,
fdParobj=basisobj)
xfd = ys$fd
plotfit.fd(y, argvals, xfd)


# B spline COOPER
n = 63
argvals = seq(0,1,len=n)
x = cooper$Open
sigerr = 0.2
y = x + rnorm(x)*sigerr
nbasis = 13
basisobj = create.bspline.basis(c(0,1),nbasis)
plot(basisobj)

ys = smooth.basis(argvals=argvals, y=y,
fdParobj=basisobj)
xfd = ys$fd
plotfit.fd(y, argvals, xfd)


# closing price all
time_numeric <- as.numeric(close_prices$Date - min(close_prices$Date))
yearRng <- c(min(time_numeric), max(time_numeric))
Y <- as.matrix(close_prices[, -1])
Tempbasis <- create.bspline.basis(rangeval = yearRng, nbasis = 20)
Tempfd <- smooth.basis(time_numeric, Y, Tempbasis)$fd
plot(Tempfd, xlab="Time (days)", ylab="Price")
meanTempfd <- mean.fd(Tempfd)

plot(Tempfd, col="grey")
lines(meanTempfd, col="red", lwd=2)

sumTempfd <- sum(Tempfd)

plot(meanTempfd - sumTempfd*(1/ncol(Y)))

Y_index <- sweep(Y, 2, Y[1, ], "/") * 100
Tempfd <- smooth.basis(time_numeric, Y_index, Tempbasis)$fd
plot(Tempfd, xlab="Time", ylab="Index (Base=100)")

#
#komentaras
#komentaras

