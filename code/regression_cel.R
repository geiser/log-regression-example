#' ---
#' title: "Linear and logist regression analysis of internet access"
#' author: Geiser C. Challco <geiser@alumni.usp.br>
#' comment: This file is automatically generate by Shiny-Statistic app (https://statistic.geiser.tech/)
#'          Author - Geiser C. Challco <geiser@alumni.usp.br>
#'          
#'          Shiny-Statistic is distributed in the hope that it will be useful,
#'          but WITHOUT ANY WARRANTY; without even the implied warranty of
#'          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'          GNU General Public License for more details.
#'          
#'          You should have received a copy of the GNU General Public License.
#'          If not, see <https://www.gnu.org/licenses/>.
#' output:
#'   html_document:
#'     toc: true
#'   github_document:
#'     toc: true
#'   word_document:
#'     toc: true
#' fontsize: 10pt
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------------------
## Install and Load Packages and functions
wants <- c('readxl','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])


get_data <- function(dat, grp, is.mean = F) {
  library(dplyr)

  years <- colnames(dplyr::select(dat, dplyr::starts_with("199"), dplyr::starts_with("20")))
  dat2 <-do.call(rbind, lapply(years, FUN = function(year) {
    values <- dat[[year]][which(dat$IncomeGroup == grp)]
    values <- values[!is.na(values)]
    if (is.mean) {
      data.frame(year=as.integer(year), IT.NET.USER.ZS=mean(values))
    } else {
      do.call(rbind, lapply(values, FUN = function(val) {
        data.frame(year=as.integer(year), IT.NET.USER.ZS=val)
      }))
    }
  }))

  return(dat2)
}


library(readxl)
library(dplyr)

#' 

#' 
#' ## Initial Variables and Data
#' 
#' 
## ------------------------------------------------------------------------------------------------
dat <- read_excel("../data/API_IT.NET.USER.ZS_DS2_en_excel_v2_4578034.xlsx", sheet = "Data")

lmdls <- c()
smdls <- c()

income_grp <- unique(dat$IncomeGroup)
(income_grp <- income_grp[!is.na(income_grp)])

#' 
#' ## Calculating data with mean values
#' 
## ------------------------------------------------------------------------------------------------
df <- get_data(dat, income_grp[1], is.mean = T)
for (i in 2:length(income_grp)) {
  grp = income_grp[i]
  df <- merge(df, get_data(dat, grp, is.mean = T), by="year", suffixes = c(income_grp[i-1], grp))
}
colnames(df) <- c("year",income_grp)

knitr::kable(df)

#' 
#' ## Linear Regression
#' 
#' ### Linear regression for high income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[1]
dat2 <- get_data(dat, grp)

fit <- lm(IT.NET.USER.ZS ~ year, data=dat2)
lmdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), max(dat2$year), 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.5)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.5)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1)

#' 
#' ### Linear regression for upper middle income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[4]
dat2 <- get_data(dat, grp)

fit <- lm(IT.NET.USER.ZS ~ year, data=dat2)
lmdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2040, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.5)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.5)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1)

#' 
#' ### Linear regression for lower middle income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[3]
dat2 <- get_data(dat, grp)

fit <- lm(IT.NET.USER.ZS ~ year, data=dat2)
lmdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2070, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.75)
axis(1, at = seq(min(pyear), max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' ### Linear regression for low income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[2]
dat2 <- get_data(dat, grp)

fit <- lm(IT.NET.USER.ZS ~ year, data=dat2)
lmdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2190, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.75)
axis(1, at = seq(min(pyear), max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' ### Summary of linear regression with average values
#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(1990, 2190, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100),
     ylab = "IT.NET.USER.ZS", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

colors <- c("blue","red","orange","green")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = T)
  points(x=gdat$year, y=gdat$IT.NET.USER.ZS, pch=16, col=colors[grp], xaxt='n', yaxt='n', cex=0.75)
  matlines(pyear, predict(lmdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=1)
}
legend("bottomright", legend=c("high income","low income","lower middle income","upper middle income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(1990, 2190, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100),
     ylab = "IT.NET.USER.ZS", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

colors <- c("blue","red","orange","green")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = F)
  points(x=gdat$year, y=gdat$IT.NET.USER.ZS, pch=16, col=colors[grp], xaxt='n', yaxt='n', cex=0.25)
  matlines(pyear, col = colors[grp], predict(lmdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=1)
}
legend("bottomright", legend=c("high income","low income","lower middle income","upper middle income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
#' ## Logistic Regression
#' 
#' ### Logistic regression for high income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[1]
dat2 <- get_data(dat, grp)

fit <- nls(IT.NET.USER.ZS ~ SSlogis(year, Asym, xmid, scal), data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
Asym = fit$m$getAllPars()["Asym"]
xmid = fit$m$getAllPars()["xmid"]
scal = fit$m$getAllPars()["scal"]

(min(dat2$year)+(xmid-min(dat2$year))*100/Asym)

#' 
## ------------------------------------------------------------------------------------------------
fit <- nls(IT.NET.USER.ZS ~ 100/(1+exp(-1*(year-2007.214)/scal)), data = dat2, start = list(scal=scal))
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
pyear <- seq(max(dat2$year), max(dat2$year)+35, 1)
myear <- min(pyear)+sum(predict(fit, newdata=list(year=pyear)) < 99.5)
pyear <- seq(myear-2,myear+2, 1)
df <- predict(fit, newdata=list(year=pyear), interval = "confidence")
names(df) <- pyear
(df)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2036, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 2), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' 
#' ### Logistic regression for upper middle income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[4]
dat2 <- get_data(dat, grp)

fit <- nls(IT.NET.USER.ZS ~ SSlogis(year, Asym, xmid, scal), data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
Asym = fit$m$getAllPars()["Asym"]
xmid = fit$m$getAllPars()["xmid"]
scal = fit$m$getAllPars()["scal"]

(min(dat2$year)+(xmid-min(dat2$year))*100/Asym)

#' 
## ------------------------------------------------------------------------------------------------
fit <- nls(IT.NET.USER.ZS ~ 100/(1+exp(-1*(year-2016.724)/scal)), data = dat2, start = list(scal=scal))
smdls[[grp]] <- fit
summary(fit)

#' 
#' 
## ------------------------------------------------------------------------------------------------
pyear <- seq(max(dat2$year), max(dat2$year)+50, 1)
myear <- min(pyear)+sum(predict(fit, newdata=list(year=pyear)) < 99.5)
pyear <- seq(myear-2,myear+2, 1)
df <- predict(fit, newdata=list(year=pyear), interval = "confidence")
names(df) <- pyear
(df)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2050, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 4), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' 
#' ### Logistic regression for lower middle income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[3]
dat2 <- get_data(dat, grp)

fit <- nls(IT.NET.USER.ZS ~ SSlogis(year, Asym, xmid, scal), data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
Asym = fit$m$getAllPars()["Asym"]
xmid = fit$m$getAllPars()["xmid"]
scal = fit$m$getAllPars()["scal"]

(min(dat2$year)+(xmid-min(dat2$year))*100/Asym)

#' 
## ------------------------------------------------------------------------------------------------
fit <- nls(IT.NET.USER.ZS ~ 100/(1+exp(-1*(year-2021.747)/scal)), data = dat2, start = list(scal=scal))
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
pyear <- seq(max(dat2$year), max(dat2$year)+150, 1)
myear <- min(pyear)+sum(predict(fit, newdata=list(year=pyear)) < 99.5)
pyear <- seq(myear-2,myear+2, 1)
df <- predict(fit, newdata=list(year=pyear), interval = "confidence")
names(df) <- pyear
(df)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2058, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 4), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' 
#' ### Logistic regression for low income countries
#' 
## ------------------------------------------------------------------------------------------------
grp = income_grp[2]
dat2 <- get_data(dat, grp)

fit <- nls(IT.NET.USER.ZS ~ SSlogis(year, Asym, xmid, scal), data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
Asym = fit$m$getAllPars()["Asym"]
xmid = fit$m$getAllPars()["xmid"]
scal = fit$m$getAllPars()["scal"]

(min(dat2$year)+(xmid-min(dat2$year))*100/Asym)

#' 
## ------------------------------------------------------------------------------------------------
fit <- nls(IT.NET.USER.ZS ~ 100/(1+exp(-1*(year-2041.83)/scal)), data = dat2, start = list(scal=scal))
smdls[[grp]] <- fit
summary(fit)

#' 
## ------------------------------------------------------------------------------------------------
pyear <- seq(max(dat2$year), max(dat2$year)+150, 1)
myear <- min(pyear)+sum(predict(fit, newdata=list(year=pyear)) < 99.5)
pyear <- seq(myear-2,myear+2, 1)
df <- predict(fit, newdata=list(year=pyear), interval = "confidence")
names(df) <- pyear
(df)

#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(min(dat2$year), 2100, 1)
plot(dat2, xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     pch=16, col="blue", main = grp, xaxt='n',yaxt='n', cex = 0.5)
axis(1, at = seq(min(pyear), max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)

#' 
#' ### Summary of logistic regression with average values
#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(1990, 2100, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100),
     ylab = "IT.NET.USER.ZS", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

colors <- c("blue","red","orange","green")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = T)
  points(x=gdat$year, y=gdat$IT.NET.USER.ZS, pch=16, col=colors[grp], xaxt='n', yaxt='n', cex=0.75)
  matlines(pyear, predict(smdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=1)
}
legend("bottomright", legend=c("high income","low income","lower middle income","upper middle income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
#' 
## ---- dpi=300, fig.width=12, fig.height=8--------------------------------------------------------
pyear <- seq(1990, 2100, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100),
     ylab = "IT.NET.USER.ZS", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 5), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

colors <- c("blue","red","orange","green")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = F)
  points(x=gdat$year, y=gdat$IT.NET.USER.ZS, pch=16, col=colors[grp], xaxt='n', yaxt='n', cex=0.25)
  matlines(pyear, col = colors[grp], predict(smdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=1)
}
legend("bottomright", legend=c("high income","low income","lower middle income","upper middle income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
