#' ---
#' title: "Linear regression for Information and Comunication Technology (ICT) Skill"
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
#'   github_document:
#'     toc: true
#'   html_document:
#'     toc: true
#'   word_document:
#'     toc: true
#' fontsize: 10pt
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
## Install and Load Packages and functions
wants <- c('readxl','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])


get_data <- function(dat, grp, is.mean = F, extra.fields=c()) {
  library(dplyr)

  years <- sort(unique(dat$year))
  dat2 <- do.call(rbind, lapply(years, FUN = function(year) {
    idx <- which(dat$IncomeGroup == grp & dat$year == year & !is.na(dat$value))
    if (length(idx) == 0) return(NULL)
    values <- dat$value[idx]
    if (is.mean) {
      data.frame(year=as.integer(year), ICT.SKILL=mean(values))
    } else {
      toReturn <- data.frame(year=as.integer(year), ICT.SKILL=values)
      if (length(extra.fields)>0) {
        toReturn <- cbind(toReturn, dat[idx,extra.fields])
        names(toReturn) <- c("year","ICT.SKILL", extra.fields)
      }
      toReturn
    }
  }))

  return(dat2)
}


library(readxl)
library(dplyr)
library(aomisc)

#' 

#' 
#' ## Initial Variables and Data
#' 
## ---------------------------------------------------------------------------------------------------------------------------
mtd.df <- read_excel("../data/ICT_and_Digital_Skill.xlsx", sheet = "metadata")
GCI.df <- read_excel("../data/ICT_and_Digital_Skill.xlsx", sheet = "Digital Skill value")
DSGI.df <- read_excel("../data/ICT_and_Digital_Skill.xlsx", sheet = "DSGI-2021")
CIL.df <- read_excel("../data/ICT_and_Digital_Skill.xlsx", sheet = "ICILS (CIL)")

#' ### getting DSGI
#' 
## ---------------------------------------------------------------------------------------------------------------------------
dsgi.idx <- which(mtd.df$`Indicator Id`=="DSGI")
dsgi.val <- 100*(DSGI.df$DSGI - mtd.df$min[dsgi.idx])/(mtd.df$max[dsgi.idx] - mtd.df$min[dsgi.idx])

dat <- data.frame(country=DSGI.df$Country,value=dsgi.val,year=2021,IncomeGroup=DSGI.df$IncomeGroup, indicator="DSGI")

#' 
#' ### getting GCI 4.0 (Digital Skills)
#' 
## ---------------------------------------------------------------------------------------------------------------------------
gci.idx <-which(mtd.df$`Indicator Id`=="41400")
gci.val <- 100*(GCI.df$`2017` - mtd.df$min[gci.idx])/(mtd.df$max[gci.idx] - mtd.df$min[gci.idx])

dat <- rbind(
  dat,
  data.frame(country=GCI.df$`Country Name`,value=gci.val,year=2017,IncomeGroup=GCI.df$IncomeGroup, indicator="GCI")
)

gci.val <- 100*(GCI.df$`2018` - mtd.df$min[gci.idx])/(mtd.df$max[gci.idx] - mtd.df$min[gci.idx])

dat <- rbind(
  dat,
  data.frame(country=GCI.df$`Country Name`,value=gci.val,year=2018,IncomeGroup=GCI.df$IncomeGroup, indicator="GCI")
)

gci.val <- 100*(GCI.df$`2019` - mtd.df$min[gci.idx])/(mtd.df$max[gci.idx] - mtd.df$min[gci.idx])

dat <- rbind(
  dat,
  data.frame(country=GCI.df$`Country Name`,value=gci.val,year=2019,IncomeGroup=GCI.df$IncomeGroup, indicator="GCI")
)

#' 
#' ### getting CIL (ICILS)
#' 
## ---------------------------------------------------------------------------------------------------------------------------
cli.idx <-which(mtd.df$`Indicator Id`=="CIL")
cli.val <- 100*(CIL.df$CIL - mtd.df$min[cli.idx])/(mtd.df$max[cli.idx] - mtd.df$min[cli.idx])

dat <- rbind(
  dat,
  data.frame(country=CIL.df$country,value=cli.val,year=CIL.df$year,IncomeGroup=CIL.df$IncomeGroup, indicator="CIL")
)

dat <- dat[!is.na(dat$IncomeGroup) & !is.na(dat$value),]


lmdls <- c()
smdls <- c()

income_grp <- unique(dat$IncomeGroup)
(income_grp <- income_grp[!is.na(income_grp)])

#' 
#' ## Calculating data with mean values
#' 
## ---------------------------------------------------------------------------------------------------------------------------
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
## ---------------------------------------------------------------------------------------------------------------------------
grp = income_grp[1]
dat2 <- get_data(dat, grp, F, c("indicator"))

fit <- lm(ICT.SKILL ~ year, data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat2$year), 2030, 1)

inds <- list("CIL"=c(17,"red"), "GCI"=c(16,"blue"), "DSGI"=c(18,"purple"))
plot(c(), c(), xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     main = grp, xaxt='n',yaxt='n', xlab = "year", ylab = "ICT.SKILL")
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
for (ind in names(inds)) {
  points(dat2$year[which(dat2$indicator==ind)], dat2$ICT.SKILL[which(dat2$indicator==ind)],
         pch=as.integer(inds[[ind]][1]), col=inds[[ind]][2], cex=0.5)
}
matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)
legend("bottomright", legend=names(inds), col=c("red","blue","purple"), lty=0,  cex = 0.75, bg = "transparent", pch=c(17,16,18), box.lty=0)

#' 
#' ### Linear regression for upper middle income countries
#' 
## ---------------------------------------------------------------------------------------------------------------------------
grp = income_grp[2]
dat2 <- get_data(dat, grp, F, c("indicator"))

fit <- lm(ICT.SKILL ~ year, data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat2$year), 2030, 1)

inds <- list("CIL"=c(17,"red"), "GCI"=c(16,"blue"), "DSGI"=c(18,"purple"))
plot(c(), c(), xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     main = grp, xaxt='n',yaxt='n', xlab = "year", ylab = "ICT.SKILL")
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
for (ind in names(inds)) {
  points(dat2$year[which(dat2$indicator==ind)], dat2$ICT.SKILL[which(dat2$indicator==ind)],
         pch=as.integer(inds[[ind]][1]), col=inds[[ind]][2], cex=0.5)
}
matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)
legend("bottomright", legend=names(inds), col=c("red","blue","purple"), lty=0,  cex = 0.75, bg = "transparent", pch=c(17,16,18), box.lty=0)

#' 
#' ### Linear regression for lower middle income countries
#' 
## ---------------------------------------------------------------------------------------------------------------------------
grp = income_grp[3]
dat2 <- get_data(dat, grp, F, c("indicator"))

fit <- lm(ICT.SKILL ~ year, data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat2$year), 2030, 1)

inds <- list("CIL"=c(17,"red"), "GCI"=c(16,"blue"), "DSGI"=c(18,"purple"))
plot(c(), c(), xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     main = grp, xaxt='n',yaxt='n', xlab = "year", ylab = "ICT.SKILL")
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
for (ind in names(inds)) {
  points(dat2$year[which(dat2$indicator==ind)], dat2$ICT.SKILL[which(dat2$indicator==ind)],
         pch=as.integer(inds[[ind]][1]), col=inds[[ind]][2], cex=0.5)
}
matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)
legend("bottomright", legend=names(inds), col=c("red","blue","purple"), lty=0,  cex = 0.75, bg = "transparent", pch=c(17,16,18), box.lty=0)

#' 
#' ### Linear regression for low income countries
#' 
## ---------------------------------------------------------------------------------------------------------------------------
grp = income_grp[4]
dat2 <- get_data(dat, grp, F, c("indicator"))

fit <- lm(ICT.SKILL ~ year, data=dat2)
smdls[[grp]] <- fit
summary(fit)

#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat2$year), 2030, 1)

inds <- list("CIL"=c(17,"red"), "GCI"=c(16,"blue"), "DSGI"=c(18,"purple"))
plot(c(), c(), xlim = c(min(pyear), max(pyear)), ylim = c(0,100),
     main = grp, xaxt='n',yaxt='n', xlab = "year", ylab = "ICT.SKILL")
axis(1, at = seq(min(pyear), max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
for (ind in names(inds)) {
  points(dat2$year[which(dat2$indicator==ind)], dat2$ICT.SKILL[which(dat2$indicator==ind)],
         pch=as.integer(inds[[ind]][1]), col=inds[[ind]][2], cex=0.5)
}
matlines(pyear, predict(fit, newdata=list(year=pyear), interval="confidence"), lwd=1.25)
legend("bottomright", legend=names(inds), col=c("red","blue","purple"), lty=0,  cex = 0.75, bg = "transparent", pch=c(17,16,18), box.lty=0)

#' 
#' ### Summary of linear regression with average values
#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat$year), 2030, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100), ylab = "ICT.SKILL", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)


colors <- c("blue","green","orange","red")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = T)
  points(x=gdat$year, y=gdat$ICT.SKILL, pch=16, col=colors[grp], xaxt='n', yaxt='n', cex=1)
  matlines(pyear, predict(smdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=0.75)
}
legend("bottomright", legend=c("high income","upper middle income","lower middle income","low income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
#' 
## ---- dpi=300, fig.width=12, fig.height=8-----------------------------------------------------------------------------------
pyear <- seq(min(dat$year), 2030, 1)
plot(x=pyear, y=c(), xlim = c(min(pyear), max(pyear)), ylim=c(0,100), ylab = "ICT.SKILL", xlab = "year", xaxt='n',yaxt='n')
axis(1, at = seq(min(pyear),max(pyear), 1), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)
axis(2, at = seq(0, 100, 10), tck = 1, lty = 4, col = "lightgray", lwd = 0.75)

colors <- c("blue","green","orange","red")
names(colors) <- income_grp
for (grp in income_grp) {
  gdat <- get_data(dat, grp, is.mean = F)
  points(x=gdat$year, y=gdat$ICT.SKILL, pch=16, col=as.character(colors[grp]), xaxt='n', yaxt='n', cex=0.75)
  matlines(pyear, col = colors[grp], predict(smdls[[grp]], newdata=list(year=pyear), interval="confidence"), lwd=1.25)
}
legend("bottomright", legend=c("high income","upper middle income","lower middle income","low income"),
       col=colors, lty=0,  cex = 0.75, bg = "transparent", pch=16, box.lty=0)

#' 
#' 
