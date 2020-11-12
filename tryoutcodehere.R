## try out code here

library(usethis)
## usethis::use_github()  ### run once then comment out - sets the local git repot to Github



## assumptions
# 1. Start with a fixed amount
# 2. Contribute to the fixed amount with reinvestments
# 3. Estimate ROI and net worth yearly
# 4. Determine exit point
# 5. Provide sensitivity analyses

fv <- function(i,r,t){
  out <- i*((1+r)^t)
  return(out)
}



tmpFixed <- data.frame(i=rep(2500000,30),r=5,t=1:30,FV=NA)
tmpFixed$FV <- fv(tmpFixed$i,tmpFixed$r/100,tmpFixed$t)
tmpFixed
library(ggplot2)
ggplot(tmpFixed,aes(x=t,y=FV)) + geom_smooth() + geom_hline(yintercept = 6000000)

tryme <- data.frame(i=2500000,r=rep(5,30),rsens= ((5/100) + (rbeta(30,1,1) - .5) * .6),t=1:30,FV=NA)
#tryme$rsens <- tryme$r/100 + tryme$sens

tryme$FVsens <- NA

for (i in 1:nrow(tryme)){
  if (i == 1) {
    tryme$FV[i] <- fv(tryme$i[i],tryme$rsens[i],1)
  } else {
    tryme$i[i] <- tryme$FV[i-1]
    tryme$FV[i] <- fv(tryme$i[i],tryme$rsens[i],1)
  }
}


tryme

### webscrape the S&P500 data:
library(rvest)

url <- 'https://en.wikipedia.org/wiki/S%26P_500_Index'
url2 <- html('https://en.wikipedia.org/wiki/S%26P_500_Index')

## note, I got the xpath by copying from the wikipedia page and using F12 to find it
sp500.d <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/div[8]/table') %>%
  html_table()
sp500.d <- sp500.d[[1]]

library(psych)
describe(sp500.d)
str(sp500.d)
sp500.d$Yr <- as.numeric(sp500.d$Year)
table(sp500.d$Yr)
sp500.d$YTDret <- sp500.d$`Change in Index`
#sp500.d$YTDret <- as.numeric(sp500.d$`Change in Index`)
#sp500.d$YTDret = as.numeric(gsub("[\\%,]", "", sp500.d$`Change in Index`))


library(dplyr)
library(readr)

d <- sp500.d[-54,10:11]
res <- cbind(d %>% select(Yr),
             d %>% select(-Yr) %>% mutate_all(funs(parse_number)))
describe(res)
res$YTDret
Negivs <- as.numeric(substr(d$YTDret,1,1))
res$YTDret[is.na(Negivs)] <- res$YTDret[is.na(Negivs)]*-1
res$YTDret
mean(res$YTDret)
sd(res$YTDret)
rnorm(1,mean(res$YTDret),sd(res$YTDret))
hist(res$YTDret)

YTDret.ts <- ts(res$YTDret[1:50],1970,2019)
YTDret.ts
plot.ts(YTDret.ts)
library(TTR)

YTDret.sma <- SMA(YTDret.ts,6)
plot(YTDret.sma)

YTDret.decomp <- decompose(YTDret.ts)
YTDret.forecast <- HoltWinters(YTDret.ts, beta=F, gamma=F)
plot(YTDret.forecast)

library(forecast)
YTDret.forecast2 <- forecast(YTDret.forecast, h=30)
YTDret.forecast2



library(ggplot2)
ggplot(res,aes(x=Yr,y=YTDret)) + geom_smooth()

