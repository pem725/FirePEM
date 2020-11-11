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
