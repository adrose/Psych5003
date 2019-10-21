rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
setwd("~/Documents/Psych5003/HW3")


# ---- load-data ----------------------------------------------------------------------
in.data <- read.csv("../HW1/salary.csv")

# ---- create-functions ----------------------------------------------------------------------
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}


# ---- q-1a ----------------------------------------------------------------------
mean.se.vals <- summarySE(data = in.data, measurevar = 'salary', groupvars = 'gender')
## Declare our variables we will need to calculate our t statistic
m.m <- mean.se.vals$salary[2]
m.sd <- mean.se.vals$sd[2]
m.size <- mean.se.vals$N[2]
f.m <- mean.se.vals$salary[1]
f.sd <- mean.se.vals$sd[1]
f.size <- mean.se.vals$N[1]

## Now calc the t stat
pooled.standard <- sqrt(((m.size-1)*m.sd^2+(f.size-1)*f.sd^2)/(m.size+f.size-2))
t.stat.by.hand <- (m.m-f.m)/(pooled.standard*sqrt(1/m.size+1/f.size))

## Now calulate the p value for the t statistic
t.stat.by.hand.p.value <- pt(t.stat.by.hand, df=298, lower.tail = F)

# ---- q-1b ----------------------------------------------------------------------
out.val <- t.test(salary ~ gender, data=in.data, var.equal=T)
to.report.stat <- out.val$statistic
to.report.prob <- out.val$p.value
print(to.report.stat)
print(to.report.prob)

# ---- q-1c ----------------------------------------------------------------------
out.val <- var.test(salary ~ gender, data=in.data)
to.report.stat <- out.val$statistic
to.report.prob <- out.val$p.value

# ---- q-2a ----------------------------------------------------------------------
age.group <- c(rep("Young Adult", 4), rep("Adult", 4), rep("Older Adult", 4))
wais.vals <- c(17,21,20,18,16,12,14,14,9,11,8,8)
in.data <- data.frame(age.group, wais.vals)
in.data.sum <- summarySE(data=in.data, measurevar = 'wais.vals', groupvars = 'age.group')
in.data$SumSquaresW <- NA
in.data$SumSquaresW[1:4] <- (in.data$wais.vals[1:4] - in.data.sum$wais.vals[which(in.data.sum$age.group=='Young Adult')])^2
in.data$SumSquaresW[5:8] <- (in.data$wais.vals[5:8] - in.data.sum$wais.vals[which(in.data.sum$age.group=='Adult')])^2
in.data$SumSquaresW[9:12] <- (in.data$wais.vals[9:12] - in.data.sum$wais.vals[which(in.data.sum$age.group=='Older Adult')])^2
in.data$SumSquaresB <- (in.data$wais.vals - mean(in.data$wais.vals))^2
sum.of.squares.between <- sum(in.data$SumSquaresB) - sum(in.data$SumSquaresW)
sum.of.squares.within <- sum(in.data$SumSquaresW)
mean.squares.between <- sum.of.squares.between / (3 - 1)
mean.squares.within <- sum.of.squares.within / (12 - 3)
f.stat <- mean.squares.between / mean.squares.within
f.stat.p.val <- pf(q=f.stat, df1=2, df2=9, lower.tail = F)

# ---- q-2b ----------------------------------------------------------------------
out.mod <- aov(wais.vals ~ age.group, data=in.data)
to.report <- summary(out.mod)
to.report.stat <- as.matrix(to.report)[[1]][1,4]
to.report.prob <- as.matrix(to.report)[[1]][1,5]

# ---- q-3a ----------------------------------------------------------------------
diff.vec <- c(4,4,1,2,-3,5,3,2,-4,2,1,-1,2,7,0,4,6,3,4,-1)
mean.diff <- mean(diff.vec)
sd.diff <- sd(diff.vec)
se.diff <- sd.diff / sqrt(length(diff.vec))
t.stat <- mean.diff / se.diff
t.stat.p.val <- pt(t.stat, df=19, lower.tail = F)