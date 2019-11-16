rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("foreign")
library("knitr")
library("reshape2")
setwd("~/Documents/Psych5003/HW4")


# ---- load-data ----------------------------------------------------------------------
in.data <- read.spss("./two_way.sav",to.data.frame = T)

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

# ---- create-marginal-means ----------------------------------------------------------------------
base.table <- dcast(data = summarySE(data = in.data, measurevar = 'score', groupvars = c('sex', 'drug'))[,c(1,2,4)], value.var = 'score', formula =  sex~ drug)
base.table.col <- c(rowMeans(base.table[,-1]),NA)
base.table.row <- c(colMeans(base.table),NA)
out.matrix <- matrix(NA, nrow = 3, ncol = 7)
for(i in 1:6){
  for(j in 1:2){
    out.matrix[j,i] <- base.table[j,i] 
  }
}
out.matrix[3,]  <- base.table.row
out.matrix[,7] <- base.table.col
out.matrix[3,7] <- mean(out.matrix[1:2,7])

## now report in a pretty format
out.matrix[,1] <- c("Male","Female","Means")

## Now print it out
kable(x = out.matrix, digits = 2, col.names = c("Sex", "1", "2", "3", "4", "5", "Means"))

# ---- plot-data ---------------------------------------------------------------------------------
plot.data <- summarySE(data = in.data, measurevar = 'score', groupvars = c('sex', 'drug'))
plot.data$sex <- factor(plot.data$sex)
in.data$sex <- factor(in.data$sex)
out.plot <- ggplot(data = in.data, aes(x=drug,y=score, group=sex, color=sex)) + 
    geom_point() + 
    geom_point(data=plot.data, aes(x=drug,y=score,group=sex, color=sex), size=8) +
    geom_errorbar(data=plot.data, mapping=aes(x=drug, ymin=score-se, ymax=score+se, group=sex, color=sex), width=0.2, size=1) +
    theme_bw() + scale_colour_grey()
out.plot


# ---- run-aov ---------------------------------------------------------------------------------
in.data$sex <- factor(in.data$sex)
in.data$drug <- factor(in.data$drug)
out.model <- aov(score ~ sex * factor(drug), data=in.data)
summary(out.model)

# ---- run-simple-effect ---------------------------------------------------------------------------------
run.1 <- t.test(score ~ sex, in.data[which(in.data$drug==1),])
run.2 <- t.test(score ~ sex, in.data[which(in.data$drug==2),])
run.3 <- t.test(score ~ sex, in.data[which(in.data$drug==3),])
run.4 <- t.test(score ~ sex, in.data[which(in.data$drug==4),])
run.5 <- t.test(score ~ sex, in.data[which(in.data$drug==5),])
