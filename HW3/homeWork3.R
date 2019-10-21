rm(list=ls(all=TRUE))

# ---- load-packages --------------------------------------------------------
library("tidyverse")
setwd("~/Documents/Psych5003/HW3")


# ---- load-data ----------------------------------------------------------------------
in.data <- read.csv("../HW1/salary.csv")

# ---- q-1b ----------------------------------------------------------------------
out.val <- t.test(salary ~ gender, data=in.data)
to.report.stat <- out.val$statistic
to.report.prob <- out.val$p.value

# ---- q-1c ----------------------------------------------------------------------
out.val <- var.test(salary ~ gender, data=in.data)
to.report.stat <- out.val$statistic
to.report.prob <- out.val$p.value

# ---- q-2b ----------------------------------------------------------------------
age.group <- c(rep("Young Adult", 4), rep("Adult", 4), rep("Older Adult", 4))
wais.vals <- c(17,21,20,18,16,12,14,14,9,11,8,8)
in.data <- data.frame(age.group, wais.vals)
out.mod <- aov(wais.vals ~ age.group, data=in.data)
to.report <- summary(out.mod)
to.report.stat <- as.matrix(to.report)[[1]][1,4]
to.report.prob <- as.matrix(to.report)[[1]][1,5]