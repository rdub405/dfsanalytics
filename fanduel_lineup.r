library(lpSolve)
library(dplyr)
library('knitr')
library(dummies)


fd <- read.csv("C:\\model\\FanDuel.csv")
fd$Id <- NULL
fd$Injury.Details <-NULL
fd$X <- NULL
fd$X.1 <- NULL
fd$Tier <- NULL
fd <- fd %>% filter(fd$Injury.Indicator != 'O')

fd <- fd[order(fd[, "Position"]), ]

f.obj <- fd[, "FPPG"]

Position.Mat <- dummy(fd[, "Position"])
colnames(Position.Mat) <- levels(fd[, "Position"])

f.obj <- fd[, "FPPG"]

f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
colnames(f.con) <- fd$Name
kable(f.con, format = "markdown", row.names = T)

# Instantiate the vectors
f.dir <- rep(0, nrow(f.con))
f.rhs <- rep(0, nrow(f.con))

f.dir[1] <- "<="
f.rhs[1] <- 60000

#center postion
f.dir[2] <- "="
f.rhs[2] <- 1

f.dir[3:nrow(f.con)] <- c("=", "=", "=", "=", "=", "=")
f.rhs[3:nrow(f.con)] <- c(2,2, 2, 2, 2, 2)

kable(data.frame(f.con, f.dir, f.rhs), format = "markdown", row.names = T)

opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
picks <- fd[which(opt$solution == 1), ]
kable(picks, format = "markdown", row.names = F)
