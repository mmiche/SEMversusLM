# Source: https://stats.oarc.ucla.edu/r/seminars/rsem/

library(lavaan)

dat <- readRDS(file="~/Desktop/TeachingClass/FS2025/TheorieseminarFS25/LastSeminar/uclarsemData.rds")
dim(dat)

# Measurement model and structural model
# 
m6a <- '
# measurement model
adjust =~ motiv + harm + stabi
risk =~ verbal + ppsych + ses
achieve =~ read + arith + spell
# structual model: multiple linear regression
achieve ~ adjust + risk'
# 1. Fit the model
fit6a <- sem(m6a, data=dat)
summary(fit6a, standardized=TRUE, fit.measures=FALSE, rsquare=TRUE)
# ------------------------
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# achieve ~                                                             
# adjust            0.375    0.046    8.085    0.000    0.372    0.372
# risk              0.724    0.078    9.253    0.000    0.564    0.564
# R-square       Estimate
# achieve           0.653
# ------------------------

# Produce dataset 'datlm' according to the measurment model.
achieve <- apply(dat[,7:9], 1, sum)
risk <- apply(dat[,4:6], 1, sum)
adjust <- apply(dat[,1:3], 1, sum)
datlm <- data.frame(achieve, risk, adjust)

# Use datlm for conventional regression, using lavaan::sem
# 2. Fit the model
fit6aNew <- sem(model='achieve ~ adjust + risk', data=datlm)
summary(fit6aNew, standardized=TRUE, fit.measures=FALSE, rsquare=TRUE)
# ------------------------
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# achieve ~                                                             
# adjust            0.529    0.037   14.358    0.000    0.529    0.502
# risk              0.583    0.066    8.859    0.000    0.583    0.310
# R-Square:      Estimate
# achieve           0.419
# ------------------------

# Use standardized datlm for conventional regression, using stats::lm.
# 3. Fit the model
lm.mod <- lm(achieve ~ adjust + risk, data=data.frame(scale(datlm)))
summary(lm.mod)
# ------------------------
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# adjust       0.5024    0.00351  14.315   <2e-16
# risk         0.3100    0.00351   8.832   <2e-16
# Multiple R-squared:  0.4189,	Adjusted R-squared:  0.4165
# ------------------------