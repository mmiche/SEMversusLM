# Source: https://benwhalley.github.io/just-enough-r/sem.html

library(lavaan)

tpb.df <- readRDS(file="/Users/mmiche/Desktop/TeachingClass/FS2025/TheorieseminarFS25/LastSeminar/tpb.df.rds")
dim(tpb.df)

# Measurement model and structural model
sem.mod <- '
  # measurement model
  AT =~ a1 + a2 + a3 + sn1
  SN =~ sn1 + sn2 + sn3 + sn4
  PBC =~ pc1 + pc2 + pc3 + pc4 + pc5

  # structual model: multiple linear regression
  intention ~ AT + SN + PBC'
# 1. Fit the model
sem.mod.fit <- lavaan::sem(sem.mod, data=tpb.df)
summary(sem.mod.fit, standardized=TRUE, fit.measures=FALSE, rsquare=TRUE)
# ------------------------
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# intention ~                                                           
# AT                0.428    0.196    2.189    0.029    0.618    0.239
# SN                0.547    0.281    1.948    0.051    0.577    0.224
# PBC               1.130    0.165    6.836    0.000    1.025    0.397
# R-Square:
#                Estimate
# intention         0.350
# ------------------------

# Produce dataset 'tpb.dfLM' according to the measurment model.
tpb.dfLM <- data.frame(
    AT=apply(tpb.df[,c("a1", "a2", "a3", "sn1")], 1, sum),
    SN=apply(tpb.df[,c("sn1", "sn2", "sn3", "sn4")], 1, sum),
    PBC=apply(tpb.df[,c("pc1", "pc2", "pc3", "pc4", "pc5")], 1, sum),
    intention=tpb.df$intention
)

# Use tpb.dfLM for conventional regression, using lavaan::sem
sem.mod1 <- 'intention ~ AT + SN + PBC'
# 2. Fit the model
sem.mod1.fit <- lavaan::sem(sem.mod1, data=tpb.dfLM)
summary(sem.mod1.fit, standardized=TRUE, fit.measures=FALSE, rsquare=TRUE)
# ------------------------
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# intention ~                                                           
# AT                0.187    0.037    5.039    0.000    0.187    0.283
# SN                0.108    0.042    2.559    0.011    0.108    0.144
# PBC               0.213    0.027    7.966    0.000    0.213    0.334
# R-Square:
#                Estimate
# intention         0.272
# ------------------------

# Use standardized tpb.dfLM for conventional regression, using stats::lm.
# 3. Fit the model
lm.mod <- lm(intention ~ ., data=data.frame(scale(tpb.dfLM)))
summary(lm.mod)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# AT          2.834e-01  5.650e-02   5.015 7.89e-07
# SN          1.440e-01  5.655e-02   2.546   0.0113
# PBC         3.336e-01  4.208e-02   7.928 2.11e-14
# Multiple R-squared:  0.2722,	Adjusted R-squared:  0.2669
# -----------------------------------------------------------