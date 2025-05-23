# https://github.com/benwhalley/just-enough-r/blob/master/cfa-sem.Rmd

library(lavaan)
library(tidyverse)

# -----------------------------------------------
model.tpb.gen <- '
  AT =~ a1 + .7*a2+ .5*a3+ .4*sn1 + .4*sn2
  SN =~ sn1 + .45*sn2 + .5*sn3 + .6*sn4 + .5*a1
  PBC =~ pc1 + .8*pc2 + .7*pc3 + .4*pc4 + .6*pc5
  intention ~ .3*AT + .3*SN + .5*PBC
  exercise ~ 1*intention + .6*PBC
  AT ~~ .3 * SN
'

set.seed(1234)
drop5pc <- function(l) ifelse(rbinom(length(l), 1, .05)==1, NA,  l)

# I updated the original code (e.g., mutate(across) instead of mutate_each)
tpb.df0 <- lavaan::simulateData(model.tpb.gen, sample.nobs=487,  debug=F) %>%
    rowwise() %>%
    mutate(
        exercise = max(0, round(exercise * 10 + 80)),
        intention = max(0, intention * 2 + 10)
    ) %>%
    mutate(across(.cols=c("a1", "a2", "sn4", "intention"),
                  .fns=drop5pc))

dim(tpb.df0)
names(tpb.df0)

# Remove lines with at least one missing value:
tpb.df <- na.omit(tpb.df0)
dim(tpb.df)
# -----------------------------------------------