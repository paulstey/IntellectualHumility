###
# EFA and CFA for German IH Scale
# Date: Dec. 19, 2015
# Author: Paul Stey
###

library(lavaan)

setwd("~/rwd")

d_raw <- read.csv("ih_explizit_german.csv")

colnames(d_raw) <- tolower(colnames(d_raw))

## quick function to reverse items
rev_score <- function(dat, vars) {
    recoded_mat <- dat
    for (i in vars) {
        recoded_mat[, i] <- 8 - dat[, i]
    }
    return(recoded_mat)
}

## based on English version scale
items_to_reverse <- c(27, 33, 50, 51, 52, 9, 10, 11, 14, 15, 16, 32, 37, 38, 43, 18, 25, 26, 29, 31, 21, 22, 2, 4)

d <- rev_score(d_raw, items_to_reverse)


###
# Here we split the full dataset into two, run an EFA 
# on one half, and the CFA on the other.
###
dc <- d[complete.cases(d), ]

set.seed(675867)                                    # set RNG for reproducibility

efa_rows <- sample(nrow(dc), size = nrow(dc)/2, replace = FALSE)
cfa_rows <- setdiff(1:nrow(dc), efa_rows)

## Run EFA
fm0 <- factanal(dc[efa_rows, ], factors = 8, rotation = "promax")
print(fm0, cutoff = 0.3, digits = 2, sort = TRUE)

# Specify models for CFA using lavaan

## specify model for CFA based on English version
m1 <- "
    f1 =~ ih08 + ih10 + ih11 + ih13 + ih15 + ih32
    f2 =~ ih27 + ih34 + ih35 + ih45 + ih50 + ih51
    f3 =~ ih37 + ih38 + ih39 + ih40 + ih43
    f4 =~ ih19 + ih20 + ih21 + ih22
"

## based on German EFA
m2 <- "
    f1 =~ ih08 + ih11 + ih12 + ih14 + ih15 + ih32 
    f2 =~ ih27 + ih33 + ih51 + ih25 + ih31
    f3 =~ ih37 + ih38 + ih39 + ih40
    f4 =~ ih19 + ih20 + ih23 + ih26 + ih24 + ih35
"

## fit the model
fm1 <- cfa(m1, data = dc[cfa_rows, ], se = "robust", test = "Satorra.Bentler")
summary(fm1, fit.measures = TRUE)

