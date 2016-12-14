###
# EFA and CFA for German IH Scale
# Date: Dec. 19, 2015
# Author: Paul Stey
###

library(lavaan)

d <- read.csv("ih_explizit_german.csv")

set.seed(675867)

efa_rows <- sample(nrow(d), size = nrow(d)/2, replace = FALSE)
cfa_rows <- setdiff(1:nrow(d), efa_rows)
colnames(d) <- tolower(colnames(d))

d1 <- d[efa_rows, ]
fm0 <- factanal(d1[complete.cases(d1), ], factors = 8, rotation = "promax")


###
# checking English version's fit
###
rev_items <- c(8, 13, 24, 27, 37, 38, 43, 50, 51)

rev_score <- function(dat, vars) {
    recoded_mat <- dat

    for (i in vars) {
        recoded_mat[, i] <- 8 - dat[, i]
    }
    return(recoded_mat)
}


###
# Fitting an exploratory factor model
###
m1 <- "
    fact1 =~ ih01 + ih02 + ih03 + ih04 + ih05 + ih06 + ih07 + ih08 + ih09 + ih10 + ih11 + ih12 + ih13 + ih14 + ih15 + ih16 + ih17 + ih18 + ih19 + ih20 + ih21 + ih22 + ih23 + ih24 + ih25 + ih26 + ih27 + ih28 + ih29 + ih30 + ih31 + ih32 + ih33 + ih34 + ih35 + ih36 + ih37 + ih38 + ih39 + ih40 + ih41 + ih42 + ih43 + ih44 + ih45 + ih46 + ih47 + ih48 + ih49 + ih50 + ih51 + ih52
    fact2 =~ ih01 + ih02 + ih03 + ih04 + ih05 + ih06 + ih07 + ih08 + ih09 + ih10 + ih11 + ih12 + ih13 + ih14 + ih15 + ih16 + ih17 + ih18 + ih19 + ih20 + ih21 + ih22 + ih23 + ih24 + ih25 + ih26 + ih27 + ih28 + ih29 + ih30 + ih31 + ih32 + ih33 + ih34 + ih35 + ih36 + ih37 + ih38 + ih39 + ih40 + ih41 + ih42 + ih43 + ih44 + ih45 + ih46 + ih47 + ih48 + ih49 + ih50 + ih51 + ih52
    fact3 =~ ih01 + ih02 + ih03 + ih04 + ih05 + ih06 + ih07 + ih08 + ih09 + ih10 + ih11 + ih12 + ih13 + ih14 + ih15 + ih16 + ih17 + ih18 + ih19 + ih20 + ih21 + ih22 + ih23 + ih24 + ih25 + ih26 + ih27 + ih28 + ih29 + ih30 + ih31 + ih32 + ih33 + ih34 + ih35 + ih36 + ih37 + ih38 + ih39 + ih40 + ih41 + ih42 + ih43 + ih44 + ih45 + ih46 + ih47 + ih48 + ih49 + ih50 + ih51 + ih52
    fact4 =~ ih01 + ih02 + ih03 + ih04 + ih05 + ih06 + ih07 + ih08 + ih09 + ih10 + ih11 + ih12 + ih13 + ih14 + ih15 + ih16 + ih17 + ih18 + ih19 + ih20 + ih21 + ih22 + ih23 + ih24 + ih25 + ih26 + ih27 + ih28 + ih29 + ih30 + ih31 + ih32 + ih33 + ih34 + ih35 + ih36 + ih37 + ih38 + ih39 + ih40 + ih41 + ih42 + ih43 + ih44 + ih45 + ih46 + ih47 + ih48 + ih49 + ih50 + ih51 + ih52
"

## fit the model, and check the fit
fm1 <- cfa(m1, data = d, missing = 'fiml')

summary(fm1, fit.measures = TRUE)
modindices(fm1)


m2 <- "
    f1 =~ ih10 + ih12 + ih13 + ih41 + ih42 + ih44 + ih46
    f2 =~ ih26 + ih30 + ih31 + ih33 + ih35 + ih50 + ih51
    f3 =~ ih37 + ih38 + ih39 + ih40
    f4 =~ ih19 + ih20 + ih21 + ih22 + ih24 + ih47
"

fm2 <- cfa(m2, data = d2, se = "robust", test = "Satorra.Bentler", missing = 'fiml')

summary(fm2, fit.measures = TRUE)

items_to_reverse <- c("ih12", "ih13", "ih41", "ih42", "ih44", "ih46", "ih35", "ih39", "ih40", "ih21", "ih22")
ds1 <- rev_score(d2, items_to_reverse)
fm3 <- cfa(m2, data = ds1, se = "robust", test = "Satorra.Bentler", missing = 'fiml')

summary(fm3, fit.measures = TRUE)

# inspect(fm2, "cov.lv")


m2 <- "
    f1 =~ ih18 + ih24 + ih25 + ih26 + ih29 + ih31
    f2 =~ ih08 + ih10 + ih11 + ih13 + ih15 + ih32
    f3 =~ ih27 + ih34 + ih35 + ih45 + ih50 + ih51
    f4 =~ ih37 + ih38 + ih39 + ih40 + ih43

    f1 ~~
        con*f3 +
        con*f4

    f2 ~~
        con*f3 +
        con*f4 
    con > 0
    f3 ~~ con*f3
"

items_to_reverse <- c("ih24", "ih10", "ih11", "ih15", "ih32", "ih34", "ih35", "ih45",  "ih39", "ih40")
ds1 <- rev_score(d2, items_to_reverse)


m3 <- "
    f1 =~ ih18 + ih24 + ih25 + ih26 + ih29 + ih31 + ih33
    f2 =~ ih08 + ih10 + ih11 + ih13 + ih15 + ih32
    f3 =~ ih27 + ih34 + ih35 + ih45 + ih50 + ih51
    f4 =~ ih37 + ih38 + ih39 + ih40 + ih43
"

m4 <- "
    f1 =~ ih18 + ih25 + ih26 + ih27 + ih29 + ih31 + ih33
    f2 =~ 
    f3 =~ 
    f4 =~ 
"

fm2 <- cfa(m3, data = ds1, se = "robust", test = "Satorra.Bentler", missing = "fiml")
summary(fm2, fit.measures = TRUE)
inspect(fm2, "cov.lv")
