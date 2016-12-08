###
# Intellectual Humility Project
# Study 1a: Exploratory Factor Analysis
# January 14, 2014
# Author: Paul Stey
###

## set working directory
setwd('~/Rwd')

## read in raw data set
d_raw <- read.csv('IH_study1_gvsu.csv')

## read in item information (i.e., names and full text) for IH scale items
item_info <- read.csv('ih_explct_items.csv', header = FALSE, stringsAsFactors = FALSE)

## extract Explicit IH scale items
d <- d_raw[, 1:52]

# Write quick function to recode negatively scored items
RevScore <- function(dat, vars){
	## bookkeeping
	recoded_mat <- dat

	## loop to recode items
	for(i in vars){
		recoded_mat[, i] <- 8 - dat[, i]
	}
	return(recoded_mat)
}

## specify items to be recoded
rc_vars <- c(1, 3, 5, 6, 7, 9, 10, 11, 14, 15, 16, 21, 25, 26, 27, 29, 30, 31, 32, 33, 37, 38, 43, 48, 49, 50, 51, 52)

## run RevScore function
dset <- RevScore(dat = d, vars = rc_vars)

## rename items using useful labels
colnames(dset) <- item_info[, 1]

## run exploratory factor analysis (EFA) for 7 factor solution
fit0 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 7, rotation = 'promax')
print(fit0, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 6 factor
fit1 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 6, rotation = 'promax')
print(fit1, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 5 factor
fit2 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 5, rotation = 'promax')
print(fit2, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 4 factor
fit3 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 4, rotation = 'promax')
print(fit3, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 3 factor
fit4 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 3, rotation = 'promax')
print(fit4, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 2 factor
fit5 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 2, rotation = 'promax')
print(fit5, digits = 2, cutoff = .3, sort = TRUE)

## run EFA for 1 factor
fit6 <- factanal(covmat = cov(dset, use = 'pairwise'), factors = 1, rotation = 'promax')
print(fit6, digits = 2, cutoff = .3, sort = TRUE)
