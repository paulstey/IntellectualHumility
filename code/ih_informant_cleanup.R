###
# IH: Informant Solicitition
# Cleaning the bogus informants out
# August 30, 2014
# Author: Paul Stey
###

setwd("~/Rwd")

d_raw <- read.csv("ih_informant_solicit.csv", stringsAsFactors = FALSE)

## cut our own pilot responses
d0 <- d_raw[13:nrow(d_raw), ]

## cut rows for non-consenting
d1 <- d0[which(d0$consent == 1), ]

## cut useless columns
d2 <- d1[, -c(2:10, 70:71, 96:99)]

## give useful column names
colnames(d2)[c(1, 61:75)] <- c("id", "inform1_name", "inform1_email", "inform1_rel", "inform2_name", "inform2_email", "inform2_rel", "inform3_name", "inform3_email", "inform3_rel", "inform4_name", "inform4_email", "inform4_rel", "inform5_name", "inform5_email", "inform5_rel")


# function for identifying questionable contacts, given a vector, returns boolean
is_questionable <- function(xrow){
	indicator <- TRUE

	## column with first informant e-mail
	start <- 62

	## loop over e-mail addresses
	for (i in seq(from = start, to = 74, by = 3)) {
		
		if (length(grep('@', xrow[i])) != 0) {

			indicator <- FALSE
			break
		}
	}

	## if we have a non-questionable candidate
	if(indicator == FALSE){
		## get e-mail addresses
		emails <- c(xrow[62], xrow[65], xrow[68], xrow[71], xrow[74])
		validemails <- emails[which(length(grep('@', xrow[i])) != 0)]

		## flag repeated e-mail
		if(length(validemails) != length(unique(validemails))){
			indicator <- TRUE
		}
	}
	return(indicator)
}


# create indicator in dataset
d <- d2
d$questionable <- apply(d2, 1, is_questionable)


# verify IDs are all unique; use these to match after human review
nrow(d) == length(unique(d$id))

# export non-obvious-questionable subset
dsub <- d[which(d$questionable == FALSE), ]

write.csv(dsub, "~/Desktop/ih_turk.csv")

## Paul's flagged subject IDs
flg1 <- c("R_0HbZENPpf4HqddX", "R_79T0hFOraw1pLlr", "R_808J08VpBfspHjn", "R_8FZytsRmmbrCtFP", "R_dcXnMtMnQIeoLKR", "R_0TCzusK1KBG7nW5", "R_40VBSPLAXK6UIN7", "R_3WoNaP5wdOccY7P", "R_5iDUW87gCQlxevP", "R_bkr3PGyRcpVYY4Z", "R_8uXZocS4Jd6ZX4V", "R_da7weJP7WEtjlI1", "R_79TAREBIItIUgvj")

## Mark's flags
flg2 <- c(8, 23, 24, 41, 46, 50, 57, 69, 85, 93, 97, 111, 114, 117, 119, 121, 127, 133, 134, 139, 143, 156, 163, 165, 225, 244, 239, 241, 251, 259, 261, 276, 277, 279, 286, 295, 297, 327, 344, 354, 359, 366, 387, 407, 423, 441, 447, 467, 469, 470, 474, 488, 491, 493, 502, 503, 508, 523, 546, 547, 559, 576, 581, 587, 591, 603, 613, 617, 623, 625, 635, 636, 637, 640, 645, 646, 650)

## Kathryn's flags
flg3 <- c(13, 19, 24, 30, 41, 43, 66, 85, 93, 97, 105, 111, 117, 119, 127, 138, 151, 163, 170, 225, 232, 239, 241, 256, 272, 277, 297, 306, 325, 327, 342, 354, 359, 366, 387, 407, 419, 431, 441, 447, 469, 472, 488, 491, 503, 523, 544, 546, 547, 565, 576, 581, 587, 591, 625, 627, 632, 645, 646, 647, 650)

# get the unique ids of any flagged participant
flagged_ids <- unique(c(dsub[unique(c(flg2, flg3)), 1], flg1)) 

# read in the informant data from Qualtrics panels
d_inf1 <- read.csv("ih_informer1.csv", stringsAsFactors = FALSE)
d_inf2 <- read.csv("ih_informer2.csv", stringsAsFactors = FALSE)
d_inf3 <- read.csv("ih_informer3.csv", stringsAsFactors = FALSE)
d_inf4 <- read.csv("ih_informer4.csv", stringsAsFactors = FALSE)
d_inf5 <- read.csv("ih_informer5.csv", stringsAsFactors = FALSE)


cut_useless <- function(d_inform){
	bad_indcs <- NULL
	n <- length(flagged_ids)
	## loop through flagged IDs
	for (i in 1:n) {
		chk <- which(flagged_ids[i] == d_inform$TriggerResponseID)
		if (length(chk) > 0) {
			if (!is.na(chk)) {
				## store the row indx from flagged ID in the panel
				bad_indcs <- c(bad_indcs, chk)
			}
		}
	}
	return(d_inform[-bad_indcs, ])
}


# eliminate flagged ids' rows from panel
# then export .csvs
dinf1_cln <- cut_useless(d_inf1)
dinf2_cln <- cut_useless(d_inf2)
dinf3_cln <- cut_useless(d_inf3)
dinf4_cln <- cut_useless(d_inf4)
dinf5_cln <- cut_useless(d_inf5)


write.csv(dinf1_cln, "~/Desktop/infmr1.csv")
write.csv(dinf2_cln, "~/Desktop/infmr2.csv")
write.csv(dinf3_cln, "~/Desktop/infmr3.csv")
write.csv(dinf4_cln, "~/Desktop/infmr4.csv")
write.csv(dinf5_cln, "~/Desktop/infmr5.csv")


