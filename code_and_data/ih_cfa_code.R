###
# Intellectual Humility Project
# Study 1b: Confirmatory Factor Analysis
# April 26, 2014
# Author: Paul Stey
###

library(lavaan)
library(ltm)
library(QuantPsyc)
library(psych)


## set working directory
setwd('~/Rwd')

## read in raw data set
d_raw <- read.csv('ih_mturk_data.csv')

## read in item information (i.e., names and full text) for IH scale items
item_info <- read.csv('ih_explct_items.csv', header = FALSE, stringsAsFactors = FALSE)


## correct reverse scoring error Kathryn identified on Nov 29, 2015
item_info[37, 3] <- "rev"
item_info[38, 3] <- "rev"
item_info[43, 3] <- "rev"
item_info[39, 3] <- ""
item_info[40, 3] <- ""


###
# Write quick function to recode negatively scored items
###
rev_score <- function(dat, vars) {
    recoded_mat <- dat

    for (i in vars) {
        recoded_mat[, i] <- 8 - dat[, i]
    }
    return(recoded_mat)
}

## specify items to be recoded
rc_vars <- which(item_info[, 3] == "rev")
rc_vars <- c(rc_vars, c(8, 10, 11, 13, 15, 32, 18, 24, 25, 26, 29, 31))


dset <- rev_score(d_raw[, 1:52], rc_vars)           # reverse required items
colnames(dset)[1:52] <- item_info[, 1]              # rename items with useful labels
dset <- cbind(dset, d_raw[, 53:60])                 # add demographic variables






# Fit some CFA models

## try individual factor models first, then try higher-order CFA

## specify first factor model: Humility
mod1 <- '
    ## factor model
    in_hum =~ ask_when_dont_undstd + inquire_about_disagrmnt + dissenters_waste_time + enjoy_diverse_views + somthng_wrong_w_dissenters + no_shame_learning_from_someone + dont_mind_being_taught + teachrs_learn_from_students + appreciate_being_corrected + never_brag + dont_mind_learning_from_lowstatus + exaggerate_knowledg_in_argumnt + only_wimps_admit_mistakes + dont_take_diff_people_serious + foreigner_have_weird_ideas

'

## fit the model, and examine fit
fit1 <- cfa(mod1, se = 'robust', test = 'Satorra.Bentler', std.lv = TRUE, data = dset)

summary(fit1, fit.measures = TRUE)

## check modification indices
modindices(fit1)


## check multivariate normality

## shows multivariate kurtosis of 69.82, so we use robust SEs above
mardia(dset[, 1:52])  


mult.norm(dset[, 1:52])$mult.test


## specify second factor: Vanity
mod2 <- '
    ## factor model
    in_van =~ ok_w_not_being_smartest + more_info_access + be_best_explainer + being_most_knowledgble + dont_know_more + like_being_smartest_in_room

'

## fit the model, check the output
fit2 <- cfa(mod2, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit2, fit.measures = TRUE)


## specify ourth factor: Boredom (note: 3rd has 3 indicators, so saturated)
mod4 <- '
    in_bord =~ read_dif_cultures + bored_by_dif_book + dislike_reasons_people_disagree + enjoy_diverse_views + bored_by_things_dont_understnd + rarely_discuss_unknown + disagremnt_like_war

'

## fit the model, check fit indices
fit4 <- cfa(mod4, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit4, fit.measures = TRUE)



## specify fifth factor: Grit
mod5 <- '
    in_grit =~ try_to_learn + push_to_master_new_concept + quit_without_mastering + learn_minimum

'

## fit the model, check the fit
fit5 <- cfa(mod5, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit5, fit.measures = TRUE)          ## RMSEA is not good.




## specify 7th factor: Machiavellianism
mod7 <- '
    in_mac =~ act_nice + not_flatter + laugh_jokes + not_pretnd_like

'

## run the model, check the fit
fit7 <- cfa(mod7, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit7, fit.measures = TRUE)




## specify 8th factor: Neuroticism
mod8 <- '
    in_neurot =~ annoyed_by_called_on_mistake + angered_by_called_on_mistake + not_embrss_by_mistake + frustrated_when_others_know_more

'

## fit the model, check fit
fit8 <- cfa(mod8, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit8, fit.measures = TRUE)





# Now we fit the above models in one higher-order
# confirmatory factor analysis 


mod0 <- '
    ## factor 1 model
    in_hum =~ dissenters_waste_time + no_shame_learning_from_someone + dont_mind_being_taught + dont_mind_learning_from_lowstatus + only_wimps_admit_mistakes + dont_take_diff_people_serious


    ## factor 2 model
    in_van =~ ok_w_not_being_smartest + more_info_access + be_best_explainer + being_most_knowledgble + dont_know_more + like_being_smartest_in_room


    ## factor 3 model
    in_klep =~ steal_ideas_if_never_caught + tempted_to_steal_ideas + copy_work


    ## factor 4 model
    in_bord =~ read_dif_cultures + bored_by_dif_book + dislike_reasons_people_disagree + enjoy_diverse_views + bored_by_things_dont_understnd + rarely_discuss_unknown + disagremnt_like_war


    ## factor 5 model
    in_grit =~ try_to_learn + push_to_master_new_concept + quit_without_mastering + learn_minimum


    ## factor 6 model
    in_uniq =~ dont_feel_special + know_alot_dont_feel_special + dont_feel_special_when_know_more


    ## factor 7 model
    in_mac =~ act_nice + not_flatter + laugh_jokes + not_pretnd_like


    ## factor 8 model
    in_neurot =~ annoyed_by_called_on_mistake + angered_by_called_on_mistake + not_embrss_by_mistake + frustrated_when_others_know_more


    ## Higher-order IH factor
    ih =~ in_hum + in_van + in_klep + in_bord + in_grit + in_uniq + in_mac + in_neurot

    ## Covariance of latent factors
    in_van ~~ in_uniq + in_grit
    in_klep ~~ in_mac
'


## fit the model, and check the fit
fit0 <- cfa(mod0, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit0, fit.measures = TRUE)
modindices(fit0)



###
# Trying to omit higher-order factor, and simply allow correlated factors
###
mod0 <- '
    ## factor 1 model
    in_hum =~ ask_when_dont_undstd + inquire_about_disagrmnt + dissenters_waste_time + enjoy_diverse_views + somthng_wrong_w_dissenters + no_shame_learning_from_someone + dont_mind_being_taught + teachrs_learn_from_students + appreciate_being_corrected + never_brag + dont_mind_learning_from_lowstatus + exaggerate_knowledg_in_argumnt + only_wimps_admit_mistakes + dont_take_diff_people_serious + foreigner_have_weird_ideas

    ## factor 2 model
    in_van =~ ok_w_not_being_smartest + more_info_access + be_best_explainer + being_most_knowledgble + dont_know_more + like_being_smartest_in_room

    ## factor 3 model
    in_klep =~ steal_ideas_if_never_caught + tempted_to_steal_ideas + copy_work

    ## factor 4 model
    in_bord =~ read_dif_cultures + bored_by_dif_book + dislike_reasons_people_disagree + enjoy_diverse_views + bored_by_things_dont_understnd + rarely_discuss_unknown + disagremnt_like_war

    ## factor 5 model
    in_grit =~ try_to_learn + push_to_master_new_concept + quit_without_mastering + learn_minimum

    ## factor 6 model
    in_uniq =~ dont_feel_special + know_alot_dont_feel_special + dont_feel_special_when_know_more

    ## factor 7 model
    in_mac =~ act_nice + not_flatter + laugh_jokes + not_pretnd_like

    ## factor 8 model
    in_neurot =~ annoyed_by_called_on_mistake + angered_by_called_on_mistake + not_embrss_by_mistake + frustrated_when_others_know_more

    ## Fixing covariance to 0
    #in_grit ~~ 0*in_van + 0*in_uniq
  
'


## fit the model, and check the fit
fit0 <- cfa(mod0, se = 'robust', test = 'Satorra.Bentler', data = dset)

summary(fit0, fit.measures = TRUE)
modindices(fit0)









# IRT graded response models for 4-factor solution 
# described in Study 2 of our manuscript



## specify indices for subscales
openmind_indcs <- c(27, 34, 51, 45, 50, 35)
modesty_indcs <- c(8, 10, 11, 13, 15, 32)
corrig_indcs <- c(37, 38, 39, 40, 43)
engagement_indcs <- c(18, 24, 25, 26, 29, 31)




###
# fit graded-response models
###

## modesty sub-scale
m1 <- grm(dset[, openmind_indcs], IRT.param = TRUE, Hessian = TRUE)

summary(m1)

plot(m1, type = "IIC")




## vanity sub-scale
m2 <- grm(dset[, modesty_indcs], IRT.param = TRUE, Hessian = TRUE)

summary(m2)
plot(m2, type = "IIC")




## neuroticism sub-scale
m3 <- grm(dset[, corrig_indcs], IRT.param = TRUE, Hessian = TRUE)

summary(m3)
plot(m3, type = "IIC")





## boredom sub-scale
m4 <- grm(dset[, engagement_indcs], IRT.param = TRUE, Hessian = TRUE)

summary(m4)

plot(m4, type = "IIC")





###
# define function to make clean parameter estimate tables
###

clean_output <- function(grm_model) {
    ## get list of parameter estimates
    pe_list <- summary(grm_model)$coefficients

    ## get dimensions 
    num_items <- length(pe_list)
    p <- nrow(pe_list[[1]])

    ## initialize resulting matrix
    result <- matrix(NA, nrow = num_items, ncol = p)

    ## loop over list elements
    for (i in 1:num_items) {
        two_columns <- pe_list[[i]][, 1:2]
        two_columns <- round(two_columns, digits = 2)

        ## pretty up the output
        one_column <- paste0(two_columns[, 1], " ", "(", two_columns[, 2], ")")

        ## transpose and save in result matrix
        result[i, ] <- t(one_column)
    }
    result <- noquote(result)
    return(result)
}

openmind_tbl <- clean_output(m1)
modesty_tbl <- clean_output(m2)
corrig_tbl <- clean_output(m3)
engagement_tbl <- clean_output(m4)


write.csv(corrig_tbl, '~/Desktop/corrig_tbl.csv')


# Make pretty plots
tiff('~/Desktop/openmindedness_subscale.tiff', width = 5.2, height = 4, units = 'in', res = 600, compression = "lzw")

plot(m1, type = 'IIC', xlab = expression(theta), cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1, main = 'Openmindedness Items', labels = c('27', '34', '51', '45', '50', '35'))

dev.off()

tiff('~/Desktop/modesty_subscale.tiff', width = 5.2, height = 4, units = 'in', res = 600, compression = "lzw")

plot(m2, type = 'IIC', xlab = expression(theta), main = 'Intellectual Modesty Items', labels = c('8', '10', '11', '13', '15', '32'))

dev.off() 


## corrigibility sub-scale
tiff('~/Desktop/corrigibility_subscale.tiff', width = 5.2, height = 4, units = 'in', res = 600, compression = "lzw")

plot(m3, type = 'IIC', xlab = expression(theta), main = 'Corrigibility Items', labels = c('37', '38', '39', '40', '43'))

dev.off()


## histogram of item 38
hist(dset[, 38], breaks = c(0, 1, 2, 3, 4, 5, 6, 7), col = "skyblue", border = "lightblue", xlab = "Response Category", main = "Distribution of Item 38")

## engagement sub-scale
tiff('~/Desktop/engagement_subscale.tiff', width = 5.2, height = 4, units = 'in', res = 600, compression = "lzw")

plot(m4, type = 'IIC', xlab = expression(theta), main = 'Engagement Sub-scale', labels = c('18', '24', '25', '26', '29', '31'))

dev.off()


hist(dset[, 24], breaks = c(0, 1, 2, 3, 4, 5, 6, 7), col = "skyblue", border = "lightblue", xlab = "Response Category", main = "Item 24")
hist(dset[, 29], breaks = c(0, 1, 2, 3, 4, 5, 6, 7), col = "skyblue", border = "lightblue", xlab = "Response Category", main = "Item 29", ylab = "")

## Plot all sub-scales in 1
tiff('~/Desktop/all_subscales.tiff', width = 5.2, height = 4, units = 'in', res = 600, compression = "lzw")

par(mfrow = c(2, 2), mai = c(0.8, 0.9, 0.3, 0.2))

plot(m1, items = 0, type = 'IIC', xlab = expression(theta), cex.main = 1, cex.lab = 1, main = 'Openmindedness', lwd = 2, col = 'blue')
plot(m2, items = 0, type = 'IIC', xlab = expression(theta), cex.main = 1, cex.lab = 1, main = 'Intellectual Modesty', lwd = 2, col = 'blue')
plot(m3, items = 0, type = 'IIC', xlab = expression(theta), cex.main = 1, cex.lab = 1, main = 'Corrigibility', lwd = 2, col = 'blue')
plot(m4, items = 0, type = 'IIC', xlab = expression(theta), cex.main = 1, cex.lab = 1, main = 'Engagement', lwd = 2, col = 'blue')

dev.off()



# exporting a given plot to .png
png('~/Desktop/ih_subscale.png', width = 5.2, height = 4, units = 'in', res = 600)
par (mai = c(0.8, 0.9, 0.3, 0.2))

plot(m4, type = 'IIC', xlab = expression(theta), main = 'Boredom Items', labels = c('18', '24', '25', '26', '29', '31'))

dev.off()
