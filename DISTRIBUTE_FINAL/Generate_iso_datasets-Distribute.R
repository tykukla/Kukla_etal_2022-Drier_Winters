# -------------------------------------------------- #
# This script generates the datasets used in         #
# Kukla et al., -- Drier winters drove Cenozoic open #
# habitat expansion in North America                 # 
#                                                    #
# ---                                                #
# T Kukla (Stanford Univ. 2021)                      #
# -------------------------------------------------- #
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

rm(list=ls())
myPath <- 'CHANGED BY USER'   # only used for working directory
setwd(myPath)

# ************************************************* # 
# This script processes the original data and 
# outputs data files for each step. 
# 
# To Run: 
# [1] set 'datpath' to the folder where input data are stored
# [2] set 'dat_fn' to the filename of the input data, or confirm it is correct
# [3] set 'save_here' to the path where you want output saved (default is datpath)
# [4] select your save option (save 'ALL'; 'FINAL'; or 'NONE' of the data when running the script)
# 
# ************************************************* # 



# ------------------------------------------------- #
# Initialize the processing script
datpath <- 'CHANGED BY USER'   # folder where data are stored
dat_fn <- 'alldata_processing-input.csv'   # filename
df1 <- as.data.table(read.csv(paste(datpath, dat_fn, sep='/')))
# folder to save output datasets 
save_here <- datpath
# select how much to save 
save_which <- 'ALL'   # select: 'ALL'; 'FINAL' or 'NONE' (case-sensitive)
# ------------------------------------------------- #

# ------------------------------------------------- #
# Define the study domain and sub-domains
# [study domain]
minlat <- 40 
maxlat <- 47
maxlon <- -100   # (no min lon, we take the most westernmost points)
# [sub domains]
regionLims <- c('min' = -117, 'max'=-110)   # longitudes of sub domain boundaries
# time domain
time_bounds <- c('max' = 50, 'min' = 2.6, 'OHT_bottom' = 26, 'OHT_top' = 15)   # ages in Myr
# ------------------------------------------------- #






# ----------------------------------------------------------------------------- #
## STEP 1: Filter for floodplain clay and carbonate samples -------------------
## [1] Previously_published=='N' removes duplicate samples that were re-compiled (appear in more than one publication)
## [2] Age_ma <= 50 removes data older than 50 Ma
## [3] Latitude <= minlat & Latitude... applies geographic restrictions
## [4] Material == 'smectite' | Material... selects clays and carbonates, excludes lacustrine data 
##     Note, there are no biogenic calcite samples in these data
## ***
# FILTER
df1 %>%
  # filter by age and geographic bounds
  filter(Previously_published=='N' & Age_ma <= time_bounds['max']  & Age_ma >= time_bounds['min'] & Latitude >= minlat & Latitude <= maxlat & Longitude <= maxlon) %>%   
  # filter by minerals
  filter(Material == 'smectite' | Material == 'kaolinite' | Material == 'calcite' & 
           Sample_Type != 'Lacustrine Carbonate') -> df2a
# find rows with NA values
missing_d18 <- which(is.na(df2a$d18O_smow))
missing_d13 <- which(is.na(df2a$d13C))
both_missing <- missing_d18[which(missing_d18 %in% missing_d13)]
# remove data with both missing
df2b <- df2a[-both_missing]
# add general labels by material
df2b$Material_x <- ifelse(df2b$Material=='smectite' | df2b$Material=='kaolinite', 'clay', 'carbonate')
# add label for domain (west, central, east)
df2b$Domain <- NA   # initialize column
df2b$Domain[which(df2b$Longitude <= regionLims['min'])] <- 'West'
df2b$Domain[which(df2b$Longitude > regionLims['min'] & df2b$Longitude <= regionLims['max'])] <- 'Central'
df2b$Domain[which(df2b$Longitude > regionLims['max'])] <- 'East'

## SAVE RESULT ********************************************** #
if(save_which=='ALL'){                                        #
  save_fn <- 'alldata_processing-Output1.csv'                 #
  write_csv(df2b, path=paste(save_here, save_fn, sep='/'))    #
  print("-- Output file 1 successfully written")  }           #                 
# *********************************************************** # 
# ----------------------------------------------------------------------------- #


# **


# ----------------------------------------------------------------------------- #
## STEP 2: Identify non-meteoric, non-background, later-updated data
## These data are labeled here so they can be easily removed in the next step
## [1] Published_interpretation is NA if interpreted meteoric; remove all non NAs
## [2] Event is NA if there is no transient climate event and isotope excursion; remove all non NAs
## [3] Filter_Reason is a column we add noting the reason why the data point is removed
## ***
# start with new data.table to update
df3 <- df2b
# Label datapoints that are removed and why
non_meteoric_idx <- which(!is.na(df3$Published_interpretation))   # index of non meteoric samples
non_meteoric_reason <- df3[non_meteoric_idx]$Published_interpretation   # vector of reasons for removing
non_background_idx <- which(!is.na(df3$Event))   # index of non background samples
non_background_reason <- df3[non_background_idx]$Event   # vector of reasons for removing (Note: MECO = Mid Eocene Climate Optimum)
# check if any data points are removed for two reasons
any(non_meteoric_idx %in% non_background_idx)   # no duplicates
# how many removed?
n_removed <- paste(length(c(non_meteoric_idx, non_background_idx)), "samples removed in filtering", sep=' ') ; n_removed
# label the filtered out points
df3$Filtered <- 'N'   # 'Y' = removed; 'N' = kept
df3$Filtered[c(non_meteoric_idx, non_background_idx)] <- 'Y'   # overwrite the filtered 'N's with 'Y's
df3$Filtered_reason <- NA  # NA if kept
df3$Filtered_reason[non_meteoric_idx] <- paste("Non-meteoric interpretation:", non_meteoric_reason, sep=' ')
df3$Filtered_reason[non_background_idx] <- paste("Climate event:", non_background_reason, sep=' ')

## SAVE RESULT ********************************************** # 
if(save_which=='ALL'){                                        #
  save_fn <- 'alldata_processing-Output2.csv'                 #
  write_csv(df3, path=paste(save_here, save_fn, sep='/'))     #
  print("-- Output file 2 successfully written") }            #
# *********************************************************** #
# ----------------------------------------------------------------------------- #


# **


# ----------------------------------------------------------------------------- #
## STEP 3: Take average of all replicate measurements
## [1] remove the data marked for filtering in step 2 
## [2] group samples by replicates and average
## [3] label data by pre and post OHT
## ***
# start with new data.table to update
df4 <- df3   # (this is redundant, but it makes me feel like it's neater...)
# remove filter=='y'
df4a <- df4[which(df4$Filtered!="Y")]
# mean of replicates
df4a %>%
  group_by(ID,            # Site location
           Number,        # Sample name (Number applied to multiple replicate 'Sample' values)
           Material,      # To ensure no mixing of different materials with similar names
           Material_x,    # keep general material info (clay / carb)
           Sample_Type,   # the summarise_if fxn (next step) removes non-numerics unless they're part of the group. This and below designations are to keep them in the group
           Reference, Notes, West_Subdomain, Domain) %>%
  summarise_if(is.numeric, mean, na.rm=T) -> df4b
# previous step changed data to tibble, but I prefer data.table so we switch back
df4b <- as.data.table(df4b)
# label by pre and post-OHT
df4b$timeslice <- NA   # initialize column
df4b$timeslice[which(df4b$Age_ma >= time_bounds['OHT_bottom'])] <- 'pre-OHT'
df4b$timeslice[which(df4b$Age_ma <= time_bounds['OHT_top'])] <- 'post-OHT'

## SAVE RESULT ********************************************** # 
if(save_which=='ALL' | save_which=='FINAL'){                  #
  save_fn <- 'alldata_processing-Output3.csv'                 #
  write_csv(df4b, path=paste(save_here, save_fn, sep='/'))    #
  print("-- Output file 3 successfully written") }            #
# *********************************************************** #


# ** 


# ----------------------------------------------------------------------------- #
## STEP 3a: Get the dataset in the site-means version
## [1] group and average by site
## ***
# Get site means
df4b %>%
  group_by(ID,           # site location
           timeslice,    # separate site means if a site spans both timeslices
           Material,     # separate site averages by material
           Material_x,   # keep general material info (clay / carb)
           Sample_Type,  # this keeps the sample type column in the output file (same for all subsequent groups)
           Reference, West_Subdomain, Domain) %>%
  # get summary stats (standard devs)
  mutate(O_sd = sd(d18O_smow, na.rm=TRUE), C_sd = sd(d13C, na.rm=TRUE)) %>%
  # take average of the groups
  summarise_if(is.numeric, mean, na.rm=T) -> df5.sm   # site means
# get back into a data.table
df5.sm <- as.data.table(df5.sm)


## SAVE RESULT ********************************************** # 
if(save_which=='ALL' | save_which=='FINAL'){                  #
  save_fn <- 'alldata_processing-Output4_sitemeans.csv'       #
  write_csv(df5.sm, path=paste(save_here, save_fn, sep='/'))  #
  print("-- Output file 4 (site means) successfully written")}#
# *********************************************************** #
# ----------------------------------------------------------------------------- #


# ** 


# ----------------------------------------------------------------------------- #
## STEP 4: Get summary statistics
## [1] print the summary stats about the dataset for the manuscript
## ***
# ALL DATA
TOTAL_start <- nrow(df1) ; print(paste("All data (before processing):", TOTAL_start))
TOTAL_refs <- length(unique(df1$Reference)) ; print(paste("Data references:", TOTAL_refs))
TOTAL_new <- nrow(df1[which(df1$Reference=="this study")]) ; print(paste("New data (before processing):", TOTAL_new))
TOTAL_new.clay <- nrow(df1[which(df1$Reference == "this study" & (df1$Material=="smectite" | df1$Material=="kaolinite"))]) ; print(paste("Total new data pts, clays:", TOTAL_new.clay))
TOTAL_new.carbonate <- nrow(df1[which(df1$Reference == "this study" & df1$Material=="calcite")]) ; print(paste("Total new data pts, carbonates:", TOTAL_new.carbonate))
# AFTER FILTERING BY GEOGRAPHY, TIME, PROXY TIME
TOTAL_step1 <- nrow(df2b) ; print(paste("Total data points analyzed:", TOTAL_step1))
TOTAL_step1.sites <- length(unique(df2b$ID)) ; print(paste("Total sites analyzed:", TOTAL_step1.sites))
# published data
TOTAL_step1.clay <- nrow(df2b[which(df2b$Material_x=='clay')]) ; print(paste("Total clay data pts:", TOTAL_step1.clay))
TOTAL_step1.published <- nrow(df2b[which(df2b$Reference != "this study")]) ; print(paste("Total published data pts:", TOTAL_step1.published))
TOTAL_step1.published.clay <- nrow(df2b[which(df2b$Reference != "this study" & df2b$Material_x=="clay")]) ; print(paste("Total published data pts, clays:", TOTAL_step1.published.clay))
TOTAL_step1.published.carbonate <- nrow(df2b[which(df2b$Reference != "this study" & df2b$Material_x=="carbonate")]) ; print(paste("Total published data pts, carbonates:", TOTAL_step1.published.carbonate))
# new data
TOTAL_step1.new <- nrow(df2b[which(df2b$Reference=="this study")]) ; print(paste("Total new data pts (after bounds):", TOTAL_step1.new))
TOTAL_step1.new.clay <- nrow(df2b[which(df2b$Reference == "this study" & df2b$Material_x=="clay")]) ; print(paste("Total new data pts, clays (after bounds):", TOTAL_step1.new.clay))
TOTAL_step1.new.carbonate <- nrow(df2b[which(df2b$Reference == "this study" & df2b$Material_x=="carbonate")]) ; print(paste("Total new data pts, carbonates (after bounds):", TOTAL_step1.new.carbonate))
# DATA FILTERING
RM_nonmeteoric <- length(non_meteoric_reason[which(!str_detect(non_meteoric_reason, 'updated'))]) ; print(paste("Removed for non-meteoric interpretation:", RM_nonmeteoric))
RM_updated <- length(non_meteoric_reason[which(str_detect(non_meteoric_reason, 'updated'))]) ; print(paste("Removed because updated in later work:", RM_updated))
RM_nonbackground <- length(non_background_reason) ; print(paste("Removed because non-background:", RM_nonbackground))
TOTAL_step2 <- nrow(df3) - length(c(non_meteoric_idx, non_background_idx)) ; print(paste("Total data after filter:", TOTAL_step2))
# AFTER AVERAGING AND FILTERING (non-meteoric, etc)
AVG_replicates <- nrow(df4b) ; print(paste("Total samples, filtered, means:", AVG_replicates))
AVG_sites <- length(unique(df4b$ID)) ; print(paste("Total sites, filtered:", AVG_sites))
# ----------------------------------------------------------------------------- #


# **


# ----------------------------------------------------------------------------- #
## STEP 5: Scale data to compare all on one axis
## [1] Separate data into three domains
## [2] one domain at a time, get average d18O before 30 Ma and subtract out
## [2a] apply the regression to the eastern domain to detrend longitude
## *** 
# start with a fresh site means data frame
dfm <- df5.sm   # site means
# set the age above which to take the residuals
minResidAge <- 30  # [Ma] 
# separate by domains
dfwm <- dfm[which(dfm$Domain=="West")]
dfcm <- dfm[which(dfm$Domain=="Central")]
dfem <- dfm[which(dfm$Domain=="East")]
# define a linear regression for de-trending the east
XeFit <- lm(d18O_smow ~ Longitude, data=dfem)

# [WESTERN DOMAIN]
# First, pull out just the John Day region, which we must correct for local 
# topography following Kukla et al., 2021 (Frontiers in Earth Sci.)
# (The non-john Day samples are subtracted by John Day East domain 
# because it's most similar geographically and there's no data pre 30 Ma to use)
# CLAYS ---
jd.idx <- which(dfwm$Material_x=='clay' & !is.na(dfwm$West_Subdomain))  # john day samples are here
dfw.jd <- dfwm[jd.idx]   # extract john day samples
dfw.other <- dfwm[-jd.idx]   # all other data
dfw.other <- dfw.other[which(dfw.other$Material_x=='clay')]  # just clay
# pre-OHT means to subtract out 
w_wst.mean <- mean(dfw.jd[which(dfw.jd$West_Subdomain=='west' & dfw.jd$Age_ma > minResidAge)]$d18O_smow, na.rm=T)  # western sub-domain of Kukla et al., 2021 
w_est.mean <- mean(dfw.jd[which(dfw.jd$West_Subdomain=='east' & dfw.jd$Age_ma > minResidAge)]$d18O_smow, na.rm=T)  # eastern sub-domain of Kukla et al., 2021 
dfw.jd$d18_scaled <- ifelse(dfw.jd$West_Subdomain=="west", dfw.jd$d18O_smow - w_wst.mean, dfw.jd$d18O_smow - w_est.mean)  # subtract the respective mean
dfw.other$d18_scaled <- dfw.other$d18O_smow - w_est.mean  # subtract by east region of John Day; it's most similar to rest of west (and there's no data pre 30 Ma to use)
# merge together
dfw.cl <- as.data.table(rbind(dfw.jd, dfw.other))
# CARBONATES --- 
# carbonates don't have mean data older than 30 so we change minResid to 25
# we don't account for east vs west facies (Kukla et al., 2021) in carbonate data because there is only one point in the west (and none after 23 Ma)
# we remove the one data point in the John Day west and set all else to John Day east
dfw.ca <- dfwm[which(dfwm$Material_x=="carbonate" & (is.na(dfwm$West_Subdomain) | dfwm$West_Subdomain != "west"))]
w_ca.mean <- mean(dfw.ca[which(dfw.ca$Age_ma > 25)]$d18O_smow, na.rm=T)  # average carbonate d18O pre-OHT
dfw.ca$d18_scaled <- dfw.ca$d18O_smow - w_ca.mean   # subtract out average

# [CENTRAL DOMAIN]
# CLAYS --- 
dfc.cl <- dfcm[which(dfcm$Material_x=="clay")]   # pull out clay data
c_cl.mean <- mean(dfc.cl[which(dfc.cl$Age_ma > minResidAge)]$d18O_smow, na.rm=T)   # average pre-OHT
dfc.cl$d18_scaled <- dfc.cl$d18O_smow - c_cl.mean   # subtract out average
# CARBONATES ---
dfc.ca <- dfcm[which(dfcm$Material_x=="carbonate")]   # pull out carbonate data
c_ca.mean <- mean(dfc.ca[which(dfc.ca$Age_ma > minResidAge)]$d18O_smow, na.rm=T)   # average pre-OHT
dfc.ca$d18_scaled <- dfc.ca$d18O_smow - c_ca.mean   # subtract out average

# [EASTERN DOMAIN]
# CLAYS ---
dfe.cl <- dfem[which(dfem$Material_x=="clay")]  # pull out clays
d18_predict_cl <- predict(XeFit, dfe.cl)   # get predicted d18O based on linear model
# temporary dataframe for getting residuals
dfe.cl.temp <- dfe.cl ; dfe.cl.temp$d18_scaled_noSub <- dfe.cl$d18O_smow - d18_predict_cl  # calculate the scaled residuals (not yet subtracted mean residual pre-OHT)
e_cl.mean <- mean(dfe.cl.temp[which(dfe.cl.temp$Age_ma > minResidAge)]$d18_scaled_noSub, na.rm=T)   # mean pre-OHT
# subtract out the mean for scaled value
dfe.cl$d18_scaled <- dfe.cl.temp$d18_scaled_noSub - e_cl.mean
# CARBONATES ---
dfe.ca <- dfem[which(dfem$Material_x=="carbonate")]  # pull out carbonates
d18_predict_ca <- predict(XeFit, dfe.ca)   # get predicted d18O based on linear model
# temporary dataframe for getting residuals
dfe.ca.temp <- dfe.ca ; dfe.ca.temp$d18_scaled_noSub <- dfe.ca$d18O_smow - d18_predict_ca  # calculate the scaled residuals (not yet subtracted mean residual pre-OHT)
e_ca.mean <- mean(dfe.ca.temp[which(dfe.ca.temp$Age_ma > minResidAge)]$d18_scaled_noSub, na.rm=T)   # mean pre-OHT
# subtract out the mean for scaled value
dfe.ca$d18_scaled <- dfe.ca.temp$d18_scaled_noSub - e_ca.mean

# [ -- COMBINE -- ]
df.allClay <- as.data.table(rbind(dfw.cl, dfc.cl, dfe.cl))
df.allCarb <- as.data.table(rbind(dfw.ca, dfc.ca, dfe.ca))

## SAVE RESULT ***************************************************** # 
if(save_which=='ALL' | save_which=='FINAL'){                         #
  # clay and carb file names                                         #
  save_fn.cl <- 'sitemeans_d18scaled_Output5_clay.csv'               #
  save_fn.ca <- 'sitemeans_d18scaled_Output5_carbonate.csv'          #
  # clay and carbonate save                                          # 
  write_csv(df.allClay, path=paste(save_here, save_fn.cl, sep='/'))  #
  write_csv(df.allCarb, path=paste(save_here, save_fn.ca, sep='/'))  #
  print("-- Output file(s) 5 successfully written")}                 #
# ****************************************************************** #
# ----------------------------------------------------------------------------- #


# **


# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
## STEP 6: Run statistics 
## *** 
# means and all datasets
dfa <- df4b     # all data
dfm <- df5.sm   # site means
# separate by domains
dfwa <- dfa[which(dfa$Domain=="West")]
dfca <- dfa[which(dfa$Domain=="Central")]
dfea <- dfa[which(dfa$Domain=="East")]

# [WESTERN DOMAIN stats] -----------------------------------------------------------------------------------
# pre-OHT
dfwa.cl.pre <- dfwa[which(dfwa$timeslice=="pre-OHT" & dfwa$Material_x=="clay" & dfwa$West_Subdomain=="east")]
dfwa.ca.pre <- dfwa[which(dfwa$timeslice=="pre-OHT" & dfwa$Material_x=="carbonate")]
# post-OHT
dfwa.cl.post <- dfwa[which(dfwa$timeslice=="post-OHT" & dfwa$Material_x=="clay")]
dfwa.ca.post <- dfwa[which(dfwa$timeslice=="post-OHT" & dfwa$Material_x=="carbonate")]
# t-tests for change in clay and carbonate
w.pre.ttest <- t.test(x=dfwa.cl.pre$d18O_smow, y=dfwa.ca.pre$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
w.post.ttest <- t.test(x=dfwa.cl.post$d18O_smow, y=dfwa.ca.post$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
w.cl.ttest <- t.test(x=dfwa.cl.pre$d18O_smow, y=dfwa.cl.post$d18O_smow, alternative='two.sided', paired=FALSE)
w.ca.ttest <- t.test(x=dfwa.ca.pre$d18O_smow, y=dfwa.ca.post$d18O_smow, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
dfwa2 <- as.data.table(rbind(dfwa.cl.pre, dfwa.ca.pre, dfwa.cl.post, dfwa.ca.post))
w.anova <- glm(d18O_smow ~ timeslice * Material_x, data=dfwa2)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
w.cl.conf.int_low <- w.cl.ttest$conf.int[1]*-1    # clay shift low conf int
w.cl.conf.int_high <- w.cl.ttest$conf.int[2]*-1   # clay shift high conf int
w.cl.OHTshift <- w.cl.ttest$estimate[2] - w.cl.ttest$estimate[1]  # clay shift
w.cl.OHTshift.pval <- w.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
w.ca.conf.int_low <- w.ca.ttest$conf.int[1]*-1    # carb shift low conf int
w.ca.conf.int_high <- w.ca.ttest$conf.int[2]*-1   # carb shift high conf int
w.ca.OHTshift <- w.ca.ttest$estimate[2] - w.ca.ttest$estimate[1]   # carb shift
w.ca.OHTshift.pval <- w.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
w.pre.conf.int_low <- w.pre.ttest$conf.int[1]   # big delta pre oht low conf int
w.pre.conf.int_high <- w.pre.ttest$conf.int[2]  # big delta pre oht high conf int
w.pre.bigdelta <- w.pre.ttest$estimate[1] - w.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
w.post.conf.int_low <- w.post.ttest$conf.int[1]  # big delta post oht low conf int
w.post.conf.int_high <- w.post.ttest$conf.int[2] # big delta post oht high conf int
w.post.bigdelta <- w.post.ttest$estimate[1] - w.post.ttest$estimate[2]  # big delta post oht 
# big delta change
w.bigdelta_difference <- w.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
w.bigdelta_pval <- coef(summary(w.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
w.before <- as.data.table(cbind('big_delta'=w.pre.bigdelta, 'CI_low'=w.pre.conf.int_low, 'CI_high'=w.pre.conf.int_high)) # pre oht data
w.before$timeslice <- 'pre-OHT'
w.before$Domain <- "West"
w.after <- as.data.table(cbind('big_delta'=w.post.bigdelta, 'CI_low'=w.post.conf.int_low, 'CI_high'=w.post.conf.int_high)) # post oht data
w.after$timeslice <- 'post-OHT'
w.after$Domain <- "West"
## ACROSS OHT DATA FRAME
w.cl.across <- as.data.table(cbind('OHT_shift'=w.cl.OHTshift, 'CI_low'=w.cl.conf.int_low, 'CI_high'=w.cl.conf.int_high, 'pval'=w.cl.OHTshift.pval ))
w.cl.across$metric <- 'clay_d18' ; w.cl.across$Domain <- 'West'
w.ca.across <- as.data.table(cbind('OHT_shift'=w.ca.OHTshift, 'CI_low'=w.ca.conf.int_low, 'CI_high'=w.ca.conf.int_high, 'pval'=w.ca.OHTshift.pval ))
w.ca.across$metric <- 'carbonate_d18' ; w.ca.across$Domain <- 'West'
w.BD.across <- as.data.table(cbind('OHT_shift'=w.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=w.bigdelta_pval ))
w.BD.across$metric <- 'big_delta' ; w.BD.across$Domain <- 'West'
## -- FINAL DOMAIN RESULTS
df.w.BeforeAfter <- as.data.table(rbind(w.before, w.after))
df.w.across <- as.data.table(rbind(w.cl.across, w.ca.across, w.BD.across))
# ----------------------------------------------------------------------------------------------------


# [CENTRAL DOMAIN stats] -----------------------------------------------------------------------------------
dfca.cl.pre <- dfca[which(dfca$timeslice=="pre-OHT" & dfca$Material_x=="clay" )]
dfca.ca.pre <- dfca[which(dfca$timeslice=="pre-OHT" & dfca$Material_x=="carbonate")]
# post-OHT
dfca.cl.post <- dfca[which(dfca$timeslice=="post-OHT" & dfca$Material_x=="clay")]
dfca.ca.post <- dfca[which(dfca$timeslice=="post-OHT" & dfca$Material_x=="carbonate")]
# t-tests for change in clay and carbonate
c.pre.ttest <- t.test(x=dfca.cl.pre$d18O_smow, y=dfca.ca.pre$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
c.post.ttest <- t.test(x=dfca.cl.post$d18O_smow, y=dfca.ca.post$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
c.cl.ttest <- t.test(x=dfca.cl.pre$d18O_smow, y=dfca.cl.post$d18O_smow, alternative='two.sided', paired=FALSE)
c.ca.ttest <- t.test(x=dfca.ca.pre$d18O_smow, y=dfca.ca.post$d18O_smow, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
c.anova <- glm(d18O_smow ~ timeslice * Material_x, data=dfca)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
c.cl.conf.int_low <- c.cl.ttest$conf.int[1]*-1    # clay shift low conf int
c.cl.conf.int_high <- c.cl.ttest$conf.int[2]*-1   # clay shift high conf int
c.cl.OHTshift <- c.cl.ttest$estimate[2] - c.cl.ttest$estimate[1]  # clay shift
c.cl.OHTshift.pval <- c.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
c.ca.conf.int_low <- c.ca.ttest$conf.int[1]*-1    # carb shift low conf int
c.ca.conf.int_high <- c.ca.ttest$conf.int[2]*-1   # carb shift high conf int
c.ca.OHTshift <- c.ca.ttest$estimate[2] - c.ca.ttest$estimate[1]   # carb shift
c.ca.OHTshift.pval <- c.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
c.pre.conf.int_low <- c.pre.ttest$conf.int[1]   # big delta pre oht low conf int
c.pre.conf.int_high <- c.pre.ttest$conf.int[2]  # big delta pre oht high conf int
c.pre.bigdelta <- c.pre.ttest$estimate[1] - c.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
c.post.conf.int_low <- c.post.ttest$conf.int[1]  # big delta post oht low conf int
c.post.conf.int_high <- c.post.ttest$conf.int[2] # big delta post oht high conf int
c.post.bigdelta <- c.post.ttest$estimate[1] - c.post.ttest$estimate[2]  # big delta post oht 
# big delta change
c.bigdelta_difference <- c.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
c.bigdelta_pval <- coef(summary(c.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
c.before <- as.data.table(cbind('big_delta'=c.pre.bigdelta, 'CI_low'=c.pre.conf.int_low, 'CI_high'=c.pre.conf.int_high)) # pre oht data
c.before$timeslice <- 'pre-OHT'
c.before$Domain <- "Central"
c.after <- as.data.table(cbind('big_delta'=c.post.bigdelta, 'CI_low'=c.post.conf.int_low, 'CI_high'=c.post.conf.int_high)) # post oht data
c.after$timeslice <- 'post-OHT'
c.after$Domain <- "Central"
## ACROSS OHT DATA FRAME
c.cl.across <- as.data.table(cbind('OHT_shift'=c.cl.OHTshift, 'CI_low'=c.cl.conf.int_low, 'CI_high'=c.cl.conf.int_high, 'pval'=c.cl.OHTshift.pval ))
c.cl.across$metric <- 'clay_d18' ; c.cl.across$Domain <- 'Central'
c.ca.across <- as.data.table(cbind('OHT_shift'=c.ca.OHTshift, 'CI_low'=c.ca.conf.int_low, 'CI_high'=c.ca.conf.int_high, 'pval'=c.ca.OHTshift.pval ))
c.ca.across$metric <- 'carbonate_d18' ; c.ca.across$Domain <- 'Central'
c.BD.across <- as.data.table(cbind('OHT_shift'=c.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=c.bigdelta_pval ))
c.BD.across$metric <- 'big_delta' ; c.BD.across$Domain <- 'Central'
## -- FINAL DOMAIN RESULTS
df.c.BeforeAfter <- as.data.table(rbind(c.before, c.after))
df.c.across <- as.data.table(rbind(c.cl.across, c.ca.across, c.BD.across))
# ----------------------------------------------------------------------------------------------------


# [EASTERN DOMAIN stats] -----------------------------------------------------------------------------------
# pre-OHT  --  first do regression
dfea.pre <- dfea[which(dfea$timeslice=="pre-OHT")]
dfea.pre$predicted <- predict(XeFit, dfea.pre)
dfea.pre$residuals <- dfea.pre$d18O_smow - dfea.pre$predicted
# ... now split to two dataframes
dfea.cl.pre <- dfea.pre[which(dfea.pre$Material_x=='clay')]
dfea.ca.pre <- dfea.pre[which(dfea.pre$Material_x=='carbonate')]
# post-OHT  --  first do regression
dfea.post <- dfea[which(dfea$timeslice=="post-OHT")]
dfea.post$predicted <- predict(XeFit, dfea.post)
dfea.post$residuals <- dfea.post$d18O_smow - dfea.post$predicted
# ... now split to two dataframes
dfea.cl.post <- dfea.post[which(dfea.post$Material_x=='clay')]
dfea.ca.post <- dfea.post[which(dfea.post$Material_x=='carbonate')]
# t-tests for change in clay and carbonate
e.pre.ttest <- t.test(x=dfea.cl.pre$residuals, y=dfea.ca.pre$residuals, alternative='two.sided', paired=FALSE, mu=-3.5)
e.post.ttest <- t.test(x=dfea.cl.post$residuals, y=dfea.ca.post$residuals, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
e.cl.ttest <- t.test(x=dfea.cl.pre$residuals, y=dfea.cl.post$residuals, alternative='two.sided', paired=FALSE)
e.ca.ttest <- t.test(x=dfea.ca.pre$residuals, y=dfea.ca.post$residuals, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
dfea2 <- as.data.table(rbind(dfea.pre, dfea.post))
e.anova <- glm(residuals ~ timeslice * Material_x, data=dfea2)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
e.cl.conf.int_low <- e.cl.ttest$conf.int[1]*-1    # clay shift low conf int
e.cl.conf.int_high <- e.cl.ttest$conf.int[2]*-1   # clay shift high conf int
e.cl.OHTshift <- e.cl.ttest$estimate[2] - e.cl.ttest$estimate[1]  # clay shift
e.cl.OHTshift.pval <- e.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
e.ca.conf.int_low <- e.ca.ttest$conf.int[1]*-1    # carb shift low conf int
e.ca.conf.int_high <- e.ca.ttest$conf.int[2]*-1   # carb shift high conf int
e.ca.OHTshift <- e.ca.ttest$estimate[2] - e.ca.ttest$estimate[1]   # carb shift
e.ca.OHTshift.pval <- e.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
e.pre.conf.int_low <- e.pre.ttest$conf.int[1]   # big delta pre oht low conf int
e.pre.conf.int_high <- e.pre.ttest$conf.int[2]  # big delta pre oht high conf int
e.pre.bigdelta <- e.pre.ttest$estimate[1] - e.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
e.post.conf.int_low <- e.post.ttest$conf.int[1]  # big delta post oht low conf int
e.post.conf.int_high <- e.post.ttest$conf.int[2] # big delta post oht high conf int
e.post.bigdelta <- e.post.ttest$estimate[1] - e.post.ttest$estimate[2]  # big delta post oht 
# big delta change
e.bigdelta_difference <- e.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
e.bigdelta_pval <- coef(summary(e.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
e.before <- as.data.table(cbind('big_delta'=e.pre.bigdelta, 'CI_low'=e.pre.conf.int_low, 'CI_high'=e.pre.conf.int_high)) # pre oht data
e.before$timeslice <- 'pre-OHT'
e.before$Domain <- "East"
e.after <- as.data.table(cbind('big_delta'=e.post.bigdelta, 'CI_low'=e.post.conf.int_low, 'CI_high'=e.post.conf.int_high)) # post oht data
e.after$timeslice <- 'post-OHT'
e.after$Domain <- "East"
# combine
df.e.BeforeAfter <- as.data.table(rbind(e.before, e.after))
## ACROSS OHT DATA FRAME
e.cl.across <- as.data.table(cbind('OHT_shift'=e.cl.OHTshift, 'CI_low'=e.cl.conf.int_low, 'CI_high'=e.cl.conf.int_high, 'pval'=e.cl.OHTshift.pval ))
e.cl.across$metric <- 'clay_d18' ; e.cl.across$Domain <- 'East'
e.ca.across <- as.data.table(cbind('OHT_shift'=e.ca.OHTshift, 'CI_low'=e.ca.conf.int_low, 'CI_high'=e.ca.conf.int_high, 'pval'=e.ca.OHTshift.pval ))
e.ca.across$metric <- 'carbonate_d18' ; e.ca.across$Domain <- 'East'
e.BD.across <- as.data.table(cbind('OHT_shift'=e.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=e.bigdelta_pval ))
e.BD.across$metric <- 'big_delta' ; e.BD.across$Domain <- 'East'
## -- FINAL DOMAIN RESULTS
df.e.BeforeAfter <- as.data.table(rbind(e.before, e.after))
df.e.across <- as.data.table(rbind(e.cl.across, e.ca.across, e.BD.across))
# ----------------------------------------------------------------------------------------------------


# **************************************************************************************************** 
# **************************************************************************************************** 
# ### BRING ALL DOMAINS TOGETHER ### 
df.before.after <- as.data.table(rbind(df.w.BeforeAfter,
                                       df.c.BeforeAfter,
                                       df.e.BeforeAfter))
df.across <- as.data.table(rbind(df.w.across,
                                 df.c.across,
                                 df.e.across))

## SAVE RESULT ***************************************************************** # 
if(save_which=='ALL' | save_which=='FINAL'){                                     #
  save_fn_across <- 'results_allpts_acrossOHT.RDS'                               #
  save_fn_beforeafter <- 'results_allpts_beforeafterOHT.RDS'                     #
  # save as RDS files (csv would work too)                                       #
  saveRDS(df.across, file=paste(save_here, save_fn_across, sep='/'))             #
  saveRDS(df.before.after, file=paste(save_here, save_fn_beforeafter, sep='/'))  #
  print("-- Statistical results successfully written")}                          #
# ****************************************************************************** #
# ----------------------------------------------------------- #
# **************************************************************************************************** 
# **************************************************************************************************** 





# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
## STEP 7: Repeat statistics for site means 
## NOTE: We use many of the same variables as the all-data script, 
##       so BE CAREFUL GOING BACK & FORTH! 
## *** 
# means and all datasets
dfa <- df4b     # all data
dfm <- df5.sm   # site means
# separate by domains
dfwa <- dfm[which(dfm$Domain=="West")]
dfca <- dfm[which(dfm$Domain=="Central")]
dfea <- dfm[which(dfm$Domain=="East")]

# [WESTERN DOMAIN stats] -----------------------------------------------------------------------------------
# pre-OHT
dfwa.cl.pre <- dfwa[which(dfwa$timeslice=="pre-OHT" & dfwa$Material_x=="clay" & dfwa$West_Subdomain=="east")]
dfwa.ca.pre <- dfwa[which(dfwa$timeslice=="pre-OHT" & dfwa$Material_x=="carbonate")]
# post-OHT
dfwa.cl.post <- dfwa[which(dfwa$timeslice=="post-OHT" & dfwa$Material_x=="clay")]
dfwa.ca.post <- dfwa[which(dfwa$timeslice=="post-OHT" & dfwa$Material_x=="carbonate")]
# t-tests for change in clay and carbonate
w.pre.ttest <- t.test(x=dfwa.cl.pre$d18O_smow, y=dfwa.ca.pre$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
w.post.ttest <- t.test(x=dfwa.cl.post$d18O_smow, y=dfwa.ca.post$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
w.cl.ttest <- t.test(x=dfwa.cl.pre$d18O_smow, y=dfwa.cl.post$d18O_smow, alternative='two.sided', paired=FALSE)
w.ca.ttest <- t.test(x=dfwa.ca.pre$d18O_smow, y=dfwa.ca.post$d18O_smow, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
dfwa2 <- as.data.table(rbind(dfwa.cl.pre, dfwa.ca.pre, dfwa.cl.post, dfwa.ca.post))
w.anova <- glm(d18O_smow ~ timeslice * Material_x, data=dfwa2)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
w.cl.conf.int_low <- w.cl.ttest$conf.int[1]*-1    # clay shift low conf int
w.cl.conf.int_high <- w.cl.ttest$conf.int[2]*-1   # clay shift high conf int
w.cl.OHTshift <- w.cl.ttest$estimate[2] - w.cl.ttest$estimate[1]  # clay shift
w.cl.OHTshift.pval <- w.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
w.ca.conf.int_low <- w.ca.ttest$conf.int[1]*-1    # carb shift low conf int
w.ca.conf.int_high <- w.ca.ttest$conf.int[2]*-1   # carb shift high conf int
w.ca.OHTshift <- w.ca.ttest$estimate[2] - w.ca.ttest$estimate[1]   # carb shift
w.ca.OHTshift.pval <- w.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
w.pre.conf.int_low <- w.pre.ttest$conf.int[1]   # big delta pre oht low conf int
w.pre.conf.int_high <- w.pre.ttest$conf.int[2]  # big delta pre oht high conf int
w.pre.bigdelta <- w.pre.ttest$estimate[1] - w.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
w.post.conf.int_low <- w.post.ttest$conf.int[1]  # big delta post oht low conf int
w.post.conf.int_high <- w.post.ttest$conf.int[2] # big delta post oht high conf int
w.post.bigdelta <- w.post.ttest$estimate[1] - w.post.ttest$estimate[2]  # big delta post oht 
# big delta change
w.bigdelta_difference <- w.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
w.bigdelta_pval <- coef(summary(w.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
w.before <- as.data.table(cbind('big_delta'=w.pre.bigdelta, 'CI_low'=w.pre.conf.int_low, 'CI_high'=w.pre.conf.int_high)) # pre oht data
w.before$timeslice <- 'pre-OHT'
w.before$Domain <- "West"
w.after <- as.data.table(cbind('big_delta'=w.post.bigdelta, 'CI_low'=w.post.conf.int_low, 'CI_high'=w.post.conf.int_high)) # post oht data
w.after$timeslice <- 'post-OHT'
w.after$Domain <- "West"
## ACROSS OHT DATA FRAME
w.cl.across <- as.data.table(cbind('OHT_shift'=w.cl.OHTshift, 'CI_low'=w.cl.conf.int_low, 'CI_high'=w.cl.conf.int_high, 'pval'=w.cl.OHTshift.pval ))
w.cl.across$metric <- 'clay_d18' ; w.cl.across$Domain <- 'West'
w.ca.across <- as.data.table(cbind('OHT_shift'=w.ca.OHTshift, 'CI_low'=w.ca.conf.int_low, 'CI_high'=w.ca.conf.int_high, 'pval'=w.ca.OHTshift.pval ))
w.ca.across$metric <- 'carbonate_d18' ; w.ca.across$Domain <- 'West'
w.BD.across <- as.data.table(cbind('OHT_shift'=w.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=w.bigdelta_pval ))
w.BD.across$metric <- 'big_delta' ; w.BD.across$Domain <- 'West'
## -- FINAL DOMAIN RESULTS
df.w.SM.BeforeAfter <- as.data.table(rbind(w.before, w.after))
df.w.SM.across <- as.data.table(rbind(w.cl.across, w.ca.across, w.BD.across))
# ----------------------------------------------------------------------------------------------------


# [CENTRAL DOMAIN stats] -----------------------------------------------------------------------------------
dfca.cl.pre <- dfca[which(dfca$timeslice=="pre-OHT" & dfca$Material_x=="clay" )]
dfca.ca.pre <- dfca[which(dfca$timeslice=="pre-OHT" & dfca$Material_x=="carbonate")]
# post-OHT
dfca.cl.post <- dfca[which(dfca$timeslice=="post-OHT" & dfca$Material_x=="clay")]
dfca.ca.post <- dfca[which(dfca$timeslice=="post-OHT" & dfca$Material_x=="carbonate")]
# t-tests for change in clay and carbonate
c.pre.ttest <- t.test(x=dfca.cl.pre$d18O_smow, y=dfca.ca.pre$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
c.post.ttest <- t.test(x=dfca.cl.post$d18O_smow, y=dfca.ca.post$d18O_smow, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
c.cl.ttest <- t.test(x=dfca.cl.pre$d18O_smow, y=dfca.cl.post$d18O_smow, alternative='two.sided', paired=FALSE)
c.ca.ttest <- t.test(x=dfca.ca.pre$d18O_smow, y=dfca.ca.post$d18O_smow, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
c.anova <- glm(d18O_smow ~ timeslice * Material_x, data=dfca)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
c.cl.conf.int_low <- c.cl.ttest$conf.int[1]*-1    # clay shift low conf int
c.cl.conf.int_high <- c.cl.ttest$conf.int[2]*-1   # clay shift high conf int
c.cl.OHTshift <- c.cl.ttest$estimate[2] - c.cl.ttest$estimate[1]  # clay shift
c.cl.OHTshift.pval <- c.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
c.ca.conf.int_low <- c.ca.ttest$conf.int[1]*-1    # carb shift low conf int
c.ca.conf.int_high <- c.ca.ttest$conf.int[2]*-1   # carb shift high conf int
c.ca.OHTshift <- c.ca.ttest$estimate[2] - c.ca.ttest$estimate[1]   # carb shift
c.ca.OHTshift.pval <- c.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
c.pre.conf.int_low <- c.pre.ttest$conf.int[1]   # big delta pre oht low conf int
c.pre.conf.int_high <- c.pre.ttest$conf.int[2]  # big delta pre oht high conf int
c.pre.bigdelta <- c.pre.ttest$estimate[1] - c.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
c.post.conf.int_low <- c.post.ttest$conf.int[1]  # big delta post oht low conf int
c.post.conf.int_high <- c.post.ttest$conf.int[2] # big delta post oht high conf int
c.post.bigdelta <- c.post.ttest$estimate[1] - c.post.ttest$estimate[2]  # big delta post oht 
# big delta change
c.bigdelta_difference <- c.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
c.bigdelta_pval <- coef(summary(c.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
c.before <- as.data.table(cbind('big_delta'=c.pre.bigdelta, 'CI_low'=c.pre.conf.int_low, 'CI_high'=c.pre.conf.int_high)) # pre oht data
c.before$timeslice <- 'pre-OHT'
c.before$Domain <- "Central"
c.after <- as.data.table(cbind('big_delta'=c.post.bigdelta, 'CI_low'=c.post.conf.int_low, 'CI_high'=c.post.conf.int_high)) # post oht data
c.after$timeslice <- 'post-OHT'
c.after$Domain <- "Central"
## ACROSS OHT DATA FRAME
c.cl.across <- as.data.table(cbind('OHT_shift'=c.cl.OHTshift, 'CI_low'=c.cl.conf.int_low, 'CI_high'=c.cl.conf.int_high, 'pval'=c.cl.OHTshift.pval ))
c.cl.across$metric <- 'clay_d18' ; c.cl.across$Domain <- 'Central'
c.ca.across <- as.data.table(cbind('OHT_shift'=c.ca.OHTshift, 'CI_low'=c.ca.conf.int_low, 'CI_high'=c.ca.conf.int_high, 'pval'=c.ca.OHTshift.pval ))
c.ca.across$metric <- 'carbonate_d18' ; c.ca.across$Domain <- 'Central'
c.BD.across <- as.data.table(cbind('OHT_shift'=c.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=c.bigdelta_pval ))
c.BD.across$metric <- 'big_delta' ; c.BD.across$Domain <- 'Central'
## -- FINAL DOMAIN RESULTS
df.c.SM.BeforeAfter <- as.data.table(rbind(c.before, c.after))
df.c.SM.across <- as.data.table(rbind(c.cl.across, c.ca.across, c.BD.across))
# ----------------------------------------------------------------------------------------------------


# [EASTERN DOMAIN stats] -----------------------------------------------------------------------------------
# pre-OHT  --  first do regression
dfea.pre <- dfea[which(dfea$timeslice=="pre-OHT")]
dfea.pre$predicted <- predict(XeFit, dfea.pre)
dfea.pre$residuals <- dfea.pre$d18O_smow - dfea.pre$predicted
# ... now split to two dataframes
dfea.cl.pre <- dfea.pre[which(dfea.pre$Material_x=='clay')]
dfea.ca.pre <- dfea.pre[which(dfea.pre$Material_x=='carbonate')]
# post-OHT  --  first do regression
dfea.post <- dfea[which(dfea$timeslice=="post-OHT")]
dfea.post$predicted <- predict(XeFit, dfea.post)
dfea.post$residuals <- dfea.post$d18O_smow - dfea.post$predicted
# ... now split to two dataframes
dfea.cl.post <- dfea.post[which(dfea.post$Material_x=='clay')]
dfea.ca.post <- dfea.post[which(dfea.post$Material_x=='carbonate')]
# t-tests for change in clay and carbonate
e.pre.ttest <- t.test(x=dfea.cl.pre$residuals, y=dfea.ca.pre$residuals, alternative='two.sided', paired=FALSE, mu=-3.5)
e.post.ttest <- t.test(x=dfea.cl.post$residuals, y=dfea.ca.post$residuals, alternative='two.sided', paired=FALSE, mu=-3.5)
# t-tests for single mineral across OHT
e.cl.ttest <- t.test(x=dfea.cl.pre$residuals, y=dfea.cl.post$residuals, alternative='two.sided', paired=FALSE)
e.ca.ttest <- t.test(x=dfea.ca.pre$residuals, y=dfea.ca.post$residuals, alternative='two.sided', paired=FALSE)
# ANOVA for big delta
dfea2 <- as.data.table(rbind(dfea.pre, dfea.post))
e.anova <- glm(residuals ~ timeslice * Material_x, data=dfea2)
# *** CONSTRUCT SUMMARY DATAFRAME
# Clay across OHT 
e.cl.conf.int_low <- e.cl.ttest$conf.int[1]*-1    # clay shift low conf int
e.cl.conf.int_high <- e.cl.ttest$conf.int[2]*-1   # clay shift high conf int
e.cl.OHTshift <- e.cl.ttest$estimate[2] - e.cl.ttest$estimate[1]  # clay shift
e.cl.OHTshift.pval <- e.cl.ttest$p.value   # clay shift p value
# Carbonate across OHT 
e.ca.conf.int_low <- e.ca.ttest$conf.int[1]*-1    # carb shift low conf int
e.ca.conf.int_high <- e.ca.ttest$conf.int[2]*-1   # carb shift high conf int
e.ca.OHTshift <- e.ca.ttest$estimate[2] - e.ca.ttest$estimate[1]   # carb shift
e.ca.OHTshift.pval <- e.ca.ttest$p.value    # carb shift p value
# pre OHT big delta
e.pre.conf.int_low <- e.pre.ttest$conf.int[1]   # big delta pre oht low conf int
e.pre.conf.int_high <- e.pre.ttest$conf.int[2]  # big delta pre oht high conf int
e.pre.bigdelta <- e.pre.ttest$estimate[1] - e.pre.ttest$estimate[2]  # big delta pre oht
# post OHT big delta
e.post.conf.int_low <- e.post.ttest$conf.int[1]  # big delta post oht low conf int
e.post.conf.int_high <- e.post.ttest$conf.int[2] # big delta post oht high conf int
e.post.bigdelta <- e.post.ttest$estimate[1] - e.post.ttest$estimate[2]  # big delta post oht 
# big delta change
e.bigdelta_difference <- e.anova$coefficients['timeslicepre-OHT:Material_xclay']*-1  # shift in big delta
e.bigdelta_pval <- coef(summary(e.anova))[4,4]   # big delta shift p value
## PRE AND POST DATAFRAME
e.before <- as.data.table(cbind('big_delta'=e.pre.bigdelta, 'CI_low'=e.pre.conf.int_low, 'CI_high'=e.pre.conf.int_high)) # pre oht data
e.before$timeslice <- 'pre-OHT'
e.before$Domain <- "East"
e.after <- as.data.table(cbind('big_delta'=e.post.bigdelta, 'CI_low'=e.post.conf.int_low, 'CI_high'=e.post.conf.int_high)) # post oht data
e.after$timeslice <- 'post-OHT'
e.after$Domain <- "East"
# combine
df.e.BeforeAfter <- as.data.table(rbind(e.before, e.after))
## ACROSS OHT DATA FRAME
e.cl.across <- as.data.table(cbind('OHT_shift'=e.cl.OHTshift, 'CI_low'=e.cl.conf.int_low, 'CI_high'=e.cl.conf.int_high, 'pval'=e.cl.OHTshift.pval ))
e.cl.across$metric <- 'clay_d18' ; e.cl.across$Domain <- 'East'
e.ca.across <- as.data.table(cbind('OHT_shift'=e.ca.OHTshift, 'CI_low'=e.ca.conf.int_low, 'CI_high'=e.ca.conf.int_high, 'pval'=e.ca.OHTshift.pval ))
e.ca.across$metric <- 'carbonate_d18' ; e.ca.across$Domain <- 'East'
e.BD.across <- as.data.table(cbind('OHT_shift'=e.bigdelta_difference, 'CI_low'=NA, 'CI_high'=NA, 'pval'=e.bigdelta_pval ))
e.BD.across$metric <- 'big_delta' ; e.BD.across$Domain <- 'East'
## -- FINAL DOMAIN RESULTS
df.e.SM.BeforeAfter <- as.data.table(rbind(e.before, e.after))
df.e.SM.across <- as.data.table(rbind(e.cl.across, e.ca.across, e.BD.across))
# ----------------------------------------------------------------------------------------------------


# **************************************************************************************************** 
# **************************************************************************************************** 
# ### BRING ALL DOMAINS SITE MEANS RESULTS TOGETHER ### 
df.SM.before.after <- as.data.table(rbind(df.w.SM.BeforeAfter,
                                       df.c.SM.BeforeAfter,
                                       df.e.SM.BeforeAfter))
df.SM.across <- as.data.table(rbind(df.w.SM.across,
                                 df.c.SM.across,
                                 df.e.SM.across))

## SAVE RESULT ******************************************************************** # 
if(save_which=='ALL' | save_which=='FINAL'){                                        #
  save_fn_across <- 'results_sitemeans_acrossOHT.RDS'                               #
  save_fn_beforeafter <- 'results_sitemeans_beforeafterOHT.RDS'                     #
  # save as RDS files (csv would work too)                                          #
  saveRDS(df.SM.across, file=paste(save_here, save_fn_across, sep='/'))             #
  saveRDS(df.SM.before.after, file=paste(save_here, save_fn_beforeafter, sep='/'))  #
  print("-- Statistical results successfully written")}                             #
# ********************************************************************************* #
# --------------------------------------------------------------------------------- #
# **************************************************************************************************** 
# **************************************************************************************************** 





# 