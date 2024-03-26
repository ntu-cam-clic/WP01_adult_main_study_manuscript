## WCST RL modelling 
# scripts from Aleya
# Sequential learning model 

require(rjags)
require(coda)
require(MCMCvis)
library(Rcpp)  
library(data.table)
library(ggplot2)
library(lme4)
library(lmerTest)
library(parallel)
library(plyr)
library(readr)
library(tidyr)
library(HDInterval)
library(jagsUI)
library(R2jags)
library(bridgesampling)

#rm(list=ls())

# configure paths

# file path to the prepared data for JAGS (.txt) 
WCST_JAGS_data_path = "path to JAGS data"

# file path to the JAGS model file (.txt)
JAGS_model_file_rpd0_path = "path to model file"

# currently we only use the winning model rpd0
# if you need to fit multiple models, you need to specify model file paths separately like below
# JAGS_model_file_rpdf_path = ""
# JAGS_model_file_rp1f_path = ""

# path to save JAGS results (.RData)
JAGS_results_path = "path of output saved"

# path to save individual parameters (.csv, default winning model rpd0)
WCST_RL_out_path = "path to save parameters"

# path to save model fitting summary (.csv)
WCST_RL_model_summary_rpd0_path = "path to save model fitting summary"

# read in datra
data<-read.table(WCST_JAGS_data_path, header=TRUE, sep=",")
data <- data.table(data)

## prepare data for jags ##
data$patient <- as.factor(data$subnum)

all_patients_map <- data.table(
  patient=sort(unique(data$patient))
)
all_patients_map[, patientseqnum := order(patient)]

# all_patients_map
data[all_patients_map, patientseqnum := patientseqnum,
         on = c(patient = "patient")]

trial_per_subj<-count(data$subnum)
trial_per_subj<-trial_per_subj$freq

#### prepare matching matrix for jags, should be in the format (n_subjects x ntrials x 1 x 3)
library(tidyverse)
deck_match_rule<-xtabs(cbind(corr_col,corr_shape,corr_num) ~ patient+trial, data)

# make 'one' to model stochastically in model
one <- matrix(1,nrow=length(unique(data$subnum)), ncol = 128)

#outcome<-data %>% select(2,10,29) 
outcome<-xtabs(outcome ~ patient + trial,data)

# prepare all data for jags
jagsdata <- list(
  #N_GROUPS = 2,  # number of groups, numbered from 1
  N_SUBJECTS = length(unique(data$subnum)),  #Numbered from 1.
  N_TRIALS = trial_per_subj,  # TOTAL number of trials per participant
  deck_match_rule=deck_match_rule, #equivalent to matching matrix (m) in matlab script
  outcome = outcome,
  one = one
)

##initialised values per chain
N_SUBJECTS = as.numeric(length(unique(data$subnum)))

# ### MODEL 1 : FULL MODEL RPDF ####
# params <- c("ocd_r_a", "r_sd","ocd_p_a", "p_sd","ocd_d_a", "d_sd","ocd_f_a", "f_sd",
#             "ctl_r_a","ctl_p_a","ctl_d_a","ctl_f_a",
#             "r", "p", "d", "f")
# #### Set up the JAGS model and settings
# #jags.m <- jags.model( file = "wcst_rpdf_hierPredk1.txt", data=jagsdata, inits=inits, n.chains=1, n.adapt=10000 )
# # run model
# samps_rpdf <- jags(jagsdata, inits=NULL, params,
#               model.file ="rpdf_dnormsd.txt", n.chains=4, n.iter=2000, 
#               n.burnin=500, n.thin=1, DIC=T)
# save(samps_rpdf, file = "samps_rpdf_ntu.RData")
# # get summary of parameter values and r-hat values
# mcmcsummary<-MCMCsummary(samps_rpdf)


### MODEL 2 : RPD0 (winning model for Glascher et al. 2019 and Marzuki et al. 2021) ####
## specify parameters to be monitored
params <- c("r_a", "r_sd","p_a", "p_sd","d_a", "d_sd",
            "r", "p", "d")
# run model
samps_rpd0 <- jags(jagsdata, 
                   inits=NULL, 
                   params,
                   model.file = JAGS_model_file_rpd0_path, 
                   n.chains=4, 
                   n.iter=2000, 
                   n.burnin=500, 
                   n.thin=1, 
                   DIC=T)
save(samps_rpd0, file = JAGS_results_path)
summary_rpd0 <- MCMCsummary(samps_rpd0)


# ### MODEL 3 : RP1F ####
# ## specify parameters to be monitored
# params <- c("ocd_r_a", "r_sd","ocd_p_a", "p_sd","ocd_f_a", "f_sd",
#             "ctl_r_a","ctl_p_a", "ctl_f_a",
#             "r", "p", "f")
# # run model
# samps_rp1f <- jags(jagsdata, inits=NULL, params,
#               model.file ="rp1f_dnormsd.txt", n.chains=4, n.iter=2000, 
#               n.burnin=500, n.thin=1, DIC=T)
# rp1f_summary <- MCMCsummary(samps_rp1f)
# save(samps_rp1f, file = "samps_rp1f_ntu.RData")
# 
# 
# ### MODEL 4 : RRDF ####
# ## specify parameters to be monitored
# params <- c("ocd_r_a", "r_sd","ocd_d_a", "d_sd","ocd_f_a", "f_sd",
#                        "ctl_r_a","ctl_d_a", "ctl_f_a", 
#                        "r", "d", "f")
# # run model
# samps_rrdf <- jags(jagsdata, inits=NULL, params,
#               model.file ="rrdf_dnormsd.txt", n.chains=4, n.iter=2000, 
#               n.burnin=500, n.thin=1, DIC=T)
# save(samps_rrdf, file = "samps_rrdf_ntu.RData")
# 
# 
# ### MODEL 5 : RPD1 (winning model for Bishara et al. 2010) ####
# ## specify parameters to be monitored
# params <- c("r_a", "r_sd","p_a", "p_sd","d_a", "d_sd",
#             "r", "p", "d")
# # run model
# samps_rpd1 <- jags(jagsdata, inits=NULL, params,
#                    model.file ="rpd1_dnormsd.txt", n.chains=4, n.iter=2000, 
#                    n.burnin=500, n.thin=1, DIC=T)
# save(samps_rpd1, file = "samps_rpd1_ntu.RData")
# summary_rpd1 <- MCMCsummary(samps_rpd1)



######################## read JAGS results #####################

# #load saved JAGS results (winning model)
load(JAGS_results_path)
summary_rpd0 <- MCMCsummary(samps_rpd0)

#### get HDI of parameters from winning model ####
winningmodel<-samps_rpd0 #put winning model here
mu_r <- winningmodel$BUGSoutput$sims.list$r_a
mu_p <- winningmodel$BUGSoutput$sims.list$p_a
mu_d <- winningmodel$BUGSoutput$sims.list$d_a

hist(mu_r)
hist(mu_p)
hist(mu_d)
 
hdi(mu_r)
hdi(mu_p)
hdi(mu_d)
 
# get subject level paramaters ##
r<-winningmodel$BUGSoutput$mean$r
p<-winningmodel$BUGSoutput$mean$p
d<-winningmodel$BUGSoutput$mean$d

# sorted by ID
ID = sort(unique(data$subnum))

WCST_RL_out<-data.frame(ID, r, p, d)
colnames(WCST_RL_out)[2] = 'WCST_Alpha_rew'
colnames(WCST_RL_out)[3] = 'WCST_Alpha_pun'
colnames(WCST_RL_out)[4] = 'WCST_Beta'

# save individual parameters
write.csv(WCST_RL_out, WCST_RL_out_path, row.names = F)

# save model summary
write.csv(summary_rpd0, WCST_RL_model_summary_rpd0_path, row.names = T)
