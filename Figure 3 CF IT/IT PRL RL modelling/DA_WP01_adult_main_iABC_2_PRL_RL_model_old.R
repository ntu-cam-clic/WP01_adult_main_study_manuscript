# WP01 adult main data preprocessing
# PRL RL modelling

library(rstan)
library(jsonlite)
library(hBayesDM)
library(RcppEigen)
source('function_WP01_adult_main_iABC_PRL_RL.R')

# customize your data_folder_path and study_code
data_folder_path = "path to folder with json files"

PRL_RL_out_path = "path of output file"

study_code = 'WP01_adult_main'

idx_complete = PR_complete(data_folder_path)
prepare_PR_data_hBayesDM(data_folder_path, idx_complete, study_code)

# PR model 1 (prl_rp model from hBayesDM)
output_PR <- prl_rp(
  data = 'WP01_02_PRL_data_all_hBDM.txt', niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4) 

# save model fitting results as .Rdata
saveRDS(output_PR, 'WP02_prl_rp.Rdata')

PRL_indPar = output_PR$allIndPars
PRL_RL_out = PRL_indPar

colnames(PRL_RL_out)[1] = 'ID'
colnames(PRL_RL_out)[2] = 'PRL_Alpha_Pun'
colnames(PRL_RL_out)[3] = 'PRL_Alpha_Rew'
colnames(PRL_RL_out)[4] = 'PRL_Beta'

PRL_RL_out = PRL_RL_out[order(PRL_RL_out$ID),]

write.csv(PRL_RL_out, PRL_RL_out_path, row.names = F)