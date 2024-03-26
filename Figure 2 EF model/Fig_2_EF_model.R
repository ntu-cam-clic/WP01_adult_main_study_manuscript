# Four-factor EF model
# Cognitive Flexibility, Working Memory, Inhibition, Generation

# rm(list=ls())
library(lavaan)
library(lavaanPlot)
library(psych)

df = read.csv("path_to_data_file.csv")
df$SWM_BE_r = -df$SWM_BE

# EF four correlated factors 
model_to_use = '
CF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_time_ratio_correct_r + TSS_WHERE_RT_cost_r
WorkingMemory =~ BDS + SWM_BE_r + RS_digit_total_recall
Inhibition =~ Stroop_acc_diff + SST_SSRT_model_r
Generation =~ VF_FAS_Avg + VF_Category + AUT_test_item_count_all
'

# # alt models for comparison
# # alt model 1: unitary EF 
# model_to_use = '
# EF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_time_ratio_correct_r + TSS_WHERE_RT_cost_r
# + BDS + SWM_BE_r + RS_digit_total_recall
# + Stroop_acc_diff + SST_SSRT_model_r
# + VF_FAS_Avg + VF_Category + AUT_test_item_count_all
# '
# 
# # alt model 2: independent EF 
# model_to_use = '
# CF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_time_ratio_correct_r + TSS_WHERE_RT_cost_r
# WorkingMemory =~ BDS + SWM_BE_r + RS_digit_total_recall
# Inhibition =~ Stroop_acc_diff + SST_SSRT_model_r
# Generation =~ VF_FAS_Avg + VF_Category + AUT_test_item_count_all
# CF ~~ 0* WorkingMemory
# CF ~~ 0* Inhibition
# WorkingMemory ~~ 0* Inhibition
# '

data_to_use = df[c("WCST_n_pError_r", 
                   "IED_ES8_r", 
                   "TMT_BA_time_ratio_correct_r",
                   "TSS_WHERE_RT_cost_r", 
                   "TSS_WHERE_Acc_cost", 
                   "PRL_Beta",
                   "BDS",  
                   "SWM_BE_r", 
                   "RS_digit_total_recall", 
                   "Stroop_acc_diff",
                   "SST_SSRT_model_r",
                   "VF_FAS_Avg",
                   "VF_Category",
                   "AUT_test_item_count_all",
                   "WASI_FSIQ",
                   "WASI_V_Norm",
                   "WASI_B_Norm",
                   "APM18_n_correct",
                   "FS_GCA"
                   )]

fit <- lavaan::cfa(model_to_use
                   , data = scale(data_to_use)
                   , std.lv = T
                   , missing = 'ML'
                   )
summary(fit, fit.measures=T, stand = T)

# plot the model
lavaanPlot(model = fit,
           edge_options = list(color = "gray")
           , cov = T
           , coefs = T
           , stand = T
           , stars = c( 'latent', 'regress','covs')
           )

# save model fit indices
fit_indices = fitmeasures(fit, c("chisq", "df", "pvalue", "aic", "srmr", "cfi", "ifi"))

