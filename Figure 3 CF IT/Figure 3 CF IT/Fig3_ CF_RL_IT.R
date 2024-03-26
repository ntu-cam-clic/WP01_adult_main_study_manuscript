# CF RL IT

#rm(list=ls())
library(lavaan)
library(lavaanPlot)
library(psych)

df = read.csv("path to merged dataset")

# CF RL IT (inverse temperature) factor
model_cf_rl_beta = 'Inverse Temp =~ WCST_Beta + PRL_Beta + IED_Beta'
data_to_use = df[c('WCST_Beta', 'PRL_Beta', 'IED_Beta')]

# # just for testing, to avoid saturated model, add sl_icd (which is conceptually similar to CF RL Beta)
# model_cf_rl_beta = 'CF_Beta =~ WCST_Beta + PRL_Beta + IED_Beta + SL_strategy_ICD'
# data_to_use = df[c('WCST_Beta', 'PRL_Beta', 'IED_Beta', 'SL_strategy_ICD')]

model_to_use = model_cf_rl_beta
fit_cfrl_beta <- lavaan::cfa(model_to_use
                              , data = scale(data_to_use)
                              , std.lv = T
                              , missing = 'ML')
summary(fit_cfrl_beta, fit.measures=T, stand = T)

# Figure3 left panel
lavaanPlot(model = fit_cfrl_beta,
           edge_options = list(color = "gray")
           , cov = T
           , coefs = T
           , stand = T
           , stars = c( 'latent', 'regress', 'covs')
)

df$FS_CF_2_separate = as.numeric(lavPredict(fit_cfrl_beta))

# Figure 3 right panel
library(ggplot2)
# Scatter plot with regression line: IT, CF factor score
ggplot(df, aes(FS_CF_2_separate, FS_CF_1_raw)) +
  geom_point() + 
  geom_smooth(method=lm) + 
  labs(x = 'Inverse Temperature Factor Score', y = 'CF Factor Score')
#ggsave("invTemp_CF_FS_adult.png", width = 1500, height = 1500, units = "px")

