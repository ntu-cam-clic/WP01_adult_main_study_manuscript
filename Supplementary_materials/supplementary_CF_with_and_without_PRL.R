# CF with PRL Beta

library(lavaan)
library(lavaanPlot)
library(lavaanExtra)

df = read.csv('path to merged dataset')

# with PRL
model_to_use = '
CF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_time_ratio_correct_r + TSS_WHERE_RT_cost_r + PRL_crit4_attained_block2'
data_to_use = df[c("WCST_n_pError_r", 
                   "IED_ES8_r", 
                   "TMT_BA_time_ratio_correct_r",
                   "TSS_WHERE_RT_cost_r", 
                   "PRL_crit4_attained_block2")]
fit <- lavaan::cfa(model_to_use
                   , data = scale(data_to_use)
                   , std.lv = T
                   , missing = 'ML'
)

summary(fit, fit.measures=T, stand = T)
lavaanPlot(model = fit,
           edge_options = list(color = "gray")
           , cov = T
           , coefs = T
           , stand = T
           , stars = c( 'latent', 'regress','covs')
)

lavaanExtra::nice_fit(fit, nice_table = T)

fitmeasures(fit, c("chisq", "df", "pvalue", "aic", "srmr", "cfi", "ifi"))


# without PRL
model_to_use2 = '
CF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_time_ratio_correct_r + TSS_WHERE_RT_cost_r'
fit2 <- lavaan::cfa(model_to_use2
                   , data = scale(data_to_use)
                   , std.lv = T
                   , missing = 'ML'
)
lavaanExtra::nice_fit(c(fit, fit2), nice_table = T)
