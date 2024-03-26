# wp01 adult main study
# supplementary materials table S4

library(lavaan)
library(lavaanPlot)
library(lavaanExtra)

data_path <- 'path to merged dataset'
data = read.csv(data_path)

# reverse coding for CF variables
data$WCST_n_pError_r = -data$WCST_n_pError
data$IED_ES8_r = -data$IED_ES8
data$TMT_BA_ratio_r = -data$TMT_BA_time_ratio_correct # using correct trials only
data$TSS_WHERE_RT_cost_r = -data$TSS_WHERE_RT_cost

# define data and model used for the cf factor 
df_cf = data[c("WCST_n_pError_r",
               "IED_ES8_r",
               "TMT_BA_ratio_r",
               "TSS_WHERE_RT_cost_r")]
model_cf = 'CF =~ WCST_n_pError_r + IED_ES8_r + TMT_BA_ratio_r + TSS_WHERE_RT_cost_r'

data_to_use = df_cf
model_to_use = model_cf

# run factor model in lavaan
fit <- lavaan::cfa(model_to_use
                   , data = scale(data_to_use)
                   , std.lv = T
                   , missing = 'ML')
summary(fit, fit.measures=T, stand = T)
lavaanPlot(model = fit,
           edge_options = list(color = "gray")
           , cov = T
           , coefs = T
           , stand = T
           , stars = c( 'latent', 'regress', 'covs'))





