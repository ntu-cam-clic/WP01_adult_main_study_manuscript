# Fig 4

library(psych)
library(lavaan)
library(lavaanPlot)
library(ggplot2)

df = read.csv("path to merged dataset")
df_gca = read.csv("path to GCA factor score")
df$FS_GCA = df_gca$FS_GCA

## regressions with CF and IT factor scores to different outcomes, controlled for intelligence (labelled as GCA, general cognitive ability)

# reading
results_reading = lm(WJ_READNG_Z ~  FS_GCA + FS_CF_raw + FS_CF_IT, data = df)
summary(results_reading)

# math
results_math = lm(WJ_MATH_Z ~  FS_GCA + FS_CF_raw + FS_CF_IT, data = df)
summary(results_math)


### figure 4 for the manuscript

model = '
WJ_READNG_Z ~ FS_CF_raw + FS_CF_IT + FS_GCA
WJ_MATH_Z ~ FS_CF_raw + FS_CF_IT + FS_GCA
'

data_to_use = df[c("WJ_READNG_Z",
                   "WJ_MATH_Z",
                   "FS_GCA",
                   "FS_CF_IT",
                   "FS_CF_raw")]

fit <- lavaan::sem(model, 
                   data = scale(data_to_use),
                   std.lv = T,
                   missing = 'ML'
)

summary_fit = summary(fit, fit.measures=T, stand = T)

lavaanPlot(model = fit,
           edge_options = list(color = "gray")
           , cov = T
           , coefs = T
           , stand = T
           , stars = c( 'latent', 'regress', 'covs')
)

fit_measures <- data.frame(FitMeasures = names(summary_fit$fit.measures),
                           Value = as.numeric(summary_fit$fit.measures))

# Write the fit measures to a CSV file
write.csv(fit_measures, "fit_measures.csv", row.names = FALSE)


