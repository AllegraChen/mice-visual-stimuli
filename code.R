library(ggplot2)
library(lme4)

session = list()
for (i in 1:5) {
    session[[i]] = readRDS(paste('data/session', i, '.rds', sep = ''))
    print(session[[i]]$mouse_name)
    print(session[[i]]$date_exp)
}

t = 0.4
data = data.frame()

for (session_id in 1:5) {
    n_trials = length(session[[session_id]]$spks)
    left_contrast = session[[session_id]]$contrast_left
    right_contrast = session[[session_id]]$contrast_right
    avg_firing_rate = sapply(session[[session_id]]$spks, 
                             FUN=function(spks_df) {
                                 n_neurons = dim(spks_df)[1]
                                 sum(spks_df)/n_neurons/t # average firing rate
                             })
    data = rbind(data,
                 cbind(rep(session_id, n_trials), 
                       left_contrast, 
                       right_contrast, 
                       avg_firing_rate))
}

names(data) = c("session_id", "left_contrast", "right_contrast", "avg_firing_rate")

me_anova_model = lmer(avg_firing_rate ~ (1|session_id) + as.factor(left_contrast) * as.factor(right_contrast), data)
summary(me_anova_model)

plot(me_anova_model)

# These plots per session/leftcontrast/rightcontrast/deltacontrast: 
#     https://stats.stackexchange.com/questions/552734/r-plotting-lmer-confidence-intervals-per-faceted-group
#     https://stackoverflow.com/questions/31075407/plot-mixed-effects-model-in-ggplot
#     https://hausetutorials.netlify.app/0003_ggplot_modelfitting.html

