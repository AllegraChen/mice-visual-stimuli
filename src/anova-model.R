library(ggplot2)
library(cowplot)
library(patchwork)
# library(lme4) # to fit the mixed effect model
library(lmerTest) # use the lmer() function which computes the p values in the ANOVA table
library(easystats) # Model diagnostic plots for lme4 models
library(car) # for the Anova() function

########### ANOVA Model fitting #####################
# Create the data frame for fitting the model
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

# Fit the mixed effect anova model
me_anova_model = lmerTest::lmer(avg_firing_rate ~ (1|session_id) + as.factor(left_contrast) * as.factor(right_contrast), data)
me_anova_model_no_interaction = lmerTest::lmer(avg_firing_rate ~ (1|session_id) + as.factor(left_contrast) + as.factor(right_contrast), data)
me_anova_model_no_ranef = lm(avg_firing_rate ~ as.factor(left_contrast) * as.factor(right_contrast), data)
summary(me_anova_model)
anova(me_anova_model, me_anova_model_no_interaction) # Likelihood Ratio Test for the interaction-effect factor
anova(me_anova_model, me_anova_model_no_ranef) # Likelihood Ratio Test for the random-effect factor

#################### Model Diagnostics ######################
p1 = check_model(me_anova_model)
p1 = plot(p1)
p2 = plot(check_normality(me_anova_model))
p1 + p2 # patchwork library

ggsave2("fig/model-diagnostics-plots.png", scale=2)




plot(data$session_id, residuals)

#TODO# ICC?





# Helper graphs


# Normality of residuals
png("fig/normal-qq-plot.png", width=3000, height=3000, res=300)
par(mfrow=c(2,2))

plot(me_anova_model, main="Residuals vs Fitted")

residuals = residuals(me_anova_model)
qqnorm(residuals)
qqline(residuals)

dev.off()


ggplot(data, mapping=aes(x=factor(left_contrast), y=avg_firing_rate, col=factor(session_id))) +
    geom_jitter(alpha=0.3, size=1) +
    stat_summary(fun=mean, geom="point", shape=21, size=2, mapping=aes(fill=factor(session_id))) + 
    stat_summary(fun=mean, geom="line", mapping=aes(group=factor(session_id))) +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0, size = 1)

ggplot(data, mapping=aes(x=factor(right_contrast), y=avg_firing_rate, col=factor(session_id))) +
    geom_jitter(alpha=0.3, size=1) +
    stat_summary(fun=mean, geom="point", shape=21, size=2, mapping=aes(fill=factor(session_id))) + 
    stat_summary(fun=mean, geom="line", mapping=aes(group=factor(session_id))) +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0, size = 1)
    
ggplot(data, mapping=aes(x=factor(left_contrast), y=avg_firing_rate)) +
    stat_summary(fun=mean, geom="point", shape=21, size=2) + 
    stat_summary(fun=mean, geom="line") +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0, size = 1)

ggplot(data, mapping=aes(x=factor(left_contrast), y=avg_firing_rate)) +
    stat_summary(fun=mean, geom="point", shape=21, size=2, mapping=aes(col=factor(session_id))) + 
    stat_summary(fun=mean, geom="line", mapping=aes(group=factor(session_id), col=factor(session_id))) +
    stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0, size = 1) +
    stat_summary(fun=mean, geom="line")

# y-axis: average firing rate
# x-axis: 4 factor levels of left contrast (0, 0.25, 0.5, 1)
# group: session id (5)
# points: y = sample average firing rate
# line: y = fitted average firing rate
data$fitted_avg_firing_rate = fitted.values(me_anova_model)

ggplot(data, mapping=aes(x=left_contrast)) +
    geom_point(mapping=aes(y=avg_firing_rate)) + 
    geom_line(mapping=aes(y=fitted_avg_firing_rate))

plot(me_anova_model)

# These plots per session/leftcontrast/rightcontrast/deltacontrast: 
#     https://stats.stackexchange.com/questions/552734/r-plotting-lmer-confidence-intervals-per-faceted-group
#     https://stackoverflow.com/questions/31075407/plot-mixed-effects-model-in-ggplot
#     https://hausetutorials.netlify.app/0003_ggplot_modelfitting.html

