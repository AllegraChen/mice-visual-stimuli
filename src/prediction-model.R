library(ggplot2)

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
    feedback_type = session[[session_id]]$feedback_type
    avg_firing_rate = sapply(session[[session_id]]$spks, 
                             FUN=function(spks_df) {
                                 n_neurons = dim(spks_df)[1]
                                 sum(spks_df)/n_neurons/t # average firing rate
                             })
    data = rbind(data,
                 cbind(rep(session_id, n_trials), 
                       left_contrast, 
                       right_contrast, 
                       feedback_type,
                       avg_firing_rate))
}

names(data) = c("session_id", "left_contrast", "right_contrast", "feedback_type", "avg_firing_rate")
data = as.data.frame(apply(data, MARGIN = 2, as.numeric))

# data[data$feedback_type == -1, "feedback_type"] = 0

test_data = data[1:100, ] # the first 100 trials in Session 1 used as testing dataset
train_data = data[101:nrow(data), ]

log_lm = glm(feedback_type ~ as.factor(left_contrast) * as.factor(right_contrast) + avg_firing_rate, data=train_data)
predict(log_reg)

threshold = 0
predicted_values = ifelse(predict(log_lm, newdata = test_data)>threshold,1,0)
actual_values = test_data$feedback_type
conf_matrix = table(predicted_values, actual_values)
conf_matrix

# sensitivity
72/(72+2)
# specificity
2/(2+24)




ggplot(train_data, aes(x=avg_firing_rate, y=feedback_type)) + 
    geom_point(alpha=.5) +
    stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
