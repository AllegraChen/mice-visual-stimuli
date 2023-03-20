library(dplyr)
library(summarytools)
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


print(summarytools::dfSummary(data), method = "render")

########### spks train


# Session 1 for the mouse Cori
ID = 1
t = 0.4 # from Background 

n_trials = length(session[[ID]]$spks)
n_neurons = dim(session[[ID]]$spks[[1]])[1]

range_trials = 1:10
range_neurons = 1:5

session_1 = session[[ID]]


trial_1_time = session_1$time[[1]]
trial_1_time_from_stimulus = trial_1_time - trial_1_time[1]
trial_1_neuron_1_spikes = session_1$spks[[1]][1,]

temp_neuron_1_data = data.frame(trial = rep(1, 39),
                                time_from_stimulus = trial_1_time_from_stimulus,
                                spikes = trial_1_neuron_1_spikes)

ggplot(data=temp_neuron_1_data,
       mapping=aes(time_from_stimulus, spikes)) +
    geom_linerange(mapping=aes(ymin=0, 
                               ymax=spikes))

ggsave2("fig/spkies.png")


library(GGally)

data %>% 
    select(session_id, right_contrast, avg_firing_rate) %>% 
    ggpairs(mapping=aes(col=factor(session_id), alpha=0.5),
            lower = list(combo = wrap("facethist", binwidth = 1)))

ggsave2("fig/ggpairs.png")


