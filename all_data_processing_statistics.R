library(dplyr)
library(lattice)
library(lme4)
library(patchwork)
library(tidyr)
library(ggplot2)

######## ACCUMULATING DATA ##############################################################

##### TO DO: SET THE FOLDER PATH TO THE DATA FOLDER######
# this folder must contain all the [PARTICIPANT_ID]_VisualSearchFaces_[[YY]-[MM]-[DD]_[hh]h[mm].csv files extracted from psychopy
folder_path <- "/Users/linkamitome/Desktop/data_statistics/all_data"

csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# combine all csv files (participants) into a single dataframe
full_data <- csv_files %>%
  lapply(read.csv) %>%
  bind_rows()

# remove extra participant
full_data <- full_data %>%
  filter(participant != "49528")

######## PREPARING DATASETS #############################################################
#### Dataframes: 
# participant_data <- 
# questionnaire_data: contains written responses to the last question ("what did you see?")
# data: contains all trials, including blank trials and practice rounds
# target_data: subset of data containing only target trials 

participant_data <-  full_data %>%
  select(c("participant", "gender", "X.corrected..normal.vision", "handedness", "exp.vse")) %>%
  distinct(participant, .keep_all = TRUE)
  
questionnaire_data <- full_data %>% 
  select(c("participant", "session", "textbox_response.text")) %>%
  filter(textbox_response.text != "")
# export questionnaire data 
write.csv(questionnaire_data, "questionnaire_data.csv", row.names = FALSE)

data <- full_data %>%
  # select relevant rows and rename them for interpretability
  select(c("block_id", "target_presence", "target_position", "background_emotion",
           "target_emotion", "keyboard_response.keys", "keyboard_response.rt",
           "participant", "session")) %>%
  rename(round = block_id,
         response = keyboard_response.keys,
         rt = keyboard_response.rt,
         ID = participant,
         condition = session) %>%

  # code new column for accuracy
  mutate(accuracy = ifelse(response=="f" & target_presence==0, 1,
                           ifelse(response=="f" & target_presence==1, 0,
                                  ifelse(response=="j" & target_presence==1, 1, 0)))) %>%
  
  # conditions hashed out into scrambled (1/0) and outline(1/0)
  mutate(scrambled = ifelse(condition == 11, 0,
                            ifelse(condition == 12, 0,
                                   ifelse(condition == 13, 1,
                                          ifelse(condition ==14, 1, NA))))) %>%
  mutate(outline = ifelse(condition == 11, 1,
                          ifelse(condition == 12, 0,
                                 ifelse(condition == 13, 1,
                                        ifelse(condition ==14, 0, NA)))))

target_data <- data  %>% 
  # select only trials containing a target (non-filler)
  filter(target_presence==1) %>%
  
  # select experimental rounds only
  filter(round %in% c("block1", "block2", "block3", "block4")) %>%
  
   # create a target-background combinations into categories 
  mutate(item_type = ifelse(background_emotion=="neutral"&target_emotion=="threatening",
                            "threat_neutral",
                            ifelse(background_emotion=="neutral"&target_emotion=="friendly",
                                   "friendly_neutral",
                                   ifelse(background_emotion=="threatening"&target_emotion=="neutral",
                                          "neutral_threat",
                                          ifelse(background_emotion=="friendly"&target_emotion=="neutral",
                                                 "neutral_friendly", NA)))))

######## DATA ANALYSIS #############################################################

#### error rates (%) per participant participant (remove if any is over 30%)
participant_summary <- target_data %>%
  group_by(ID) %>%
  summarise(
    total_responses = n(),
    incorrect_responses = sum(accuracy == 0),
    percent_incorrect = (incorrect_responses / total_responses) * 100
  )

print(participant_summary, n=20)

#### error rates (%) per condition
error_stats <- target_data %>%
  mutate(condition = factor(condition)) %>%
  group_by(condition) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(accuracy == 1),
    percent_correct = (correct_trials / total_trials)*100,
    percent_error = 100 - percent_correct
  )

print(error_stats)

# graphing error rates per condition
ggplot(error_stats, aes(x = condition, y = percent_error, fill=condition)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(
    title = "Error Rates by Condition",
    x = "Condition",
    y = "Error Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),  
  )

#### error rates (%) per item type
error_by_item_condition <- target_data %>%
  group_by(item_type, condition) %>%
  summarise(
    n_trials = n(),
    n_errors = sum(accuracy == 0),
    error_rate = (n_errors / n_trials) * 100
  ) %>%
  ungroup() %>%
  mutate(item_type = recode(item_type,
                            "friendly_neutral" = "Friendly in Neutral",
                            "threat_neutral" = "Threatening in Neutral",
                            "neutral_friendly" = "Neutral in Friendly",
                            "neutral_threat" = "Neutral in Threatening")) %>%
  mutate(condition = recode(condition,
                            "11" = "Original / With outline",
                            "12" = "Original / No outline",
                            "13" = "Scrambled / With outline",
                            "14" = "Scrambled / No outline"))

error_by_item_condition$item_type <- factor(
  error_by_item_condition$item_type,
  levels = c(
    "Friendly in Neutral",
    "Threatening in Neutral",
    "Neutral in Friendly",
    "Neutral in Threatening"
  )
)

ggplot(error_by_item_condition, aes(x = item_type, y = error_rate, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  labs(
    title = "Error Rates by Item Type and Condition",
    x = "Item Type",
    y = "Error Rate (%)",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#### error rates by target position
error_by_position <- target_data %>%
  group_by(target_position) %>%
  summarise(
    n_trials = n(),
    n_errors = sum(accuracy == 0),
    error_rate = (n_errors / n_trials) * 100
  ) 

ggplot(error_by_position, aes(x = target_position, y = error_rate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  labs(
    title = "Error Rates by Item Type and Condition",
    x = "Item Type",
    y = "Error Rate (%)",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#### mean and standard deviation of RT 
rt_stats <- target_data %>% summarise(
  meanRT = mean(rt, na.rm = TRUE),
  sdRT = sd(rt, na.rm = TRUE)
)
mean_rt <- rt_stats$meanRT
sd_rt <- rt_stats$sdRT

print(mean_rt)
print(sd_rt)

#### identify outliers (i.e. with RT more than 3 sd's away from the mean)
num_outliers <- target_data %>%
  filter(abs(rt - mean_rt) > 3 * sd_rt) %>%
  summarise(count = n())

print(num_outliers) # 64 outliers 
print((num_outliers/nrow(target_data))*100) #2%

#### identify incorrect responses
incorrect_responses <- target_data %>% 
  filter(accuracy==0) %>%
  summarise(count = n())
print(incorrect_responses) # 233
print((incorrect_responses/nrow(target_data))*100) # 7.28125%

######## DATA CLEANING #############################################################

#### remove incorrect responses and response times more than 3 sd's away from the mean
target_data_filtered <- filter(target_data, accuracy==1) %>%
  filter(abs(rt - mean_rt) <= 3 * sd_rt)


######## MODEL (+ VALIDATION) #############################################################
#### set the baseline condition (centre, friendly target in neutral context)
target_data_filtered$target_position <- factor(target_data_filtered$target_position)
target_data_filtered$item_type <-factor(target_data_filtered$item_type)
target_data_filtered$target_position <- relevel(target_data_filtered$target_position, ref = "centre")
target_data_filtered$item_type <- relevel(target_data_filtered$item_type, ref = "friendly_neutral") 

#### develop two models with raw RT and log-transformed RT
model_raw <- lmer(rt ~ 1 + scrambled * outline * target_position * item_type + (1|ID),
                   target_data_filtered)

model_log <- lmer(log(rt) ~ 1 + scrambled * outline * target_position * item_type + (1|ID),
              target_data_filtered)


#### inspect residuals
par(mfrow = c(3, 2))

plot(fitted(model_raw), resid(model_raw), main = "Residuals vs Fitted (Raw)")
plot(fitted(model_log), resid(model_log), main = "Residuals vs Fitted (Log)")

qqnorm(resid(model_raw), main = "QQ Plot (Raw)")
qqline(resid(model_raw))
qqnorm(resid(model_log), main = "QQ Plot (Log)")
qqline(resid(model_log))

hist(resid(model_raw), main = "Raw RT Residuals", xlab = "Residuals")
hist(resid(model_log), main = "Log RT Residuals", xlab = "Residuals")

# COMMENT: all of these plots show that the model with log-transformed RT has 
# a more normal distribution of residuals, and therefore is the better model.

#### summary of the best model: model_log
print(summary(model_log))

coefs <- summary(model_log)$coefficients
coefs_df <- as.data.frame(coefs)
coefs_df <- round(coefs_df, 3)
write.csv(coefs_df, "model_results.csv")

# plot random effects (participants)
dotplot(ranef(model_log, condVar = TRUE))

#### check whether model estimates of mean RT corresponds with the mean RT of each condition
## mean RT by condition in the dataset
rt_by_condition <- target_data_filtered %>% 
  group_by(condition) %>% 
  summarise(meanRT=mean(rt))

print(rt_by_condition)

## model estimates of mean RT
# condition 11 (non-scrambled, with outline)
condition_11_estimate <- fixef(model_log)["(Intercept)"] + fixef(model_log)['outline']
print(exp(condition_11_estimate))

# condition 12 (non-scrambled, without outline)
condition_12_estimate <- fixef(model_log)["(Intercept)"]
print(exp(condition_12_estimate))

# condition 13 (scrambled, with outline)
condition_13_estimate <- fixef(model_log)["(Intercept)"] + fixef(model_log)['outline'] + fixef(model_log)['scrambled']
print(exp(condition_13_estimate))

# condition 14 (scrambled, without outline)
condition_14_estimate <- fixef(model_log)["(Intercept)"] + fixef(model_log)['scrambled']
print(exp(condition_14_estimate))

# compare model estimates and actual mean RTs
model_preds <- tibble(
  condition = c("11", "12", "13", "14"),
  predictedRT = c(
    exp(condition_11_estimate),
    exp(condition_12_estimate),
    exp(condition_13_estimate),
    exp(condition_14_estimate)
  )
)
rt_by_condition <- rt_by_condition %>%
  mutate(condition = as.character(condition))

rt_comparison <- left_join(rt_by_condition, model_preds, by = "condition")

rt_comparison <- rt_comparison %>%
  mutate(
    difference = predictedRT - meanRT,
    percent_error = ((predictedRT - meanRT) / meanRT)*100
  )
print(rt_comparison)


####### GRAPHING DATA ############################################################

##### plot interaction between scrambling and outline 
newdata <- expand.grid(
  scrambled = c(0, 1),
  outline = c(0, 1),
  target_position = "centre",
  item_type = "threat_neutral",
  ID = NA 
)

newdata$pred_logRT <- predict(model_log, newdata = newdata, re.form = NA)
newdata$pred_RT <- exp(newdata$pred_logRT)

ggplot(newdata, aes(x = factor(outline), y = pred_RT, group = scrambled, color = factor(scrambled))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Interaction: Scrambled × Outline\n(Threatening Target in Neutral Background)",
    x = "Outline",
    y = "Predicted RT",
    color = "Scrambled"
  ) +
  theme_minimal(base_size = 14)


##### plot mean RT of participants by condition
mean_rt_by_participant_condition <- target_data_filtered %>%
  group_by(ID, condition) %>%
  summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop")

#### plot the distribution of individual mean RTs by condition

mean_rt_by_participant <- target_data_filtered %>%
  group_by(ID, condition) %>%
  summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop") %>%
  mutate(condition = recode(condition,
                            "11" = "Original / With outline",
                            "12" = "Original / No outline",
                            "13" = "Scrambled / With outline",
                            "14" = "Scrambled / No outline"),
         condition = factor(condition, levels = c(
           "Original / With outline",
           "Original / No outline",
           "Scrambled / With outline",
           "Scrambled / No outline")))


p1 <- ggplot(mean_rt_by_participant, aes(x = factor(condition), y = mean_rt)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  geom_point(position = position_dodge(width = 0.75), size = 2, alpha = 0.6, color = "darkblue") +
  labs(
    title = "Distribution of Individual Mean RTs by Condition",
    x = "Condition",
    y = "Mean Reaction Time (s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### plot the distribution of error rates 
error_rate_by_participant <- target_data %>%  
  group_by(ID, condition) %>%
  summarise(
    error_rate = mean(accuracy == 0, na.rm = TRUE) * 100,  
    .groups = "drop") %>%
  mutate(condition = recode(condition,
                              "11" = "Original / With outline",
                              "12" = "Original / No outline",
                              "13" = "Scrambled / With outline",
                              "14" = "Scrambled / No outline"),
         condition = factor(condition, levels = c(
           "Original / With outline",
           "Original / No outline",
           "Scrambled / With outline",
           "Scrambled / No outline"
         )))

p2 <- ggplot(error_rate_by_participant, aes(x = factor(condition), y = error_rate)) +
  geom_boxplot(fill = "lightpink", color = "black")+
  geom_point(position = position_dodge(width = 0.75), size = 2, alpha = 0.6, color = "darkred")+
  labs(
    title = "Distribution of Individual Error Rates by Condition",
    x = "Condition",
    y = "Error Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# combine plots
p1 + p2

####### EXPLORATORY STATISTICS ############################################################

#### t-test (difference in error rates between neutral targets and emotional targets)
target_data$group <- NA

# group trials: neutral target or neutral background
target_data$group[target_data$item_type %in% c("friendly_neutral", "threat_neutral")] <- "neutral_target"
target_data$group[target_data$item_type %in% c("neutral_friendly", "neutral_threat")] <- "neutral_background"
df_grouped <- subset(target_data, !is.na(group))

participant_error_rates <- df_grouped %>%
  group_by(ID, group) %>%
  summarise(error_rate = mean(accuracy == 0, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = error_rate)

t.test(participant_error_rates$neutral_target,
       participant_error_rates$neutral_background,
       paired = FALSE)
