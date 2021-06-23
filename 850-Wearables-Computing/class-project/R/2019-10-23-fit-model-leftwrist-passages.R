
#' @author 
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Build rnn model to classify study participant (out of 32 total) based on 
#' 5-long seconds of accelerometry data. 
#' 
#' Inspired by: 
#' https://blog.goodaudience.com/introduction-to-1d-convolutional-neural-networks-in-keras-for-time-sequences-3a7ff801a2cf


rm(list = ls())

project.dir   <- "/Users/martakaras/Dropbox/JHU-coursework/850-Wearables-Computing/class-project/"

library(adeptdata)
library(keras)
library(ggplot2)
library(reshape2)
library(dplyr)

## -----------------------------------------------------------------------------
## PREPARE DATA

## - Use raw walking accelerometry data from acc_walking_IU package 
##   (subset of the study on Identification of Walking, Stair Climbing, and 
##   Driving Using Wearable Accelerometers)
## - Discard 1st few seconds of walking data
## - Compute vector magnitude (VM)
## - Subset location to "left ankle" 
## - Define single X data observation to be 5 second-long vector of VM values
## - Extract same number of single data observations per study participant

## Param 
win_s <- 5
win_vl <- 100 * win_s
subj_n <- length(unique(acc_walking_IU$subj_id))
  
## Filter, mutate accelerometry data frame
acc_f <- 
  acc_walking_IU %>% 
  filter(loc_id == "left_wrist") %>%
  filter(time_s > 5) %>%
  group_by(subj_id) %>%
  arrange(subj_id, time_s) %>%
  mutate(rn = row_number(),
         vm = sqrt(x^2 + y^2 + z^2))

## Number of VM values we use per each participant
rn_upper_bound <- acc_f %>% group_by(subj_id) %>% summarize(rn_max = max(rn)) %>% 
  pull() %>% min()
rn_upper_bound <- rn_upper_bound - rn_upper_bound %% win_vl
## Number of single X data observations we get per each participant
obs_per_subj <- rn_upper_bound / win_vl

## X data
X_all <- 
  acc_f %>%
  filter(rn <= rn_upper_bound) %>%
  group_by(subj_id) %>%
  arrange(subj_id, rn) %>%
  group_map(~ do.call(rbind, split(.$vm, ceiling(.$rn/win_vl))))
X_all <- do.call(rbind, X_all)
## Y data
Y_all <- as.vector(sapply(seq(0, by = 1, length.out = subj_n), function(x) rep(x, obs_per_subj)))



## -----------------------------------------------------------------------------
## BUILD AND PREDICT WITH CONVOLUTIONAL (1D) NEURAL NETWORK

## Split into train and test set
train_pct <- 0.7 
X_all_nrow <- nrow(X_all) 
train_size_per_subj <- round(obs_per_subj * train_pct)
train_mask0 <- c(rep(TRUE, train_size_per_subj), rep(FALSE, obs_per_subj - train_size_per_subj))
train_mask <- as.vector(rep(train_mask0, subj_n))
length(train_mask) == X_all_nrow
train_idx <- (1:X_all_nrow)[train_mask]
## Reshape into 3D array (add "artificial" 3rd dimmension)
x_train = array_reshape(X_all[train_idx, ], c(dim(X_all[train_idx, ]), 1))
x_test = array_reshape(X_all[-train_idx, ], c(dim(X_all[-train_idx, ]), 1))
y_train <- to_categorical(Y_all[train_idx])
y_test  <- to_categorical(Y_all[-train_idx])

dim(x_train); dim(x_test)
dim(y_train); dim(y_test)


## -----------------------------------------------------------------------------
## Add Dropout

## Fit model2
model2 <- keras_model_sequential() 
model2 %>% 
  layer_conv_1d(filters = 50, kernel_size = 50,  activation = "relu",  
                input_shape = c(win_vl, 1)) %>%
  layer_max_pooling_1d(pool_size = 200) %>%
  layer_flatten() %>%
  layer_dropout(0.5) %>%
  layer_dense(units = subj_n, activation = 'softmax')
summary(model2)
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model2 %>% fit(
  x_train, 
  y_train, 
  epochs = 200, 
  batch_size = 30,   
  validation_split = 0.3   
)

## Evaluate performance on test set
test_eval <- model2 %>% evaluate(x_test, y_test)
plot(history) + labs(title = paste0("Test set accuracy = ", round(test_eval$acc, 3)))

plt.path <- file.path(project.dir, "figures/fit_history_leftwrist_2_passage.jpeg")
ggsave(plt.path, width = 22, height = 12, units = "cm")


## Prediction per person
pred_df <- model2 %>% predict(x_test) %>% as.data.frame()
colnames(pred_df) <- paste0(1:subj_n) 
pred_df$subj_id_true <- (Y_all[-train_idx] + 1)
plt.df <- 
  pred_df %>% 
  reshape2::melt(id.vars = c("subj_id_true"), 
       variable.name = "subj_id_pred",
       value.name = "pred_prob") %>%
  mutate(subj_id_pred = as.numeric(as.character(subj_id_pred))) %>%
  group_by(subj_id_true, subj_id_pred) %>%
  summarise(subj_id_pred_mean = mean(pred_prob)) %>%
  mutate(true_pred = as.numeric(subj_id_true == subj_id_pred))


ggplot(plt.df, aes(x = factor(subj_id_true), y = subj_id_pred_mean, color = factor(true_pred))) + 
  geom_point(alpha = 0.8, size = 2) + 
  labs(x = "Subject ID", y = "Classification probability", color = "Correct\nparticipant\nindicator",
       title = "Participant classification probability\nbased on 5-sec wrist-worn accelerometry data fragments") + 
  theme_minimal(base_size = 10) + 
  scale_color_manual(values = c("black", "red"))

plt.path <- file.path(project.dir, "figures/fit_history_leftwrist_2_class_prob.jpeg")
ggsave(plt.path, width = 20, height = 16, units = "cm")


















