
## setup
source("1.initial.R"); source("2.key-defns.R")
test_raw <- read.csv("data/test.csv"); test_data <- clean(test_raw)

survival_lr <- readRDS("models/lr-survival.RDS")
survival_rf <- readRDS("models/rf-survival.RDS")
survival_gbt <- readRDS("models/gbt-survival.RDS")
survival_svm <- readRDS("models/svm-survival.RDS")

## get predictions
preds_lr <- predict(survival_lr, test_data, type = "response")
preds_rf <- predict(survival_rf, test_data, type = "prob") %>% 
  data.frame() %>% give_names(1:2, c("p0", "p1"))
preds_gbt <- predict(survival_gbt, xg_input(test_data))
preds_svm <- predict(survival_svm, test_data, probability = TRUE) %>%
  attr("probabilities") %>% data.frame() %>% give_names(1:2, c("p0", "p1"))

preds_all <- data.frame(p_lr = preds_lr, p_rf = preds_rf$p1,
                        p_gbt = preds_gbt, p_svm = preds_svm$p1)
pview <- cbind(test_raw, preds_all)

## create submission
p_ens <- as.numeric(rowSums(preds_all) >= 2)
create_submission(p_ens, "preds-ensemble")
