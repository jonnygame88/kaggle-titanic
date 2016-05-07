
## setup
source("1.initial.R"); source("2.key-defns.R")
build_data <- clean(read.csv("data/train.csv")) %>% 
  mutate(Survived = as.factor(Survived))

## build rf
# tuneRF(x = select(build_data, -Survived), y = build_data$Survived, 
#        ntreeTry = 1000, mtryStart = 4)
survival_rf <- randomForest(Survived ~ ., data = build_data, 
                            ntree = 50000, mtry = 4)
survival_rf; importance_list(survival_rf)
saveRDS(survival_rf, "models/rf-survival.RDS")

## build glm
survival_lr <- glm(Survived ~ ., data = build_data, family = binomial(link='logit'))
saveRDS(survival_lr, "models/lr-survival.RDS")

## build gbt
# xgb.cv(data = xg_input(build_data, "Survived"), 
#        label = xg_label(build_data$Survived),
#        objective = "binary:logistic", eval_metric = "error",
#        nfold = 5, nrounds = 100, early.stop.round = 10, eta = 0.1)
survival_gbt <- xgboost(data = xg_input(build_data, "Survived"), 
                        label = xg_label(build_data$Survived),
                        objective = "binary:logistic", eval_metric = "error",
                        nrounds = 15, eta = 0.1)
saveRDS(survival_gbt, "models/gbt-survival.RDS")

## build svm
# tune(svm, Survived ~ ., data = build_data,
#      ranges = list(gamma = c(0.1, 0.25, 0.5, 1, 1.5, 2), cost = 1:5),
#      tunecontrol = tune.control(sampling = "boot"))
survival_svm <- svm(Survived ~ ., data = build_data, 
                    gamma = 0.1, cost = 1, probability = TRUE)
saveRDS(survival_svm, "models/svm-survival.RDS")
