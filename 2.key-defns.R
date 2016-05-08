
## rf for missing ages
# source("1.initial.R")
# df <- read.csv("data/train.csv") %>% 
#   mutate_each(funs(as.character), c(matches("Name"), matches("Sex"), matches("Ticket"), 
#                                     matches("Cabin"), matches("Embarked")))
# age_rf <- randomForest(
#   Age ~ Pclass + Male + SibSp + Parch, 
#   data = df %>% filter(!is.na(Age)) %>% mutate(Male = as.numeric((Sex=='male'))), 
#   ntree = 100)
# saveRDS(age_rf, "models/rf-age.RDS")

## build/calibration sample
# n <- nrow(read.csv("data/train.csv"))
# list(s_build  = sample(1:n, 700), s_cal = (1:n)[-s_build]) %>%
#   saveRDS("data/sample-def.RDS")

## function: clean df
clean <- function(df) {
  age_rf <- readRDS("models/rf-age.RDS")
  # name -> mr, mrs
  f <- function(v, pattern) {
    g <- gregexpr(pattern, v, fixed=TRUE) %>% unlist
    g[1]
  }
  commas <- Map(f, df$Name, ", ") %>% unlist
  dots <- Map(f, df$Name, ". ") %>% unlist
  titles <- Map(substr, df$Name, commas+2, dots-1) %>% unlist()
  df <- mutate(df, title_mr = (titles == "Mr"), title_mrs = (titles == "Mrs"))
  # age N/A -> age rf pred
  df <- mutate(df, Male = as.numeric((Sex == 'male')))
  pred_ages <- predict(age_rf, df)
  df <- mutate(df, pred_age = pred_ages) %>%
    mutate(Age = ifelse(is.na(Age), pred_age, Age), pred_age = NULL)
  # cabin -> letter, number
  df <- mutate(df, cabin_letter = ifelse(Cabin == "", "X", substr(Cabin, 1, 1)),
               cabin_letter = match(cabin_letter, toupper(letters)),
               cabin_number = as.numeric(substr(Cabin, 2, 4))) %>% replace_na(1000)
  # embarked -> binary
  df <- mutate(df, embQ = ifelse(Embarked == "Q", 1, 0),
               embC = ifelse(Embarked == "C", 1, 0),
               embS = ifelse(!Embarked %in% c("C", "Q"), 1, 0))
  # remove cols
  df %>% select(-c(PassengerId, Name, Sex, Ticket, Cabin, Embarked))
}

## function: create submission file
create_submission <- function(preds, file_name) {
  test_raw <- read.csv("data/test.csv")
  data.frame(PassengerId = test_raw$PassengerId, Survived = preds) %>%
    write.csv(paste0("data/", file_name, ".csv"), row.names = FALSE)
}
