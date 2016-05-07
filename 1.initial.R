
## load packages
pkgs <- c("dplyr", "reshape2", "ggplot2", "foreach", "doParallel",
          "randomForest", "xgboost", "nnet", "caret", "e1071", "Matrix")
lapply(pkgs, require, character.only = TRUE)

## rename columns
give_names <- function(df, col_pos, col_names) {
  names(df)[col_pos] <- col_names
  df
}

## order vector
order_vector <- function(vector, type=1) {
  if (type==1) {vector[order(vector)]} else {vector[order(-vector)]}
}

## replace nas with value
replace_na <- function(df, x) {
  for (i in seq_len(ncol(df))) {
    df[which(is.na(df[, i])), i] <- x
  }
  df
}

## clean column
clean_col <- function(x) {
  if (is.factor(x)) {y <- levels(x)[x]} else {y <- x}
  y <- gsub(" ", "", y, fixed = TRUE)
  gsub("[[:punct:]]", "", y)
}

## remove NAs
remove_nas <- function(df) {
  check_col <- function(cl) which(is.na(df[, cl]))
  nas <- lapply(1:ncol(df), check_col) %>% unlist %>% unique
  if (sum(nas) > 0) {df[-nas, ]} else {df}
}

## show model importance
importance_list <- function(model) {
  metric_name <- rownames(model$importance)
  metric_importance <- as.numeric(round(model$importance))
  as.data.frame(cbind(metric_name, metric_importance))[order(-metric_importance), ]
}

## xgboost helper functions
xg_input <- function(df, target = NULL) {
  col_include <- setdiff(names(df)[sapply(df, is.numeric)], target)
  sparse.model.matrix(~ ., df[, col_include])
}
xg_label <- function(y) {
  fctr <- as.factor(y)
  as.numeric(match(fctr, levels(fctr))) - 1
}
xg_predclass <- function(model, target, df, class_names) {
  predict(model, xg_input(df, target)) %>% 
    matrix(ncol = length(class_names), byrow = T) %>%
    data.frame() %>% give_names(1:length(class_names), class_names)
}
