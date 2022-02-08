# read in libraries
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tidyverse)

# set location for final results output
out_report <- "C://Users/rosenfn/Desktop/Decision Tree and Variable Importance.pdf"
test_output <- "C://Users/rosenfn/Desktop/Decision Tree Test Output.csv"

# load in data
date_set <- read_csv("C:\\Users\\rosenfn\\Desktop\\Sample data.csv")

# filter data to 'US' accounts
date_set <- date_set[date_set$CtryCode=="US",]

# function to prepare data for analysis
# create subsample of data - easier for processing
# calculate age of invoice and add column to dataset (set >= 0)
# filter data to 'Invoice' type
# create age buckets and add column to dataset
prepareData <- function(input_df, sample_size){
  set.seed(123)
  tm <- proc.time()[[3]]
  print(paste("size of df input to prepareData is",dim(input_df)))
  input_df<-sample_frac(input_df,sample_size)
  
  input_df <- input_df %>% mutate(MaxTerms = ifelse(is.na(MaxTerms), 0, MaxTerms))
  
  input_df$Age <- day(days((input_df$ClrgDate-input_df$BlineDate) - input_df$MaxTerms))
  
  #make age always >= 0
  input_df$Age[input_df$Age<0] <- 0
  input_df <- input_df[!is.na(input_df$Age),]
  
  #invoices only
  out <- input_df[input_df$DRCR=="Invoice",]
  
  # create factors from character variables
  factored_data <- as.data.frame(unclass(out),stringsAsFactors = TRUE)
  
  # create age buckets
  factored_data <- factored_data %>% mutate(buckets = case_when(Age <= 30 ~ '30 day bucket',
                                                               Age > 30  & Age <= 60 ~ '60 day bucket',
                                                               Age > 60  & Age <= 90 ~ '90 day bucket',
                                                               Age > 90  & Age <= 180 ~ '180 day bucket',
                                                               Age > 180  & Age <= 360 ~ '360 day bucket',
                                                               Age > 360 ~ '>360 days bucket'))
  
  #create ordered factor for buckets
  factored_data$buckets <- factor(factored_data$buckets, levels=c('30 day bucket',
                                                                  '60 day bucket',
                                                                  '90 day bucket',
                                                                  '180 day bucket',
                                                                  '360 day bucket',
                                                                  '>360 days bucket'))
  
  #print frequency of age buckets to console
  print(table(factored_data$buckets))
  
  print(paste("size of df output to prepareData is",dim(factored_data)))
  
  print(paste("prepareData took",round((proc.time()[[3]] - tm)),
              "seconds to complete"))
  
  return(factored_data)
}

#set sample size value to 40% - easier for processing
sampleSize<-.4

# run prepareData function
analysis_out<-prepareData(date_set, sample_size = sampleSize)
rm(date_set)

# function to train decision tree
# create training data (70% of total) and testing data (30% of total)
# plot decision tree
# predict on testing data
# print confusion matrix and calculate accuracy
# calculate error
plotDecisionTree <- function(input_df, min_split, min_bucket, max_depth, x_val, max_surrogate, cp_value) {
  set.seed(123)
  tm <- proc.time()[[3]]
  sample_size = round(nrow(input_df)*.70)
  train_ind <- sample(seq_len(nrow(input_df)), size = sample_size)
  train <- input_df[train_ind, ]
  print(paste("the size of the training data is", dim(train)))
  test <- input_df[-train_ind, ]
  print(paste("the size of the testing data is", dim(test)))
  
  # create numeric values for buckets - to be used when calculating error
  test <- test %>% mutate(bucket_num = case_when(buckets == '30 day bucket' ~ '1',
                                                 buckets == '60 day bucket' ~ '2',
                                                 buckets == '90 day bucket' ~ '3',
                                                 buckets == '180 day bucket' ~ '4',
                                                 buckets == '360 day bucket' ~ '5',
                                                 buckets == '>360 days bucket'~ '6'))
  
  test$bucket_num <- as.numeric(test$bucket_num)
  
  # decision tree - training data
  my_train_tree <- rpart(
    buckets ~ Year + Period + AmountLoc + TotalReceivables + LastPayAmt + CreditLimit, 
    data = train, 
    method = "class",
    minsplit = min_split,
    minbucket = min_bucket,
    maxdepth = max_depth,
    xval = x_val,
    maxsurrogate = max_surrogate,
    parms = list(split = 'information'),
    cp = cp_value
  ) 
  
  # plot decision tree
  print(paste("plotting decision tree"))
  print(fancyRpartPlot(my_train_tree, caption = NULL, cex.main = .15))
  
  # print confusion matrix for test data
  pred_test <- predict(my_train_tree, newdata = test, type="class")
  print(paste("printing confusion matrix for test data"))
  conf_matrix_test <- table(pred_test, test$buckets)
  print(conf_matrix_test)
  print(paste('Accuracy for test is found to be', round(sum(diag(conf_matrix_test)) / sum(conf_matrix_test), 2)))
  
  # calculate average error between predicted and actual
  pred_test_num <- as.numeric(pred_test)
  error <- mean(abs(pred_test_num-test$bucket_num))
  print(paste('Error for test is found to be', round(error,2)))
  
  print(paste("plotDecisionTree took",round((proc.time()[[3]] - tm)),
              "seconds to complete"))  
  
  test$pred_bucket <- pred_test
  test$pred_bucket_num <- pred_test_num
  
  test_out <- test
  
  return(test_out)
  return(my_train_tree)
  
}

#function to plot importance of variables in decision tree model
plotVarImp <- function(input_df) {
  df <- data.frame(imp = input_df$variable.importance)
  df2 <- df %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename("variable" = rowname) %>% 
    dplyr::arrange(imp) %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))
  
  print("Printing variable importance plot")  
  var_imp <- ggplot2::ggplot(df2) +
    geom_col(aes(x = variable, y = imp),
             col = "black", show.legend = F) +
    coord_flip() +
    scale_fill_grey() +
    theme_bw()
  print(var_imp)
}

# write plots to pdf
pdf(file=out_report, title="Decision Tree and Variable Importance")

# run plotDecisionTree function
decision_tree_out <- plotDecisionTree(input_df=analysis_out, 
                                      min_split = 500, 
                                      min_bucket = 100, 
                                      max_depth = 5,
                                      x_val = 10,
                                      max_surrogate = 0,
                                      cp_value = 0.0001)

# run plotVarImp function
variableImportance <- plotVarImp(input_df = decision_tree_out)

dev.off()

#write test results to csv file
write_csv(x=decision_tree_out, file=test_output)
