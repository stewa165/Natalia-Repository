# read in libraries
library(readr)
library(ggplot2)
library(dplyr)
library(splitstackshape)
library(lubridate)
library(xgboost)
library(pryr)
library(prophet)

# load in data
# set location for final results output
date_set<-read_csv("C:\\Users\\rosenfn\\Desktop\\Sample data.csv")
out_report<-"C://Users/rosenfn/Desktop/US_report.pdf"
out_report_imp<-"C://Users/rosenfn/Desktop/US_report_imp.pdf"
out_encode<-"C://Users/rosenfn/Desktop/encode.csv"
out_analysis<-"C://Users/rosenfn/Desktop/analysis.csv"

# filter data to 'US' accounts
date_set<-date_set[date_set$CtryCode=="US",]

# function to prepare data for analysis
# create subsample of data - easier for processing
# calculate age of invoice and add column to dataset (set >= 0)
# filter data to 'Invoice' type
prepareData<-function(input_df, sample_size=.02){
  tm <- proc.time()[[3]]
  print(paste("size of df input to prepareData is",dim(input_df)))
  input_df<-sample_frac(input_df,sample_size)
  
  input_df <- input_df %>% mutate(MaxTerms = ifelse(is.na(MaxTerms), 0, MaxTerms))

  input_df$Age<-day(days((input_df$ClrgDate-input_df$BlineDate) - input_df$MaxTerms))

  #make delay always >=0
  input_df$Age[input_df$Age<0]<-0
  input_df<-input_df[!is.na(input_df$Age),]

  #invoices only
  out<-input_df[input_df$DRCR=="Invoice",]
  print(paste("size of df output to prepareData is",dim(out)))
  
  print(paste("prepareData took",round((proc.time()[[3]] - tm)),
              "seconds to complete"))
  
  return(out)
}

# function to encode data for xgboost model
# turns categorical data into 0/1
# removes unneccessary columns
encodeData<-function(input_df){
  tm_all <- proc.time()[[3]]
  print(paste("size of df input to encodeData is",dim(input_df)))
  
  input_df$ERNAM_Createdby[input_df$ERNAM_Createdby!="CONVERSION"]<-"OTHER"
  
  #convert some variables back to character
  
  input_df$GL<-as.character(input_df$GL)
  input_df$Cocd<-as.character(input_df$Cocd)
  
  data_set_date<-input_df$DocDate
  
  data_set_date<-order(data_set_date)
  weight<-seq(from=1,to=0,length.out=length(data_set_date))
  
  data_set_train_char<-input_df[, sapply(input_df, class) == 'character']
  
  # remove unneccessary columns
  data_set_train_x<-select(data_set_train_char,-contains(c("date","Profit",
                                                           "address","Doc",
                                                           "SearchTerm","Account",
                                                           "SAPText","State",
                                                           "DRCR","Acct",
                                                           "Assignment",
                                                           "CustClassCode",
                                                           "gbsRepNum","RepName",
                                                           "City", "PostCode",
                                                           "CashCur","SAPCurr",
                                                           "DataLine","SPRAS",
                                                           "LAND1","gbsCountry",
                                                           "CtryCode","GblDistSub",
                                                           "GblDistRegion","WAERS",
                                                           "XZAHL","GL","Cocd",
                                                           "GblDistCountry" #, "Age"
                                                           #Removing GL for now, creates a huge DF
  )))
  
  names_char<-names(data_set_train_x)
  
  init <-1
  
  for (name in names_char){  
    tm <- proc.time()[[3]]
    if(init==1) (data_base<-data_set_train_x) else data_base<-out
    
    print(paste(name,"will create",length(unlist(unique(data_base[,name]))),
                "new features"))
    missing_count<-sum(is.na(data_base[,name]))
    print(paste(name,"has",missing_count,"missing"))
    
    out <- tryCatch(cSplit_e(data_base, name, sep="-", fill = '0', 
                    type = 'character', drop = TRUE), error = function(e)
                      print("column encode failed, skipping column"))
    init<-2
    print(paste("Process took",round((proc.time()[[3]] - tm)),
                "seconds to encode",name))
    print(paste("number of columns in dataset is now",ncol(out)))
    print(mem_used())
  }
  print("encoding complete")
  gc()
  names(out) <- sub('V1_', '', names(out))
  
  out[names(out)] <- sapply(out[names(out)],as.numeric)
  sapply(out, class)
  print(paste("encodeData took",round((proc.time()[[3]] - tm_all)),
              "seconds to complete"))
  data_set_train_num<-input_df[, sapply(input_df, class) == 'numeric']
  
  # remove unneccessary columns
  data_set_train_comb<-select(data_set_train_num,-contains(c("date","Profit",
                                                             "address","Doc",
                                                             "SearchTerm","Account",
                                                             "SAPText","State",
                                                             "DRCR","Acct",
                                                             "Assignment",
                                                             "CustClassCode",
                                                             "gbsRepNum","RepName",
                                                             "City", "PostCode",
                                                             "CashCur","SAPCurr",
                                                             "DataLine","SPRAS",
                                                             "LAND1","gbsCountry",
                                                             "GLRecon","Reference",
                                                             "GblDistRegion","GJAHR",
                                                             "Disc", "Cocd" #, "Age"
                                                             )))
  
  
  
  out<-data.frame(out, data_set_train_comb, weight, data_set_date)
  gc()

  return(out)
  
}

# function to plot data
# 4 plots:
# Raw Invoice Delays by Size of Invoice
# Trend Line in Invoice Age Since 2005
# Forecast and trends by year, day of week, and day of year
# Trends for Each Profit Center
plotData<-function(input_df, sub_sample_size, display_size){

  ##plotting
  #input_df<-analysis_out
  data_set_plot_1<-sample_frac(input_df,sub_sample_size)
  #data_set_plot<-input_df
  
  sub_display_size=display_size*sub_sample_size

  a<-ggplot(data=data_set_plot_1,
       aes(y=Age, x=as.Date(DocDate),color=ProfitCtrName, size=AmountLoc)) + 
  geom_point() +
  ylab("Age from Clearing Date after Terms (days)") +
  xlab("Doc Creation Date")+
  ylim(0,365)+
  scale_x_date(date_breaks="5 years")+
  labs(title="Raw Invoice Delays by Size of Invoice",
      subtitle=paste(sub_display_size*100,"% of all invoices"))+
  theme_minimal()
  print("Printing plot 1")
  print(a)
  
  forecast_set<-sample_n(input_df,20000)
  forecast_set<-filter(forecast_set,Year>2005)
  df<-tibble(ds=forecast_set$DocDate, y=forecast_set$Age)
  m_prophet<-prophet(df, fit=TRUE)
  future <- make_future_dataframe(m_prophet, periods = 365)
  forecast <- predict(m_prophet,future)
  tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

  a_2<-ggplot(data=forecast, aes(x=ds,y=yhat))+
        geom_point(alpha=.02)+
        geom_line(aes(y=trend), color="blue")+
    labs(title="Trend Line in Invoice Age Since 2005")+
         xlab("Date")+ ylab("Age (days)")+
    theme_minimal()
  
  print(a_2)
  print("Printing plot 2")
  
  
  a_3<-prophet_plot_components(m_prophet, forecast)
  
  #print(a_3)
  print("Printing plot 3")
  
  b<-ggplot(data=input_df,
         aes(y=Age, x=as.Date(DocDate))) + 
    #geom_smooth(method="gam", se=FALSE,n=1500)+
    geom_smooth(span=25)+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)+
    geom_smooth(method = lm, se = FALSE)+
    ylab("Age of Invoice (days) Past Terms") +
    xlab("Doc Creation Date")+
    #ylim(0,15)+
    scale_x_date(date_breaks="2 years", 
                 limits=c(as.Date("2011-01-01"),as.Date( "2021-01-11")))+
    labs(title="Overall Trend in Invoice Age",
        subtitle=paste(round(display_size*100),"% of all invoices"))+
    theme_minimal()
  #print("Printing plot 4")

  #print(b)
  
  c<-ggplot(data=input_df,
       aes(y=Age, x=as.Date(DocDate),color=ProfitCtrName)) + 
  #geom_smooth(method="gam", se=FALSE,n=1500)+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)+
  ylab("Age of Invoice (days) Past Terms") +
  xlab("Doc Creation Date")+
  #ylim(0,15)+
  scale_x_date(date_breaks="2 years", 
               limits=c(as.Date("2017-01-01"),as.Date( "2021-01-11")))+
  labs(title="Trends for Each Profit Center",
      subtitle=paste(round(display_size*100),"% of all invoices"))+
  theme_minimal()
  print("Printing plot 5")
  
  print(c)
}  

# function to train xgboost model
# create training data (70% of total) and testing data (30% of total)
# predict on test data
trainModel<-function(input_df){

  sample_size = round(nrow(input_df)*.70) # setting what is 70%
  
  weight<-input_df$weight
  input_df<-select(input_df,-weight)
  
  train <- sample_n(input_df, sample_size)
  sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
  test <- input_df[-sample_id,]
  test_date<-input_df$data_set_date[-sample_id]
  
  train<-select(train,-data_set_date)
  train_label<-train$Age
  train_predict<-select(train,-c("Age","AgeDays"))
  
  test<-select(test,-data_set_date)
  test_label<-test$Age
  test_predict<-select(test,-c("Age","AgeDays"))
  
  xgb_train<-xgb.DMatrix(data=as.matrix(train_predict),label=train_label)
  xgb_test<-xgb.DMatrix(data=as.matrix(test_predict),label=test_label)
  
  cv.nround <- 24
  cv.nfold <- 3

  m1_xgb <-
    xgboost(
      data = xgb_train,
      #nrounds = 1000,
      nrounds = 100,
      objective = "reg:squarederror",
      early_stopping_rounds = 3,
      max_depth = 12,
      eta = .25
    )   
  
  pred <- predict(m1_xgb, xgb_test)
  
                       #feature_selector="greedy",top_k=50)
  return(list(model=m1_xgb, data_train=xgb_train, truth=test_label, 
              pred=pred, date= test_date))
}

# function to plot residuals
plotResiduals<-function(model_results){
  date_ind<-model_results$date
  #date_ind<-test_date
  resid_all<-abs(model_results$pred-model_results$truth)
  #resid_all<-abs(pred-test_label)
  
  b<-ggplot()+
    geom_bin_2d(aes(date_ind, resid_all))+
    ylab("Error in days")+
    xlab("Year")+
    theme_light()
  print(b)
  
  c<-ggplot()+
    stat_ecdf(aes(resid_all))+
    ylab("Percent")+
    xlab("Days")+
    coord_cartesian(xlim=c(0,28))+
    theme_light()
  print(c)
  
  hist(resid_all, xlim=c(0,21), breaks=c(0,7,14,21,28,10000))
  plot(ecdf(resid_all),xlim=c(0,21),xlab="Days")
  
  c<-ggplot(model_results$truth,aes(y=resid_all))+
    stat_ecdf()+
    #ylab("Error in days")+
    theme_light()
  print(c)
  
}

# function to plot variable importance
plotImportance<-function(model_object, data, top_number=50){
  
  importance_matrix <- xgb.importance(model = model_object)
  print(importance_matrix)
  a<-xgb.ggplot.importance(importance_matrix = importance_matrix, 
                           top_n = top_number, 
                        xlab = "Feature importance", rel_to_first = TRUE)
  print(a)
  
  #c<-xgb.ggplot.shap.summary(data = as.matrix(data), model = model_object,
                             #subsample = .1,
                             #top_n = 12)
  
  #print(c)
}

#set sample size value to 40% - easier for processing
sampleSize<-.4

# run prepareData function
analysis_out<-prepareData(date_set, sample_size = sampleSize)
rm(date_set)

# run encodeData function
encode_out<-encodeData(input_df=analysis_out)
encode_out <- read_csv("C:\\Users\\rosenfn\\Desktop\\encode.csv")
gc()

# write results to .csv files
write_csv(x=encode_out, file=out_encode)
write_csv(x=analysis_out, file=out_analysis)

# write plots to pdf
pdf(file=out_report, title="US Model Analysis")

sub_sample_size<-.005

# run plotData function
plotData(input_df=analysis_out, sub_sample_size = sub_sample_size,
                        display_size = sampleSize)
dev.off()

##TO::DO create true test/train split (coded but not tested)
##TO::DO create much heavier weighting toward recent examples (coded but not tested)
##TO::DO add a plot showing out of sample residuals over time (done but not added to this workflow)
##TO::DO look up English equivalent of payment terms(done but not added to this workflow)
##TO::DO add SHAP interpretations to residuals

# write plots to pdf
pdf(file=out_report_imp, title="US Model Importance")

#encode_out<-encode_out[,-302]

# run trainModel function
results_model<-trainModel(input_df=encode_out)

# run plotImportance function
plotImportance(model_object=results_model$model, data=results_model$data_train,
               top_number = 20)

# run plotResiduals function
plotResiduals(model_results=results_model)

dev.off()

