library(tidyverse)
library(stringr)

school_questions <- c('IDSCHOOL', 'BCBG02','BCBG03A', 'BCBG03B', 'BCBG04', 'BCBG07', 'BCBG10BB')

school_codebook <- readxl::read_excel('bcgtmsm5.xls') %>%
  select(FIELD_NAME, FIELD_LABL, FIELD_CODE) %>%
  filter(FIELD_NAME %in% school_questions) %>%
  
chile_school_general_11 <- haven::read_spss('bcgchlm5.sav') %>%
  select_(.dots = names(.)[names(.) %in% school_questions]) %>%
  gather(question, answer, -IDSCHOOL) %>%
  right_join(school_codebook, by = c('question' = 'FIELD_NAME'))

teacher_questions <- c('IDSCHOOL', 'BTBG16A', 'BTBM23AB', 'BTBM23AC', 'BTBM23AD', 'BTBM23AE', 'BTBM23BA',
                       'BTBM23BB', 'BTBM23BC', 'BTBM23BD', 'BTBM23BE', 'BTBM23CA', 'BTBM23CB',
                       'BTBM23CC', 'BTBM23CD', 'BTBM23CE', 'BTBM23CF', 'BTBM23DA', 'BTBM23DB', 
                       'BTBM23DC', 'BTBM25CE', 'BTBM27')

teacher_codebook <- readxl::read_excel('btmtmsm5.xls') %>%
  select(FIELD_NAME, FIELD_LABL, FIELD_CODE)

chile_teacher_general_11 <- haven::read_spss('btmchlm5.sav') %>%
  select_(.dots = names(.)[names(.) %in% teacher_questions]) %>%
  gather(question, answer, -c(IDSCHOOL)) %>%
  right_join(teacher_codebook, by = c('question' = 'FIELD_NAME')) %>%
  filter(!is.na(IDSCHOOL))

student_questions <- c('IDSTUD', 'BSDAGE', 'BSDGEDUP', 'BSBG11A', 'BSBG11D', 'BSBG05E', 'BSBG05F', 'BSBG05G',
                       'BSBG05H', 'BSBG05I', 'BSBG05J', 'BSBG05K')

student_general_codebook <- readxl::read_excel('bsgtmsm5.xls') %>%
  select(FIELD_NAME, FIELD_LABL, FIELD_CODE)

chile_student_general_11 <- haven::read_spss('bsgchlm5.sav') %>%
  select_(.dots = names(.)[names(.) %in% student_questions]) %>%
  mutate(diff_from_mean_age = BSDAGE - mean(BSDAGE)) %>%
  mutate(diff_from_mean_age_value = sapply(.$diff_from_mean_age, function(x){
    if(x > 2) {
      return('Much Older')
    } else if (x>1){
      return('Older')
    } else if (x > -1) {
      return('Average')
    } else if (x > -2){
      return('Younger')
    } else {
      return('Much Younger')
    }
  })) %>%
  gather(question, answer, -IDSTUD) %>%
  right_join(student_general_codebook, by = c('question' = 'FIELD_NAME')) %>%
  filter(!is.na(IDSTUD)) 

chile_timss_math_11 <- read_csv('2011_TIMSS_CHILE.csv')

#============== student level ==================================#
 
timss_simple_student_model_11 <- function(df){
  df %>%
    mutate(benchmark_math_avg = (BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05)/5) %>%
    mutate(benchmark_math_avg_value = sapply(benchmark_math_avg, function(x){
      if(x >= 625){
        'Advanced'
      } else if (x > 550) {
        'High'
      } else if (x > 475) {
        'Intermediate'
      } else if (x > 400) {
        'Low'
      } else {
        'Below Low'
      }
    })) %>%
    group_by(IDSTUD, IDSCHOOL, ITSEX, benchmark_math_avg_value) %>% 
    nest() %>%
    select(-data) %>%
    mutate_all(as.factor)
}

chile_simple_student_11 <- timss_simple_student_model_11(chile_timss_math_11)

#-------------------------------------------------------------------------------------#
#------------------------------ Naive Bayes ------------------------------------------#
#-------------------------------------------------------------------------------------#

require(caret)

#----------------------------- Student Level -----------------------------------------#

timss_student_nb <- function(df){
  clean_df <- df %>%
    dplyr::select(-c(benchmark_math_avg_value, IDSTUD))
  
  truth <- df$benchmark_math_avg_value %>%
    as.factor()
  
  model <- train(clean_df, truth, 'nb', trControl = trainControl(method = 'cv', number = 10))
  
  prediction <- predict(model$finalModel, clean_df)
  ed_guess <- prediction$class
  
  table(ed_guess, truth)
}

analyze_model <- function(df, model_func){
  model_table <- model_func(df)
  model_info <- list()
  model_info[[1]] = model_table
  
  correct_in_col <- c()
  total_in_col <- c()
  correct_ratio <- c()
  
  for(i in 1:ncol(model_table)){
    correct_in_col[i] = model_table[i,i]
    total_in_col[i] = model_table[,i] %>%
      sum()
    correct_ratio[i] = str_c(dimnames(model_table)[[1]][i], ': ', round((100 * correct_in_col[i]/total_in_col[i]), 2), '%')
  }
  correct_ratio_mean <- mean(correct_in_col/total_in_col)
  correct_ratio[length(correct_ratio) + 1] = str_c('Overall: ',round((100 * sum(correct_in_col)/sum(total_in_col)), 2), '%')
  model_info[[2]] = c(correct_in_col/total_in_col, sum(correct_in_col)/sum(total_in_col))
  model_info[[3]] = correct_ratio
  
  guess_table <- table(df$benchmark_math_avg_value, df$benchmark_math_avg_value)
  model_info[[4]] = guess_table
  model_info[[5]] = max(total_in_col)/sum(total_in_col)
  model_info[[6]] = str_c('Just guessing \'', dimnames(model_table)[[1]][which.max(total_in_col)], '\': ', round((100 * max(total_in_col)/sum(total_in_col)), 2),'%')
  model_info[[7]] = correct_ratio_mean
  names(model_info) <- c('confusion_matrix', 'accuracy', 'accuracy_explained', 'truth_matrix', 'optimized_single_guess','optimized_single_guess_explained', 'correct_ratio_mean')
  model_info
}

nb_model_simple <-  timss_student_nb(chile_simple_student_11)
#nb for chile_2011 is 57.35 ; Random guessing is 20 ; choosing only 'Below Low International' is 37.74

#Let's just see what IDSCHOOL performs without ITSEX
##57.17 It turns out that using gender is helpful, but doesn't make a huge difference
nb_model_simple_sexless <- timss_student_nb(chile_simple_student_11 %>% dplyr::select(-ITSEX))

#let's try the first model (just ITSEX and IDSCHOOL), but combine andvanced with high and combine low with below low
adjust_student_benchmark <- function(x){
  sapply(as.character(x), function(y){
    if(y == 'Below Low'){
      'Low'
    } else if (y == 'Advanced') {
      'High'
    } else {
      y
    }}) %>%
    as.factor()}

chile_simple_student_adjusted_11 <- chile_simple_student_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#nb model is 78.03 ; guessing is about 33.33 ; choosing just 'Low International' is 70.27
nb_model_simple_adjusted <- timss_student_nb(chile_simple_student_adjusted_11)

chile_student_school_11 <- chile_simple_student_11 %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL)) %>%
  left_join(chile_school_general_11) %>%
  filter(!is.na(question)) %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>% 
  spread(question, answer) %>%
  dplyr::select(-BCBG04) %>%
  na.omit() %>%
  group_by(BCBG02, IDSCHOOL, BCBG07) %>%
  mutate_all(as.factor) %>%
  ungroup()

#48.69 ; random guess is 20;just low is 33.11
nb_model_student_school <- timss_student_nb(chile_student_school_11)

chile_student_school_adjusted_11 <- chile_student_school_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#69.96; random guess 33.33; 66.16 just guessing low
nb_model_student_school_adjusted <- timss_student_nb(chile_student_school_adjusted_11)

chile_student_teacher_11 <- chile_simple_student_11 %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL)) %>%
  left_join(chile_teacher_general_11) %>% 
  filter(!is.na(answer)) %>% 
  dplyr::select(-c(FIELD_LABL, FIELD_CODE)) %>%
  spread(question, answer) %>%
  dplyr::select(-BTBM25CE) %>%
  na.omit() %>%
  group_by(IDSCHOOL) %>%
  mutate_all(as.factor) %>%
  ungroup() 

#46.62; 20; 33.82 below low
nb_model_student_teacher <- timss_student_nb(chile_student_teacher_11)  

chile_student_teacher_adjusted_11 <- chile_student_teacher_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#69.96; 33.33; 67.61
nb_model_student_teacher_adjusted <- timss_student_nb(chile_student_teacher_adjusted_11)

chile_student_student_11 <- chile_simple_student_11 %>%
  mutate(IDSTUD = IDSTUD %>% as.character() %>% as.numeric()) %>%
  left_join(chile_student_general_11) %>% 
  filter(!is.na(answer)) %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>%
  spread(question, answer) %>%
  dplyr::select(-BSDAGE) %>%
  na.omit() %>%
  group_by(IDSCHOOL) %>%
  mutate_all(as.factor) %>%
  ungroup()

#45.25; 20; 34.36 below low
nb_model_student_student <- timss_student_nb(chile_student_student_11)

chile_student_student_adjusted_11 <- chile_student_student_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#71.76; 33.33; 67.96 low
nb_model_student_student_adjusted <- timss_student_nb(chile_student_student_adjusted_11)

chile_student_student_111 <- chile_student_student_11 %>%
  mutate_all(as.character) %>%
  dplyr::select(-benchmark_math_avg_value, -ITSEX, -IDSCHOOL, -BSDAGE)

chile_student_teacher_111 <- chile_student_teacher_11 %>%
  mutate_all(as.character) %>%
  dplyr::select(-benchmark_math_avg_value, -ITSEX, -IDSCHOOL)

chile_student_school_111 <- chile_student_school_11 %>%
  mutate_all(as.character) %>%
  dplyr::select(-benchmark_math_avg_value, -ITSEX, -IDSCHOOL)

chile_all_11 <- chile_student_student_111 %>%
  full_join(chile_student_teacher_111) %>%
  full_join(chile_student_school_111) %>%
  left_join(chile_simple_student_11) %>%
  na.omit() %>% 
  group_by(BCBG02, IDSCHOOL, BCBG07) %>%
  mutate_all(as.factor) %>%
  ungroup() %>%
  mutate(BCBG02 = as.numeric(BCBG02)) %>%
  mutate(BCBG07 = as.numeric(BCBG07)) %>%
  mutate(IDSCHOOL = as.numeric(as.character(IDSCHOOL)))
  
#48.42 ; 20; 34.27 just low
nb_model_all <- timss_student_nb(chile_all_11)

chile_all_11_adjusted <- chile_all_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#70.15 ; 33.33; 64.46 if just low
nb_model_all_adjusted <- timss_student_nb(chile_all_11_adjusted)

detach(package:caret)
detach(package:klaR)
detach(package:MASS)

nb_models_original <- list(nb_model_simple, nb_model_simple_sexless, nb_model_student_school, nb_model_student_teacher, nb_model_student_student, nb_model_all)
nb_models_adjusted <- list(nb_model_simple_adjusted, nb_model_student_school_adjusted, nb_model_student_teacher_adjusted, nb_model_student_student_adjusted, nb_model_all_adjusted)

accuracy_explained <- ''
x = c('hi', 'bye', 'whatever')
for(i in seq_along(nb_models_original)){
  accuracy_explained <- str_c(accuracy_explained, '\n', str_c(nb_models_original[[i]][[3]], collapse = '\t'))
}
cat(accuracy_explained)

#------------------------------------------------------------------------------------#
#------------------------------------- SVM ------------------------------------------#
#------------------------------------------------------------------------------------#

require(e1071)

#---------------------------- Student Level -----------------------------------------#

set.seed(1)

timss_student_svm <- function(model){
  train <- sample(nrow(model), nrow(model)/2)
  #factors with more than 2 levels like school need to be turned into a yes/no matrix
  model <- model %>%
    mutate(count = 1) %>%
    spread(IDSCHOOL, count, fill = 0)
  
  svm_train <- model[train,] 
  
  svm_test <- model[-train,] 
  
  out = svm(benchmark_math_avg_value~.-IDSTUD, data = svm_train, kernel='linear', cost = 10)
  pred.te = predict(out, newdata = svm_test)
  table(pred.te, svm_test$benchmark_math_avg_value)
}

#svm is 53.71 and 77.69 for adjusted
svm_model_simple <- analyze_model(chile_simple_student_11, timss_student_svm)
svm_model_simple_adjusted <- analyze_model(chile_simple_student_adjusted_11, timss_student_svm)

prep_for_svm <- function(df, cols2ignore){
  svm_factors <- df %>%
    select_(.dots = names(.)[!names(.) %in% cols2ignore]) %>%
#    select(-IDSTUD, -ITSEX, -benchmark_math_avg_value) %>%
    Filter(f = is.factor) %>%
    names()
  
  for(i in seq_along(svm_factors)){
    df[svm_factors[i]] <- sapply(df[svm_factors[i]], function(x){
        str_c(i, '_', x)
    })
    df <- df %>%
      mutate(count = 1) %>%
      spread_(svm_factors[i], 'count', fill = 0)
  }
  df
}

chile_student_school_11_svmprep <- prep_for_svm(chile_student_school_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value'))
svm_model_student_school <- analyze_model(chile_student_school_11_svmprep, timss_student_svm)
svm_model_student_school_adjusted <- analyze_model(prep_for_svm(chile_student_school_adjusted_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')), timss_student_svm)

str(chile_student_teacher_11)
svm_model_student_teacher <- analyze_model(prep_for_svm(chile_student_teacher_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')), timss_student_svm)
svm_model_student_teacher_adjusted <- analyze_model(prep_for_svm(chile_student_teacher_adjusted_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')), timss_student_svm)

str(chile_student_student_11)
svm_model_student_student <- analyze_model(prep_for_svm(chile_student_student_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value', 'IDSCHOOL')), timss_student_svm)
svm_model_student_student_adjusted <- analyze_model(prep_for_svm(chile_student_student_adjusted_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value', 'IDSCHOOL')), timss_student_svm)

str(chile_all_11)
svm_model_all <- analyze_model(prep_for_svm(chile_all_11, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')), timss_student_svm)
svm_model_all_adjusted <- analyze_model(prep_for_svm(chile_all_11_adjusted, c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')), timss_student_svm)

detach(e1071)

#==================================================================================#
#============================ Trees ===============================================#
#==================================================================================#

library(tree)
library(randomForest)

#require(Amelia)
#missmap(timss_all_model_11)
#trees can't have any factors with more than 32 levels()
#needs to be even numbered rows for future comparison
set.seed(1)
tree_model = chile_all_11 %>%
  select(-IDSCHOOL) %>%
  .[sample(1:(nrow(.)-1)),]


tree.timss = tree(benchmark_math_avg_value~.-IDSTUD, data = tree_model)
summary(tree.timss)
plot(tree.timss)
text(tree.timss, pretty=0)

train <- sample(1:nrow(tree_model), nrow(tree_model)*3/4)
timss_tree_test <- tree_model[-train,]
test_benchmark <- tree_model$benchmark_math_avg_value[-train]
tree.timss <- tree(benchmark_math_avg_value~.-IDSTUD, data = tree_model, subset = train)
tree.pred = predict(tree.timss, timss_tree_test, type = 'class')
table(tree.pred, test_benchmark)


cv.timss <- cv.tree(tree.timss, FUN = prune.misclass)
#dev is cross validation error. smaller cv corresponds to ideal size
cv.timss
prune.timss = prune.misclass(tree.timss, best = 3)
plot(prune.timss)
text(prune.timss, pretty = 0)

tree.pred = predict(prune.timss, timss_tree_test, type = 'class')
#worst still...
table(tree.pred, test_benchmark)

summary(tree.timss)


set.seed(2)
#mtry = number of predictors we should try
bag.timss <- randomForest(benchmark_math_avg_value~.-IDSTUD, 
                          data = tree_model, 
                          subset = train, mtry=36, 
                          importance=T)
bag.timss

yhat.bag = predict(bag.timss, newdata = tree_model[-train,])
plot(yhat.bag, timss_tree_test$benchmark_math_avg_value)

bag.timss = randomForest(benchmark_math_avg_value~.-IDSTUD,
                         data = tree_model,
                         subset = train,
                         mtry = 36,
                         ntree=25)
yhat.bag = predict(bag.timss, newdata = tree_model[-train,])

table(yhat.bag,timss_tree_test$benchmark_math_avg_value)

rf.timss = randomForest(benchmark_math_avg_value~.-IDSTUD,
                        data = tree_model,
                        subset = train,
                        mtry = 18,
                        importance = T)
yhat.rf = predict(rf.timss, newdata = tree_model[-train,])
table(yhat.rf, timss_tree_test$benchmark_math_avg_value)
importance(rf.timss)
varImpPlot(rf.timss)

library(gbm)
boost.timss <- gbm(benchmark_math_avg_value~.-IDSTUD,
                   data = tree_model[train,],
                   distribution = 'gaussian',
                   n.trees = 5000,
                   interaction.depth = 4)
summary(boost.timss)
yhat.boost = predict(boost.timss, 
                     newdata = tree_model[-train,], 
                     n.trees = 5000)
table(yhat.boost, timss_tree_test$benchmark_math_avg_value)
