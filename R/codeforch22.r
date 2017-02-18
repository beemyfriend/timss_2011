#===============================================#
#=================== Setup =====================#
#===============================================#

#-----------------------------------------------#
#-------------- Packages -----------------------#
#-----------------------------------------------#


library(tidyverse)
library(stringr)

#-----------------------------------------------#
#-------------- Functions ----------------------#
#-----------------------------------------------#


format_codebook <- function(excel_file){
  readxl::read_excel(excel_file) %>%
    mutate(type_response = sapply(.$FIELD_CODE, function(x){
      if(str_detect(x, 'ID:VALUE')){
        return('ID')
      } else if(str_detect(x, ':VALUE')){
        return('Free-Response')
      } else {
        return('Multiple-Choice')
      }
    })) %>%
    select(FIELD_NAME, FIELD_LABL, type_response, FIELD_CODE)
}

find_important_columns <- function(df, codebook, seed = 1234){
  set.seed(seed)
  influence <- gbm(benchmark_math_avg_value~.-IDSTUD,
                   data = df,
                   distribution = 'gaussian',
                   n.trees = 200, 
                   interaction.depth = 4,
                   shrinkage = .1,
                   verbose = F) %>%
    summary() %>%
    left_join(codebook, by = c('var' = 'FIELD_NAME'))
  influence
}

extract_important_columns <- function(df, column_df, cut_off = 0, get_rid = c()){
  influential_columns <- column_df %>%
    filter(rel.inf >= cut_off) %>%
    filter(!var %in% get_rid) %>%
    .$var
  
  free_response_columns <- column_df %>%
    filter(type_response == 'Free-Response') %>%
    .$var
  
  df %>% 
    select_(.dots = names(.)[names(.) %in% c(influential_columns, c('benchmark_math_avg_value', 'IDSTUD'))]) %>%
    group_by_(.dots = names(.)[names(.) %in% free_response_columns]) %>%
    mutate_all(as.factor) %>%
    ungroup
}

timss_student_nb <- function(df, train_ratio = .75, seed = 4321){
  set.seed(seed)
  train <- sample(nrow(df), (nrow(df) * train_ratio))
  
  clean_df <- df %>%
    dplyr::select(-c(benchmark_math_avg_value, IDSTUD))

  train_df <- clean_df[train,]
  test_df <- clean_df[-train,]
    
  train_y <- df$benchmark_math_avg_value[train]
  test_y <- df$benchmark_math_avg_value[-train]
  
  model <- train(train_df, train_y, 'nb', trControl = trainControl(method = 'cv', number = 10))
  
  prediction <- predict(model$finalModel, test_df)
  ed_guess <- prediction$class
  
  table(ed_guess, test_y)
}

analyze_model <- function(df, model_func, model_name){
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
  model_info[[8]] = model_name
  names(model_info) <- c('confusion_matrix', 'accuracy', 'accuracy_explained', 'truth_matrix', 'optimized_single_guess','optimized_single_guess_explained', 'correct_ratio_mean', 'model_name')
  model_info
}

adjust_student_benchmark <- function(df){
  df %>%
    mutate(benchmark_math_avg_value = sapply(as.character(benchmark_math_avg_value), function(y){
    if(y == 'Below Low'){
      'Low'
    } else if (y == 'Advanced') {
      'High'
    } else {
      y
    }}) %>%
    as.factor())}

compare_accuracy <- function(list_models){
  accuracy_explained <- ''
  for(i in seq_along(list_models)){
    accuracy_explained <- str_c(accuracy_explained, '\n', str_c(list_models[[i]][[3]], collapse = '\t'))
  }
  cat(accuracy_explained)
}

#factors with more than 2 levels like school need to be turned into a yes/no matrix
prep_for_svm <- function(df, cols2ignore = c()){
  svm_factors <- df %>%
    select_(.dots = names(.)[!names(.) %in% cols2ignore]) %>%
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

timss_student_svm <- function(model, train_ratio = .75, seed = 1111){
  set.seed(seed)

  train <- sample(nrow(model), (nrow(model) * train_ratio))
  
  svm_train <- model[train,] 
  
  svm_test <- model[-train,] 
  
  out = svm(benchmark_math_avg_value~.-IDSTUD, data = svm_train, kernel='linear', cost = 10)
  pred.te = predict(out, newdata = svm_test)
  table(pred.te, svm_test$benchmark_math_avg_value)
}

timss_student_tree <- function(model, train_ratio = .75, seed = 7777){
  set.seed(seed)
  
  train <- sample(1:nrow(model), nrow(model)*train_ratio)
  
  tree_train <- model[train,]
  tree_test <- model[-train,]
  
  test_benchmark <- model$benchmark_math_avg_value[-train]
  
  tree.timss <- tree(benchmark_math_avg_value~.-IDSTUD, data = model, subset = train)
  tree.pred = predict(tree.timss, tree_test, type = 'class')

  table(tree.pred, test_benchmark)
  
}

timss_student_rforest <- function(df, train_ratio = .75, seed = 1212){
  set.seed(seed)
  
  train <- sample(nrow(df), (nrow(df) * train_ratio))
  
  rf.timss <- randomForest(benchmark_math_avg_value~.-IDSTUD,
                           data = df,
                           subset = train,
                           importance = T)
  
  yhat.rf <- predict(rf.timss, newdata = df[-train,])
  table(yhat.rf, df[-train,]$benchmark_math_avg_value)
}

#===============================================#
#============= Dataframes ======================#
#===============================================#

library(gbm)

#-----------------------------------------------#
#------------- Student Achievement -------------#
#-----------------------------------------------#

chile_timss_math_11 <- read_csv('2011_TIMSS_CHILE.csv')

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

rm(chile_timss_math_11)

#-----------------------------------------------#
#------ student-school combo -------------------#
#-----------------------------------------------#

school_codebook <- format_codebook('bcgtmsm5.xls')

chile_school_general_11 <- haven::read_spss('bcgchlm5.sav') %>%
  gather(question, answer, -IDSCHOOL) %>%
  left_join(school_codebook, by = c('question' = 'FIELD_NAME'))

chile_student_school_11 <- chile_simple_student_11 %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL)) %>%
  left_join(chile_school_general_11) %>%
  filter(!is.na(question)) %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>% 
  spread(question, answer, fill = 0) %>%
  na.omit()

student_school_columns <- find_important_columns(chile_student_school_11, school_codebook)
student_school_influential_columns <- extract_important_columns(chile_student_school_11,
                                                                student_school_columns,
                                                                1.1,
                                                                c('STOTWGTU', 'CSYSTEM', 'BCDG06HY', 'IDSTRATI', 'SCHWGT', 'JKCZONE', 'WGTFAC1'))

#-----------------------------------------------#
#-------------- student-teacher combo ----------#
#-----------------------------------------------#

teacher_codebook <- format_codebook('btmtmsm5.xls')

chile_teacher_general_11 <- haven::read_spss('btmchlm5.sav') %>%
  gather(question, answer, -c(IDSCHOOL)) %>%
  left_join(teacher_codebook, by = c('question' = 'FIELD_NAME')) %>%
  filter(!is.na(IDSCHOOL)) %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL))

chile_student_teacher_11 <- chile_simple_student_11 %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL)) %>%
  mutate(IDSTUD = as.double(IDSTUD)) %>%
  left_join(chile_teacher_general_11) %>% 
  filter(question != 'IDLINK' & question != 'TSYSTEM' & question != 'IDTEALIN') %>%
  distinct() %>%
  filter(!is.na(answer)) %>% 
  dplyr::select(-c(FIELD_LABL, FIELD_CODE)) %>%
  spread(question, answer, fill = 0) %>%
  na.omit() #%>%

student_teacher_columns <- find_important_columns(chile_student_teacher_11, teacher_codebook)
student_teacher_influential_columns <- extract_important_columns(chile_student_teacher_11, 
                                                                 student_teacher_columns,
                                                                 .8)

#-----------------------------------------------#
#--------------- student-student combo ---------#
#-----------------------------------------------#

student_general_codebook <- format_codebook('bsgtmsm5.xls')

chile_student_general_11 <- haven::read_spss('bsgchlm5.sav') %>%
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
  left_join(student_general_codebook, by = c('question' = 'FIELD_NAME')) %>%
  filter(!is.na(IDSTUD)) 

chile_student_student_11 <- chile_simple_student_11 %>%
  mutate(IDSTUD = IDSTUD %>% as.character() %>% as.numeric()) %>%
  left_join(chile_student_general_11) %>% 
  mutate(answer = as.numeric(answer)) %>%
  filter(!is.na(answer)) %>%
  filter(!str_detect(question, '^BSM|^BSS|^BSBS')) %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>%
  filter(question != 'IDSCHOOL' & question != 'ITSEX') %>%
  filter(!is.na(answer)) %>%
  spread(question, answer, fill = 0) %>%
  dplyr::select(-BSDAGE) %>%
  na.omit()# %>%

student_student_columns <- find_important_columns(chile_student_student_11, student_general_codebook)
student_student_influential_columns <- extract_important_columns(chile_student_student_11,
                                                                student_student_columns,
                                                                .09,
                                                                c('BSDSLOWP')) %>%
  mutate(diff_from_mean_age = diff_from_mean_age %>% as.character() %>% as.numeric())


#-----------------------------------------------#
#------------ All Info combo -------------------#
#-----------------------------------------------#

chile_all_11a <- rbind(chile_teacher_general_11, chile_school_general_11) %>%
  filter(question != 'IDLINK' & question != 'TSYSTEM' & question != 'IDTEALIN') %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>%
  distinct() %>%
  filter(!is.na(answer)) %>%
  spread(question, answer, fill = 0)

chile_all_11b <- chile_student_general_11 %>%
  distinct() %>%
  filter(question != 'IDSCHOOL' & question != 'ITSEX') %>%
  filter(!str_detect(question, '^BSM|^BSS|^BSBS')) %>%
  dplyr::select(-FIELD_LABL, -FIELD_CODE) %>%
  mutate(answer = as.numeric(answer)) %>%
  filter(!is.na(answer)) %>%
  spread(question, answer, fill = 0) %>%
  na.omit()

chile_all_11 <- chile_simple_student_11 %>%
  mutate(IDSTUD = IDSTUD %>% as.character() %>% as.numeric()) %>%
  left_join(chile_all_11b) %>%
  mutate(IDSCHOOL = IDSCHOOL %>% as.character() %>% as.numeric) %>%
  dplyr::select(-DPCDATE, -IDCNTRY, -IDGRADE, -IDGRADER, -IDPOP, -IDSTRATE, -IDSTRATI, -WGTADJ1, -WGTFAC1) %>% 
  left_join(chile_all_11a)

rm(chile_all_11a, chile_all_11b)

all_codebook <- rbind(student_general_codebook, teacher_codebook, school_codebook) %>% distinct()

chile_all_columns <- find_important_columns(chile_all_11, all_codebook)
#Need a better understanding of jacknife before I include jkzone and idstrat in analysis
chile_all_influential_columns <- extract_important_columns(chile_all_11, 
                                                                 chile_all_columns,
                                                                 .8,
                                                                 c('STOTWGTU', 'HOUWGT', 'BSDSLOWP', 'IDSTRATE', 
                                                                   'SSYSTEM', 'JKZONE', 'IDSTRATI', 'BSDMLOWP'))

detach(package:gbm)

#===============================================#
#=============== Models ========================#
#===============================================#

#-----------------------------------------------#
#------------ Naive Bayes ----------------------#
#-----------------------------------------------#

library(caret)

nb_model_simple <-  analyze_model(chile_simple_student_11, timss_student_nb, 'NB: Simple')
nb_model_simple_adjusted <- chile_simple_student_11 %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_nb, 'NB: Simple Adjusted')
  
nb_model_student_school <- analyze_model(student_school_influential_columns, timss_student_nb, 'NB: Student-School Combo')
nb_model_student_school_adjusted <- student_school_influential_columns %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_nb, 'NB: Student-School Combo Adjusted')

nb_model_student_teacher <- analyze_model(student_teacher_influential_columns, timss_student_nb, 'NB: Student-Teacher Combo')
nb_model_student_teacher_adjusted <- student_school_influential_columns %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_nb, 'NB: Student-Teacher Combo Adjusted')

nb_model_student_student <- analyze_model(student_student_influential_columns, timss_student_nb, 'NB: Student-Student Combo')
nb_model_student_student_adjusted <- student_student_influential_columns %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_nb, 'NB: Student-Student Combo Adjusted')

nb_model_chile_all <- analyze_model(chile_all_influential_columns, timss_student_nb, 'NB: Full Combo')
nb_model_chile_all_adjusted <- chile_all_influential_columns %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_nb, 'NB: Full Combo Adjusted')

detach(package:caret)
detach(package:klaR)
detach(package:MASS)

nb_models_original <- list(nb_model_simple, nb_model_student_school, nb_model_student_teacher, nb_model_student_student, nb_model_chile_all)
nb_models_adjusted <- list(nb_model_simple_adjusted, nb_model_student_school_adjusted, nb_model_student_teacher_adjusted, nb_model_student_student_adjusted, nb_model_chile_all_adjusted)

compare_accuracy(nb_models_original)
compare_accuracy(nb_models_adjusted)

#-----------------------------------------------#
#------- Support Vector Machine ----------------#
#-----------------------------------------------#

library(e1071)

svm_model_simple <- chile_simple_student_11 %>%
  prep_for_svm(c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Simple')

svm_model_simple_adjusted <- chile_simple_student_11 %>%
  adjust_student_benchmark() %>%
  prep_for_svm(c('IDSTUD', 'ITSEX', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Simple Adjusted')

svm_model_student_school <- student_school_influential_columns %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value' )) %>%
  analyze_model(timss_student_svm, 'SVM: Student-School Combo')

svm_model_student_school_adjusted <- student_school_influential_columns %>%
  adjust_student_benchmark() %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Student-School Combo Adjusted')

svm_model_student_teacher <- student_teacher_influential_columns %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Student-Teacher Combo')

svm_model_student_teacher_adjusted <- student_teacher_influential_columns %>%
  adjust_student_benchmark() %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Student-Teacher Combo Adjusted')

svm_model_student_student <- student_student_influential_columns %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Student-Student Combo')

svm_model_student_student_adjusted <-student_student_influential_columns %>%
  adjust_student_benchmark() %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Student-Student Combo Adjusted')

svm_model_chile_all <- chile_all_influential_columns %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Full Combo')

svm_model_chile_all_adjusted <- chile_all_influential_columns %>%
  adjust_student_benchmark() %>%
  prep_for_svm(c('IDSTUD', 'benchmark_math_avg_value')) %>%
  analyze_model(timss_student_svm, 'SVM: Full Combo Adjusted')

detach(package:e1071)

svm_models_original <- list(svm_model_simple, svm_model_student_school, svm_model_student_teacher, svm_model_student_student, svm_model_chile_all)
svm_models_adjusted <- list(svm_model_simple_adjusted, svm_model_student_school_adjusted, svm_model_student_teacher_adjusted, svm_model_student_student_adjusted, svm_model_chile_all_adjusted)

compare_accuracy(svm_models_original)
compare_accuracy(svm_models_adjusted)

#-----------------------------------------------#
#----------- Trees -----------------------------#
#-----------------------------------------------#

library(tree)

tree_model_simple <- chile_simple_student_11 %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_tree, 'TREE: Simple')

tree_model_simple_adjusted <- chile_simple_student_11 %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_tree, 'TREE: Simple Adjusted')

tree_model_student_school <- student_school_influential_columns %>% 
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_tree, 'TREE: Student-School Combo')

tree_model_student_school_adjusted <- student_school_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_tree, 'TREE: Student-School Combo Adjusted')

tree_model_student_teacher <- student_teacher_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_tree, 'TREE: Student-Teacher Combo')

tree_model_student_teacher_adjusted <- student_teacher_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_tree, 'TREE: Student-Teacher Combo Adjusted')

tree_model_student_student <- student_student_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_tree, 'TREE: Student-Student Combo')

tree_model_student_student_adjusted <- student_student_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_tree, 'TREE: Student-Student Combo Adjusted')

tree_model_chile_all <- chile_all_11 %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_tree, 'TREE: Full Combo')

tree_model_chile_all_adjusted <- chile_all_11 %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_tree, 'TREE: Full Combo Adjusted')

detach(package:tree)

tree_models_original <- list(tree_model_simple, tree_model_student_school, tree_model_student_teacher, tree_model_student_student, tree_model_chile_all)
tree_models_adjusted <- list(tree_model_simple_adjusted, tree_model_student_school_adjusted, tree_model_student_teacher_adjusted, tree_model_student_student_adjusted, tree_model_chile_all_adjusted)

compare_accuracy(tree_models_original)
compare_accuracy(tree_models_adjusted)

#-----------------------------------------------#
#---------- Random Forests ---------------------#
#-----------------------------------------------#

library(randomForest)

rforest_model_simple <- chile_simple_student_11 %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_rforest, 'FOREST: Simple')

rforest_model_simple_adjusted <- chile_simple_student_11 %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_rforest, 'FOREST: Simple Adjusted')

rforest_model_student_school <- student_school_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-School Combo')

rforest_model_student_school_adjusted <- student_school_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-School Combo Adjusted')

rforest_model_student_teacher <- student_teacher_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-Teacher Combo')

rforest_model_student_teacher_adjusted <- student_teacher_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-Teacher Combo Adjusted')

rforest_model_student_student <- student_student_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-Student Combo') 

rforest_model_student_student_adjusted <- student_student_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_rforest, 'FOREST: Student-Student Combo Adjusted')

rforest_model_chile_all <- chile_all_influential_columns %>%
  select(-IDSCHOOL) %>%
  analyze_model(timss_student_rforest, 'FOREST: Full Combo')

rforest_model_chile_all_adjusted <- chile_all_influential_columns %>%
  select(-IDSCHOOL) %>%
  adjust_student_benchmark() %>%
  analyze_model(timss_student_rforest, 'FOREST: Full Combo Adjusted')

detach(package:randomForest)

rforest_models_original <- list(rforest_model_simple, rforest_model_student_school, rforest_model_student_teacher, rforest_model_student_student, rforest_model_chile_all)
rforest_models_adjusted <- list(rforest_model_simple_adjusted, rforest_model_student_school_adjusted, rforest_model_student_teacher_adjusted, rforest_model_student_student_adjusted, rforest_model_chile_all_adjusted)

compare_accuracy(rforest_models_original)
compare_accuracy(rforest_models_adjusted)
