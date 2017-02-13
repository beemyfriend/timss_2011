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

timss_student_nb <- function(model){
  x <- model %>%
    dplyr::select(-c(benchmark_math_avg_value, IDSTUD))
  
  truth <- model$benchmark_math_avg_value %>%
    as.factor()
  
  model <- train(x, truth, 'nb', trControl = trainControl(method = 'cv', number = 10))
  
  prediction <- predict(model$finalModel, x)
  ed_guess <- prediction$class
  table(ed_guess, truth)
}

timss_student_nb(chile_simple_student_11)
#nb for chile_2011 is 57.35 ; Random guessing is 20 ; choosing only 'Below Low International' is 37.74
(0 + 1679 + 163 + 526 + 961)/nrow(chile_simple_student_11)
table(chile_simple_student_11$benchmark_math_avg_value, chile_simple_student_11$benchmark_math_avg_value)
(2191)/nrow(chile_simple_student_11)

#Let's just see what IDSCHOOL performs without ITSEX
##57.17 It turns out that using gender is helpful, but doesn't make a huge difference
timss_student_nb(chile_simple_student_11 %>% dplyr::select(-ITSEX))
(0 + 1650 + 163 + 532 + 974)/nrow(chile_simple_student_11)

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
timss_student_nb(chile_simple_student_adjusted_11)
(219 + 452 + 3859)/nrow(chile_simple_student_adjusted_11)
table(chile_simple_student_adjusted_11$benchmark_math_avg_value, chile_simple_student_adjusted_11$benchmark_math_avg_value)
(4079)/nrow(chile_simple_student_adjusted_11)

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
timss_student_nb(chile_student_school_11)
(0 + 600 + 181 + 106 + 585)/nrow(chile_student_school_11)
table(chile_student_school_11$benchmark_math_avg_value, chile_student_school_11$benchmark_math_avg_value)
1001/nrow(chile_student_school_11)

chile_student_school_adjusted_11 <- chile_student_school_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#69.96; random guess 33.33; 66.16 just guessing low
timss_student_nb(chile_student_school_adjusted_11)
(203 + 70 + 1842)/nrow(chile_student_school_adjusted_11)
table(chile_student_school_adjusted_11$benchmark_math_avg_value, chile_student_school_adjusted_11$benchmark_math_avg_value)
(2000)/nrow(chile_student_school_adjusted_11)

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
timss_student_nb(chile_student_teacher_11)  
(14 + 858 + 154 + 239 + 439)/nrow(chile_student_teacher_11)
table(chile_student_teacher_11$benchmark_math_avg_value, chile_student_teacher_11$benchmark_math_avg_value)
(1235)/nrow(chile_student_teacher_11)

chile_student_teacher_adjusted_11 <- chile_student_teacher_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#69.96; 33.33; 67.61
timss_student_nb(chile_student_teacher_adjusted_11)
(192 + 223 + 2142)/nrow(chile_student_teacher_adjusted_11)
table(chile_student_teacher_adjusted_11$benchmark_math_avg_value, chile_student_teacher_adjusted_11$benchmark_math_avg_value)
(2471)/nrow(chile_student_teacher_adjusted_11)

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
timss_student_nb(chile_student_student_11)
(0 + 1142 + 288 + 32 + 717)/nrow(chile_student_student_11)
table(chile_student_student_11$benchmark_math_avg_value, chile_student_student_11$benchmark_math_avg_value)
1655/nrow(chile_student_student_11)

chile_student_student_adjusted_11 <- chile_student_student_11 %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value))

#71.76; 33.33; 67.96 low
timss_student_nb(chile_student_student_adjusted_11)
(339 + 348 + 2769)/nrow(chile_student_student_adjusted_11)
table(chile_student_student_adjusted_11$benchmark_math_avg_value, chile_student_student_adjusted_11$benchmark_math_avg_value)
3273/nrow(chile_student_student_adjusted_11)

chile_all_11 <- chile_simple_student_11 %>%
  mutate(IDSTUD = IDSTUD %>% as.character() %>% as.numeric()) %>%
  mutate(IDSCHOOL = as.double(IDSCHOOL)) %>%
  left_join(chile_student_general_11) %>%
  left_join(chile_teacher_general_11) %>%
  left_join(chile_school_general_11) 

detach(package:caret)
detach(package:klaR)
detach(package:MASS)


#------------------------------------------------------------------------------------#
#------------------------------------- SVM ------------------------------------------#
#------------------------------------------------------------------------------------#

require(e1071)

#---------------------------- Student Level -----------------------------------------#

set.seed(1)
train_student_chile <- sample(nrow(chile_simple_student), nrow(chile_simple_student)/2)

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
timss_student_svm(chile_simple_student)
(0 + 774 + 47 + 246 + 492)/(nrow(chile_model_simple)/2)
timss_student_svm(chile_simple_student_adjusted)
(74  + 214 + 1967)/(nrow(chile_model_simple)/2)

#svm is 51.51 and 62.06 for adjusted
timss_student_svm(us_simple_student)
(112 + 70 + 680 + 1270 + 557)/(nrow(us_simple_student)/2)
timss_student_svm(us_simple_student_adjusted)
(1052 + 1198 + 990)/(nrow(us_simple_student_adjusted)/2)
