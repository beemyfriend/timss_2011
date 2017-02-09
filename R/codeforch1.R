library(tidyverse)
library(stringr)
library(haven)

chl_achievement_11 <- read_spss('bsachlm5.sav')
kor_achievement_11 <- read_spss('bsakorm5.sav')
ltu_achievement_11 <- read_spss('bsaltum5.sav')
usa_achievement_11 <- read_spss('bsausam5.sav')

detach(package:haven)

library(readxl)

achievement_codebook_11 <- read_excel('bsatmsm5.xls') %>%
  select(FIELD_NAME, FIELD_LABL, MEAS_CLASS, COMMENT1) %>%
  mutate(MEAS_CLASS = str_replace(MEAS_CLASS, '^M', '')) %>%
  separate(COMMENT1, into = c('content_domain', 'cognitive_domain'), sep = '\\\\') %>%
  mutate(cognitive_domain = str_extract(cognitive_domain, '\\w+')) %>%
  mutate(question_type = sapply(MEAS_CLASS, function(x){
    if(str_detect(x, '\\d')){
      'Multiple Choice'
    } else if (str_detect(x, 'SA|DPC_D')){
      'Free Response'
    } else {
      'Other'
    }
  }))

detach(package:readxl)

  
grab_math_questions <- function(df){
  df %>%
    #^M grabs all the math questions
    #^BSM grabs all the benchmark scores
    select_(.dots = names(.)[str_detect(names(.), '^M|IDSTUD|IDBOOK|IDSCHOOL|ITSEX|^BSM')]) %>%
    gather_('question', 'answer', names(.)[str_detect(names(.), '^M')]) %>% 
    #about 30 students who responded NA to everything will disappear
    #by getting rid of these students I end up having the same number of students per question as the almanac
    filter(!is.na(answer))}

get_performance_score <- function(df){
  df %>%
    mutate(benchmark_math_avg = (BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05)/5) %>%
    mutate(benchmark_math_avg_value = sapply(benchmark_math_avg, function(x){
      if(x >= 625){
        'Advanced International'
      } else if (x > 550) {
        'High International'
      } else if (x > 475) {
        'Intermediate International'
      } else if (x > 400) {
        'Low International'
      } else {
        'Below Low International'
      }
    }))}

get_book_info <- function(df){
  df %>%
    group_by(IDBOOK) %>% 
    nest() %>%
    mutate(
      students_per_book = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>% 
          group_by(IDSTUD) %>%
          count() %>%
          nrow()
      }) %>% 
        unlist()) %>%
    mutate(
      students_per_book_female = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>% 
          group_by(IDSTUD, ITSEX) %>%
          count() %>%
          filter(ITSEX == 1) %>%
          nrow()
      }) %>%
        unlist()) %>%
    mutate(
      students_per_book_male = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>%
          group_by(IDSTUD, ITSEX) %>%
          count() %>%
          filter(ITSEX == 2) %>%
          nrow()
      }) %>%
        unlist()) %>%
    unnest()}

combine_datasets <- function(df, cdbook){
  df %>%
    left_join(cdbook, by = c('question' = 'FIELD_NAME')) %>%
    #for open ended questions answers in the 20s are all correct however, those in the 10s can either be partially correct or fully correct
    #until I can figure out how to parse the two, I will mark them all as correct
    #for multiple choice the correct answer is stored in the 'MEAS_CLASs' variable
    mutate(
      student_gave_correct_answer = lapply(seq_along(.$MEAS_CLASS), function(i){
        if(.$MEAS_CLASS[i] %in% 1:4){
          .$MEAS_CLASS[i] == .$answer[i]
        } else {
          str_detect(.$answer[i], '^[12]')
        }}) %>%
        unlist()) %>%
    #maybe I can figure out the open ended problems with the correct answers derived from $FIELD_LABL
    mutate(
      correct_answer_derived_from_labl = FIELD_LABL %>% 
        str_extract( '\\(\\d\\)|\\(\\w\\)')
    ) %>%
    mutate(
      FIELD_LABL = FIELD_LABL %>%
        str_replace(' \\(\\d\\)|\\(\\w\\)', '')
    )}

get_question_info <- function(df){
  df %>%  
    group_by(question) %>%
    nest() %>%
    mutate(
      students_per_question = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>% 
          group_by(IDBOOK, students_per_book) %>% 
          count() %>%
          .$students_per_book %>%
          sum()
      }) %>%
        unlist()) %>%
    mutate(
      students_per_question_female = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>%
          group_by(IDBOOK, students_per_book_female) %>%
          count() %>%
          .$students_per_book_female %>%
          sum()
      }) %>%
        unlist()) %>%
    mutate(
      students_per_question_male = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>%
          group_by(IDBOOK, students_per_book_male) %>%
          count() %>%
          .$students_per_book_male %>%
          sum()
      }) %>%
        unlist()) %>%
    #according to the math_itemalmanac I am off by about 5% fo each question
    #but I actually think that the almanac is wrong
    #at least when I compare the percent of each response for the multiple choice
    #another posibility is that there was some kind of mix up with regards to the
    #conversion of SAS/SPSS to R
    mutate(
      correct_ratio_per_question = lapply(seq_along(.$data), function(i){
        tot_students  = .$students_per_question[i]
        .$data[[i]] %>%
          filter(student_gave_correct_answer) %>%
          nrow()/tot_students
      }) %>% 
        unlist()) %>%
    mutate(
      correct_ratio_per_question_female = lapply(seq_along(.$data), function(i){
        tot_female = .$students_per_question_female[i]
        .$data[[i]] %>%
          filter(student_gave_correct_answer, ITSEX == 1) %>%
          nrow()/ tot_female
      }) %>% 
        unlist()) %>%
    mutate(
      correct_ratio_per_question_male = lapply(seq_along(.$data), function(i){
        tot_male = .$students_per_question_male[i]
        .$data[[i]] %>%
          filter(student_gave_correct_answer, ITSEX == 2) %>%
          nrow()/ tot_male
      }) %>%
        unlist()) %>%
    unnest()}

clean_math <- function(df, cdbook){
  df %>%
    grab_math_questions() %>%
    get_performance_score() %>%
    get_book_info() %>%
    combine_datasets(cdbook) %>%
    get_question_info()}


chl_timss_math_11 <- clean_math(chl_achievement_11, achievement_codebook_11)
ltu_timss_math_11 <- clean_math(ltu_achievement_11, achievement_codebook_11)
kor_timss_math_11 <- clean_math(kor_achievement_11, achievement_codebook_11)
usa_timss_math_11 <- clean_math(usa_achievement_11, achievement_codebook_11)


basic_graph_setup <- function(math_df, country){
  math_df %>%
    group_by(question, students_per_question, correct_ratio_per_question, correct_ratio_per_question_female,
             correct_ratio_per_question_male, FIELD_LABL, content_domain, cognitive_domain, question_type) %>%
    nest() %>%
    arrange(desc(correct_ratio_per_question)) %>%
    mutate(question_rank = 1:nrow(.)) %>%
    mutate(country = country) %>%
    mutate(diff_male_female = correct_ratio_per_question_male - correct_ratio_per_question_female) %>%
    mutate(dominant_gender = sapply(.$diff_male_female, function(x){
      if(x >0) {
        'Male'
      } else if(x<0){
        'Female'
      } else {
        'Tie'
      }
    }))
}

chl_basic_graph_info <- basic_graph_setup(chl_timss_math_11, 'Chile')
ltu_basic_graph_info <- basic_graph_setup(ltu_timss_math_11, 'Lithuania')
kor_basic_graph_info <- basic_graph_setup(kor_timss_math_11, 'Korea')
usa_basic_graph_info <- basic_graph_setup(usa_timss_math_11, 'USA')

international_basic_graph_info <- rbind(chl_basic_graph_info, ltu_basic_graph_info, kor_basic_graph_info, usa_basic_graph_info)

ggplot(international_basic_graph_info, aes(question_rank, diff_male_female)) +
  geom_col(aes(fill = dominant_gender)) +
  scale_y_continuous(limits = c(-.15, .15)) +
  facet_wrap(~country) +
  labs(x = 'Questions Ranked From Easiest to Hardest According to Country',
       y = 'Males Correct Ratio - Females Correct Ratio',
       fill = 'Gender',
       title = 'Gender Comparison Within Countries',
       subtitle = 'Barplot')

ggplot(international_basic_graph_info, aes(correct_ratio_per_question, diff_male_female)) +
  geom_text(aes(label = question, color = dominant_gender)) +
  facet_wrap(~country) +
  labs(x = 'n Students Who Correctly Answered Question / Total Students',
       y = 'Males Correct Ratio - Females Correct Ratio',
       color = 'Gender',
       title = 'Gender Comparison Within Countries',
       subtitle = 'Scatter-Text Plot')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


combomain <- ggplot(international_basic_graph_info, aes(correct_ratio_per_question, diff_male_female)) +
  geom_point(aes(color = country), size = 3, alpha = 1/3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = .5) +
  labs(x = 'n Students Who Correctly Answered Question / Total Students',
       y = 'Males Correct Ratio - Females Correct Ratio',
       color = NULL)

combolegend <- get_legend(combomain)

combomain <- combomain +
  theme(legend.position = 'none')

blank_graph <-  list(
  labs(x = NULL, y = NULL, color = NULL),
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank()) 
)
  

combotop = ggplot(international_basic_graph_info, aes(correct_ratio_per_question, color = country, fill = country)) +
  geom_density(alpha = .25) + 
  labs(title = 'Gender and Correct Ratio Comparison of All Countries',
       subtitle = 'Scatter and Density Combination Plot') +
  blank_graph

comboright = ggplot(international_basic_graph_info, aes(diff_male_female, color = country, fill = country)) +
  geom_density(alpha = .25) +
  coord_flip() +
  blank_graph

library(gridExtra)

grid.arrange(combotop, combolegend, combomain, comboright, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

detach(package:gridExtra)

ggplot(international_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = content_domain)) +
  scale_y_continuous(limits = c(0,1 )) +
  facet_wrap(~country) +
  labs(x = 'Questions Ranked From Easiest to Hardest According to Country',
       y = 'n Students Who Correctly Answered Question / Total Students', 
       fill = 'Content Domain',
       title = 'Content Domain Comparison of Countries', 
       subtitle = 'Barplot')


ggplot(international_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = cognitive_domain)) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~country) +
  labs(x = 'Questions Ranked From Easiest to Hardest According to Country',
       y = 'n Students Who Correctly Answered Question / Total Students', 
       fill = 'Cognitive Domain',
       title = 'Cognitive Domain Comparison of Countries', 
       subtitle = 'Barplot')

international_domain <- international_basic_graph_info %>%
  group_by(question, cognitive_domain, content_domain) %>%
  nest() %>%
  gather(type, domain, -c(question, data)) %>% 
  unnest()

ggplot(international_domain, aes(domain, diff_male_female)) +
  geom_boxplot(aes(color = country)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Domain',
       y = 'Difference Between Correction Rate (Male - Female)',
       color = 'Country',
       title = 'TIMSS: Comparison of Male and Female Respone Per Domain',
       subtitle = 'Boxplot')

ggplot(international_basic_graph_info, aes(correct_ratio_per_question, diff_male_female)) +
  geom_point(aes(color = content_domain, shape = cognitive_domain), position = 'jitter') +
  facet_wrap(~country) +
  labs(x = 'n Students Who Correctly Answered Question / Total Students',
       y = 'Males Correct Ratio - Females Correct Ratio',
       color = 'Gender',
       title = 'Gender Comparison Within Countries',
       subtitle = 'Scatter-Text Plot')

network_graph_setup <- function(basic_graph, str_abr, link1, link2, from_top){
  basic_graph %>%
    select(-data) %>%
    mutate(id = str_c(str_abr, '_', question)) %>%
    mutate(link1 = link1) %>%
    mutate(link2 = link2) %>%
    mutate(order = from_top)
}

chl_network <- network_graph_setup(chl_basic_graph_info, 'chl', 'chl_ltu', 'bottom', 4)

ltu_network <- network_graph_setup(ltu_basic_graph_info, 'ltu', 'ltu_usa', 'chl_ltu', 3)
usa_network <- network_graph_setup(usa_basic_graph_info, 'usa', 'usa_kor', 'ltu_usa', 2)
kor_network <- network_graph_setup(kor_basic_graph_info, 'kor', 'top', 'usa_kor', 1)

international_nodes <- rbind(chl_network, ltu_network, usa_network, kor_network) %>%
  group_by(country, link1, link2) %>%
  nest() %>%
  gather(link_type, link, -c(data, country)) %>%
  unnest() %>%
  mutate(link = str_c(question, '_', link)) %>%
  arrange(order, question_rank) %>%
  mutate(country = factor(country, levels = unique(country), ordered = T)) %>%
  mutate(FIELD_LABL = str_to_title(FIELD_LABL)) %>%
  mutate(content_domain = str_to_title(content_domain)) %>%
  mutate(cognitive_domain = str_to_title(cognitive_domain))

ggplot(international_nodes, aes(question_rank, country, color = correct_ratio_per_question)) +
  geom_point(size = 2) +
  scale_color_gradient2(high = 'forestgreen', mid = 'yellow', low = 'saddlebrown', midpoint = .5) +
  geom_line(aes(group = as.factor(link)), color = 'grey80') +
  labs(x = 'Question Rank: From easiest to hardest',
       y = 'Country',
       color = 'Question Correct Ratio',
       title = 'TIMSS: Comparison of Country Correct Response Per Question',
       subtitle = 'Bi-Partite Network Graph') 
