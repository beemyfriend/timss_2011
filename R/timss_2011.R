require(tidyverse)
require(stringr)

#=======================================================================#
#============================== SETUP ==================================#
#=======================================================================#

require(foreign)
require(haven)

#' 'B' = 8th grade
#' 'C' = School, 'T' = Teacher, 'S' = Student
#' 'A' = Achievment Book, 'G' = Gen Background Questions, 'M' = Math Background,
#' 'R' = Free-Response scoring reliability, 'S' = Science background, 'T' = Student-Teacher link
#'  next three digits is country code 'chl'=152
#'  'M1' = 95 files, 'M2' = 99 files

chile_achievement_11 <- read_sas('bsachlm5.sas7bdat')
jap_achievement_11 <- read_sas('bsajpnm5.sas7bdat')
us_achievement_11 <- read_spss('bsausam5.sav')
isr_achievement_11 <- read_spss('bsaisrm5.sav')
kor_achievement_11 <- read_spss('bsakorm5.sav')
  
chile_school_11 <- read_sas('timss/sas_timss11/bcgchlm5.sas7bdat')
us_school_11 <- read_spss('D://TIMSS/spss_11_chileus/TIMSS/Grade 08/Y2011/Data/SPSS/bcgusam5.sav')

chile_general_student_11 <- read_sas('timss/sas_timss11/bsgchlm5.sas7bdat')
us_general_student_11 <- read_spss('D://TIMSS/spss_11_chileus/TIMSS/Grade 08/Y2011/Data/SPSS/bsgusam5.sav')


#BSDGEDUP = education of parents (1 university... 6 not applicable)
#BSBG13E = hurt in school (1 at least once a week... 4 never)
#BSBG07 = how far in edu do you expect to go (1 FINISH <ISCED 2> ...  6   BEYOND <ISCED 5A, FIRST DEGREE> , 7 IDK )
#BG05E = has internet (1 yes 2 no)
chile_general_student_11_filtered <- read_sas('timss/sas_timss11/bsgchlm5.sas7bdat') %>%
  dplyr::select(IDSTUD, BSDGEDUP, BSBG13E, BSBG07, BSBG05E) %>%
  mutate_each_(funs(factor), names(.))

achievement_codebook_11 <- read_tsv('bsatmsm5.csv') %>%
  select(FIELD_NAME, FIELD_LABL, MEAS_CLASS, COMMENT1) %>%
  mutate(MEAS_CLASS = str_replace(MEAS_CLASS, '^M', '')) %>%
  separate(COMMENT1, into = c('content_domain', 'cognitive_domain'), sep = '\\\\') %>%
  mutate(cognitive_domain = str_extract(cognitive_domain, '\\w+')) %>%
  mutate(question_type = sapply(MEAS_CLASS, function(x){
    if(str_detect(x, '\\d')){
      'Multiple Choice'
    #there are so few derived questions that it messes with the models. They're essentially free responses anyway
    #} else if (str_detect(x, 'DPC_D')) {
    #  'Derived'
    } else if (str_detect(x, 'SA|DPC_D')){
      'Free Response'
    } else {
      'Other'
    }
  }))

detach(package:foreign)
detach(package:haven)

clean_math <- function(df, cdbook){
  df %>%
    #^M grabs all the math questions
    #^BSM grabs all the benchmark scores
    select_(.dots = names(.)[str_detect(names(.), '^M|IDSTUD|IDBOOK|IDSCHOOL|ITSEX|^BSM')]) %>%
    gather_('question', 'answer', names(.)[str_detect(names(.), '^M')]) %>% 
    #about 30 students who responded NA to everything will disappear
    #by getting rid of these students I end up having the same number of students per question as the almanac
    filter(!is.na(answer)) %>%
    mutate(benchmark_math_avg = (BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05)/5) %>%
    mutate(benchmark_math_avg_value = lapply(benchmark_math_avg, function(x){
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
    }) %>%
      unlist()) %>%
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
    unnest() %>%
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
    ) %>%
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
    mutate(
      question_country_difficulty = sapply(correct_ratio_per_question, function(x){
        if(x > .9){
          'Really Easy'
        } else if (x > .66){
          'Easy'
        } else if (x > .33){
          'Normal'
        } else if (x > .1){
          'Difficult'
        } else {
          'Really Difficult'
        }
      })
    ) %>%
    unnest()
  }


chile_timss_math_11 <- clean_math(chile_achievement_11, achievement_codebook_11)
isr_timss_math_11 <- clean_math(isr_achievement_11, achievement_codebook_11)
jap_timss_math_11 <- clean_math(jap_achievement_11, achievement_codebook_11)
kor_timss_math_11 <- clean_math(kor_achievement_11, achievement_codebook_11)
us_timss_math_11 <- clean_math(us_achievement_11, achievement_codebook_11)

#=====================================================================================#
#========================= Basic Graphs ==============================================#
#=====================================================================================#

require(gridExtra)
#require(igraph)

#-------------------------------------------------------------------------------------#
#------------------------- Focus on Questions ----------------------------------------#
#-------------------------------------------------------------------------------------#

basic_graph_setup <- function(x){
  x %>%
    group_by(question, students_per_question, correct_ratio_per_question, correct_ratio_per_question_female,
             correct_ratio_per_question_male, FIELD_LABL, content_domain, cognitive_domain, question_type) %>%
    nest() %>%
    arrange(desc(correct_ratio_per_question)) %>%
    mutate(question_rank = 1:nrow(.)) %>%
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

chile_basic_graph_info <- basic_graph_setup(chile_timss_math_11)
isr_basic_graph_info <- basic_graph_setup(isr_timss_math_11)
jap_basic_graph_info <- basic_graph_setup(jap_timss_math_11)
kor_basic_graph_info <- basic_graph_setup(kor_timss_math_11)
us_basic_graph_info <- basic_graph_setup(us_timss_math_11)

#us has one more question than chile...M032331
us_basic_graph_info$question[!(us_basic_graph_info$question %in% chile_basic_graph_info$question)]

chile_male_female_graph <- ggplot(chile_basic_graph_info, aes(question_rank, diff_male_female)) +
  geom_col(aes(fill = dominant_gender)) +
  ggtitle('Chile 2011: Males vs. Females') +
  scale_y_continuous(limits = c(-.15, .15)) +
  xlab('Questions: Most correctly answered to least correctly answered') +
  ylab('Males correct ratio - Females correct ratio') +
  labs(fill = 'Gender')

us_male_female_graph <- ggplot(us_basic_graph_info, aes(question_rank, diff_male_female)) +
  geom_col(aes(fill= dominant_gender)) +
  ggtitle('USA 2011: Males vs. Females') +
  scale_y_continuous(limits = c(-.15, .15)) +
  xlab('Questions: Most correctly answered to least correctly answered') +
  ylab('Males correct ratio - Females correct ratio') +
  labs(fill = 'Gender')

grid.arrange(chile_male_female_graph, us_male_female_graph)
rm(chile_male_female_graph)
rm(us_male_female_graph)

chile_content_domain_graph <- ggplot(chile_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = content_domain)) +
  ggtitle('Chile 2011: Content Domain') + 
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Questions: Most correctly answered to least correctly answered',
       y = 'Correct Ratio',
       fill = 'Content Domain')

chile_cognitive_domain_graph <- ggplot(chile_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = cognitive_domain)) +
  ggtitle('Chile 2011: Cognitive Domain') +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = 'Questions: Most correctly answered to least correctly answered',
       y = 'Correct Ratio',
       fill = 'Cognitive Domain')

us_content_domain_graph <- ggplot(us_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = content_domain)) +
  ggtitle('USA 2011: Content Domain') +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = 'Questions: Most correctly answered to least correctly answered',
       y = 'Correct Ratio',
       fill = 'Content Domain')

us_cognitive_domain_graph <- ggplot(us_basic_graph_info, aes(question_rank, correct_ratio_per_question)) +
  geom_col(aes(fill = cognitive_domain)) + 
  ggtitle('USA 2011: Cognitive Domain') +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = 'Questions: Most correctly answered to least correctly answered',
       y = 'Correct Ratio',
       fill = 'Cognitive Domain')

grid.arrange(chile_content_domain_graph, us_content_domain_graph, chile_cognitive_domain_graph, us_cognitive_domain_graph)
rm(chile_content_domain_graph)
rm(chile_cognitive_domain_graph)
rm(us_content_domain_graph)
rm(us_cognitive_domain_graph)

network_graph_setup <- function(basic_graph, str_abr, str_country, link1, link2){
  basic_graph %>%
    select(-data) %>%
    mutate(id = str_c(str_abr, '_', question)) %>%
    mutate(country = str_country) %>%
    mutate(link1 = link1) %>%
    mutate(link2 = link2)
}


chile_network <- network_graph_setup(chile_basic_graph_info, 'chl', 'Chile', 'chl_isr', 'bottom')
isr_network <- network_graph_setup(isr_basic_graph_info, 'isr', 'Israel', 'isr_jap', 'chl_isr')
jap_network <- network_graph_setup(jap_basic_graph_info, 'jap', 'Japan', 'jap_kor', 'isr_jap')
kor_network <- network_graph_setup(kor_basic_graph_info, 'kor', 'Korea', 'kor_usa', 'jap_kor')
us_network <- network_graph_setup(us_basic_graph_info, 'usa', 'USA', 'top', 'kor_usa')

international_nodes <- rbind(chile_network, isr_network, jap_network, kor_network, us_network) %>%
  group_by(country, link1, link2) %>%
  nest() %>%
  gather(link_type, link, -c(data, country)) %>%
  unnest() %>%
  mutate(link = str_c(question, '_', link)) %>%
  arrange(country, question_rank) %>%
  mutate(FIELD_LABL = str_to_title(FIELD_LABL)) %>%
  mutate(content_domain = str_to_title(content_domain)) %>%
  mutate(cognitive_domain = str_to_title(cognitive_domain))
  
d3_links_setup <- function(network1, network2){
  country1 = network1$country[1]
  country2 = network2$country[1]
  
  rbind(network1, network2) %>%
    #I think only chile doesn't have 'M032331', but I'll remove the link from everyone just in case
    filter(question != 'M032331') %>%
    select(question, country, question_rank) %>%
    group_by(question) %>%
    nest() %>%
    mutate(
      x1 = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>%
          filter(.$country == country1 ) %>%
          #should be the first row of question_rank 
          .[1,2]}) %>%
        unlist()) %>%
    mutate(
      x2 = lapply(seq_along(.$data), function(i){
        .$data[[i]] %>%
          filter(country == country2) %>%
          .[1,2]}) %>%
        unlist()) %>%
    mutate( y1 = country1) %>%
    mutate( y2 = country2) %>%
    select(-data)
}

chile_jap_d3_links <- d3_links_setup(chile_network, jap_network)  
kor_us_d3_links <- d3_links_setup(kor_network, us_network)
jap_kor_d3_links <- d3_links_setup(jap_network, kor_network)

chl_jap_kor_us_nodes_json <- jsonlite::toJSON(international_nodes %>% filter(country != 'Israel'))
chl_jap_json <- jsonlite::toJSON(chile_jap_d3_links)
jap_kor_json <- jsonlite::toJSON(jap_kor_d3_links)
kor_us_json <- jsonlite::toJSON(kor_us_d3_links)
d3_timss_json <- str_c('[', chl_jap_kor_us_nodes_json, ',', chl_jap_json, ',', jap_kor_json, ',', kor_us_json, ']')
write(d3_timss_json, 'd3_timss.json')

jpeg(filename = 'static_correct_response_ratio_network.jpg', height = 700, width = 1000)

ggplot(international_nodes, aes(question_rank, country, color = correct_ratio_per_question)) +
  geom_point(size = 2) +
  scale_color_distiller(palette = "RdYlBu") +
  geom_line(aes(group = as.factor(link)), color = 'grey80') +
  labs(x = 'Question Rank: From easiest to hardest',
       y = 'Country',
       color = 'Question Correct Ratio',
       title = 'TIMSS: Comparison of Correct Response Per Question') 
  
dev.off()

chile_network <- chile_basic_graph_info %>%
  select(-data) %>%
  mutate(id = str_c('chl_', question)) %>%
  mutate(country = 'Chile')

us_network <- us_basic_graph_info %>%
  select(-data) %>%
  mutate(id = str_c('usa_', question)) %>%
  mutate(country = 'USA')

chile_us_nodes <- rbind(chile_network, us_network) %>%
  arrange(country, question_rank)

chile_us_links_d3 <- chile_us_nodes %>%
  filter(question != 'M032331') %>%
  select(question, country, question_rank) %>%
  group_by(question) %>%
  nest() %>%
  mutate(y1 = 'Chile') %>%
  mutate(y2 = 'USA') %>%
  mutate(
    x1 = lapply(seq_along(.$data), function(i){
      .$data[[i]] %>% 
        filter(country == 'Chile') %>%
        .[1,2]}) %>%
      unlist()
    ) %>%
  mutate(
    x2 = lapply(seq_along(.$data), function(i){
      .$data[[i]] %>% 
        filter(country == 'USA') %>%
        .[1,2]}) %>%
      unlist()
  ) %>%
  select(-data)

chile_us_links <- chile_us_nodes %>%
  filter(country == 'Chile') %>%
  mutate(to = str_replace_all(id, 'chl', 'usa')) %>%
  rename(from = id) %>%
  select(from, to)

ggplot(chile_us_nodes, aes(question_rank, country, color = correct_ratio_per_question)) +
  geom_point(size = 2) +
  scale_color_distiller(palette = "RdYlBu") +
  geom_line(aes(group = as.factor(question)), color = 'grey80')

#chile_us_network <- graph_from_data_frame(d = chile_us_links, vertices = chile_us_nodes, directed = T)
#chile_us_network <- graph.data.frame(chile_us_links)
#V(chile_us_network)$type <- V(chile_us_network)$name %>% str_detect('chl')
#chile_us_network
#bipartite.projection(chile_us_network)
#plot(chile_us_network, vertex.label = NA, vertex.size = 5, layout = layout.bipartite,
#     edge.width = 1, edge.arrow.mode = 0 )
#

rm(chile_basic_graph_info)
rm(us_basic_graph_info)
detach(package:gridExtra)
#detach(package:igraph)


#-------------------------------------------------------------------------------------#
#-------------------------- Focus on the Students ------------------------------------#
#-------------------------------------------------------------------------------------#



#=====================================================================================#
#================================ MODELS =============================================#
#=====================================================================================#

timss_simple_student_model_11 <- function(x){
  x %>% 
    group_by(IDSTUD, IDSCHOOL, ITSEX, benchmark_math_avg_value) %>% 
    nest() %>%
    select(-data) %>%
    mutate(IDSTUD = as.factor(IDSTUD)) %>%
    mutate(IDSCHOOL = as.factor(IDSCHOOL)) %>%
    mutate(ITSEX = as.factor(ITSEX)) %>%
    mutate(benchmark_math_avg_value = as.factor(benchmark_math_avg_value)) 
  }

chile_simple_student <- timss_simple_student_model_11(chile_timss_math_11)
us_simple_student <- timss_simple_student_model_11(us_timss_math_11)

  #I want to do some text analysis, but I'm afraid that the labels are too specific to the question
  #I need to generalize the the text...this will take a while to get right
  

find_math_concepts <- function(x){
  x %>%
    str_replace('ANGLE A', 'ANGLE X') %>%
    str_replace(' A |5th', ' ')
  
  y = ''
  
  if(str_detect(x, 'ABC|AB|XY|BOC|PQR| \\w | \\d+\\w| \\d*\\w+/\\d+|A\\+B|A-B|4\\(3+x\\)| \\w$')) {
    y = paste(y, 'algabraic')
  }
  if(str_detect(x, '\\d+\\.\\d+|DECIMAL')){
    y = paste(y, 'decimal')
  }
  if(str_detect(x, '\\d+\\w*/\\d*\\w*|FRACTION|FIFTH|HALF')){
    y = paste(y, 'fraction')
  }
  if(str_detect(x, '\\+|-|/|PLUS')){
    y = paste(y, 'arithmetic')
  }
  if(str_detect(x, 'MORE|LESS|THAN|SHORTER|EQUI|LOW|LONGEST|REDUCED')){
    y = paste(y, 'comparison')
  }
  if(str_detect(x, 'YEAR|SPEED|TIME|MINUTES')){
    y = paste(y, 'time')
  }
  if(str_detect(x, 'SHAPE|CIRCLE|SQUARE|LINE|TRIANGLE|AREA|CUBES|PERIMETER|OCTAGON|FIG|PENTAGON')){
    y = paste(y, 'shape')
  }
  if(str_detect(x, 'AREA')){
    y = paste(y, 'area')
  }
  if(str_detect(x, 'SYMMETRY')){
    y = paste(y, 'symmetry')
  }
  if(str_detect(x, '\\d+ |NUMBER')){
    y = paste(y, 'number')
  }
  if(str_detect(x, 'CHART|GRAPH|SLOPE')){
    y = paste(y, 'graph')
  }
  if(str_detect(x, 'LIKELY|PROBABILITY')){
    y = paste(y, 'probability')
  }
  if(str_detect(x, 'PERCENT')){
    y = paste(y, 'percent')
  }
  if(str_detect(x, 'PATTERN')){
    y = paste(y, 'pattern')
  }
  if(str_detect(x, ' ANGLE')){
    y = paste(y, 'angle')
  }
  if(y == ''){
    y = 'none'
  }
  y
}

timss_simple_question_model_11 <- function(x){
  x %>%
    select(question, FIELD_LABL, content_domain, cognitive_domain,
           question_country_difficulty, question_type) %>%
    distinct() %>%
    group_by(question) %>%
    summarise_all(as.factor)
}

#not as helpful as i wanted it to be...
timss_simple_question_model_w_text_11 <- function(x){
  x %>%
    select(question, FIELD_LABL, content_domain, cognitive_domain,
           question_country_difficulty, question_type) %>%
    distinct() %>%
    mutate(question = question %>% as.factor()) %>%
    mutate(content_domain = content_domain %>% as.factor()) %>%
    mutate(cognitive_domain = cognitive_domain %>% as.factor()) %>%
    mutate(question_country_difficulty = question_country_difficulty %>% as.factor()) %>%
    mutate(question_type = question_type %>% as.factor()) %>%
    mutate(question_topics = sapply(FIELD_LABL, find_math_concepts) %>% as.character()) %>%
    unnest_tokens(topics, question_topics) %>%
    mutate(count = 1) %>%
    spread(topics, count, fill = 0) %>%
    group_by(question) %>%
    summarise_all(as.factor)
}


chile_simple_question <- timss_simple_question_model_11(chile_timss_math_11)
us_simple_question <- timss_simple_question_model_11(us_timss_math_11)

timss_all_model_11 <- function(timss_math, timss_general_student, timss_school){
  timss_math %>% 
  group_by(IDSTUD, IDSCHOOL, ITSEX, benchmark_math_avg_value) %>%
  nest() %>%
  select(-data) %>%
  left_join(timss_general_student %>% 
              select(-c(IDSCHOOL, ITSEX)),
            by = 'IDSTUD') %>%
  left_join(timss_school %>%
              select(-c(IDGRADE, IDPOP, IDCNTRY, IDGRADER, IDSTRATE, IDSTRATI, WGTADJ1, WGTFAC1, DPCDATE)),
            by = 'IDSCHOOL') %>%
  mutate(IDSCHOOL = as.factor(IDSCHOOL)) %>%
  mutate(ITSEX = as.factor(ITSEX)) %>%
  mutate(benchmark_math_avg_value = as.factor(benchmark_math_avg_value)) -> x
  
  #there are too many columns with just NA values. These might cause an error with the tree model
  naCols <- vector(length = ncol(x))
  for(i in 1:ncol(x)){
    naCols[i]<- sum(is.na(x[,i]))
  }
  
  toomanynas <- naCols > nrow(x)/2
  
  x <- x[, !toomanynas]
  
  nothelpful <- x %>%
    names() %>%
    str_detect('^BSM|^BSS|WGT|^JK|^BSD|STRAT|BSBGSCM|LOW|SYSTEM|IDPOP|COMM|GRADER|ITDATE|ITADMIN|DPC')
  
  x[,!nothelpful]
}

chile_model_all <- timss_all_model_11(chile_timss_math_11, chile_general_student_11, chile_school_11)
us_model_all <- timss_all_model_11(us_timss_math_11, us_general_student_11, us_school_11)

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

timss_student_nb(chile_simple_student)
#nb for chile_2011 is 57.32 ; Random guessing is 20 ; choosing only 'Below Low International' is 37.74
(0 + 1679 + 163 + 526 + 961)/nrow(chile_model_simple)

timss_student_nb(us_simple_student)
#nb for us_2011 is 57.38 ; random guessing is 20 ; choosing only 'Intermediate International' is 37.80
(265 + 209 + 1439 + 2667 + 1411)/nrow(us_model_simple)

#Let's just see what IDSCHOOL performs without ITSEX
#It turns out that using gender is helpful, but doesn't make a huge difference
timss_student_nb(chile_simple_student %>% dplyr::select(-ITSEX))
(0 + 1650 + 163 + 532 + 974)/nrow(chile_model_simple)
timss_student_nb(us_simple_student %>% dplyr::select(-ITSEX))
(276 + 225 + 1474 + 2623 + 1387)/nrow(us_model_simple)

#let's try the first model (just ITSEX and IDSCHOOL), but combine andvanced with high and combine low with below low
adjust_student_benchmark <- function(x){
  sapply(as.character(x), function(y){
  if(y == 'Below Low International'){
    'Low International'
  } else if (y == 'Advanced International') {
    'High International'
  } else {
    y
  }})}

chile_simple_student_adjusted <- chile_simple_student %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value) %>%
           as.factor())

us_simple_student_adjusted <- us_simple_student %>%
  mutate(benchmark_math_avg_value = adjust_student_benchmark(benchmark_math_avg_value) %>%
           as.factor())

#nb model is 78.03 ; guessing is about 33.33 ; choosing just 'Low International' is 70.27
timss_student_nb(chile_simple_student_adjusted)
(219 + 452 + 3859)/nrow(chile_simple_student_adjusted)

#nb model is 66.93 ; guess is about 33.33 ; choosing just 'Intermediate International' is 37.80
timss_student_nb(us_simple_student_adjusted)
(2204 + 2498 + 2286)/nrow(us_simple_student_adjusted)

#------------------------------ Question Level ------------------------------------------#

timss_question_nb <- function(model){
  x <- model %>%
    dplyr::select(-c(question, FIELD_LABL, question_country_difficulty))
  
  truth <- model$question_country_difficulty %>%
    as.factor()
  
  model <- train(x, truth, 'nb', trControl = trainControl(method = 'cv', number = 10))
  
  prediction <- predict(model$finalModel, x)
  ed_guess <- prediction$class
  table(ed_guess, truth)
}

#55.50 for nb chile ; 20 for guess ; 44.04 for choosing just normal (really easy isn't introduced in dataset, but I'm still factoring it)
timss_question_nb(chile_simple_question)
(48 + 0 + 68 + 5)/nrow(chile_simple_question)

#60.27 for nb us ; 20 for guess ; 55.71 for choosing just normal
timss_question_nb(us_simple_question)
(17 + 20 + 95 + 0)/nrow(us_simple_question)

adjust_question_difficulty <- function(x){
  sapply(as.character(x), function(y){
    if(y == 'Really Easy'){
      'Easy'
    } else if (y == 'Really Difficult') {
      'Difficult'
    } else {
      y
    }})}

chile_simple_question_adjusted <- chile_simple_question %>%
  mutate(question_country_difficulty = adjust_question_difficulty(question_country_difficulty) %>%
           as.factor())

us_simple_question_adjusted <- us_simple_question %>%
  mutate(question_country_difficulty = adjust_question_difficulty(question_country_difficulty) %>%
           as.factor()) 

#67.89 is nb ; 33.33 is random guess ; 53.21 if just choose 'Difficult'
timss_question_nb(chile_simple_question_adjusted)
(82+0+66)/nrow(chile_simple_question_adjusted)

#60.73 is nb ; 33.33 is random guess ; 51.14 if just choose 'Normal'
timss_question_nb(us_simple_question_adjusted)
(18+20+95)/nrow(us_simple_question_adjusted)

#caret is dependent on klaR and MASS
#MASS has a select() function that masks dplyr's select() function
#let's get rid of it...
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
train_student_us <- sample(nrow(us_simple_student), nrow(us_simple_student)/2)

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

#------------------- Question Level ---------------------------------------------#

set.seed(1)

timss_question_svm <- function(model){
  train <- sample(nrow(model), nrow(model)/2)
  #factors with more than 2 levels like school need to be turned into a yes/no matrix
  model <- model %>%
    select(-FIELD_LABL) %>%
    mutate(count = 1) %>%
    spread(content_domain, count, fill = 0) %>%
    mutate(count = 1) %>%
    spread(cognitive_domain, count, fill = 0) %>%
    mutate(count = 1) %>%
    spread(question_type, count, fill = 0)
  
  #factors with more than 2 levels like school need to be turned into a yes/no matrix
  svm_train <- model[train,]
  
  svm_test <- model[-train,]
  
  out = svm(question_country_difficulty~.-question, data = svm_train, kernel='linear', cost = 10)

  pred.te = predict(out, newdata = svm_test)
  table(pred.te, svm_test$question_country_difficulty)
}

#50.46 for chile ; 66 for us
timss_question_svm(chile_simple_question)
timss_question_svm(us_simple_question)

timss_question_svm(chile_simple_question_adjusted)
timss_question_svm(us_simple_question_adjusted)

#for this dataset (only student achievement) svm performs worse than nb and is computationally more demanding than nb
detach(package:e1071)



#trees cannot accept factors with more than 32 levels
#will need to wait for some kind of key that connects school id with geolocation and then rank location from poor to wealthy
#will need to clean the variables of the timss_model_all data.frames to avoid redundant information, constants, and nas
#I'm going to table this...

#--------------------------------------------------------------------------------#
#---------------------------------- Trees ---------------------------------------#
#--------------------------------------------------------------------------------#

require(tree)
require(randomForest)

#require(Amelia)
#missmap(timss_all_model_11)
#trees can't have any factors with more than 32 levels()
#needs to be even numbered rows for future comparison
set.seed(1)
tree_model = timss_all_model_11 %>%
  select(-IDSCHOOL) %>%
  .[sample(1:(nrow(.)-1)),]


tree.timss = tree(benchmark_math_avg_value~.-IDSTUD, data = tree_model)
summary(tree.timss)
plot(tree.timss)
text(tree.timss, pretty=0)

train <- sample(1:nrow(tree_model), nrow(tree_model)/2)
timss_tree_test <- tree_model[-train,]
test_benchmark <- tree_model$benchmark_math_avg_value[train]
tree.timss <- tree(benchmark_math_avg_value~.-IDSTUD, data = tree_model, subset = train)
tree.pred = predict(tree.timss, timss_tree_test, type = 'class')
#.309 correct rate. Just guessing 'Below low International' for everything would have given me .374!!!
#guesssing is better than this tree...
table(tree.pred, test_benchmark)

cv.timss <- cv.tree(tree.timss, FUN = prune.misclass)
#an 11 node tree seems optimal
cv.timss
prune.timss = prune.misclass(tree.timss, best = 11)
tree.pred = predict(prune.timss, timss_tree_test, type = 'class')
#worst still...
table(tree.pred, test_benchmark)

set.seed(2)
bag.timss <- randomForest(benchmark_math_avg_value~.-IDSTUD, data = tree_model, subset = train, mtry=13, importance=T)
