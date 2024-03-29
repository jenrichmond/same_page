#install packages
#install.packages("ggplot2")
#install.packages("grid")
#install.packages("cowplot")
#install.packages("httr")
#install.packages("extrafont")
#install.packages("tidyverse")
#install.packages("janitor")

#load packages
library(ggplot2)
library(grid)
library(cowplot)
library(httr)
library(extrafont)
library(tidyverse)
library(janitor)
library(here)

# load data
kidwell <- read_csv(here("data_files", "master_dataset.csv")) %>%
  clean_names()


# remove experiments that are not empirical
empirical <- kidwell %>%
  filter(number_of_experiments > 0)


# cleaning names of articles and dates
empirical<- empirical %>%
  mutate(journal = case_when(
    str_detect(article_id_number,'CPS') ~ "Clinical Psychological Science",
    str_detect(article_id_number,'DP') ~ "Developmental Psychology",
    str_detect(article_id_number,'JEPLMC') ~ "Journal of Experimental Psychology: Learning, Memory, and Cognition",
    str_detect(article_id_number,'JPSP') ~ "Journal of Personality and Social Psychology",
    str_detect(article_id_number,'PS') ~ "Psychological Science",  TRUE ~ "other")) %>%
  mutate(year = case_when(str_detect(article_id_number,'2012') ~ "2012",
                          str_detect(article_id_number,'2013') ~ "2013",
                          str_detect(article_id_number,'2014') ~ "2014",
                          str_detect(article_id_number,'2015') ~ "2015",
                          TRUE ~ "other")) %>%
  relocate(journal, .after = article_id_number) %>%
  relocate(year, .before = article_id_number) %>%
  separate(article_id_number, into = c("date", "journal_code"), sep = "\\s", remove = FALSE)

unique(empirical$journal)
unique(empirical$year)

# First let add a columns/variable separating the non-badge journals from the badge journals (i.e. PS)

empirical_clean <- empirical %>%
  mutate(journal = case_when(journal_code == "PS" ~ "PS",
                             journal_code %in% c("DP","CPS","JEPLMC","JPSP") ~ "Non_PS",
                             TRUE ~ "other"))

# Now let's separate the badge journal articles from pre-badge to post-badge

empirical_prepost <- empirical_clean %>%
  mutate(pre_or_post_badge = case_when(journal == "PS" & year %in% c("2014","2015") ~ "Post_badge",
                                       journal == "PS" & year %in% c("2012","2013") ~ "Pre_badge", 
                                       TRUE ~ "other"))

# Now let's separate the post-badge PS articles which earned a data badge to those that didn't
empirical_earned <- empirical_prepost %>%
  mutate(badge_earned = case_when(pre_or_post_badge =='Post_badge' & did_the_article_receive_a_badge_for_open_data == "Yes" ~ "yes",
         pre_or_post_badge == 'Post_badge' & did_the_article_receive_a_badge_for_open_data == "No" ~ "no",
         TRUE ~ "other"))

# We've got all the variables we need, now we need to count up the number of responses in each variable/column

# count how many papers were PS vs non_PS (PS = 838, non = 1640)
empirical_earned %>%
  count(journal) 

# count how many papers were pre (n = 307) vs post badge (N= 320) (filtering for just PS here to avoid a lot of other)

empirical_earned %>%
  filter(journal == "PS") %>%
  count(pre_or_post_badge)


# count how many in PS, post badge earned the basge vs. not (are there really only 46?) ## CR Yes! I just checked the paper and only 46 of the 73 articles which reported data availability actually earned a badge!
empirical_earned %>%
  filter(journal == "PS") %>%
  filter(pre_or_post_badge == "Post_badge") %>%
  count(badge_earned)

## JENNY UP TO HERE





# Now let's calculate the number of articles that were reportedly available for each group
reportedly_available_counts <- empirical_clean_count %>%
  transmute(journals_without_badges_reportedly_available = case_when(journal = "Non_PS", data_statement_indicates_that_data_are == "Available" ~ "reportedly_available",
  TRUE ~ "other")) %>%
  transmute(PS_before_badges_reportedly_available = case_when(pre_or_post_badge = "Pre_badge", data_statement_indicates_that_data_are == "Available" ~ "reportedly_available",
                                                                TRUE ~ "other")) %>%
  transmute(PS_badges_not_earned_reportedly_available = case_when(badge_earned = "no", data_statement_indicates_that_data_are == "Available" ~ "reportedly_available",
                                                            TRUE ~ "other")) %>%
  transmute(PS_badges_earned_reportedly_available = case_when(badge_earned = "yes", data_statement_indicates_that_data_are == "Available" ~ "reportedly_available",
                                                                TRUE ~ "other"))
# continue for all other x-axis variables (i.e. actually available, correct data, usable, data, complete data) and then count them up

----------
  
  
  
  
  
  
  
# First lets separate the non-badge journals from the badge journals (i.e. PS)


nobadge <- empirical %>%
  filter(journal_code != "PS")
# this '!=' code means 'not including'


# PS before badges (pre2014)

PS_pre_badges <- empirical %>%
  filter(journal_code =="PS") %>%
  filter(year %in% c("2012", "2013"))

# Psych Science after badges (post2014) not earned

PSnotearned <- empirical %>%
  filter(journal_code == "PS") %>%
  filter(did_the_article_receive_a_badge_for_open_data == "No") %>%
  filter(year %in% c("2014", "2015"))


# Psych Science after badges (post2014) EARNED

PSearned <- empirical %>%
  filter(journal_code == "PS") %>%
  filter(did_the_article_receive_a_badge_for_open_data == "Yes") %>%
  filter(year %in% c("2014", "2015"))


#Now lets focus on the x axis of the graph; we want to distinguish the counts of 5 categories:
# reportedly available = "data_statement_indicates_that_data_are"
# actually available = "are_the_data_located_at_a_working_page"
# correct data = "does_the_data_correspond_to_what_is_being_reported_in_the_article"
# usable data = "are_the_data_understandable_and_usable_after_brief_review"
#complete data = "are_the_data_complete"

# And we need to do this for each badge grouped papers
# We are also calculating percentages for these counts against reportedly available count (y-axis)

# no badge all reportedly available: "data_statement_indicates_that_data_are"
count_percentage_nobadge_reportedly_available <- nobadge %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  count() %>%
  rename(total_nobadge_reportedly_available = n) %>%
  mutate(percent_nobadge_reportedly_available = (total_nobadge_reportedly_available/total_nobadge_reportedly_available)*100)

# is the denominator and the numerator the same here?

# Splitting percentages from count
count_nobadge_reportedly_available <- count_percentage_nobadge_reportedly_available[1]
percentage_nobadge_reportedly_available <- count_percentage_nobadge_reportedly_available[2]

# PS before badge reportedly available: "data_statement_indicates_that_data_are"

count_percentage_PS_pre_badges_reportedly_available <- PS_pre_badges %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  count() %>%
  rename(total_PS_pre_badges_reportedly_available = n) %>%
  mutate(percent_PS_pre_badges_reportedly_available = (total_PS_pre_badges_reportedly_available/total_PS_pre_badges_reportedly_available)*100)

# Splitting percentages from count
count_PS_pre_badges_reportedly_available <- count_percentage_PS_pre_badges_reportedly_available[1]
percentage_PS_pre_badges_reportedly_available <- count_percentage_PS_pre_badges_reportedly_available[2]

# PS badge NOT earned reportedly available: "data_statement_indicates_that_data_are"

count_percentage_PSnotearned_reportedly_available <- PSnotearned %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  count() %>%
  rename(total_PSnotearned_reportedly_available = n) %>%
  mutate(percent_PSnotearned_reportedly_available = (total_PSnotearned_reportedly_available/total_PSnotearned_reportedly_available)*100)

# Splitting percentages from counts

count_PSnotearned_reportedly_available <- count_percentage_PSnotearned_reportedly_available[1]
percentage_PSnotearned_reportedly_available <- count_percentage_PSnotearned_reportedly_available[2]

# PS badge earned  reportedly available: "data_statement_indicates_that_data_are"

count_percentage_PSearned_reportedly_available <- PSearned %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  count() %>%
  rename(total_PSearned_reportedly_available = n) %>%
  mutate(percent_PSearned_reportedly_available = (total_PSearned_reportedly_available/total_PSearned_reportedly_available)*100)

# Splitting percentages from counts

count_PSearned_reportedly_available <- count_percentage_PSearned_reportedly_available[1]
percentage_PSearned_reportedly_available <- count_percentage_PSearned_reportedly_available[2]

# binding percentages and count into one df on reportedly available
reportedly_available_count <- cbind(count_nobadge_reportedly_available,
                                    count_PS_pre_badges_reportedly_available,
                                    count_PSnotearned_reportedly_available,
                                    count_PSearned_reportedly_available) %>%
  rename("nobadge_ravailable" = total_nobadge_reportedly_available,
         "PSprebadges_ravailable" = total_PS_pre_badges_reportedly_available,
         "PSnotearned_ravailable" = total_PSnotearned_reportedly_available,
         "PSearned_ravailable"  = total_PSearned_reportedly_available) %>%
  pivot_longer(1:4, names_to = "category", values_to = "count") %>%
  separate(category, c("badge_type", "real_condition"), "_")


reportedly_available_pecentage <- cbind(percentage_nobadge_reportedly_available,
                                        percentage_PS_pre_badges_reportedly_available,
                                        percentage_PSnotearned_reportedly_available,
                                        percentage_PSearned_reportedly_available) %>%
  pivot_longer(1:4, names_to = "category", values_to = "percent")

reportedly_available <- cbind(reportedly_available_count, reportedly_available_pecentage)

reportedly_available <- reportedly_available[-c(4)]


reportedly_available_done <- reportedly_available


# no badge actually available "are_the_data_located_at_the_working_page"

count_nobadge_actually_available <- nobadge %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  count() %>%
  rename(count_nobadge_actually_available = n)

# PS before badge  all actually available "are_the_data_located_at_the_working_page"

count_PS_pre_badges_actually_available <- PS_pre_badges %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  count() %>%
  rename(count_PS_pre_badges_actually_available = n)

# PS badge not earned all actually available "are_the_data_located_at_the_working_page"

count_PSnotearned_actually_available <- PSnotearned %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  count() %>%
  rename(count_PSnotearned_actually_available = n)

# PS badge earned all actually available "are_the_data_located_at_the_working_page"

count_PSearned_actually_available <- PSearned %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  count() %>%
  rename(count_PSearned_actually_available = n)

# binding all count for "are_the_data_located_at_the_working_page"

count_actually_available <- cbind(count_nobadge_actually_available,
                                   count_PS_pre_badges_actually_available,
                                   count_PSnotearned_actually_available,
                                   count_PSearned_actually_available) %>%
  rename("nobadge_savailable" = count_nobadge_actually_available,
         "PSprebadges_savailable" = count_PS_pre_badges_actually_available,
         "PSnotearned_savailable" = count_PSnotearned_actually_available,
         "PSearned_savailable"  = count_PSearned_actually_available) %>%
  pivot_longer(1:4, names_to = "category", values_to = "count") %>%
  separate(category, c("badge_type", "real_condition"), "_")


# exporting count column from reportedly available dataframe for precentage calculating
reportedly_available_totalforpercentagecalc <- reportedly_available_done %>%
  select(count) %>%
  rename("reportedly_available_total" = count)

count_actually_available[4] = reportedly_available_totalforpercentagecalc["reportedly_available_total"]

count_percentage_actually_available <- count_actually_available %>%
  mutate(percent = (count/reportedly_available_total)*100) %>%
  select(-reportedly_available_total)

actually_available_done <- count_percentage_actually_available



# no badge all correct data "does_the_data_correspond_to_what_is_reported_in_the_article"

count_nobadge_correct_data <- nobadge %>%
  filter(does_the_data_correspond_to_what_is_reported_in_the_article == "Yes") %>%
  count() %>%
  rename(count_nobadge_correct_data = n)  

# PS before badge correct data "does_the_data_correspond_to_what_is_reported_in_the_article"

count_PS_pre_badges_correct_data <- PS_pre_badges %>%
  filter(does_the_data_correspond_to_what_is_reported_in_the_article == "Yes") %>%
  count() %>%
  rename(count_PS_pre_badges_correct_data = n)  

# PS badge not earned correct data "does_the_data_correspond_to_what_is_reported_in_the_article"

count_PSnotearned_correct_data <- PSnotearned %>%
  filter(does_the_data_correspond_to_what_is_reported_in_the_article == "Yes") %>%
  count() %>%
  rename(count_PSnotearned_correct_data = n)  

# PS badge earned correct data "does_the_data_correspond_to_what_is_reported_in_the_article"

count_PSearned_correct_data <- PSearned %>%
  filter(does_the_data_correspond_to_what_is_reported_in_the_article == "Yes") %>%
  count() %>%
  rename(count_PSearned_correct_data = n)


# binding all count for "does_the_data_correspond_to_what_is_reported_in_the_article"

count_correct_data <- cbind(count_nobadge_correct_data,
                            count_PS_pre_badges_correct_data,
                            count_PSnotearned_correct_data,
                            count_PSearned_correct_data) %>%
  rename("nobadgle_correctdata" = count_nobadge_correct_data,
         "PS_pre_badges_correctdata" = count_PS_pre_badges_correct_data,
         "PSnotearned_correctdata" = count_PSnotearned_correct_data,
         "PSearned_correctdata"  = count_PSearned_correct_data) %>%
  pivot_longer(1:4, names_to = "category", values_to = "count") %>%
  separate(category, c("badge_type", "real_condition"), "_")


# exporting count column from reportedly available dataframe for precentage calculating
reportedly_available_totalforpercentagecalc <- reportedly_available_done %>%
  select(count) %>%
  rename("reportedly_available_total" = count)

count_correct_data[4] = reportedly_available_totalforpercentagecalc["reportedly_available_total"]

count_percentage_correct_data <- count_correct_data %>%
  mutate(percent = (count/reportedly_available_total)*100) %>%
  select(-reportedly_available_total)

correct_data_done <- count_percentage_correct_data


# no badge usable data "are_the_data_understandable_and_usable_after_brief_review"

count_nobadge_usable_data <- nobadge %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_nobadge_usable_data = n)  

# PS before badge usable data "are_the_data_understandable_and_usable_after_brief_review"

count_PS_pre_badges_usable_data <- PS_pre_badges %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PS_pre_badges_usable_data = n)  

# PS badge not earned usable data "are_the_data_understandable_and_usable_after_brief_review"

count_PSnobadge_usable_data <- PSnotearned %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PSnobadge_usable_data = n)

# PS badge earned usable data "are_the_data_understandable_and_usable_after_brief_review"

count_PSbadge_usable_data <- PSearned %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PSbadge_usable_data = n)

# binding all count for "are_the_data_understandable_and_usable_after_brief_review"

count_usable_data <- cbind(count_nobadge_usable_data,
                           count_PS_pre_badges_usable_data,
                           count_PSnobadge_usable_data,
                           count_PSbadge_usable_data) %>%
  rename("nobadge_usabledata" = count_nobadge_usable_data,
         "PSprebadges_usabledata" = count_PS_pre_badges_usable_data,
         "PSnotearned_usabledata" = count_PSnobadge_usable_data,
         "PSearned_usabledata"  = count_PSbadge_usable_data) %>%
  pivot_longer(1:4, names_to = "category", values_to = "count") %>%
  separate(category, c("badge_type", "real_condition"), "_")


# exporting count column from usable data for precentage calculating
reportedly_available_totalforpercentagecalc <- reportedly_available_done %>%
  select(count) %>%
  rename("reportedly_available_total" = count)

count_usable_data[4] = reportedly_available_totalforpercentagecalc["reportedly_available_total"]

count_percentage_usable_data <- count_usable_data %>%
  mutate(percent = (count/reportedly_available_total)*100) %>%
  select(-reportedly_available_total)

usable_data_done <- count_percentage_usable_data


# no badge complete data "are_the_data_complete"

count_nobadge_complete_data <- nobadge %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_nobadge_complete_data = n)


# PS before badge complete data "are_the_data_complete"

count_PS_pre_badges_complete_data <- PS_pre_badges %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PS_pre_badges_complete_data = n)

# PS no badge earned complete data "are_the_data_complete"

count_PSnobadge_complete_data <- PSnotearned %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PSnobadge_complete_data = n)

# PS badge earned complete data "are_the_data_complete"

count_PSbadge_complete_data <- PSearned %>%
  filter(are_the_data_understandable_and_usable_after_brief_review == "Yes") %>%
  count() %>%
  rename(count_PSbadge_complete_data = n)


# binding all count for "are_the_data_complete"

count_complete_data <- cbind(count_nobadge_complete_data,
                             count_PS_pre_badges_complete_data,
                             count_PSnobadge_complete_data,
                             count_PSbadge_complete_data) %>%
  rename("nobadge_completedata" = count_nobadge_complete_data,
         "PSprebadges_completedata" = count_PS_pre_badges_complete_data,
         "PSnotearned_completedata" = count_PSnobadge_complete_data,
         "PSearned_completedata"  = count_PSbadge_complete_data) %>%
  pivot_longer(1:4, names_to = "category", values_to = "count") %>%
  separate(category, c("badge_type", "real_condition"), "_")


# exporting count column from complete data for precentage calculating
reportedly_available_totalforpercentagecalc <- reportedly_available_done %>%
  select(count) %>%
  rename("reportedly_available_total" = count)

count_complete_data[4] = reportedly_available_totalforpercentagecalc["reportedly_available_total"]

count_percentage_complete_data <- count_complete_data %>%
  mutate(percent = (count/reportedly_available_total)*100) %>%
  select(-reportedly_available_total)


complete_data_done <- count_percentage_complete_data

# rbind all the 'done' data frame
fig4df <- rbind(reportedly_available_done,
                actually_available_done,
                correct_data_done,
                usable_data_done,
                complete_data_done) %>%
  arrange(badge_type)


# ggplot
#v reordering of real condition
fig4df$real_condition <- factor(fig4df$real_condition,levels = c("ravailable",
                                                                 "savailable",
                                                                 "correctdata",
                                                                 "usabledata",
                                                                 "completedata"))


ggplot(fig4df, aes(y=percent,  group = badge_type, colour = badge_type)) +
  coord_cartesian(ylim=c(0,100)) +
  geom_line(aes(size=badge_type, colour=badge_type, x=real_condition)) +
  geom_point(aes(x=real_condition), size=8, shape=21, fill="white", position = position_dodge(width=0)) +
  geom_text(aes(x=real_condition,
                label=count,
                hjust=.5,
                vjust=.5),
            size=3,
            show.legend = F) +
  scale_color_manual(values=c("#E69F00","#009E73", "#D55E00", "#0072B2"),
                     labels=c("Journals Without Badges",
                              "PS Before Badges",
                              "PS Badges Not Earned",
                              "PS Badges Earned"
                     )) +
  scale_size_manual(name= "badge_type",
                    values=c(3,
                             3,
                             3,
                             3), guide = F) +
  theme(axis.text.y = element_text(size=9),
        axis.title.y = element_text(size=9)) +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_discrete(limits=c("ravailable",
                            "savailable",
                            "correctdata",
                            "usabledata",
                            "completedata"))+
  ylab("Percentage of Articles with Data Reportedly Available") +
  xlab("") +
  theme(legend.position=c(.190, .25)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(1, "cm")) +
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA))) +
  theme(axis.line= element_line(), axis.title.x=element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.border = element_rect(colour = "black", linetype = "solid", size=1.5))






newplot

