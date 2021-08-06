# Having ago at recreating Kidwell's fig 4 for 1A

# Load packages
library(qualtRics)
library(tidyverse)
library(janitor)
library(ggplot2)
library(grid)
library(cowplot)
library(httr)
library(extrafont)
library(here)
library(Hmisc)
library(dplyr)
library(extrafont)

# Read and clean data -----

# read in 1A data from csv

data1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

# First factor: Subfield ------

# first, let's collapse the subfields into 4 main groups: developmental, cognition, social and 'other'
subfield_groups <- data1A %>%
  mutate(subfield_groups = case_when(subfield == "Behavioural Neuroscience" ~ "Other",
                                     subfield == "Cognitive Neuroscience" ~ "Other",
                                     subfield == "Health Psychology" ~ "Other",
                                     subfield == "Perception" ~ "Other",
                                     TRUE ~ as.character(subfield))) %>%
  relocate(subfield_groups, .after = subfield)

# we can now delete the original subfield column
subfield_groups <- subfield_groups %>%
  select(-subfield)

# Second factor: Time -----

# we want to group the data in 3 six months: first half of 2014, second half of 2014 and first half of 2015

dates <- subfield_groups %>%
  mutate(time_period = case_when(
    str_detect(article_id_number, "1-2014|2-2014|3-2014|4-2014|5-2014|6-2014") ~ "1st half 2014",
    str_detect(article_id_number, "7-2014|8-2014|9-2014|10-2014|11-2014|12-2014") ~ "2nd half 2014",
    str_detect(article_id_number, "1-2015|2-2015|3-2015|4-2015|5-2015") ~ "1st half 2015")) %>%
  relocate(time_period, .after = article_id_number)

# Select and rename variables we're interested in
select <- dates %>%
  select(article_id_number, subfield_groups, time_period, data_statement_indicates_that_data_are, are_the_data_located_at_the_working_page, does_the_data_correspond_to_what_is_reported_in_the_article, are_the_data_understandable_and_usable_after_brief_review, are_the_data_complete)

# let's create some summary tables

# reportedly available
reportedly_available <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, data_statement_indicates_that_data_are) %>%
  mutate("Percent" = Available/(Available)*100) %>%
  select(subfield_groups, Number = Available, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Reportedly Available"))

# actually available
actually_available <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_the_data_located_at_the_working_page) %>%
  mutate("Percent" = Yes/(Yes + No + `Requires permission`)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Actually Available"))

# correct data 
correct_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, does_the_data_correspond_to_what_is_reported_in_the_article) %>%
  mutate("Percent" = Yes/(Yes + Unclear)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Correct Data"))

# usable data 
usable_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_the_data_understandable_and_usable_after_brief_review) %>%
  mutate("Percent" = Yes/(Yes + No + Unclear)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Usable Data"))

# complete data
complete_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_the_data_complete) %>%
  mutate("Percent" = `Yes, all of the data appear to be available`/(`Yes, all of the data appear to be available` + `Yes, but only some of the data are available` + `No, not all of the data are available` + `Unclear whether or not all of the data are available`)*100) %>%
  select(subfield_groups, Number = `Yes, all of the data appear to be available`, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Complete Data "))

# Let's bind all the statistics together 

all_stats <- rbind(reportedly_available, actually_available, correct_data, usable_data, complete_data)%>%
  relocate(subfield_groups, .after = `Real_Stage`) %>%
  relocate(Number, .after = subfield_groups) %>%
  relocate(Percent, .after = Number)

# let's make subfield a factor
all_stats$subfield_groups <- factor(all_stats$subfield_groups, levels=c("Developmental Psychology",
                                                            "Social Psychology",
                                                            "Cognition",
                                                            "Other"))

# and let's plot! - stuck here 

plot <- ggplot(all_stats, aes(y=Percent,  group = subfield_groups, colour = subfield_groups)) +
  coord_cartesian(ylim=c(0,100)) +
  geom_line(aes(size=subfield_groups, colour=subfield_groups, x=Real_Stage)) +
  geom_point(aes(x=pointlocation), size=8, shape=21, fill="white", position = position_dodge(width=0)) +
  geom_text(aes(x=pointlocation,
                label=Number,
                hjust=.5,
                vjust=.5),
            size=3,
            show.legend = F) +
  scale_color_manual(values=c("#E69F00","#009E73", "#D55E00", "#0072B2"),
                     labels=c("Developmental Psychology",
                              "Social Psychologys",
                              "Cognition",
                              "Other")) +
  scale_size_manual(name= "subfield_groups",
                    values=c(2,
                             2,
                             2,
                             2),
                    guide=F)+
  theme(axis.text.y = element_text(size=9),
        axis.title.y = element_text(size=9)) +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_discrete(limits=c("Reportedly Available","Actually Available","Correct Data","Usable Data","Complete Data"))+
  ylab("Percentage of Articles with Data Reportedly Available") +
  theme(legend.position=c(.125, .16)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(1, "cm")) +
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA))) +
  theme(axis.line= element_line(), axis.title.x=element_blank(), 
        panel.background = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", linetype = "solid", size=1.5)) +
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(fig4attempt.png)

# save as eps
font_import()
fonts()
loadfonts(device= "postscript")

postscript("Fig4attempt.eps", height = 6.69, width = 7.5, family = "Arial", paper = "special", onefile = FALSE,
           horizontal = FALSE)
p
dev.off()

embed_fonts("Fig4attempt.eps", outfile = "Fig4attempt.eps", options = "-dEPSCrop")

# save as tiff
ggsave(file = "Fig4attempt.tiff", plot = plot, dpi=300, height = 17, width = 19.05, units = 'cm')

ggsave(fig4attempt.png)




