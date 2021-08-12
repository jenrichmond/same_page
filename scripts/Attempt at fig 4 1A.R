# Having ago at recreating Kidwell's fig 4 for 1A

# Load packages
library(tidyverse)
library(extrafont)
library(here)

# Read and clean data -----

# read in 1A data from csv

data1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

# first, let's collapse the subfields into 4 main groups: developmental, cognition, social and 'other'
subfield_groups <- data1A %>%
  mutate(subfield_groups = case_when(subfield == "Behavioural Neuroscience" ~ "Other",
                                     subfield == "Cognitive Neuroscience" ~ "Other",
                                     subfield == "Health Psychology" ~ "Other",
                                     subfield == "Perception" ~ "Other",
                                     subfield == "Developmental Psychology" ~ "Development",
                                     subfield == "Social Psychology" ~  "Social",
                                     TRUE ~ as.character(subfield))) %>%
  relocate(subfield_groups, .after = subfield)

# we can now delete the original subfield column
subfield_groups <- subfield_groups %>%
  select(-subfield)

# Select and rename variables we're interested in
select <- dates %>%
  select(article_id_number, subfield_groups, data_statement_indicates_that_data_are, are_the_data_located_at_the_working_page, does_the_data_correspond_to_what_is_reported_in_the_article, software, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables, are_analysis_scripts_included_with_the_data, are_the_data_complete)

# let's create some summary tables

# reportedly available
reportedly_available_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, data_statement_indicates_that_data_are) %>%
  mutate("Percent" = Available/(Available)*100) %>%
  select(subfield_groups, Number = Available, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Reportedly Available"))

# loctable data
locatable_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_the_data_located_at_the_working_page) %>%
  mutate("Percent" = Yes/(Yes + No + `Requires permission`)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Locatable Data"))

# correct data 
correct_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, does_the_data_correspond_to_what_is_reported_in_the_article) %>%
  mutate("Percent" = Yes/(Yes + Unclear)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Correct Data"))

# complete data
complete_data <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_the_data_complete) %>%
  mutate("Percent" = `Yes, all of the data appear to be available`/(`Yes, all of the data appear to be available` + `Yes, but only some of the data are available` + `No, not all of the data are available` + `Unclear whether or not all of the data are available`)*100) %>%
  select(subfield_groups, Number = `Yes, all of the data appear to be available`, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Complete Data"))

# software specified
software_specified <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, software) %>%
  mutate("Percent" = Yes/(Yes + No)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Software Specified"))

# codebook available 
data_codebook_available <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate("Percent" = Yes/(Yes + No)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Codebook Available"))

# scripts available 
data_scripts_available <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_analysis_scripts_included_with_the_data) %>%
  mutate("Percent" = Yes/(Yes + No)*100) %>%
  select(subfield_groups, Number = Yes, Percent) %>%
  mutate("Real_Stage" = NA) %>%
  mutate(Real_Stage = coalesce(Real_Stage, "Scripts Available"))

# Let's bind all the statistics together 

all_stats <- rbind(reportedly_available_data, locatable_data, correct_data, complete_data, software_specified, data_codebook_available, data_scripts_available)%>%
  relocate(subfield_groups, .after = `Real_Stage`) %>%
  relocate(Number, .after = subfield_groups) %>%
  relocate(Percent, .after = Number)

# let's make subfield a factor
all_stats$subfield_groups <- factor(all_stats$subfield_groups, levels=c("Development",
                                                            "Social",
                                                            "Cognition",
                                                            "Other"))

# let's add a column that specifies the "stage" of each variable
all_stats <- all_stats %>%
  mutate(Stage = case_when(Real_Stage == "Reportedly Available" ~ 1,
                           Real_Stage == "Locatable Data" ~ 2,
                           Real_Stage == "Correct Data" ~ 3,
                           Real_Stage == "Complete Data" ~ 4,
                           Real_Stage == "Codebook Available" ~ 5,
                           Real_Stage == "Software Specified" ~ 6,
                           Real_Stage == "Scripts Available" ~ 7))

# and let's plot! 

plot <- ggplot(all_stats, aes(y=Percent,  group = subfield_groups, colour = subfield_groups)) +
  coord_cartesian(ylim=c(0,100)) +
  geom_line(aes(size=subfield_groups, colour=subfield_groups, x=Real_Stage)) +
  geom_point(aes(x=Stage), size=8, shape=21, fill="white", position = position_dodge(width=0)) +
  geom_text(aes(x=Stage,
                label=Number,
                hjust=.5,
                vjust=.5),
            size=3,
            show.legend = F) +
  scale_color_manual(values=c("#E69F00","#009E73", "#D55E00", "#0072B2"),
                     labels=c("Development",
                              "Social",
                              "Cognition",
                              "Other")) +
  scale_size_manual(name= "subfield_groups",
                    values=c(2,
                             2,
                             2,
                             2),
                    guide=F) +
  theme(axis.text.y = element_text(size=9),
        axis.title.y = element_text(size=9)) +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_discrete(limits=c("Reportedly Available","Locatable Data","Correct Data","Complete Data", "Codebook Available", "Software Specified", "Scripts Available"))+
  ylab("Percentage of Articles with Data Reportedly Available") +
  theme(legend.position=c(.125, .16)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(1, "cm")) +
  guides(colour = guide_legend(override.aes = list(size = 2, shape = NA))) +
  theme(axis.line= element_line(), axis.title.x=element_blank(), 
        panel.background = element_blank(), 
        panel.grid.minor = element_blank()) 

plot

# Still trying to figure out how to code it so the dots don't overlap one another
# I think Kidwell's coding might be helpful, but unsure

# Printing the plot

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




