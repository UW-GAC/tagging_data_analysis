#!/usr/bin/env Rscript

# Generate figures and tables for the harmonization paper and supplement.

## ----setup, message=FALSE------------------------------------------------
library(dplyr)
library(forcats)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(readr)
library(stringr)
library(tidyr)
options(width=400)

## ----files---------------------------------------------------------------
out_dir <- 'paper_figures_and_tables'
cleaned_data <- file.path(out_dir, 'tagged_variables_cleaned.txt')
study_data <- file.path(out_dir, 'tagged_study_versions_cleaned.txt')

## ----read-data, results='hide'-----------------------------------
study_version_col_types <- cols(
  Study = col_factor(NULL),
  `Versioned study accession` = col_character(),
  `N dbGaP variables` = col_double(),
  study_shortname = col_factor(NULL),
  funded = col_logical()
)
study_version_counts <- read_tsv(study_data, na=c('', 'None'), col_types=study_version_col_types)
cat('Problems found:', sep='\n')
problems(study_version_counts)  # No problems!
str(study_version_counts)
funded_studies <- study_version_counts$study_shortname[study_version_counts$funded]

tagged_variable_col_types <- cols(
  tag_id = col_factor(NULL),
  tag_title = col_factor(NULL),
  variable_phv = col_character(),
  variable_name = col_character(),
  variable_description = col_character(),
  dataset_pht = col_character(),
  dataset_name = col_character(),
  dataset_description = col_character(),
  study_name = col_factor(NULL),
  study_phs = col_factor(NULL),
  tagged_by = col_character(),
  created = col_datetime(format = ""),
  modified = col_datetime(format = ""),
  dcc_review_status = col_factor(NULL),
  dcc_review_comment = col_character(),
  dcc_review_creator = col_factor(NULL),
  study_response_status = col_factor(NULL),
  study_response_comment = col_character(),
  study_response_creator = col_character(),
  dcc_decision_decision = col_factor(NULL),
  dcc_decision_comment = col_character(),
  dcc_decision_creator = col_factor(NULL),
  is_archived = col_logical(),
  milestone = col_factor(NULL),
  study_shortname = col_factor(NULL)
)
tagged_variables <- read_tsv(cleaned_data, na=c('', 'None'), col_types=tagged_variable_col_types)
cat('Problems found:', sep='\n')
problems(tagged_variables)  # No problems!
str(tagged_variables)

## ----visual-settings-----------------------------------------------------
# Standard plot size
plot_width <- 8
plot_height <- 8
plot_dpi <- 300

# Save some ggplot layers for convenience (will reuse many times).
current_theme <- theme_bw()
angled_x_axis_labels <- theme(axis.text.x=element_text(angle=60, hjust=1))
legend_on_top <- theme(legend.position='top')
no_legend <- theme(legend.position='none')
no_legend_title <- theme(legend.title=element_blank())

# Save some color palettes and manual fill scales for reuse.
study_colors <- c(brewer.pal(8, 'Set2'), brewer.pal(9, 'Set1'))[1:length(levels(tagged_variables$study_shortname))]
names(study_colors) <- levels(tagged_variables$study_shortname)
study_fill <- scale_fill_manual(values=study_colors)
milestone_colors <- brewer.pal(length(levels(tagged_variables$milestone)), 'Dark2')
names(milestone_colors) <- levels(tagged_variables$milestone)
milestone_fill <- scale_fill_manual(values=milestone_colors)
bar_border_color <- 'gray25'

# Save ggplot layer for facet wrapping by milestone, with prettier labels.
longer_milestone_labels <- paste('Milestone', levels(tagged_variables$milestone))
names(longer_milestone_labels) <- levels(tagged_variables$milestone)
milestone_facets <- facet_wrap(~ milestone, scales='free', ncol=1, labeller=labeller(milestone=longer_milestone_labels))

## ----number-of-variables-------------------------------------------------
(n_unique_variables <- length(unique(tagged_variables$variable_phv)))
(n_tagged_variables <- nrow(tagged_variables))
(n_funded <- nrow(tagged_variables %>% filter(study_shortname %in% funded_studies)))
(n_nonfunded <- nrow(tagged_variables %>% filter(!(study_shortname %in% funded_studies))))
n_funded / n_nonfunded
(n_tagged_variables_passed <- sum(!tagged_variables$is_archived))
(n_tagged_variables_failed <- sum(tagged_variables$is_archived))

## ----proportion-total-vars-tagged----------------------------------------
tagged_variable_counts_by_study <- 
  tagged_variables %>% 
  group_by(study_shortname, study_name, study_phs) %>% 
  summarize(tagged_variable_count=n())

total_variables_and_tagged_counts <- 
    inner_join(study_version_counts, tagged_variable_counts_by_study) %>% 
    mutate(proportion_tagged=tagged_variable_count/`N dbGaP variables`) %>% 
    mutate(proportion_tagged_formatted = as.character(round(proportion_tagged, 2)))
total_variables_and_tagged_counts$proportion_tagged_formatted[3] <- 
    paste(total_variables_and_tagged_counts$proportion_tagged_formatted[3], 
          '(Proportion of variables tagged)')

total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=proportion_tagged, fill=`N dbGaP variables`), color=bar_border_color) +
    scale_fill_distiller(palette="Blues", direction=1, trans='log10', name='Number of dbGaP variables') +
    current_theme +
    ylab('Proportion of dbGaP study variables tagged') +
    xlab('Study') +
    coord_flip() +
    theme(legend.justification=c(1, 1),
          legend.position=c(1.0, 1.0),
          legend.background=element_rect(color=bar_border_color, size=0.3)
          )
ggsave(file.path(out_dir, 'tagged_proportion_by_study_n_variables_fill.png'), plot=total_variables_and_tagged_counts_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.75)
# This also doesn't look great.

## ----final-confirmed-proportion-by-tag-----------------------------------
review_status_by_tag <-
  tagged_variables %>% 
  group_by(tag_title, is_archived, milestone) %>% 
  summarize(tagged_variable_count=n()) %>% 
  mutate(review_status = ifelse(is_archived, 'Failed review', 'Passed review'))
review_status_by_tag$review_status <- as.factor(review_status_by_tag$review_status)

# Get the count from the tag with the highest number of tagged variables, after Medication use is removed.
highest_non_med_count <-
  review_status_by_tag %>%
  filter(tag_title != "Medication/supplement use") %>%
  group_by(tag_title) %>%
  summarize(tagged_variable_count=sum(tagged_variable_count)) %>% 
  filter(tagged_variable_count == max(tagged_variable_count)) %>%
  pull(tagged_variable_count)

med_use_count <- review_status_by_tag %>%
  filter(tag_title == "Medication/supplement use") %>%
  ungroup() %>%
  summarize(tagged_variable_count=sum(tagged_variable_count)) %>% 
  pull(tagged_variable_count)

# Reorder by tag name instead
# tagged_variables$study_shortname <- factor(tagged_variables$study_shortname, levels=tagged_variable_counts_by_study$study_shortname[order(-tagged_variable_counts_by_study$tagged_variable_count)])
review_status_by_tag$tag_title <- factor(review_status_by_tag$tag_title, levels=sort(levels(review_status_by_tag$tag_title)))
review_status_by_tag <- review_status_by_tag[order(review_status_by_tag$tag_title), ]
review_status_by_tag$in_first_half <- as.numeric(rownames(review_status_by_tag)) >= nrow(review_status_by_tag)/2

annotation_data <- review_status_by_tag %>%
  filter(tag_title=="Medication/supplement use") %>%
  ungroup() %>%
  group_by(tag_title, in_first_half) %>%
  summarize(tagged_variable_count=sum(tagged_variable_count))

review_status_by_tag_ggp_horiz <-
  ggplot(review_status_by_tag) +
  geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=review_status), stat='identity', position=position_stack(reverse = TRUE), color=bar_border_color) +
  scale_fill_brewer(palette='Paired', name='', direction=-1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  facet_wrap(~ in_first_half, scales='free', nrow=2) +
  current_theme +
  angled_x_axis_labels +
  coord_cartesian(ylim=c(0, highest_non_med_count*1.05)) +
  theme(strip.text=element_blank(), 
        legend.justification=c(1, 1),
        legend.position=c(0.95, 0.95),
        legend.background=element_rect(color=bar_border_color)) +
  no_legend_title +
  theme(axis.text.x=element_text(size=7)) +
  ylab('Number of tagged variables') +
  xlab('Tag') +
  geom_segment(data=annotation_data, aes(x=tag_title, xend=tag_title),
               y=highest_non_med_count, yend=highest_non_med_count*1.08,
               arrow=arrow(length=unit(0.03, 'npc'))) +
  geom_label(data=annotation_data, aes(x=tag_title, label=format(tagged_variable_count, big.mark=',')),
            y=highest_non_med_count*0.95)
ggsave(file.path(out_dir, 'confirmed_tagged_variables_by_tag.png'), plot=review_status_by_tag_ggp_horiz, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.9)


## ----collapse-confirmed-or-not-by-tag-domain-------------------------------------
review_status_by_domain <-
    tagged_variables %>% 
    group_by(phenotype_domain, is_archived) %>% 
    summarize(tagged_variable_count=n()) %>% 
    mutate(review_status = ifelse(is_archived, 'Failed review', 'Passed review'))

# Get the count from the domain with the highest number of tagged variables, after Medication use is removed.
domain_highest_non_med_count <-
  review_status_by_domain %>%
  filter(phenotype_domain != "Medication/supplement use") %>%
  summarize(tagged_variable_count=sum(tagged_variable_count)) %>% 
  ungroup() %>%
  filter(tagged_variable_count == max(tagged_variable_count)) %>%
  pull(tagged_variable_count)

med_use_count <- review_status_by_domain %>%
  filter(phenotype_domain == "Medication/supplement use") %>%
  summarize(tagged_variable_count=sum(tagged_variable_count)) %>% 
  ungroup() %>%
  pull(tagged_variable_count)

review_status_by_domain_ggp <-
    ggplot(review_status_by_domain) +
    geom_bar(aes(x=phenotype_domain, y=tagged_variable_count, fill=review_status), stat='identity', position=position_stack(reverse=TRUE), color=bar_border_color) +
    scale_fill_brewer(palette='Paired', direction=-1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    # facet_wrap(~ in_first_half, scales='free', nrow=2) +
    current_theme +
    legend_on_top +
    no_legend_title +
    theme(legend.justification=c(0.5,1), legend.direction='horizontal', legend.background=element_blank()) +
    angled_x_axis_labels +
    theme(strip.text=element_blank()) +
    coord_cartesian(ylim=c(0, domain_highest_non_med_count*1.05)) +
    # coord_cartesian(ylim=c(0, 2000)) +
    ylab('Count') +
    xlab('Phenotype area') +
    annotate("label", x="Medication/supplement use", y=domain_highest_non_med_count*0.95, label=format(med_use_count, big.mark=',')) +
    annotate("segment", x="Medication/supplement use", xend="Medication/supplement use", 
             y=domain_highest_non_med_count, yend=domain_highest_non_med_count*1.08,
             arrow=arrow(length=unit(0.03, 'npc')))
ggsave(file.path(out_dir, 'confirmed_tagged_variables_by_domain.png'), plot=review_status_by_domain_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.6)

## ----reviewed-status-by-study-and-tag-table---------------------------------
# Trying to make the table shown in scratchpad with date 2019-12-06

str(tagged_variables)

tag_summary_by_study <-
  tagged_variables %>%
  subset(is_archived==FALSE) %>%
  group_by(tag_title, study_shortname, .drop=FALSE) %>% 
  summarize(tagged_variable_count=n()) %>%
  pivot_wider(names_from=study_shortname, values_from=tagged_variable_count) %>%
  rename(`Phenotype tag`=tag_title)

n_studies <- apply(tag_summary_by_study[-1], MARGIN=1, FUN=function(x){sum(x > 0)})
n_studies <- c(n_studies, NA)

tag_summary_by_study <-
  tag_summary_by_study %>%
  adorn_totals(c('row', 'col')) %>%
  mutate(`N studies`=n_studies)

write.table(tag_summary_by_study, file=file.path(out_dir, 'tag_summary_by_study.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

# Cumulative frequency distribution of N studies
n_studies_tab <- as.data.frame(table(tag_summary_by_study$`N studies`[seq(nrow(tag_summary_by_study) - 1)]))
n_studies_tab <- as_tibble(n_studies_tab)
n_studies_tab <- 
  n_studies_tab %>%
  mutate(Var1=as.integer(Var1)) %>%
  rename(
    `N studies`=Var1,
    `Frequency`=Freq
    ) %>%
  arrange(-`N studies`) %>%
  mutate(`Cumulative frequency`=cumsum(Frequency))

write.table(n_studies_tab, file=file.path(out_dir, 'n_studies_cumfreq.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

n_studies_cumulative_freq_ggp <-
  ggplot(n_studies_tab) +
  geom_line(aes(x=`N studies`, y=`Cumulative frequency`)) +
  scale_x_reverse() +
  theme_bw()
ggsave(file.path(out_dir, 'n_studies_cumulative_frequency.png'), plot=n_studies_cumulative_freq_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.6)

# This version is more explicit about what's going on in the review process, at the expense of being overcomplicated
# tag_summary_by_review_status <-
#   tagged_variables %>%
#   group_by(tag_title, .drop=FALSE) %>%
#   summarize(
#     total=n(),
#     confirmed_at_1=sum(dcc_review_status=='1', na.rm=TRUE),  # status is confirmed
#     flagged_at_1=sum(dcc_review_status=='0', na.rm=TRUE),  # status is flagged for study review
#     removed_at_2=sum(dcc_review_status=='0' & study_response_status=='1', na.rm=TRUE),  # study response agreed to remove
#     explanation_at_2=sum(dcc_review_status=='0' & study_response_status=='0', na.rm=TRUE),  # study response gave explanation
#     confirmed_at_3=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='1', na.rm=TRUE),  # dcc decision is to confirm
#     removed_at_3=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='0', na.rm=TRUE),  # dcc decision is to remove
#     total_passed=sum(!is_archived, na.rm=TRUE),  # 
#     total_failed=sum(is_archived, na.rm=TRUE)  #
#       )

# This is the simplified version, in terms of number removed every round of review
tag_summary_by_review_status_funded <-
  tagged_variables %>%
  filter(study_shortname %in% funded_studies) %>%
  group_by(tag_title, .drop=FALSE) %>%
  summarize(
    `Total tagged`=n(),
    `Flagged in step 1`=sum(dcc_review_status=='needs study followup', na.rm=TRUE),  # status is flagged for study review
    `Failed in step 2`=sum(dcc_review_status=='needs study followup' & study_response_status=='agreed to remove', na.rm=TRUE),  # study response agreed to remove
    `Failed in step 3`=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='removed', na.rm=TRUE),  # dcc decision is to remove
    `Total failed`=sum(is_archived, na.rm=TRUE),
    `Total passed`=sum(!is_archived, na.rm=TRUE) 
  ) %>%
  arrange(tag_title) %>%
  rename(`Phenotype tag`=tag_title) %>%
  adorn_totals("row")
write.table(tag_summary_by_review_status_funded, file=file.path(out_dir, 'tag_summary_by_review_status_funded.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

tag_summary_by_review_status_nonfunded <-
  tagged_variables %>%
  filter( !(study_shortname %in% funded_studies) ) %>%
  group_by(tag_title, .drop=FALSE) %>%
  summarize(
    `Total tagged`=n(),
    `Total failed`=sum(is_archived, na.rm=TRUE),
    `Total passed`=sum(!is_archived, na.rm=TRUE) 
  ) %>%
  arrange(tag_title) %>%
  rename(`Phenotype tag`=tag_title) %>%
  adorn_totals("row")
write.table(tag_summary_by_review_status_nonfunded, file=file.path(out_dir, 'tag_summary_by_review_status_nonfunded.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

