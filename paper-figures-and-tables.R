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

## ----input-files---------------------------------------------------------
tagged_variables_file <- '/projects/topmed/phenotype_tagging/internal_use_exported_data/2019-12-03_1713_dcc_tagging_data/tagged_variables.txt'
total_variable_counts_file <- 'total_traits.txt'
umls_mappings_file <- '2019-04-16_tags_mapped_to_UMLS.tsv'

## ----output-files--------------------------------------------------------
out_dir <- 'paper_figures_and_tables'

## ----read-tagging-data, results='hide'-----------------------------------
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
  is_archived = col_logical(),
  milestone = col_factor(NULL)
)
all_tagged_variables <- read_tsv(tagged_variables_file, na=c('', 'None'), col_types=tagged_variable_col_types)
problems(all_tagged_variables)  # No problems!

## ----add-shortname-------------------------------------------------------
funded_studies <- c('MESA' = 209, 'JHS' = 286, 'ARIC' = 280, 'CHS' = 287, 'WHI' = 200, 'FHS' = 7, 'CARDIA' = 285)
# I was previously using funded_studies to subset the data to only show funded studies, but now I'm not really using it.

study_shortnames <- c('MESA' = 209, 'JHS' = 286, 'ARIC' = 280, 'CHS' = 287, 'WHI' = 200, 'FHS' = 7, 'CARDIA' = 285,
                      'COPDGene' = 179, 'CFS' = 284, 'Mayo_VTE' = 289, 'GOLDN' = 741, 'HCHS/SOL' = 810,
                      'SAS' = 914, 'Amish' = 956, 'CRA' = 988, 'HVH' = 1013, 'GENOA' = 1238)
study_shortnames <- study_shortnames[order(names(study_shortnames))]

# Add a study_shortname column.
phs_to_shortname <- names(study_shortnames)
names(phs_to_shortname) <- as.character(study_shortnames)
phs_to_shortname <- as.list(phs_to_shortname)
tagged_variables <- all_tagged_variables %>% 
    mutate(study_shortname=recode_factor(study_phs, !!!phs_to_shortname))

## ----total-variable-data-process, results='hide'-------------------------
total_variables_col_types <- cols(
  study_name = col_factor(NULL),
  n_variables = col_number(),
  phs = col_character()
)
total_variables <- read_tsv(total_variable_counts_file, col_types=total_variables_col_types)
# Convert the phs string to a number, and to a factor type.
total_variables <- total_variables %>%
    mutate(phs = as.numeric(str_replace(phs, 'phs', ''))) %>% 
    rename(study_phs = phs) %>% 
    mutate(study_phs=factor(study_phs))

## ----umls-sheet-data-process, results='hide'-----------------------------
umls_sheet_col_types <- cols(
  `Phenotype domain` = col_character(),
  `Tag name (phenotype concept)` = col_factor(NULL),
  Description = col_character(),
  Instructions = col_character(),
  `UMLS CUI` = col_character(),
  `UMLS term` = col_character()
)
umls_sheet <- read_tsv(umls_mappings_file, comment='#', col_types=umls_sheet_col_types)
colnames(umls_sheet) <- c('phenotype_area', 'tag_title', 'description', 'instructions', 'cui', 'umls_term')
# Set medication to be its own phenotype area.
umls_sheet$phenotype_area[umls_sheet$tag_title == 'Medication/supplement use'] <- 'Medication/supplement use'
umls_sheet <- umls_sheet %>% mutate(phenotype_area = factor(phenotype_area))

## ----set-factor-levels, fig.show='hide', results='hold'------------------
# Order the milestone levels properly.
all_tagged_variables <- all_tagged_variables %>% 
    mutate(milestone=fct_relevel(milestone, c('1', '2', '3', '4')))

# Add sensible labels for the DCC status levels.
all_tagged_variables <- all_tagged_variables %>%
    mutate(dcc_review_status=recode_factor(dcc_review_status, !!!list('1'='confirmed', '0'='needs study followup')))

# Add sensible labels for the study response status levels.
all_tagged_variables <- all_tagged_variables %>%
    mutate(study_response_status=recode_factor(study_response_status, !!!list('1'='agreed to remove', '0'='gave explanation')))

# Add sensible labels for the dcc decision status levels.
all_tagged_variables <- all_tagged_variables %>%
  mutate(dcc_decision_decision=recode_factor(dcc_decision_decision, !!!list('1'='confirmed', '0'='removed')))

## ----print-cleaned-tagging-data, results='hold'--------------------------
options(width=400)
print(tagged_variables, n_extra=30)
print(total_variables)
print(umls_sheet)

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
n_unique_variables <- length(unique(tagged_variables$variable_phv))
n_tagged_variables <- nrow(tagged_variables)
n_tagged_variables_passed <- sum(!tagged_variables$is_archived)
n_tagged_variables_failed <- sum(tagged_variables$is_archived)

## ----proportion-total-vars-tagged----------------------------------------
tagged_variable_counts_by_study <- 
  tagged_variables %>% 
  group_by(study_shortname, study_name, study_phs) %>% 
  summarize(tagged_variable_count=n())

total_variables_and_tagged_counts <- 
    inner_join(total_variables, tagged_variable_counts_by_study) %>% 
    mutate(proportion_tagged=tagged_variable_count/n_variables) %>% 
    mutate(proportion_tagged_formatted = as.character(round(proportion_tagged, 2)))
total_variables_and_tagged_counts$proportion_tagged_formatted[3] <- 
    paste(total_variables_and_tagged_counts$proportion_tagged_formatted[3], 
          '(Proportion of variables tagged)')

total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=proportion_tagged, fill=n_variables), color=bar_border_color) +
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
  tagged_variables %>%
  group_by(tag_title) %>%
  filter(tag_title != "Medication/supplement use") %>% 
  summarize(count=n()) %>%
  filter(count == max(count))
highest_non_med_count <- highest_non_med_count$count[1]

# Reorder by tag name instead
# tagged_variables$study_shortname <- factor(tagged_variables$study_shortname, levels=tagged_variable_counts_by_study$study_shortname[order(-tagged_variable_counts_by_study$tagged_variable_count)])
review_status_by_tag$tag_title <- factor(review_status_by_tag$tag_title, levels=sort(levels(review_status_by_tag$tag_title)))
review_status_by_tag <- review_status_by_tag[order(review_status_by_tag$tag_title), ]
review_status_by_tag$in_first_half <- as.numeric(rownames(review_status_by_tag)) >= nrow(review_status_by_tag)/2

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
  xlab('Tag')
ggsave(file.path(out_dir, 'confirmed_tagged_variables_by_tag.png'), plot=review_status_by_tag_ggp_horiz, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.9)


## ----collapse-confirmed-or-not-by-tag-domain-------------------------------------
tagged_variables_with_domains <- 
    right_join(tagged_variables, umls_sheet, by='tag_title')
str(tagged_variables_with_domains)

review_status_by_domain <-
    tagged_variables_with_domains %>% 
    group_by(phenotype_area, is_archived) %>% 
    summarize(tagged_variable_count=n()) %>% 
    mutate(review_status = ifelse(is_archived, 'Failed review', 'Passed review'))

review_status_by_domain_ggp <-
    ggplot(review_status_by_domain) +
    geom_bar(aes(x=phenotype_area, y=tagged_variable_count, fill=review_status), stat='identity', position=position_stack(reverse=TRUE), color=bar_border_color) +
    scale_fill_brewer(palette='Paired', direction=-1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    # facet_wrap(~ in_first_half, scales='free', nrow=2) +
    current_theme +
    legend_on_top +
    no_legend_title +
    theme(legend.justification=c(0.5,1), legend.direction='horizontal', legend.background=element_blank()) +
    angled_x_axis_labels +
    # coord_cartesian(ylim=c(0, highest_non_med_count*1.05)) +
    theme(strip.text=element_blank()) +
    coord_cartesian(ylim=c(0, 2000)) +
    ylab('Count') +
    xlab('Phenotype area')
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
  rename(`Phenotype tag`=tag_title) %>%
  adorn_totals(c('row', 'col'))
write.table(tag_summary_by_study, file=file.path(out_dir, 'tag_summary_by_study.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

# This version is more explicit about what's going on in the review process, at the expense of being overcomplicated
tag_summary_by_review_status <-
  tagged_variables %>%
  group_by(tag_title, .drop=FALSE) %>%
  summarize(
    total=n(),
    confirmed_at_1=sum(dcc_review_status=='1', na.rm=TRUE),  # status is confirmed
    flagged_at_1=sum(dcc_review_status=='0', na.rm=TRUE),  # status is flagged for study review
    removed_at_2=sum(dcc_review_status=='0' & study_response_status=='1', na.rm=TRUE),  # study response agreed to remove
    explanation_at_2=sum(dcc_review_status=='0' & study_response_status=='0', na.rm=TRUE),  # study response gave explanation
    confirmed_at_3=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='1', na.rm=TRUE),  # dcc decision is to confirm
    removed_at_3=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='0', na.rm=TRUE),  # dcc decision is to remove
    total_passed=sum(!is_archived, na.rm=TRUE),  # 
    total_failed=sum(is_archived, na.rm=TRUE)  #
      )

# This is the simplified version, in terms of number removed every round of review
tag_summary_by_review_status <-
  tagged_variables %>%
  group_by(tag_title, .drop=FALSE) %>%
  summarize(
    `Total tagged`=n(),
    `Flagged in step 1`=sum(dcc_review_status=='0', na.rm=TRUE),  # status is flagged for study review
    `Failed in step 2`=sum(dcc_review_status=='0' & study_response_status=='1', na.rm=TRUE),  # study response agreed to remove
    `Failed in step 3`=sum(!is.na(dcc_decision_decision) & dcc_decision_decision=='0', na.rm=TRUE),  # dcc decision is to remove
    `Total failed`=sum(is_archived, na.rm=TRUE),
    `Total passed`=sum(!is_archived, na.rm=TRUE) 
  ) %>%
  rename(`Phenotype tag`=tag_title)
write.table(tag_summary_by_review_status, file=file.path(out_dir, 'tag_summary_by_review_status.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

