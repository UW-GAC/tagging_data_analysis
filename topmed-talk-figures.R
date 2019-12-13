#!/usr/bin/env Rscript

# An updated version of the script used to generate files for my April 2019
# TOPMed presentation.

## ----setup, message=FALSE------------------------------------------------
library(tidyverse)
library(RColorBrewer)

## ----input-files---------------------------------------------------------
tagged_variables_file <- '/projects/topmed/phenotype_tagging/internal_use_exported_data/2019-12-03_1713_dcc_tagging_data/tagged_variables.txt'
total_variable_counts_file <- 'total_traits.txt'
umls_mappings_file <- '2019-04-16_tags_mapped_to_UMLS.tsv'

## ----output-files--------------------------------------------------------
out_dir <- 'april_topmed_talk_figures'

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
  `Phenotype Area` = col_character(),
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
# ggplot(all_tagged_variables) + geom_bar(aes(x=milestone))
all_tagged_variables <- all_tagged_variables %>% 
    mutate(milestone=fct_relevel(milestone, c('1', '2', '3', '4')))
# ggplot(all_tagged_variables) + geom_bar(aes(x=milestone))

# Add sensible labels for the DCC status levels.
# ggplot(all_tagged_variables) + geom_bar(aes(x=dcc_review_status))
all_tagged_variables <- all_tagged_variables %>%
    mutate(dcc_review_status=recode_factor(dcc_review_status, !!!list('1'='confirmed', '0'='needs study followup')))
# ggplot(all_tagged_variables) + geom_bar(aes(x=dcc_review_status))

# Add sensible labels for the study response status levels.
# ggplot(all_tagged_variables) + geom_bar(aes(x=study_response_status))
all_tagged_variables <- all_tagged_variables %>%
    mutate(study_response_status=recode_factor(study_response_status, !!!list('1'='agreed to remove', '0'='gave explanation')))
# ggplot(all_tagged_variables) + geom_bar(aes(x=study_response_status))

## ----print-cleaned-tagging-data, results='hold'--------------------------
options(width=400)
print(tagged_variables, n_extra=30)
print(total_variables)
print(umls_sheet)

## ----visual-settings-----------------------------------------------------
# Standard plot size
plot_width <- 11
plot_height <- 5
plot_dpi <- 300

# Save some ggplot layers for convenience (will reuse many times).
current_theme <- theme_bw()
angled_x_axis_labels <- theme(axis.text.x=element_text(angle=60, hjust=1))
legend_on_top <- theme(legend.position='top')
no_legend <- theme(legend.position='none')

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

## ----tagged-var-count-by-study-------------------------------------------
tagged_variable_counts_by_study <- 
    tagged_variables %>% 
    group_by(study_shortname, study_name, study_phs) %>% 
    summarize(tagged_variable_count=n())

tagged_variable_counts_by_study_ggp <- 
    ggplot(tagged_variables) +
    geom_bar(aes(x=study_shortname, fill=study_shortname), color=bar_border_color) +
    study_fill +
    current_theme +
    angled_x_axis_labels +
    no_legend +
    xlab('Study') +
    ylab('Number of tagged variables') +
    coord_flip()
ggsave(file.path(out_dir, 'tagged_variable_count_by_study.png'), plot=tagged_variable_counts_by_study_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.75)

## ----proportion-total-vars-tagged----------------------------------------
total_variables_and_tagged_counts <- 
    inner_join(total_variables, tagged_variable_counts_by_study) %>% 
    mutate(proportion_tagged=tagged_variable_count/n_variables) %>% 
    mutate(proportion_tagged_formatted = as.character(round(proportion_tagged, 2)))
total_variables_and_tagged_counts$proportion_tagged_formatted[3] <- 
    paste(total_variables_and_tagged_counts$proportion_tagged_formatted[3], 
          '(Proportion of variables tagged)')

total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=proportion_tagged, fill=study_shortname), color=bar_border_color) +
    study_fill +
    current_theme +
    angled_x_axis_labels +
    ylab('Proportion of variables tagged') +
    xlab('Study') +
    no_legend

# proportional to number of variables
    # ggplot(total_variables_and_tagged_counts, aes(x=study_shortname, y=tagged_variable_count/n_variables, width=log(n_variables)/10)) +
    # geom_bar(aes(fill=study_shortname), color=bar_border_color, stat='identity', position='identity') +
    # study_fill +
    # current_theme +
    # angled_x_axis_labels +
    # no_legend

total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=n_variables, fill=proportion_tagged), color=bar_border_color) +
    scale_fill_distiller(palette="Blues", direction=1) +
    current_theme +
    angled_x_axis_labels +
    ylab('dbGaP variables') +
    xlab('Study') +
    coord_flip()
ggsave(file.path(out_dir, 'tagged_variable_count_by_study_proportion_fill.png'), plot=total_variables_and_tagged_counts_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.75)
# These are all so faint it's very difficult to see.

total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=proportion_tagged, fill=n_variables), color=bar_border_color) +
    scale_fill_distiller(palette="Blues", direction=1, trans='log10', name='Number of dbGaP variables') +
    current_theme +
    ylab('Proportion of dbGaP variables tagged') +
    xlab('Study') +
    coord_flip() +
    theme(legend.justification=c(1, 1), legend.position=c(0.95, 0.95), legend.background=element_rect(color=bar_border_color))
ggsave(file.path(out_dir, 'tagged_proportion_by_study_n_variables_fill.png'), plot=total_variables_and_tagged_counts_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.75)
# This also doesn't look great.

y_pad <- 1200
text_size <- 3
total_variables_and_tagged_counts_ggp <- 
    ggplot(total_variables_and_tagged_counts) +
    geom_col(aes(x=study_shortname, y=n_variables, fill=study_shortname), color=bar_border_color) +
    geom_text(aes(x=study_shortname, y=n_variables+y_pad, label=proportion_tagged_formatted), size=text_size, hjust=0) +
    study_fill +
    current_theme +
    ylab('dbGaP variables') +
    xlab('Study') +
    no_legend +
    coord_cartesian(ylim=c(0, max(total_variables_and_tagged_counts$n_variables)+(y_pad*2))) +
    coord_flip()
ggsave(file.path(out_dir, 'tagged_variable_count_and_proportion_by_study.png'), plot=total_variables_and_tagged_counts_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.75)

## ----tagged-var-count-by-tag---------------------------------------------
tagged_variable_counts_by_tag <- 
    tagged_variables %>% 
    group_by(tag_title, milestone) %>% 
    summarize(tagged_variable_count=n())

# Reorder tag by decreasing tagged variable count.
tagged_variables$tag_title <- factor(tagged_variables$tag_title, levels=tagged_variable_counts_by_tag$tag_title[order(-tagged_variable_counts_by_tag$tagged_variable_count)])

tagged_variable_counts_by_tag_ggp <- 
    ggplot(tagged_variables) +
    geom_bar(aes(x=tag_title, fill=milestone), color=bar_border_color) +
    milestone_fill +
    milestone_facets +
    current_theme +
    angled_x_axis_labels +
    no_legend

tagged_variable_counts_by_tag_ggp_horiz <- 
    ggplot(tagged_variables) +
    geom_bar(aes(x=tag_title, fill=milestone), color=bar_border_color) +
    milestone_fill +
    facet_wrap(~ milestone, scales='free', ncol=2, labeller=labeller(milestone=longer_milestone_labels)) +
    current_theme +
    angled_x_axis_labels +
    no_legend

## ----tagged-var-count-by-study-and-tag-----------------------------------
tagged_variable_counts_by_study_and_tag <-
    tagged_variables %>%
    group_by(study_shortname, tag_title, milestone) %>% 
    summarize(tagged_variable_count=n())

# Reorder tags by decreasing tagged variable count.
tagged_variable_counts_by_study_and_tag$tag_title <- factor(tagged_variable_counts_by_study_and_tag$tag_title, levels=tagged_variable_counts_by_tag$tag_title[order(-tagged_variable_counts_by_tag$tagged_variable_count)])
# Make study a factor, for discrete scale.
tagged_variable_counts_by_study_and_tag$study_shortname <- factor(tagged_variable_counts_by_study_and_tag$study_shortname)

tag_columns <- 5
tagged_variable_counts_by_study_and_tag_ggp <-
    ggplot(tagged_variable_counts_by_study_and_tag) +
    geom_bar(aes(x=study_shortname, y=tagged_variable_count, fill=study_shortname), stat='identity', color=bar_border_color) +
    study_fill +
    facet_wrap(~ tag_title, scales='free_y', ncol=tag_columns) +
    current_theme +
    legend_on_top +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          strip.text=element_text(size=rel(0.75))
    )

## ----tagged-var-count-for-specific-tags----------------------------------
tags_to_plot <- c('Medication/supplement use')
# tag <- tags_to_plot[1]
for (tag in tags_to_plot)
{
    specific_tag_tagged_variable_count_by_study_ggp <-
        ggplot(tagged_variable_counts_by_study_and_tag %>% subset(tag_title == tag) ) +
        geom_bar(aes(x=study_shortname, y=tagged_variable_count, fill=study_shortname), stat='identity', color=bar_border_color) +
        study_fill +
        current_theme +
        no_legend +
        ggtitle(tag)
    cleaned_tag <- str_to_lower(str_replace_all(tag, "[^[:alnum:]]", "_"))
    ggsave(file.path(out_dir, paste0(cleaned_tag, '_counts_by_study.png')), specific_tag_tagged_variable_count_by_study_ggp)
}

## ----round-1-review-confirmed-proportion-by-study---------------------------------------
review_status_by_study <-
    tagged_variables %>% 
    subset(!is.na(dcc_review_status)) %>% 
    group_by(study_shortname, dcc_review_status) %>% 
    summarize(tagged_variable_count=n())

review_status_by_study_ggp <-
    ggplot(review_status_by_study) +
    geom_bar(aes(x=study_shortname, y=tagged_variable_count, fill=dcc_review_status), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='DCC review status') +
    current_theme +
    legend_on_top

## ----round-1-review-confirmed-proportion-by-reviewer------------------------------------
review_status_by_reviewer <-
    tagged_variables %>% 
    subset(!is.na(dcc_review_status)) %>% 
    subset(dcc_review_creator != 'Leslie Emery') %>% 
    group_by(dcc_review_creator, dcc_review_status) %>% 
    summarize(tagged_variable_count=n())

review_status_by_reviewer_ggp <-
    ggplot(review_status_by_reviewer) +
    geom_bar(aes(x=dcc_review_creator, y=tagged_variable_count, fill=dcc_review_status), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='DCC review status') +
    current_theme +
    legend_on_top

## ----round-1-review-confirmed-proportion-by-tag-----------------------------------------
review_status_by_tag <-
    tagged_variables %>% 
    subset(!is.na(dcc_review_status)) %>% 
    group_by(tag_title, dcc_review_status, milestone) %>% 
    summarize(tagged_variable_count=n())

review_status_by_tag_ggp <-
    ggplot(review_status_by_tag) +
    geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=dcc_review_status), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='DCC review status') +
    milestone_facets +
    current_theme +
    legend_on_top +
    angled_x_axis_labels

review_status_by_tag_ggp_horiz <-
    ggplot(review_status_by_tag) +
    geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=dcc_review_status), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='DCC review status') +
    facet_wrap(~ milestone, scales='free', ncol=2, labeller=labeller(milestone=longer_milestone_labels)) +
    current_theme +
    legend_on_top +
    angled_x_axis_labels

## ----study-response-status-by-study--------------------------------------
study_response_status_by_study <-
    tagged_variables %>%
    subset(dcc_review_status=='needs study followup') %>% 
    group_by(study_shortname, study_phs, study_response_status) %>% 
    summarize(tagged_variable_count=n())

study_response_status_by_study_ggp <- 
    ggplot(tagged_variables %>% subset(dcc_review_status=='needs study followup')) +
    geom_bar(aes(x=study_shortname, fill=study_response_status)) +
    current_theme + 
    angled_x_axis_labels

## ----study-response-agree-proportion-------------------------------------
study_response_agree_proportion_by_study <-
    tagged_variables %>%
    subset(!is.na(study_response_status)) %>% 
    group_by(study_shortname, study_phs) %>% 
    summarize(response_count=n(),
              agreed_count=sum(study_response_status=='agreed to remove'),
              agreed_proportion=sum(study_response_status=='agreed to remove')/n())

study_response_by_study_without_na_ggp <-
    ggplot(tagged_variables %>% subset(!is.na(study_response_status))) +
    geom_bar(aes(x=study_shortname, fill=study_response_status)) +
    current_theme +
    angled_x_axis_labels

## ----study-response-agree-proportion-by-tag------------------------------
study_response_agree_proportion_by_tag <-
    tagged_variables %>%
    subset(!is.na(study_response_status)) %>% 
    group_by(tag_id, tag_title, milestone) %>% 
    summarize(response_count=n(),
              agreed_count=sum(study_response_status=='agreed to remove'),
              agreed_proportion=sum(study_response_status=='agreed to remove')/n())

study_response_by_tag_without_na_ggp <-
    ggplot(study_response_agree_proportion_by_tag) +
    geom_bar(aes(x=tag_title, y=agreed_proportion, fill=milestone), stat='identity', position='stack', color=bar_border_color) +
    milestone_facets +
    milestone_fill + 
    current_theme +
    legend_on_top +
    angled_x_axis_labels

## ----final-confirmed-proportion-by-tag-----------------------------------
review_status_by_tag <-
    tagged_variables %>% 
    group_by(tag_title, is_archived, milestone) %>% 
    summarize(tagged_variable_count=n()) %>% 
    mutate(is_confirmed = !is_archived)

review_status_by_tag_ggp <-
    ggplot(review_status_by_tag) +
    geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=is_confirmed), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='Passed review') +
    milestone_facets +
    current_theme +
    legend_on_top +
    angled_x_axis_labels

highest_non_med_count <- tagged_variables %>%
    group_by(tag_title) %>%
    filter(tag_title != "Medication/supplement use") %>% 
    summarize(count=n()) %>%
    filter(count == max(count))
highest_non_med_count <- highest_non_med_count$count[1]
    
review_status_by_tag_ggp_horiz <-
    ggplot(review_status_by_tag) +
    geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=is_confirmed), stat='identity', position='stack', color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='Passed review') +
    facet_wrap(~ milestone, scales='free', ncol=2, labeller=labeller(milestone=longer_milestone_labels)) +
    current_theme +
    legend_on_top +
    angled_x_axis_labels +
    coord_cartesian(ylim=c(0, highest_non_med_count*1.05))
    # ylim(0, highest_non_med_count * 1.05)

# Reorder by tag name instead
# tagged_variables$study_shortname <- factor(tagged_variables$study_shortname, levels=tagged_variable_counts_by_study$study_shortname[order(-tagged_variable_counts_by_study$tagged_variable_count)])
review_status_by_tag$tag_title <- factor(review_status_by_tag$tag_title, levels=sort(levels(review_status_by_tag$tag_title)))
review_status_by_tag <- review_status_by_tag[order(review_status_by_tag$tag_title), ]
review_status_by_tag$in_first_half <- as.numeric(rownames(review_status_by_tag)) >= nrow(review_status_by_tag)/2

review_status_by_tag_ggp_horiz <-
    ggplot(review_status_by_tag) +
    geom_bar(aes(x=tag_title, y=tagged_variable_count, fill=is_confirmed), stat='identity', position=position_stack(reverse = TRUE), color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='Passed review', direction=-1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    facet_wrap(~ in_first_half, scales='free', nrow=2) +
    current_theme +
    legend_on_top +
    angled_x_axis_labels +
    coord_cartesian(ylim=c(0, highest_non_med_count*1.05)) +
    theme(strip.text=element_blank()) +
    ylab('Number of tagged variables') +
    xlab('Tag')

ggsave(file.path(out_dir, 'confirmed_tagged_variables_by_tag.png'), plot=review_status_by_tag_ggp_horiz, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.9)


## ----collapse-confirmed-or-not-by-tag-------------------------------------
tagged_variables_with_domains <- 
    right_join(tagged_variables, umls_sheet, by='tag_title')
str(tagged_variables_with_domains)

review_status_by_domain <-
    tagged_variables_with_domains %>% 
    group_by(phenotype_area, is_archived) %>% 
    summarize(tagged_variable_count=n()) %>% 
    mutate(is_confirmed = ifelse(is_archived, 'Failed review', 'Passed review'))

review_status_by_domain_ggp <-
    ggplot(review_status_by_domain) +
    geom_bar(aes(x=phenotype_area, y=tagged_variable_count, fill=is_confirmed), stat='identity', position=position_stack(reverse=TRUE), color=bar_border_color) +
    scale_fill_brewer(palette='Paired', name='', direction=-1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    # facet_wrap(~ in_first_half, scales='free', nrow=2) +
    current_theme +
    theme(legend.position=c(0.4, 1), legend.justification=c(0.5,1), legend.direction='horizontal', legend.background=element_blank()) +
    angled_x_axis_labels +
    # coord_cartesian(ylim=c(0, highest_non_med_count*1.05)) +
    theme(strip.text=element_blank()) +
    coord_cartesian(ylim=c(0, 2000)) +
    ylab('Count') +
    xlab('Phenotype area')
ggsave(file.path(out_dir, 'confirmed_tagged_variables_by_domain.png'), plot=review_status_by_domain_ggp, width=plot_width, height=plot_height, dpi=plot_dpi, scale=0.6)

## ----plot-harmonized-trait---------------------------------
subcohort_file <- '/projects/topmed/phenotype_harmonization/datasets/demographic/v3/combined_dataset/topmed_dcc_demographic_v3.txt'
height_file <- '/projects/topmed/phenotype_harmonization/datasets/baseline_common_covariates/v2/combined_dataset/topmed_dcc_baseline_common_covariates_v2.txt'

demographics <- read_tsv(subcohort_file)
common_covariates <- read_tsv(height_file)

str(demographics)
str(common_covariates)

# Combine datasets into one with topmed_abbreviation, subcohort, and height
subcohort_tib <- demographics %>%
    select(unique_subject_key, topmed_abbreviation, subcohort_1)
height_tib <- common_covariates %>% 
    select(unique_subject_key, topmed_abbreviation, height_baseline_1)

harmonized_data <- inner_join(subcohort_tib, height_tib)

# Fix the subcohort names
harmonized_data <- harmonized_data %>%
    mutate(subcohort_name = str_replace(subcohort_1, '.+_', '')) %>% 
    mutate(subcohort_name = replace(subcohort_name, subcohort_name=='NoSubcohortStructure', NA))

# Investigating low height values in CFS and CRA
summary(harmonized_data$height_baseline_1)
common_covariates[which.min(common_covariates$height_baseline_1), ]
nrow(common_covariates)
sum(na.omit(common_covariates$age_at_height_baseline_1 < 18)) # 1037 are younger than 18
sum(is.na(common_covariates$age_at_height_baseline_1)) # 2037 are missing values

# Plot it
ggp <- ggplot(harmonized_data %>% filter(!is.na(height_baseline_1))) +
    geom_density(aes(x=height_baseline_1, fill=subcohort_name), alpha=0.75) +
    facet_wrap( ~ topmed_abbreviation, ncol=2, scales='free_y') +
    theme_bw() +
    theme(legend.position='none') +
    xlab('Harmonized height (cm)')
ggsave(file.path(out_dir, 'harmonized_height_density_facets.png'), ggp, height=5, width=11, units='in')

ggp <- ggplot(harmonized_data %>% filter(!is.na(height_baseline_1))) +
    geom_violin(aes(y=height_baseline_1, x=topmed_abbreviation, fill=subcohort_name), alpha=0.75) +
    # facet_grid(topmed_abbreviation ~ ., scales='free_y') +
    theme_bw() +
    theme(legend.position='none') +
    ylab('Harmonized height (cm)') +
    coord_flip()
ggsave(file.path(out_dir, 'harmonized_height_violins.png'), ggp, height=5, width=11, units='in')

ggp <- ggplot(harmonized_data %>% filter(!is.na(height_baseline_1))) +
    geom_density(aes(x=height_baseline_1, fill=topmed_abbreviation), alpha=0.75) +
    study_fill +
    theme_bw() +
    xlab('Harmonized height (cm)')
ggsave(file.path(out_dir, 'harmonized_height_density.png'), ggp, height=5, width=11, units='in')

ggp <- ggplot(harmonized_data %>% filter(!is.na(height_baseline_1))) +
    geom_boxplot(aes(y=height_baseline_1, x=topmed_abbreviation, fill=topmed_abbreviation),
                 varwidth=TRUE, outlier.size=0.3, outlier.stroke=0.25, lwd=0.25, fatten=2) +
    study_fill +
    theme_bw() +
    ylab('Harmonized height (cm)') +
    xlab('Study') +
    coord_flip() +
    no_legend
ggsave(file.path(out_dir, 'harmonized_height_boxplot.png'), ggp, height=5, width=11, units='in', scale=0.75)

ggp <- ggplot(harmonized_data %>% filter(!is.na(height_baseline_1))) +
    geom_boxplot(aes(y=height_baseline_1, x=topmed_abbreviation, fill=topmed_abbreviation),
                 outlier.size=0.3, outlier.stroke=0.25, lwd=0.25, fatten=2) +
    study_fill +
    theme_bw() +
    ylab('Harmonized height (cm)') +
    xlab('Study') +
    coord_flip() +
    no_legend
ggsave(file.path(out_dir, 'harmonized_height_boxplot_no_varwidth.png'), ggp, height=5, width=11, units='in', scale=0.75)
## -----get-cardia-cac-variables-that-may-be-confusing-------------------------------------------
cardia_cac_examples <- tagged_variables %>%
    filter(study_shortname=='CARDIA', tag_title=='CAC', is_archived==FALSE) %>%
    select(variable_name, variable_description)

print(cardia_cac_examples, n=30)