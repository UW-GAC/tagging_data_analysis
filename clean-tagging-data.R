#!/usr/bin/env Rscript

# Generate figures and tables for the harmonization paper and supplement.

## ----setup, message=FALSE------------------------------------------------
library(dplyr)
library(forcats)
library(readr)
library(stringr)

## ----input-files---------------------------------------------------------
tagged_variables_file <- '/projects/topmed/phenotype_tagging/internal_use_exported_data/2019-12-23_1349_dcc_tagging_data/tagged_variables.txt'
total_variable_counts_file <- '/projects/topmed/phenotype_tagging/internal_use_exported_data/2019-12-23_1349_dcc_tagging_data/study_version_variable_counts.txt'
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
    study_version_accession = col_character(),
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
tagged_variables <- read_tsv(tagged_variables_file, na=c('', 'None'), col_types=tagged_variable_col_types)
cat('Problems found:', sep='\n')
problems(tagged_variables)  # No problems!

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
tagged_variables <- tagged_variables %>% 
    mutate(study_shortname=recode_factor(study_phs, !!!phs_to_shortname))

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
colnames(umls_sheet) <- c('phenotype_domain', 'tag_title', 'description', 'instructions', 'cui', 'umls_term')

# Set medication to be its own phenotype domain
umls_sheet$phenotype_domain[umls_sheet$tag_title == 'Medication/supplement use'] <- 'Medication/supplement use'
umls_sheet <- umls_sheet %>% mutate(phenotype_domain = factor(phenotype_domain))

# Add phenotype domain to the tagged variable data
tagged_variables <-
    tagged_variables %>%
    mutate(phenotype_domain = umls_sheet$phenotype_domain[match(tag_title, umls_sheet$tag_title)],
           umls_cui = umls_sheet$cui[match(tag_title, umls_sheet$tag_title)],
           umls_term = umls_sheet$umls_term[match(tag_title, umls_sheet$tag_title)])

## ----set-factor-levels, fig.show='hide', results='hold'------------------
# Order the milestone levels properly.
tagged_variables <- tagged_variables %>% 
    mutate(milestone=fct_relevel(milestone, c('1', '2', '3', '4')))

# Add sensible labels for the DCC status levels.
tagged_variables <- tagged_variables %>%
    mutate(dcc_review_status=recode_factor(dcc_review_status, !!!list('1'='confirmed', '0'='needs study followup')))

# Add sensible labels for the study response status levels.
tagged_variables <- tagged_variables %>%
    mutate(study_response_status=recode_factor(study_response_status, !!!list('1'='agreed to remove', '0'='gave explanation')))

# Add sensible labels for the dcc decision status levels.
tagged_variables <- tagged_variables %>%
    mutate(dcc_decision_decision=recode_factor(dcc_decision_decision, !!!list('1'='confirmed', '0'='removed')))

# Reorder factor levels for tag_title
tagged_variables <- tagged_variables %>%
    mutate(tag_title = fct_relevel(tag_title, sort))

# Save the final data to put in the paper repository
write.table(tagged_variables, file=file.path(out_dir, 'tagged_variables_cleaned.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

## ----total-variable-data-process-------------------------
total_variables_col_types <- cols(
    Study = col_factor(NULL),
    `Versioned study accession` = col_character(),
    `N dbGaP variables` = col_double()
)
total_variables <- read_tsv(total_variable_counts_file, col_types=total_variables_col_types)

# Add a study_shortname column.
total_variables <- total_variables %>% 
    mutate(study_shortname=names(study_shortnames)[
        match(as.integer(str_match(total_variables$`Versioned study accession`, 'phs(\\d{6})')[,2]), study_shortnames)
        ]
    ) %>%
    mutate(funded=study_shortname %in% names(funded_studies))

# Save the final data to put in the paper repository
write.table(total_variables, file=file.path(out_dir, 'tagged_study_versions_cleaned.txt'), quote=FALSE, sep='\t', na='', row.names=FALSE)

# Run these in the terminal pane
# cp paper_figures_and_tables/tagged_variables_cleaned.txt ~/devel/phenotype-harmonization-paper/data
# cp paper_figures_and_tables/tagged_study_versions_cleaned.txt ~/devel/phenotype-harmonization-paper/data