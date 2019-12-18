# This code needs to be run in a Django shell, and I'm not quire sure how to make
# that happen. As a result, I just run it interactively after launching
# shell_plus on modu.

# ssh modu
# workon phenotype_inventory
# Start with shell_plus
# ./manage.py shell_plus --settings=phenotype_inventory.settings.production

import datetime
import os
from subprocess import check_call
from unidecode import unidecode  # pip install unidecode

# Make a time-stamped file prefix for output files
current_date = datetime.datetime.now().strftime('%Y-%m-%d_%H%M')
output_dir = '/home/www-staff/emeryl/internal_use_exported_data/{}_dcc_tagging_data'.format(current_date)
os.makedirs(output_dir, exist_ok=True)

milestone1 = [
	'Height', 'Weight', 'BMI', 'Waist circumference', 'Hip circumference', 'Waist-hip ratio',
	'Resting arm systolic BP', 'Resting arm diastolic BP', 'LDL in blood', 'HDL in blood',
	'Triglycerides in blood', 'Total cholesterol in blood', 'Gender', 'Race/ancestry/ethnicity', 
]
milestone2 = [
	'Subcohort', 'Clinic visit', 'Geographic site', 'Ischemic stroke', 'Hemorrhagic stroke',
	'Other stroke', 'CAC', 'Carotid IMT', 'Myocardial infarction', 'Coronary angioplasty',
	'Coronary artery bypass graft', 'Heart failure', 'Hypertension', 'Blood glucose',
	'Insulin in blood', 'HbA1c', 'Diabetes', 'Atrial fibrillation/flutter', 'QRS duration from EKG',
	'QT interval from EKG', 'PR interval from EKG', 'Resting heart rate from EKG', 'LVH from EKG', 
]
milestone3 = [
	'Pacemaker', 'Hematocrit', 'Hemoglobin', 'Platelet count', 'Red blood cell count',
	'White blood cell count', 'Fibrinogen in blood', 'Factor VII', 'Factor VIII',
	'von Willebrand factor', 'VTE', 'CRP in blood', 'Interleukin 6 in blood',
	'Creatinine in blood', 'Cystatin C in blood', 'Albumin-creatinine ratio in urine', 'GFR', 
]
milestone4 = [
	'Medication/supplement use', 'Age at enrollment/collection', 'FVC', 'FEV1', 'Asthma',
	'Asthma severity', 'COPD', 'Sleep apnea', 'AHI', 'Cigarette smoking', 'Fasting',
]

milestones = dict(zip(milestone1 + milestone2 + milestone3 + milestone4,
                      [1]*len(milestone1) + [2]*len(milestone2) + [3]*len(milestone3) + [4]*len(milestone4)))

########################################################################################################################
# Print the tag-variable links to a tab-delimited file.
header = ('tag_id', 'tag_title', 'variable_phv', 'variable_name', 'variable_description',
          'dataset_pht', 'dataset_name', 'dataset_description', 'study_name', 'study_phs',
          'tagged_by', 'created', 'modified',
          'dcc_review_status', 'dcc_review_comment', 'dcc_review_creator',
          'study_response_status', 'study_response_comment', 'study_response_creator',
          'dcc_decision_decision', 'dcc_decision_comment', 'dcc_decision_creator',
          'is_archived', 'milestone')

# Filter out deprecated source study versions (handles CARDIA issue with 3 taggedtraits)
# Pull out the relevant fields to an LoL
tagged_trait_data = TaggedTrait.objects.filter(trait__source_dataset__source_study_version__i_is_deprecated=False
    ).select_related('trait__source_dataset',
                     'dcc_review',
                     'dcc_review__creator',
                     'dcc_review__study_response',
                     'dcc_review__dcc_decision',
).values_list(
    'tag__pk',
    'tag__title',
    'trait__full_accession',
    'trait__i_trait_name',
    'trait__i_description',
    'trait__source_dataset__full_accession',
    'trait__source_dataset__dataset_name',
    'trait__source_dataset__i_dbgap_description',
    'trait__source_dataset__source_study_version__study__i_study_name',
    'trait__source_dataset__source_study_version__study__pk',
    'creator__name',
    'created',
    'modified',
    'dcc_review__status',
    'dcc_review__comment',
    'dcc_review__creator__name',
    'dcc_review__study_response__status',
    'dcc_review__study_response__comment',
    'dcc_review__study_response__creator__name',
    'dcc_review__dcc_decision__decision',
    'dcc_review__dcc_decision__comment',
    'dcc_review__dcc_decision__creator__name',
    'archived',
)
# tagged_trait_data.count() # 17063 as of 2019-12-18 after final results (excluding CARDIA duplicates)

tagged_trait_data_with_milestone = [row + (milestones[row[header.index('tag_title')]], ) for row in tagged_trait_data]

# Check that all rows have the correct number of values.
all([len(header) == len(row) for row in tagged_trait_data_with_milestone])

# Use unidecode to translate non-ascii characters to ascii.
tagged_trait_data_converted_to_strings = [[unidecode(str(el)) for el in row] for row in tagged_trait_data_with_milestone]

# # Figuring out what the problem is with the lines that are choking readr's read_tsv
# bad_row = [row for (idx, row) in enumerate(tagged_trait_data_with_milestone) if row[1]=='CRP in blood' and row[2]=='phv00125940.v1.p1'][0]
# bad_row_unidecoded = [row for (idx, row) in enumerate(tagged_trait_data_converted_to_strings) if row[1]=='CRP in blood' and row[2]=='phv00125940.v1.p1'][0]
# bad_row[-4] == bad_row_unidecoded[-4]
# bad_row_unidecoded[-4]
# len(tagged_trait_data_converted_to_strings)
# len([row for row in tagged_trait_data_converted_to_strings if '\r\n' in row[-4]])

# Replace the problem characters.
tagged_trait_data_weirdos_removed = [[el.replace('\r\n', '\n') for el in row] for row in tagged_trait_data_converted_to_strings]

# # Look for any line breaks or tabs in the comment fields.
# comment_idx = header.index('study_response_comment')
# for row in tagged_trait_data_converted_to_strings:
#     if '\t' in row[comment_idx]:
#         print(row[comment_idx])

tagged_trait_data_cleaned = [[el.replace('\n', ' ').replace('\t', ' ') for el in row] for row in tagged_trait_data_weirdos_removed]
# tagged_trait_data_cleaned[0]
# tagged_trait_data_cleaned[1]

output_to_print = ['\t'.join(header)] + ['\t'.join(row) for row in tagged_trait_data_cleaned]

mapping_fn = os.path.join(output_dir, 'tagged_variables.txt')
mapping_file = open(mapping_fn, 'w')
mapping_file.write('\n'.join(output_to_print))
mapping_file.close()
print('Saved tag-variable mapping data to', mapping_fn)

# Transfer to it to network storage
rsync_args = ['rsync', '-avzhe ssh', output_dir, 'emeryl@fisher:/projects/topmed/phenotype_tagging/internal_use_exported_data/']
check_call(rsync_args)

