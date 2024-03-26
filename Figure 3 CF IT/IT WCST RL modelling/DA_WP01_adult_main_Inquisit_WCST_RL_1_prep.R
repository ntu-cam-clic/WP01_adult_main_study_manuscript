# Prepare Inquisit WCST data for JAGS modelling 

# extract WCST raw data from csv file
# calculate matching idx for color, shape, and number: 1 if resp match stimulus, 0 if resp does not match stimulus

# output will be one comma separated txt file with the following columns 
# subnum: Subject number
# trial: Trial 
# outcome: Feedback (1 or 0) 
# corr_col: Whether the test card matched the deck based on colour (1 or 0)
# corr_shape: Whether the test card matched the deck based on shape (1 or 0)
# corr_num: Whether the test card matched the deck based on number (1 or 0)

# set data file path

# path to save prepared data for JAGS (output of this script), should be a .txt file
WCST_JAGS_out_path = "path of output"

# folder path with WCST raw data from Inquisit
data_folder_path = "path to folder with raw data"

# double check the first name matching file is what you need
#file_name = list.files(data_folder_path, pattern = "clic_wp01_adult_test_wcst_wcst_wp01adult_raw_.*\\.csv")[1] 
data_file_path = "path to data"

# read in raw data
#data_WCST_raw = read.csv(data_file_path)
data_WCST_raw = readxl::read_xlsx(data_file_path)

# exclusion copied from early quality checks. 
# please update this part when new data come in
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "99999"), ]
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "90001"), ]
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "90003"), ]
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "90004"), ]
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "10060" & data_WCST_raw$sessionid == 2), ]
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$subject == "10060" & data_WCST_raw$sessionid == 3), ]


# remove intro and scoring rows
data_WCST_raw = data_WCST_raw[-which(is.na(data_WCST_raw$category)), ] # scoring rows
data_WCST_raw = data_WCST_raw[-which(data_WCST_raw$category == 0), ] # intro

# get the useful variables
data_WCST_use = data_WCST_raw[c('subject', 'trialCount', 'correct', 'ResponseCategory')]

# sort by ID
data_WCST_use = data_WCST_use[order(data_WCST_use$subject),]

# coding for JAGS
data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'C'] = 1
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'C'] = 0
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'C'] = 0

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'F'] = 0
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'F'] = 1
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'F'] = 0

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'N'] = 0
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'N'] = 0
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'N'] = 1

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'CF'] = 1
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'CF'] = 1
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'CF'] = 0

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'CN'] = 1
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'CN'] = 0
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'CN'] = 1

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'FN'] = 0
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'FN'] = 1
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'FN'] = 1

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'CFN'] = 1
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'CFN'] = 1
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'CFN'] = 1

data_WCST_use$corr_col[data_WCST_use$ResponseCategory == 'O'] = 0
data_WCST_use$corr_shape[data_WCST_use$ResponseCategory == 'O'] = 0
data_WCST_use$corr_number[data_WCST_use$ResponseCategory == 'O'] = 0

# prepare the data out
WCST_JAGS_out = data_WCST_use[c('subject', 
                                'trialCount',
                                'correct',
                                'corr_col',
                                'corr_shape',
                                'corr_number'
                                )]

# rename columns to match JAGS modelling needs
colnames(WCST_JAGS_out)[1] <- 'subnum'
colnames(WCST_JAGS_out)[2] <- 'trial'
colnames(WCST_JAGS_out)[3] <- 'outcome'
colnames(WCST_JAGS_out)[6] <- 'corr_num'

# save data for JAGS
write.table(WCST_JAGS_out, WCST_JAGS_out_path, sep = ',', row.names = F)

