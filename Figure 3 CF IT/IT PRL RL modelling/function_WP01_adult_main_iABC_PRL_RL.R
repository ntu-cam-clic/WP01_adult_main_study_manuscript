# preprocess functions for PRL


# extract_PR()
# extract PR data from json file
extract_PR <- function(filename){
  data_all = fromJSON(filename)
  idx_completePR = which(data_all$taskId =="reversal" & data_all$disposition=="completed")
  data_PR = data_all[idx_completePR,]
  # output
  data_PR
}


# PR_completion()
# read all json files in the folder and check which participant completed PRL task. 
# input: iABC data folder path.
# output: index of participant who completed PRL task (1 = completed, 0 = not completed)
PR_complete <- function(data_folder_path){
  filenames_all = list.files(data_folder_path, pattern = "\\.json$") # get all json file names in the path
  nSub = length(filenames_all) # number of subjects (number of json files)
  idx_PR_complete = rep(1, nSub) 
  userref_all = rep(1, nSub) 
    for (idx_file in 1:nSub){
      filename = filenames_all[idx_file] # filename of the json data file
      userref_all[idx_file] = sub('.*data-(.+).json.*','\\1', filename) # iABC userref
      PR_all = extract_PR(paste0(data_folder_path,'/',filename))
      if (dim(PR_all)[1] == 0){
        idx_PR_complete[idx_file] = 0 
      }
    }
  idx_name_order = 1:nSub
  ID_mapping = data.frame(idx_name_order,userref_all)
  write.csv(ID_mapping, 'ID_mapping_iABC.csv', row.names = F)
  idx_PR_complete
  }


# prepare_PR_data_hBayesDM()
# prepare data for hBayesDM package modelling use
prepare_PR_data_hBayesDM <- function(data_folder_path, idx_complete, study_code){
  filenames_all = list.files(data_folder_path, pattern = "\\.json$") # get all file names of the json files in the path
  for (idx_file in which(idx_complete==1)){
    filename = filenames_all[idx_file]
    PR_all = extract_PR(paste0(data_folder_path,'/',filename))
    PR = PR_all$items[[1]]
    PR = PR[!is.na(PR$trial),]
    
    # data prep for modelling (subID, outcome, choice)
    PR$subjID = idx_file # just use the file idx here. This is not mapping to subject ID for other tasks!
    PR$outcome = replace(as.numeric(PR$feedback), as.numeric(PR$feedback)==0, -1)
    # choice label rule: yellow = 1, blue = 2
    PR$choice = as.numeric(PR$selected == "yellow") + 2*as.numeric(PR$selected == "blue")
    # Note: for timeout trials, both choice and outcome will be marked as NA
    filename_out = paste0(study_code, '_PR_sub_', idx_file, '_hBDM.txt')
    PR_out = PR[,c('subjID', 'choice', 'outcome')]
    write.table(PR_out, filename_out, sep = '\t', row.names = FALSE)
  }
  
  # merge into a big data file
  idx_merge = which(idx_complete==1)
  data_all = read.table(paste0(study_code, '_PR_sub_', idx_merge[1], '_hBDM.txt'), header = T)
  for (ii in idx_merge[2:length(idx_merge)]){
    data_temp = read.table(paste0(study_code, '_PR_sub_', ii, '_hBDM.txt'), header = T)
    data_all = rbind(data_all, data_temp)
  }
  write.table(data_all, paste0(study_code, '_PR_data_all_hBDM.txt'), sep = '\t', row.names = FALSE)
}


# analyze_PR
# conventional PRL measure (perseveration, switch_prob)
analyze_PR <- function(data_PR){
  data_PR = data_PR$items[[1]]
  data_PR_trial  = data_PR[!is.na(data_PR$trial),]
  # set learning criteria
  learning_criteria = 8
  # calculate perseveration
  # "the number of trials until the participant updates their response after the rule reversal"
  # currently calculated as the number of trials when participant give the first "correct" response after the reversal
  perseveration = match("correct", data_PR_trial$response[41:80])
  # calculate switching probability
  # Switching probability is the number of switches in the participant responses following negative feedback (i.e. trap trials: when the less likely object is correct)
  # switchProb will be NA if the last trial of a block is a "trap" trial (no next trial)
  idx_trap = which(data_PR_trial$isTrap)
  if(max(idx_trap) == 80)
  n_trap = length(idx_trap)
  n_switch = sum(data_PR_trial$selected[idx_trap] != data_PR_trial$selected[idx_trap+1])
  switch_prob = n_switch / n_trap
  print(paste('n_switch:', n_switch))
  print(paste('n_trap:', n_trap))
  result_PR = list(perseveration = perseveration, switch_prob = switch_prob)
  result_PR
}
