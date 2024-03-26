
# User's guide for SemNet packages given in:
# Christensen & Kenett 2021 - https://psycnet.apa.org/doiLanding?doi=10.1037%2Fmet0000463
# also on psyArXiv - https://psyarxiv.com/eht87/

#NOTES:
# 1. the function textcleaner() launches a semi-automated (partly manual) text cleaning interface in the console
# 2. the function SemNeTShiny() launches a GUI for network estimation. Before launching, change the assignment 
# "group <- group_animals_xxxxx" to the desired comparison variable (e.g., group_animals_CF) 
# 3. For creating network figures, change "resultShiny$network$xxxxx" to the desired group name (e.g., "CF_flexible")

# ================================================================================================
# Load packages
library(SemNetDictionaries) 
library(SemNetCleaner) 
library(SemNeT)
library(readxl)
library(tidyr)
library(stringr)
library(igraph)
library(dplyr)

VFdata_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/CREATIVITY/VF_355transcripts/" # human-transcribed verbal fluency data
keyvariables_core_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/final data/wp01_adult_bootcamp3_data_merged_core.csv"
WM_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/final data/FS_WM.csv" # working memory factor scores (with a column for participant IDs)
GCA_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/final data/FS_GCA.csv" # IQ factor score ("general cognitive ability")
inhib_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/final data/FS_EF_four_factors.csv" # CSV contains several EF factor scores but using only Inhibition here
output_path <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/CREATIVITY/semNA output/" # output directory

WM_fscores <- read.csv(WM_path)
IQ_fscores <- read.csv(GCA_path)
inhib_fscores <- read.csv(inhib_path)[c("ID","Inhibition")]
data_adult <- read.csv(keyvariables_core_path)[c("ID","FS_CF_1","FS_CF_2separate","WASI_FSIQ","APM18_n_correct")] 
data_adult <- cbind(data_adult, "WM"=WM_fscores$FS_WM[match(data_adult$ID, WM_fscores$ID)], "IQ"=IQ_fscores$FS_GCA[match(data_adult$ID, IQ_fscores$ID)], "inhib"=inhib_fscores$Inhibition[match(data_adult$ID, inhib_fscores$ID)])

filename_all <- list.files(VFdata_path, pattern = ".xlsx")
filename_all <- filename_all[!str_detect(filename_all, "~\\$")] # some duplicate files with "~$" at the start get added, so removing 
n_subj <- length(filename_all)
VF_responses_animals <- as.data.frame(matrix(NA, n_subj, 200))

CF_group <- ifelse(data_adult$FS_CF_1 > mean(data_adult$FS_CF_1, na.rm=T), "CF_high", "CF_low")
invTemp_group <- ifelse(data_adult$FS_CF_2separate > mean(data_adult$FS_CF_2separate, na.rm=T), "invTemp_high", "invTemp_low")
WM_group <- ifelse(data_adult$WM > mean(data_adult$WM, na.rm=T), "WM_high", "WM_low")
IQ_group <- ifelse(data_adult$IQ > mean(data_adult$IQ, na.rm=T), "IQ_high", "IQ_low")
factorscores_groups <- data.frame("ID"=data_adult$ID, CF_group, invTemp_group, WM_group, IQ_group)


# Sheet 1, Category Fluency "animals"
for (i in 1:n_subj){
  print(i)
  print(paste(i, " -- ", filename_all[i]))
  dat <- read_excel(paste0(VFdata_path, filename_all[i]), sheet=1, col_names=F) 
  ID <- str_extract(filename_all[i], "\\d+") # extracting the ID from the filename
  responserows <- dplyr::pull(dat[,1]) %in% c(as.character(1:1000), paste0(as.character(1:1000), ".0")) # allowing for numeric-characters ending with ".0" 
  responseinds <- which(responserows==T)
  validinds_col3 <- responseinds[pull(dat[responserows,3])==1 | pull(dat[responserows,3])=="1.0"] # need both col3 and col4 because validity coding is on one or the other
  validinds_col4 <- responseinds[pull(dat[responserows,4])==1 | pull(dat[responserows,4])=="1.0"]
  validinds <- na.omit(c(validinds_col3, validinds_col4))
  if (length(validinds)>0){ VF_responses_animals[i,1:(length(validinds)+1)] <- c(ID, pull(dat[validinds,2])) } # add data if there are valid entries
  }
VF_responses_animals <-  VF_responses_animals[,which(colSums(is.na(VF_responses_animals)) < n_subj)] # cutting away columns with no responses
responsenums <- formatC(1:(dim(VF_responses_animals)[2]-1), width = 2, format = "d", flag = "0")
colnames(VF_responses_animals) <- c("ID", paste0("response_", responsenums)) 
factorscores_groups_matched <- factorscores_groups[match(VF_responses_animals$ID, factorscores_groups$ID),][,-which(colnames(factorscores_groups)=="ID")]

VF_responses_animals <- data.frame(VF_responses_animals, factorscores_groups_matched)
outputdir <- "/Users/ryutaro/Desktop/CLIC/WP01 data analysis/creativity/adult VF processed for SemNet/"
write.csv(VF_responses_animals, paste0(outputdir,"VF_responses_animals_new.csv"))

# -------
load.dictionaries("animals")
VF_responses_animals_noDemog <- VF_responses_animals[,colnames(VF_responses_animals) %in% c("ID", paste0("response_", responsenums)) ]


VF_animals_cleaned <- textcleaner(data = VF_responses_animals_noDemog, miss = "<NA>", partBY = "row", dictionary = "animals", spelling="UK")
# ^^^ key function


VF_animals_cleaned_CF <- VF_animals_cleaned
VF_animals_cleaned_invTemp <- VF_animals_cleaned
VF_animals_cleaned_WM <- VF_animals_cleaned
VF_animals_cleaned_IQ <- VF_animals_cleaned

rowsTF_animals_CF <- !is.na(VF_responses_animals$CF_group)
rowsTF_animals_invTemp <- !is.na(VF_responses_animals$invTemp_group)
rowsTF_animals_WM <- !is.na(VF_responses_animals$WM_group)
rowsTF_animals_IQ <- !is.na(VF_responses_animals$IQ_group)

VF_animals_cleaned_CF$responses$original <- VF_animals_cleaned$responses$original[rowsTF_animals_CF,]
VF_animals_cleaned_CF$responses$clean <- VF_animals_cleaned$responses$clean[rowsTF_animals_CF,]
VF_animals_cleaned_CF$responses$binary <- VF_animals_cleaned$responses$binary[rowsTF_animals_CF,]
VF_animals_cleaned_CF$behavioral <- VF_animals_cleaned$behavioral[rowsTF_animals_CF,]
VF_animals_cleaned_invTemp$responses$original <- VF_animals_cleaned$responses$original[rowsTF_animals_invTemp,]
VF_animals_cleaned_invTemp$responses$clean <- VF_animals_cleaned$responses$clean[rowsTF_animals_invTemp,]
VF_animals_cleaned_invTemp$responses$binary <- VF_animals_cleaned$responses$binary[rowsTF_animals_invTemp,]
VF_animals_cleaned_invTemp$behavioral <- VF_animals_cleaned$behavioral[rowsTF_animals_invTemp,]
VF_animals_cleaned_WM$responses$original <- VF_animals_cleaned$responses$original[rowsTF_animals_WM,]
VF_animals_cleaned_WM$responses$clean <- VF_animals_cleaned$responses$clean[rowsTF_animals_WM,]
VF_animals_cleaned_WM$responses$binary <- VF_animals_cleaned$responses$binary[rowsTF_animals_WM,]
VF_animals_cleaned_WM$behavioral <- VF_animals_cleaned$behavioral[rowsTF_animals_WM,]
VF_animals_cleaned_IQ$responses$original <- VF_animals_cleaned$responses$original[rowsTF_animals_IQ,]
VF_animals_cleaned_IQ$responses$clean <- VF_animals_cleaned$responses$clean[rowsTF_animals_IQ,]
VF_animals_cleaned_IQ$responses$binary <- VF_animals_cleaned$responses$binary[rowsTF_animals_IQ,]
VF_animals_cleaned_IQ$behavioral <- VF_animals_cleaned$behavioral[rowsTF_animals_IQ,]


group_animals_CF <- VF_responses_animals$CF[!is.na(VF_responses_animals$CF_group)]
group_animals_invTemp <- VF_responses_animals$invTemp_group[!is.na(VF_responses_animals$invTemp_group)]
group_animals_WM <- VF_responses_animals$WM_group[!is.na(VF_responses_animals$WM_group)]
group_animals_IQ <- VF_responses_animals$IQ_group[!is.na(VF_responses_animals$IQ_group)]

VF_animals_cleaned_CF_shared <- VF_animals_cleaned_CF
VF_animals_cleaned_invTemp_shared <- VF_animals_cleaned_invTemp
VF_animals_cleaned_WM_shared <- VF_animals_cleaned_WM
VF_animals_cleaned_IQ_shared <- VF_animals_cleaned_IQ


#--- using only responses shared across both groups, necessary for Correlation-based Networks---
unique_responses_CFinflexible <- unique(c(VF_animals_cleaned_CF$responses$clean[group_animals_CF=="CF_inflexible",]))
unique_responses_CFflexible <- unique(c(VF_animals_cleaned_CF$responses$clean[group_animals_CF=="CF_flexible",]))
unshared_responses_CF <- setdiff(c(unique_responses_CFinflexible, unique_responses_CFflexible), intersect(unique_responses_CFinflexible, unique_responses_CFflexible))
animals_CF_nonshared_ind <- which(VF_animals_cleaned_CF_shared$responses$clean %in% unshared_responses_CF)
VF_animals_cleaned_CF_shared$responses$clean[animals_CF_nonshared_ind] <- NA
animals_CF_nonshared_binary_colind <- which(colnames(VF_animals_cleaned_CF_shared$responses$binary) %in% unshared_responses_CF)
VF_animals_cleaned_CF_shared$responses$binary <- VF_animals_cleaned_CF_shared$responses$binary[,-animals_CF_nonshared_binary_colind]

unique_responses_invTempHot <- unique(c(VF_animals_cleaned_invTemp$responses$clean[group_animals_invTemp=="invTemp_hot",]))
unique_responses_invTempCool <- unique(c(VF_animals_cleaned_invTemp$responses$clean[group_animals_invTemp=="invTemp_cool",]))
unshared_responses_invTemp <- setdiff(c(unique_responses_invTempHot, unique_responses_invTempCool), intersect(unique_responses_invTempHot, unique_responses_invTempCool))
animals_invTemp_nonshared_ind <- which(VF_animals_cleaned_invTemp_shared$responses$clean %in% unshared_responses_invTemp)
VF_animals_cleaned_invTemp_shared$responses$clean[animals_invTemp_nonshared_ind] <- NA
animals_invTemp_nonshared_binary_colind <- which(colnames(VF_animals_cleaned_invTemp_shared$responses$binary) %in% unshared_responses_invTemp)
VF_animals_cleaned_invTemp_shared$responses$binary <- VF_animals_cleaned_invTemp_shared$responses$binary[,-animals_invTemp_nonshared_binary_colind]

unique_responses_WMhigh <- unique(c(VF_animals_cleaned_WM$responses$clean[group_animals_WM=="WM_high",]))
unique_responses_WMlow <- unique(c(VF_animals_cleaned_WM$responses$clean[group_animals_WM=="WM_low",]))
unshared_responses_WM <- setdiff(c(unique_responses_WMhigh, unique_responses_WMlow), intersect(unique_responses_WMhigh, unique_responses_WMlow))
animals_WM_nonshared_ind <- which(VF_animals_cleaned_WM_shared$responses$clean %in% unshared_responses_WM)
VF_animals_cleaned_WM_shared$responses$clean[animals_WM_nonshared_ind] <- NA
animals_WM_nonshared_binary_colind <- which(colnames(VF_animals_cleaned_WM_shared$responses$binary) %in% unshared_responses_WM)
VF_animals_cleaned_WM_shared$responses$binary <- VF_animals_cleaned_WM_shared$responses$binary[,-animals_WM_nonshared_binary_colind]

unique_responses_IQhigh <- unique(c(VF_animals_cleaned_IQ$responses$clean[group_animals_IQ=="IQ_high",]))
unique_responses_IQlow <- unique(c(VF_animals_cleaned_IQ$responses$clean[group_animals_IQ=="IQ_low",]))
unshared_responses_IQ <- setdiff(c(unique_responses_IQhigh, unique_responses_IQlow), intersect(unique_responses_IQhigh, unique_responses_IQlow))
animals_IQ_nonshared_ind <- which(VF_animals_cleaned_IQ_shared$responses$clean %in% unshared_responses_IQ)
VF_animals_cleaned_IQ_shared$responses$clean[animals_IQ_nonshared_ind] <- NA
animals_IQ_nonshared_binary_colind <- which(colnames(VF_animals_cleaned_IQ_shared$responses$binary) %in% unshared_responses_IQ)
VF_animals_cleaned_IQ_shared$responses$binary <- VF_animals_cleaned_IQ_shared$responses$binary[,-animals_IQ_nonshared_binary_colind]

#---- Activate Shiny app

group <- group_animals_CF # set variable "group" to desired grouping vector for SemNeTShiny()
SemNeTShiny() # Launch SemNeT Shiny Application for network estimation

# from Network Estimation section
network_variable_high <-  resultShiny$network$CF_flexible
network_variable_low <-  resultShiny$network$CF_inflexible
# Convert networks to igraph's format 
igraph_variable_high <- convert2igraph(network_variable_high)
igraph_variable_low <-  convert2igraph(network_variable_low) 

plot(igraph_variable_high, edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray", vertex.label.color="black", edge.color="gray70",
     vertex.label.cex=.8, vertex.label.dist=1, edge.curved=0.2,layout=layout_with_mds)

plot(igraph_variable_low, edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray", vertex.label.color="black", edge.color="gray70",
     vertex.label.cex=.8, vertex.label.dist=1, edge.curved=0.2,layout=layout_with_mds)

write.csv(convert2cytoscape(network_variable_high), file = paste0(output_path,"cytoscape_CF_CbN_high.csv"), row.names = FALSE) 
write.csv(convert2cytoscape(network_variable_low), file = paste0(output_path,"cytoscape_CF_CbN_low.csv"), row.names = FALSE) 


# Plot with no vertex labels
plot(igraph_variable_high, edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray20", vertex.label.color="black", edge.color="gray50",
     vertex.label.cex=.8, vertex.label.dist=1, edge.curved=0.2,layout=layout_with_mds,
     vertex.label=NA)

plot(igraph_variable_low, edge.arrow.size=.5, vertex.color="gold", vertex.size=3, 
     vertex.frame.color="gray20", vertex.label.color="black", edge.color="gray50",
     vertex.label.cex=.8, vertex.label.dist=1, edge.curved=0.2,layout=layout_with_mds,
     vertex.label=NA)












