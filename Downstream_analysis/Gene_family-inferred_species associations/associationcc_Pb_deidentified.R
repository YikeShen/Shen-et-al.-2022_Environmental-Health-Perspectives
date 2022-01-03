setwd("/mnt/home/shenyike/masslin2")

library("Maaslin2")
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(useful)

GESTERAW <- read.csv("GESTEY6RAW.csv")
ChildMetalRaw <- GESTERAW[,1:6]

#more covariates from the other datasheet
Covariates <- read.csv("Mothercovariates.csv")
Childrencovariates <- Covariates[,c(1:14,198)]
Childrencovariates <- Childrencovariates[-1,-1]
Childrencovariates <- Childrencovariates %>% dplyr::rename(medication_c=medication)
GESTERAWwcovaraites <- cbind(GESTERAW,Childrencovariates)


Socio <- GESTERAWwcovaraites %>% select(record_id,cadmium,manganese,mercury,lead, selenium,child_age,sexe_enfan,revenufami,ecolespeci,allaite,proxim_ind,ant_ch_bmi_imp,medication_c,ga_wks_bb)

Socioprocessing <- Socio[complete.cases(Socio[ , 2:6]),]
Socioprocessing <- Socioprocessing[,-c(2:6)]
row.names(Socioprocessing) <- Socioprocessing$record_id
Socioprocessing <- Socioprocessing %>% as.tibble() %>%
  dplyr::rename(Sex=sexe_enfan) %>% 
  dplyr::rename(Special_school=ecolespeci) %>% 
  dplyr::rename(Breastfeed=allaite) %>% 
  dplyr::rename(Family_income_Y6=revenufami) %>% 
  dplyr::rename(Children_age=child_age) %>% 
  dplyr::rename(Proximity_industry=proxim_ind) %>% 
  dplyr::rename(Gestational_age=ga_wks_bb)

Socioprocessing$ant_ch_bmi_imp <- as.numeric(Socioprocessing$ant_ch_bmi_imp)
Socioprocessing$medication_c <- as.numeric(Socioprocessing$medication_c)
Socioprocessing[54,4] <- NUMBER #PARTICIPANT_ID
Socioprocessing[45,4] <- NUMBER #PARTICIPANT_ID


map <- read_excel("GESTE_OTU.xlsx", sheet = "ENV_Children")
SAMPLEIDROWNAMES <- map$SampleID
map <- map %>% as.matrix()
row.names(map) <- SAMPLEIDROWNAMES
#row.names(map) <- paste("P",row.names(map),sep="_")
map <- map[,c(-1,-7)]
ROANAMESLISTSAMPLESID <- row.names(map)
map <- map %>% as.data.frame()


Correlationscreening <- cbind(map,Socioprocessing)
Correlationscreening <- Correlationscreening %>% select(-record_id)
Correlationscreening <- Correlationscreening %>% as.matrix()




dataframewithcovariates <- Correlationscreening %>%as.data.frame() %>%  select(Cd_c,Mn_c,Hg_c,Pb_c,Se_c,Breastfeed,Gestational_age,Family_income_Y6,Sex)


#below summary statistics

dataframewithcovariates$Sex[Socioprocessing$Sex == "1"] <- 'Female'
dataframewithcovariates$Sex[Socioprocessing$Sex == "2"] <- 'Male'
dataframewithcovariates$Breastfeed[dataframewithcovariates$Breastfeed == "1"] <- 'Yes'
dataframewithcovariates$Breastfeed[dataframewithcovariates$Breastfeed == "0"] <- 'No'

dataframewithcovariates[,1:5] <- lapply(dataframewithcovariates[,1:5], function(x) as.numeric(as.character(x)))
dataframewithcovariates$Family_income_Y6 <- as.numeric(dataframewithcovariates$Family_income_Y6)
dataframewithcovariates$Gestational_age <- as.numeric(dataframewithcovariates$Gestational_age)


FUNC <- lapply(list.files(pattern = "*humann_genefamily_relab.tsv"), function(i){read_tsv(i)}) 

IDS <- lapply(FUNC,function(x) substring(names(x),1,regexpr("_dehost_Abundance",names(x))-1))
IDS <- unlist(IDS)[-1]

FUNC <- lapply(FUNC, function(x)
  separate(data = x, col = "# Gene Family", into = c("protein", "gene"), sep = "\\|"))
FUNC <- lapply(FUNC, subset, is.na(gene))

FUNC <- FUNC %>%
  reduce(full_join, by = "protein")
FUNC <- FUNC[,which(!grepl("gene", colnames(FUNC)))] 

colnames(FUNC)[2:ncol(FUNC)] <- IDS
protein <- FUNC$protein
names(protein) <- ifelse(grepl(":", protein), substring(protein,1,regexpr(":",protein)-1), protein)
FUNC <- FUNC[,-1]
FUNC <- as.data.frame(t(FUNC))
FUNC[is.na(FUNC)] <- 0
colnames(FUNC) <- names(protein)


MATCHING <- rownames(dataframewithcovariates) %>% as.data.frame()
colnames(MATCHING) <- "ID"
MATCHING <- cbind(MATCHING,dataframewithcovariates)

FUNCMATCHING <- rownames(FUNC) %>% as.data.frame()
colnames(FUNCMATCHING) <- "ID"
FUNCXX <- cbind(FUNCMATCHING,FUNC)

#Matching missing samples
FUNCXX <- match_df(FUNCXX, MATCHING, on ="ID")
MATCHING <- match_df(MATCHING,FUNCXX, on ="ID")

FUNCXX <- FUNCXX[,colSums(FUNCXX == 0) <= 59]

input_metadata_phy <- MATCHING %>% as.data.frame()
input_data_phy <- FUNCXX %>% as.data.frame()
rownames(input_metadata_phy) <- rownames(input_data_phy)
input_data_phy <- input_data_phy[,-1]


#Pb
fit_data4 = Maaslin2(
    input_data = input_data_phy,
    input_metadata = input_metadata_phy,
    output = "masslin_pathway_Pb",
    fixed_effects = c("Pb_c","Breastfeed","Gestational_age","Family_income_Y6","Sex"))
FDRq_Pb <- read.table(file="./masslin_pathway_Pb/all_results.tsv",sep = '\t', header = TRUE)
FDRq_Pb <- FDRq_Pb[which(FDRq_Pb$metadata=='Pb_c', ),]
correctedq_Pb <- p.adjust(FDRq_Pb$pval, method = "fdr")
FDRq_Pb <- cbind(FDRq_Pb,correctedq_Pb)
write.table(FDRq_Pb, file = "FDRq_Pb.tsv", sep="\t")


