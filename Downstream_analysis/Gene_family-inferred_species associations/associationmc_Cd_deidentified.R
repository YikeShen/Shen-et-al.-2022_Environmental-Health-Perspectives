setwd("/mnt/home/shenyike/masslin2_mc")

library("Maaslin2")
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(useful)

GESTERAW <- read.csv("GESTERAWMothers.csv")
GESTESEX <- read.csv("GESTEY6RAW.csv") %>% select(record_id,sexe_enfan)


IDLIST <- GESTERAW[,1] %>% as.data.frame()
Deliverycon <- GESTERAW[,c(7:11)] %>% as.data.frame()
ChildMetalRaw <- c(IDLIST,Deliverycon) %>% as.data.frame()

ChildMetalprocessing<- apply(ChildMetalRaw, 2, function(x) gsub("^$|^ $", NA, x)) %>% as.data.frame()

ChildMetalprocess <- ChildMetalprocessing %>% as.tibble() %>%
  dplyr::rename(Cd_D=cd_acc) %>% 
  dplyr::rename(Mn_D=mn_acc) %>% 
  dplyr::rename(Hg_D=hgt_acc) %>% 
  dplyr::rename(Pb_D=pb_acc) %>% 
  dplyr::rename(Se_D=se_acc) %>% 
  dplyr::rename(SampleID=".")

#more covariates from the other datasheet
Covariates <- read.csv("Mothercovariates.csv")
mothercovariates <- Covariates[,c(15:197,198)]
#mothercovariates <- mothercovariates[-1,]



GESTERAWwcovaraites <- cbind(ChildMetalprocess,mothercovariates)
GESTERAWwcovaraites <- GESTERAWwcovaraites[-1,]
GESTERAWwcovaraites <- cbind(GESTERAWwcovaraites,GESTESEX)

#############Diagnoses################
#pregnancy related diagnoses
diagnosesprg <- GESTERAWwcovaraites[,8:9]
diagnosesprg[] <- lapply(diagnosesprg, function(x) as.numeric(as.character(x)))
diagnosesprglist <- rowSums(diagnosesprg) %>% as.data.frame()
colnames(diagnosesprglist) <- "DiagnoseIndex_prgnancy"

#preexisting diagnoses
diagnoses <- GESTERAWwcovaraites[,10:16]
diagnoses[] <- lapply(diagnoses, function(x) as.numeric(as.character(x)))
diagnoseslist <- rowSums(diagnoses) %>% as.data.frame()
colnames(diagnoseslist) <- "DiagnoseIndex_preexisting"

diagnoseTWO <- cbind(diagnosesprglist,diagnoseslist)
diagnoseTWO[] <- lapply(diagnoseTWO, function(x) as.factor(as.numeric(x)))

####################Exposure###############
riskfactors <- GESTERAWwcovaraites[,46:157]
riskfactors[] <- lapply(riskfactors, function(x) as.numeric(as.character(x)))

num_column2<-112
num_row2<-86

sum_df<-data.frame(matrix(0,nrow=86,ncol=28))
for (i in 1:num_row2){
  for (j in 1:((num_column2)/4)){
    Process_vector<-riskfactors[i, c(j*4-3,j*4-2,j*4-1,j*4)]
      sumdiagnose <- rowSums(Process_vector)
    sum_df[i,j] = sumdiagnose
  }
}
   
Colnamesexposure <- cbind("dust","smoke","emanations","odors","combustion","leather_dust","printing_inks", "metal_dust", "tars/bitumen", "oils/grease", "fuel/heating_oil","non-flammable_sheaths","fertilizers", "pesticides","paints", "photographic_goods", "colles","solvent/degreasing_fluids", "radiation", "industrial_phamaceuticals", "industrial_food_products", "antioxidnts","dyes", "heat", "cold", "temp","humidity", "noise")

colnames(sum_df) <- Colnamesexposure
sum_df[] <- lapply(sum_df, function(x) as.factor(as.numeric(x)))

importantcov <- GESTERAWwcovaraites %>% select(SampleID,acc_cs,bmipostpregn,revenu_delivery,ga_wks_bb,sexe_enfan) %>% 
  dplyr::rename(Delivery_mode=acc_cs) %>% 
  dplyr::rename(BMI_delivery=bmipostpregn) %>% 
  dplyr::rename(Family_Income=revenu_delivery) %>% 
  dplyr::rename(Gestational_age=ga_wks_bb) %>% 
  dplyr::rename(Sex=sexe_enfan)

importantcov <- cbind(importantcov,diagnoseTWO)

#Missing income, impute from year6 income, for those without year6 income, impute with median 65000
importantcov[11,4] <- NUMBER #participant PARTICIPANT_ID
importantcov[16,4] <- NUMBER #participant PARTICIPANT_ID
importantcov[32,4] <- NUMBER #participant PARTICIPANT_ID, NA fron Y6 impute with median
importantcov[35,4] <- NUMBER #participant PARTICIPANT_ID
importantcov[62,4] <- NUMBER #participant PARTICIPANT_ID
importantcov[82,4] <- NUMBER #participant PARTICIPANT_ID, NA from Y6 impute with median



importantcov$Sex[importantcov$Sex == "1"] <- 'Female'
importantcov$Sex[importantcov$Sex == "2"] <- 'Male'



###############
importantcov$DiagnoseIndex_prgnancy <- as.character(importantcov$DiagnoseIndex_prgnancy)
importantcov$DiagnoseIndex_prgnancy[importantcov$DiagnoseIndex_prgnancy == "1"] <- 'Yes'
importantcov$DiagnoseIndex_prgnancy[importantcov$DiagnoseIndex_prgnancy == "2"] <- 'Yes'
importantcov$DiagnoseIndex_prgnancy[importantcov$DiagnoseIndex_prgnancy == "0"] <- 'No'

importantcov$DiagnoseIndex_preexisting <- as.character(importantcov$DiagnoseIndex_preexisting)
importantcov$DiagnoseIndex_preexisting[importantcov$DiagnoseIndex_preexisting == "1"] <- 'Yes'
importantcov$DiagnoseIndex_preexisting[importantcov$DiagnoseIndex_preexisting == "2"] <- 'Yes'
importantcov$DiagnoseIndex_preexisting[importantcov$DiagnoseIndex_preexisting == "3"] <- 'Yes'
importantcov$DiagnoseIndex_preexisting[importantcov$DiagnoseIndex_preexisting == "0"] <- 'No'

Socio <- GESTERAWwcovaraites %>% select(SampleID,Cd_D,Mn_D,Hg_D,Pb_D, Se_D,age_deliverymoth)



#GESTERAWwcovaraites
covariatesmotherdelivery <- cbind(Socio,importantcov)

Socioprocessing <- covariatesmotherdelivery[complete.cases(covariatesmotherdelivery[ , 2:6]),]


Socioprocessing <- Socioprocessing[,-c(2:6)]
Socioprocessing <- Socioprocessing %>% as.tibble() %>% dplyr::rename(Age=age_deliverymoth)
Socioprocessing <- Socioprocessing[,-3]


row.names(Socioprocessing) <- Socioprocessing$SampleID
#######
Socioprocessing[,c(1:2,4:6)] <- lapply(Socioprocessing[,c(1:2,4:6)], function(x) as.numeric(as.character(x)))
Socioprocessing$Delivery_mode[Socioprocessing$Delivery_mode == "1"] <- 'Vaginal'
Socioprocessing$Delivery_mode[Socioprocessing$Delivery_mode == "2"] <- 'C-Section'
row.names(Socioprocessing) <- Socioprocessing$SampleID


map <- read_excel("GESTE_OTU.xlsx", sheet = "ENV_MotherD")
SAMPLEIDROWNAMES <- map$SampleID
map <- map %>% as.matrix()
row.names(map) <- SAMPLEIDROWNAMES
#row.names(map) <- paste("P",row.names(map),sep="_")
map <- map[,c(-1,-7)]
ROANAMESLISTSAMPLESID <- row.names(map)
map <- map %>% as.data.frame()


dataframewcovariates <- cbind(map,Socioprocessing)
dataframewcovariates <- dataframewcovariates[,-6]
#dataframewcovariates <- dataframewcovariates %>% as.matrix()
dataframewcovariates[,1:5] <- lapply(dataframewcovariates[,1:5], function(x) as.numeric(x))


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


MATCHING <- rownames(dataframewcovariates) %>% as.data.frame()
colnames(MATCHING) <- "ID"
MATCHING <- cbind(MATCHING,dataframewcovariates)

FUNCMATCHING <- rownames(FUNC) %>% as.data.frame()
colnames(FUNCMATCHING) <- "ID"
FUNCXX <- cbind(FUNCMATCHING,FUNC)

#Matching missing samples
FUNCXX <- match_df(FUNCXX, MATCHING, on ="ID")
MATCHING <- match_df(MATCHING,FUNCXX, on ="ID")

FUNCXX <- FUNCXX[,colSums(FUNCXX == 0) <= 61]

input_metadata_phy <- MATCHING %>% as.data.frame()
input_data_phy <- FUNCXX %>% as.data.frame()
rownames(input_metadata_phy) <- rownames(input_data_phy)
input_data_phy <- input_data_phy[,-1]

#remove outlier
input_metadata_phy <- input_metadata_phy[!(row.names(input_metadata_phy) %in% "PARTICIPANT_ID"), ]
input_data_phy <- input_data_phy[!(row.names(input_data_phy) %in% "PARTICIPANT_ID"), ]

input_metadata_phy <- input_metadata_phy[!(row.names(input_metadata_phy) %in% "PARTICIPANT_ID"), ]
input_data_phy <- input_data_phy[!(row.names(input_data_phy) %in% "PARTICIPANT_ID"), ]

write.table(input_data_phy, file = "input_data_gene_Cd_mc.tsv", sep="\t")
write.table(input_metadata_phy, file = "input_metadata_Cd_mc.tsv", sep="\t")


#Cd NONE
fit_data1 = Maaslin2(
    input_data = input_data_phy,
    input_metadata = input_metadata_phy,
    output = "masslin_pathway_Cd_mother",
    fixed_effects = c("Cd_D_c","Delivery_mode","BMI_delivery","DiagnoseIndex_prgnancy","DiagnoseIndex_preexisting","Sex","Family_Income"))
FDRq_Cd <- read.table(file="./masslin_pathway_Cd_mother/all_results.tsv",sep = '\t', header = TRUE)
FDRq_Cd <- FDRq_Cd[which(FDRq_Cd$metadata=='Cd_D_c', ),]
correctedq_Cd <- p.adjust(FDRq_Cd$pval, method = "fdr")
FDRq_Cd <- cbind(FDRq_Cd,correctedq_Cd)
write.table(FDRq_Cd, file = "FDRq_Cd.tsv", sep="\t")



