Downstream analysis in R

Disclaimer:\
You can not run this script without metadata, this aims to provide the workflow of our paper and offer pseudo code example for analysis.\
Request for data must go through approval with GESTE cohort, after approval I can share the original scripts with participants name and input processed sequence data.\
All participant names are de-identified as "PARTICIPANT_ID"; All name of the inputsheets are redacted as "INPUTDATA.csv"\

Code file #1 :EHP_Shen_childexposure_childmicrobiome_1.Rmd \
includes:
- child exposure-child microbiome
- data clean up
- summary statistics
- alpha diversity (16S rRNA gene amplicon sequencing)
- beta diversity (16S rRNA gene amplicon sequencing)
- taxa (phylum & family association) (16S rRNA gene amplicon sequencing)
- metaphlan species association (shotgun metagenome sequencing)
- alpha diveristy sensitivity analysis (16S rRNA gene amplicon sequencing)


Code file #2 :EHP_Shen_perinatalexposure_childmicrobiome_2.Rmd \
includes:
- Perinatal exposure-child microbiome
- data clean up
- summary statistics
- alpha diversity (16S rRNA gene amplicon sequencing)
- alpha diveristy sensitivity analysis (16S rRNA gene amplicon sequencing)
- beta diversity (16S rRNA gene amplicon sequencing)
- taxa (phylum & family association) (16S rRNA gene amplicon sequencing)
- metaphlan species association (shotgun metagenome sequencing)


Code file #3 :EHP_Shen_pathway_childexposure_childmicrobiome_3.Rmd \
includes:
- child exposure-child microbiome
- Potential functional pathway analysis

Code file #4 :EHP_Shen_pathway_perinatalexposure_childmicrobiome_4.Rmd \
includes:
- perinatal exposure-child microbiome
- Potential functional pathway analysis
