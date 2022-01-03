Gene family-inferred species association consists of ~530,000 gene families. Therefore, R script for each metal runs over 15 hours. It is highly suggested to run in high performance computing clusters (HPC)\
The scripts are very similar MaAsLin2 based association screnning, with exposures changing to different metals and metadata changing to different timepoints (i.e., perinatal & childhood)\
Steps:
1. Running individual R scripts in HPC
2. Download raw output from HPC to local computer
3. Run R markdown (RMD) file for for quality control to remove false associations (i.e., non-identified species, less than 10% species present, and fitness of associations) & data cleanup
4. Run the R script for visualization (Figure 4)

file name: cc denotes child exposure-child microbiome; mc denotes perinatal exposure-child microbiome. All participants are de-dentified.
