# Jialuo Ding 44732024
# This file aim to pre-process the data

# Extract data
bcw<-read.table("./data/breast-cancer-wisconsin.data", sep = ',')

# Assign names to variables
names(bcw)<-c("Sample.code.number", "Clump.Thickness", "Uniformity.of.Cell.Size",
              "Uniformity.of.Cell.Shape", "Marginal.Adhesion", "Single.Epithelial.Cell.Size",
              "Bare.Nuclei", "Bland.Chromatin", "Normal.Nucleoli", "Mitoses", "Class")

# Check if there is a missing data
apply(is.na(bcw),2,sum)

# Check all columns' types
sapply(bcw, class)

# Change the types of Bare.Nuclei and Classs
bcw$Bare.Nuclei = as.integer(as.character(bcw$Bare.Nuclei))
bcw$Class = as.factor(as.character(bcw$Class))

# Check if there is a missing data again
apply(is.na(bcw),2,sum)

# Remove all rows with missing values
complete_bcw <- na.omit(bcw)

# Check the pre-process result
apply(is.na(complete_bcw),2,sum)
sapply(complete_bcw, class)
new_bcw <- bcw[rowSums(is.na(bcw)) > 0,]
nrow(new_bcw)
nrow(complete_bcw)
nrow(bcw)

# Remove the first column
pre_bcw <- complete_bcw[-1]
ncol(pre_bcw)

# Save the dataframe into a file with filename bcw_processed.Rda
saveRDS(pre_bcw, file = "./data/bcw_processed.Rda")

