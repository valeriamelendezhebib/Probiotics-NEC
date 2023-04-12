#Continuation of data_manipulation.R

#Goal: Subset amplicon GI data into segments 

#Subset metadata
metadata_amplicon_GI_data_Stom <- metadata_amplicon_GI_data[grep("Stomach Contents", metadata_amplicon_GI_data$`Sample type`), ]
metadata_amplicon_GI_data_SI <- metadata_amplicon_GI_data[grep("SmallIntestine Contents", metadata_amplicon_GI_data$`Sample type`), ]
metadata_amplicon_GI_data_Col <- metadata_amplicon_GI_data[grep("Colon Contents", metadata_amplicon_GI_data$`Sample type`), ]
# view(metadata_amplicon_GI_data_Col)
# view(metadata_amplicon_GI_data_SI)
# view(metadata_amplicon_GI_data_Stom)

#Make amplicon tables match metadata samples
amplicon_table_GI_data_tax_Stom <- amplicon_table_GI_data_tax[, metadata_amplicon_GI_data_Stom$Sample_ID]
amplicon_table_GI_data_tax_SI <- amplicon_table_GI_data_tax[, metadata_amplicon_GI_data_SI$Sample_ID]
amplicon_table_GI_data_tax_Col <- amplicon_table_GI_data_tax[, metadata_amplicon_GI_data_Col$Sample_ID]

#Prepare to make summary barplot displaying most abundant taxa

#Fix diagnosis and treatment group codes

metadata_amplicon_GI_data_Stom$Diagnosis <- sub("Y", "NEC", metadata_amplicon_GI_data_Stom$Diagnosis,ignore.case = FALSE, perl = FALSE,
                                                fixed = TRUE, useBytes = FALSE)
metadata_amplicon_GI_data_Stom$Diagnosis <- sub("N$", "Healthy", metadata_amplicon_GI_data_Stom$Diagnosis)
metadata_amplicon_GI_data_Stom$Treatment_Group <- sub("HDM", "DHM", metadata_amplicon_GI_data_Stom$Treatment_Group)
metadata_amplicon_GI_data_Stom$Treatment_Group <- sub("EP", "Formula", metadata_amplicon_GI_data_Stom$Treatment_Group)

#The $ is telling it to substitute the N for healthy only when it is the only letter and its not part of a bigger word 

metadata_amplicon_GI_data_SI$Diagnosis <- sub("Y", "NEC", metadata_amplicon_GI_data_SI$Diagnosis,ignore.case = FALSE, perl = FALSE,
                                              fixed = TRUE, useBytes = FALSE)

metadata_amplicon_GI_data_SI$Diagnosis <- sub("N$", "Healthy", metadata_amplicon_GI_data_SI$Diagnosis)
view(metadata_amplicon_GI_data_SI)
metadata_amplicon_GI_data_SI$Treatment_Group <- sub("HDM", "DHM", metadata_amplicon_GI_data_SI$Treatment_Group)
metadata_amplicon_GI_data_SI$Treatment_Group <- sub("EP", "Formula", metadata_amplicon_GI_data_SI$Treatment_Group)


metadata_amplicon_GI_data_Col$Diagnosis <- sub("Y", "NEC", metadata_amplicon_GI_data_Col$Diagnosis,ignore.case = FALSE, perl = FALSE,
                                               fixed = TRUE, useBytes = FALSE)

metadata_amplicon_GI_data_Col$Diagnosis <- sub("N$", "Healthy", metadata_amplicon_GI_data_Col$Diagnosis)
metadata_amplicon_GI_data_Col$Treatment_Group <- sub("HDM", "DHM", metadata_amplicon_GI_data_Col$Treatment_Group)
metadata_amplicon_GI_data_Col$Treatment_Group <- sub("EP", "Formula", metadata_amplicon_GI_data_Col$Treatment_Group)

view(metadata_amplicon_GI_data_Col)

#STOM

# Remove the "Unassigned" taxon from the count data table
# view(amplicon_table_GI_data_tax_Stom)
amplicon_table_GI_data_tax_Stom <- amplicon_table_GI_data_tax_Stom %>%
  dplyr::filter(rownames(amplicon_table_GI_data_tax_Stom) != "Unassigned")

#Calculate relative abundance 
amplicon_table_GI_data_tax_Stom_relabund <- as.data.frame(apply(amplicon_table_GI_data_tax_Stom, 2, function(x) x/sum(x)))


# Set taxa strings as new column and separate into constituent parts
amplicon_table_GI_data_tax_Stom_relabund_temp <- amplicon_table_GI_data_tax_Stom_relabund %>%
  rownames_to_column("Taxon") %>%
  separate("Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";")

# Manually adjust any "NA" values at each taxonomic level to be "Other" strings at this level #I should change this to unassigned 
amplicon_table_GI_data_tax_Stom_relabund_temp$Domain[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Domain)] <- "D_0__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Phylum[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Phylum)] <- "D_1__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Class[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Class)] <- "D_2__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Order[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Order)] <- "D_3__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Family[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Family)] <- "D_4__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Genus[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Genus)] <- "D_5__Unassigned"
amplicon_table_GI_data_tax_Stom_relabund_temp$Species[is.na(amplicon_table_GI_data_tax_Stom_relabund_temp$Species)] <- "D_6__Unassigned"


#Delete the D_5__ from Genus 
amplicon_table_GI_data_tax_Stom_relabund_temp$Genus <- sub("D_5__", "", amplicon_table_GI_data_tax_Stom_relabund_temp$Genus)
# view(amplicon_table_GI_data_tax_Stom_relabund_temp)

# Process the table to unite at the given taxonomic level in the loop, then to summarise and aggregate abundances
# at that level. Finally, sort by descending sum relative abundance and retain only the Top 10 taxa

amplicon_table_GI_data_tax_Stom_relabund_temp <- amplicon_table_GI_data_tax_Stom_relabund_temp %>%
  dplyr::select(c("Genus", c(colnames( amplicon_table_GI_data_tax_Stom_relabund)))) %>%
  dplyr::group_by(Genus) %>%
  summarise_all(list(sum)) %>%
  column_to_rownames("Genus") %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  dplyr::arrange(desc(Sum)) %>%
  slice(1:10)

# view( amplicon_table_GI_data_tax_Stom_relabund_temp)


# Remove the Sum column that was created
amplicon_table_GI_data_tax_Stom_relabund_temp$Sum = NULL


#Check column sums
colSums (amplicon_table_GI_data_tax_Stom_relabund_temp)

# Transpose dataframe and creat an Other category to account for all the species that did not make the Top 10 cut so we can see how much of the sample is represented by top 10
amplicon_table_GI_data_tax_Stom_relabund_temp <- as.data.frame(t(amplicon_table_GI_data_tax_Stom_relabund_temp))
amplicon_table_GI_data_tax_Stom_relabund_temp <- amplicon_table_GI_data_tax_Stom_relabund_temp %>%
  dplyr::mutate(Other = 1 - rowSums(.))

#Add sample type, diagnosis, subject id, and treatment group as metadata categories for plotting
amplicon_table_GI_data_tax_Stom_relabund_temp$Sample_Type <- metadata_amplicon_GI_data_Stom$`Sample type`
amplicon_table_GI_data_tax_Stom_relabund_temp$Diagnosis <- metadata_amplicon_GI_data_Stom$`Diagnosis`
amplicon_table_GI_data_tax_Stom_relabund_temp$Subject_ID <- metadata_amplicon_GI_data_Stom$`Subject_ID`
amplicon_table_GI_data_tax_Stom_relabund_temp$TreatmentGroup <- metadata_amplicon_GI_data_Stom$`Treatment_Group`
amplicon_table_GI_data_tax_Stom_relabund_temp$NEC_Severity_Score <- metadata_amplicon_GI_data_Stom$`NEC_Severity_Score`

# Name first column SampleID
amplicon_table_GI_data_tax_Stom_relabund_temp <- rownames_to_column(amplicon_table_GI_data_tax_Stom_relabund_temp,
                                                                    "SampleID")


# Melt the dataframe for easier plotting with ggplot; retain the new metadata categories columns
amplicon_table_GI_data_tax_Stom_relabund_temp_melt <- melt( amplicon_table_GI_data_tax_Stom_relabund_temp,
                                                            id.vars = c("Sample_Type", "SampleID", "Diagnosis", "Subject_ID", "TreatmentGroup", "NEC_Severity_Score"))
view(amplicon_table_GI_data_tax_Stom_relabund_temp_melt)

# SMALL INTESTINE 

# Remove the "Unassigned" taxon from the count data table
# view(amplicon_table_GI_data_tax_SI)
amplicon_table_GI_data_tax_SI <- amplicon_table_GI_data_tax_SI %>%
  dplyr::filter(rownames(amplicon_table_GI_data_tax_SI) != "Unassigned")

#Calculate relative abundance 
amplicon_table_GI_data_tax_SI_relabund <- as.data.frame(apply(amplicon_table_GI_data_tax_SI, 2, function(x) x/sum(x)))


# Set taxa strings as new column and separate into constituent parts
amplicon_table_GI_data_tax_SI_relabund_temp <- amplicon_table_GI_data_tax_SI_relabund %>%
  rownames_to_column("Taxon") %>%
  separate("Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";")

# Manually adjust any "NA" values at each taxonomic level to be "Unassigned" strings at this level 
amplicon_table_GI_data_tax_SI_relabund_temp$Domain[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Domain)] <- "D_0__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Phylum[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Phylum)] <- "D_1__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Class[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Class)] <- "D_2__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Order[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Order)] <- "D_3__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Family[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Family)] <- "D_4__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Genus[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Genus)] <- "D_5__Unassigned"
amplicon_table_GI_data_tax_SI_relabund_temp$Species[is.na(amplicon_table_GI_data_tax_SI_relabund_temp$Species)] <- "D_6__Unassigned"


#Delete the D_5__ from Genus 
amplicon_table_GI_data_tax_SI_relabund_temp$Genus <- sub("D_5__", "", amplicon_table_GI_data_tax_SI_relabund_temp$Genus)
# view(amplicon_table_GI_data_tax_SI_relabund_temp)

# Process the table to unite at the given taxonomic level in the loop, then to summarise and aggregate abundances
# at that level. Finally, sort by descending sum relative abundance and retain only the Top 10 taxa

amplicon_table_GI_data_tax_SI_relabund_temp <- amplicon_table_GI_data_tax_SI_relabund_temp %>%
  # unite("Taxon", Domain:all_of(taxa_level), sep = ";") %>%
  dplyr::select(c("Genus", c(colnames( amplicon_table_GI_data_tax_SI_relabund)))) %>%
  dplyr::group_by(Genus) %>%
  summarise_all(list(sum)) %>%
  column_to_rownames("Genus") %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  dplyr::arrange(desc(Sum)) %>%
  slice(1:10)
# view( amplicon_table_GI_data_tax_SI_relabund_temp)


# Remove the Sum column that was created
amplicon_table_GI_data_tax_SI_relabund_temp$Sum = NULL


#colsums
colSums (amplicon_table_GI_data_tax_SI_relabund_temp)

# Transpose dataframe and create "other" for the rest of the sample
amplicon_table_GI_data_tax_SI_relabund_temp <- as.data.frame(t(amplicon_table_GI_data_tax_SI_relabund_temp))
amplicon_table_GI_data_tax_SI_relabund_temp <- amplicon_table_GI_data_tax_SI_relabund_temp %>%
  dplyr::mutate(Other = 1 - rowSums(.))

#Add sample type, diagnosis, subject id, and treatment group as metadata categories for plotting
amplicon_table_GI_data_tax_SI_relabund_temp$Sample_Type <- metadata_amplicon_GI_data_SI$`Sample type`
amplicon_table_GI_data_tax_SI_relabund_temp$Diagnosis <- metadata_amplicon_GI_data_SI$`Diagnosis`
amplicon_table_GI_data_tax_SI_relabund_temp$Subject_ID <- metadata_amplicon_GI_data_SI$`Subject_ID`
amplicon_table_GI_data_tax_SI_relabund_temp$TreatmentGroup <- metadata_amplicon_GI_data_SI$`Treatment_Group`
amplicon_table_GI_data_tax_SI_relabund_temp$NEC_Severity_Score <- metadata_amplicon_GI_data_SI$`NEC_Severity_Score`

# Name first column SampleID
amplicon_table_GI_data_tax_SI_relabund_temp <- rownames_to_column(amplicon_table_GI_data_tax_SI_relabund_temp,
                                                                    "SampleID")


# Melt the dataframe for easier plotting with ggplot; retain "Sample_Type" and "SampleID" columns
amplicon_table_GI_data_tax_SI_relabund_temp_melt <- melt( amplicon_table_GI_data_tax_SI_relabund_temp,
                                                            id.vars = c("Sample_Type", "SampleID", "Diagnosis", "Subject_ID", "TreatmentGroup", "NEC_Severity_Score"))
# view(amplicon_table_GI_data_tax_SI_relabund_temp_melt)

#COLON

# Remove the "Unassigned" taxon from the count data table
# view(amplicon_table_GI_data_tax_Col)
amplicon_table_GI_data_tax_Col <- amplicon_table_GI_data_tax_Col %>%
  dplyr::filter(rownames(amplicon_table_GI_data_tax_Col) != "Unassigned")

#Calculate relative abundance 
amplicon_table_GI_data_tax_Col_relabund <- as.data.frame(apply(amplicon_table_GI_data_tax_Col, 2, function(x) x/sum(x)))


# Set taxa strings as new column and separate into constituent parts
amplicon_table_GI_data_tax_Col_relabund_temp <- amplicon_table_GI_data_tax_Col_relabund %>%
  rownames_to_column("Taxon") %>%
  separate("Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";")

# Manually adjust any "NA" values at each taxonomic level to be "Other" strings at this level #I should change this to unassigned 
amplicon_table_GI_data_tax_Col_relabund_temp$Domain[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Domain)] <- "D_0__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Phylum[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Phylum)] <- "D_1__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Class[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Class)] <- "D_2__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Order[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Order)] <- "D_3__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Family[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Family)] <- "D_4__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Genus[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Genus)] <- "D_5__Unassigned"
amplicon_table_GI_data_tax_Col_relabund_temp$Species[is.na(amplicon_table_GI_data_tax_Col_relabund_temp$Species)] <- "D_6__Unassigned"


#Delete the D_5__ from Genus 
amplicon_table_GI_data_tax_Col_relabund_temp$Genus <- sub("D_5__", "", amplicon_table_GI_data_tax_Col_relabund_temp$Genus)
# view(amplicon_table_GI_data_tax_Col_relabund_temp)

# Process the table to unite at the given taxonomic level in the loop, then to summarise and aggregate abundances
# at that level. Finally, sort by descending sum relative abundance and retain only the Top 10 taxa

amplicon_table_GI_data_tax_Col_relabund_temp <- amplicon_table_GI_data_tax_Col_relabund_temp %>%
  # unite("Taxon", Domain:all_of(taxa_level), sep = ";") %>%
  dplyr::select(c("Genus", c(colnames( amplicon_table_GI_data_tax_Col_relabund)))) %>%
  dplyr::group_by(Genus) %>%
  summarise_all(list(sum)) %>%
  column_to_rownames("Genus") %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  dplyr::arrange(desc(Sum)) %>%
  slice(1:10)
# view( amplicon_table_GI_data_tax_Col_relabund_temp)
#Edit the slice for realtive abundance and generate all the new plots to be absolutely sure I can use them. 

# Remove the Sum column that was created
amplicon_table_GI_data_tax_Col_relabund_temp$Sum = NULL


#colsums
colSums (amplicon_table_GI_data_tax_Col_relabund_temp)

# # Transpose dataframe and create "other" for the rest of the sample
amplicon_table_GI_data_tax_Col_relabund_temp <- as.data.frame(t(amplicon_table_GI_data_tax_Col_relabund_temp))
amplicon_table_GI_data_tax_Col_relabund_temp <- amplicon_table_GI_data_tax_Col_relabund_temp %>%
  dplyr::mutate(Other = 1 - rowSums(.))

#Add sample type, diagnosis, subject id, and treatment group as metadata categories for plotting
amplicon_table_GI_data_tax_Col_relabund_temp$Sample_Type <- metadata_amplicon_GI_data_Col$`Sample type`
amplicon_table_GI_data_tax_Col_relabund_temp$Diagnosis <- metadata_amplicon_GI_data_Col$`Diagnosis`
amplicon_table_GI_data_tax_Col_relabund_temp$Subject_ID <- metadata_amplicon_GI_data_Col$`Subject_ID`
amplicon_table_GI_data_tax_Col_relabund_temp$TreatmentGroup <- metadata_amplicon_GI_data_Col$`Treatment_Group`
amplicon_table_GI_data_tax_Col_relabund_temp$NEC_Severity_Score <- metadata_amplicon_GI_data_Col$`NEC_Severity_Score`

# Name first column SampleID
amplicon_table_GI_data_tax_Col_relabund_temp <- rownames_to_column(amplicon_table_GI_data_tax_Col_relabund_temp,
                                                                    "SampleID")


# Melt the dataframe for easier plotting with ggplot; retain "Sample_Type" and "SampleID" columns
amplicon_table_GI_data_tax_Col_relabund_temp_melt <- melt( amplicon_table_GI_data_tax_Col_relabund_temp,
                                                            id.vars = c("Sample_Type", "SampleID", "Diagnosis", "Subject_ID", "TreatmentGroup", "NEC_Severity_Score"))
view(amplicon_table_GI_data_tax_Col_relabund_temp_melt)

#Fix treatment names in all the tables

#STOMACH

amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup <- sub("BI_HMO", "FormBoth", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup <- sub("BI", "FormBI", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup <- sub("HMO", "Form3SL", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup <- sub("Formula", "Form", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)

view(amplicon_table_GI_data_tax_Stom_relabund_temp_melt)

#SMALL INTESTINE

amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup <- sub("BI_HMO", "FormBoth", amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup <- sub("BI", "FormBI", amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup <- sub("HMO", "Form3SL", amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup <- sub("Formula", "Form", amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)

view(amplicon_table_GI_data_tax_SI_relabund_temp_melt)

#COLON


amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup <- sub("BI_HMO", "FormBoth", amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup <- sub("BI", "FormBI", amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                          fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup <- sub("HMO", "Form3SL", amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                          fixed = TRUE, useBytes = FALSE)
amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup <- sub("Formula", "Form", amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup,ignore.case = FALSE, perl = FALSE,
                                                                         fixed = TRUE, useBytes = FALSE)
view(amplicon_table_GI_data_tax_Col_relabund_temp_melt)

#Run a kruskal wallis to determine differentially abundant taxa between all treatment groups with an FDR p value adjustment
kruskalTest = as.data.frame(amplicon_table_GI_data_tax_Col_relabund_temp_melt %>% 
                                      group_by(variable) %>% 
                                      kruskal_test(value ~ TreatmentGroup)) %>% adjust_pvalue(method = "fdr")


# Run a dunnTest to see which groups are different form each other
amplicon_table_GI_data_tax_Col_relabund_temp_melt %>%
  group_by(variable) %>%
  dunn_test(value ~ Diagnosis )
