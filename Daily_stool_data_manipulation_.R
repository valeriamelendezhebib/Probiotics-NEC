#Goal: Prepare daily stool sample data for plots 

class(metadata_amplicon_time_data$Timepoint)

# view(amplicon_table_time_data_tax)

metadata_amplicon_time_data$Diagnosis <- sub("Y", "NEC", metadata_amplicon_time_data$Diagnosis,ignore.case = FALSE, perl = FALSE,
                                             fixed = TRUE, useBytes = FALSE)
metadata_amplicon_time_data$Diagnosis <- sub("N$", "Healthy", metadata_amplicon_time_data$Diagnosis)
metadata_amplicon_time_data$Treatment_Group <- sub("BI_HMO", "FormBoth", metadata_amplicon_time_data$Treatment_Group)
metadata_amplicon_time_data$Treatment_Group <- sub("BI", "FormBI", metadata_amplicon_time_data$Treatment_Group)
metadata_amplicon_time_data$Treatment_Group <- sub("HMO", "Form3SL", metadata_amplicon_time_data$Treatment_Group)
metadata_amplicon_time_data$Treatment_Group <- sub("HDM", "DHM", metadata_amplicon_time_data$Treatment_Group)
metadata_amplicon_time_data$Treatment_Group <- sub("EP", "Form", metadata_amplicon_time_data$Treatment_Group)

view(metadata_amplicon_time_data)
#Change Sample_ID to ID
colnames(metadata_amplicon_time_data)[1] <- "ID"


# view(metadata_amplicon_time_data)

amplicon_table_time_data_tax <-amplicon_table_time_data_tax %>%
  dplyr::filter(rownames(amplicon_table_time_data_tax) != "Unassigned")

#Calculate relative abundance 
amplicon_table_time_data_tax_relabund <- as.data.frame(apply(amplicon_table_time_data_tax, 2, function(x) x/sum(x)))


# Set taxa strings as new column and separate into constituent parts
amplicon_table_time_data_tax_relabund_temp <- amplicon_table_time_data_tax_relabund %>%
  rownames_to_column("Taxon") %>%
  separate("Taxon", into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";")

# Manually adjust any "NA" values at each taxonomic level to be "Other" strings at this level #I should change this to unassigned 
amplicon_table_time_data_tax_relabund_temp$Domain[is.na(amplicon_table_time_data_tax_relabund_temp$Domain)] <- "D_0__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Phylum[is.na(amplicon_table_time_data_tax_relabund_temp$Phylum)] <- "D_1__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Class[is.na(amplicon_table_time_data_tax_relabund_temp$Class)] <- "D_2__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Order[is.na(amplicon_table_time_data_tax_relabund_temp$Order)] <- "D_3__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Family[is.na(amplicon_table_time_data_tax_relabund_temp$Family)] <- "D_4__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Genus[is.na(amplicon_table_time_data_tax_relabund_temp$Genus)] <- "D_5__Unassigned"
amplicon_table_time_data_tax_relabund_temp$Species[is.na(amplicon_table_time_data_tax_relabund_temp$Species)] <- "D_6__Unassigned"


#Delete the D_5__ from Genus 
amplicon_table_time_data_tax_relabund_temp$Genus <- sub("D_5__", "", amplicon_table_time_data_tax_relabund_temp$Genus)


# Process the table to unite at the given taxonomic level in the loop, then to summarise and aggregate abundances
# at that level. Finally, sort by descending sum relative abundance and retain only the Top 10 taxa

view(amplicon_table_time_data_tax_relabund_temp)
amplicon_table_time_data_tax_relabund_temp <- amplicon_table_time_data_tax_relabund_temp %>%
  # unite("Taxon", Domain:all_of(taxa_level), sep = ";") %>%
  dplyr::select(c("Genus", c(colnames(amplicon_table_time_data_tax_relabund)))) %>%
  dplyr::group_by(Genus) %>%
  summarise_all(list(sum)) %>%
  column_to_rownames("Genus") %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  dplyr::arrange(desc(Sum)) 
# %>%
#   slice(1:5)



# Remove the Sum column that was created
amplicon_table_time_data_tax_relabund_temp$Sum = NULL


#colsums
colSums (amplicon_table_time_data_tax_relabund_temp)

# Transpose dataframe and 
amplicon_table_time_data_tax_relabund_temp <- as.data.frame(t(amplicon_table_time_data_tax_relabund_temp))
amplicon_table_time_data_tax_relabund_temp <- amplicon_table_time_data_tax_relabund_temp %>%
  dplyr::mutate(Other = 1 - rowSums(.))



#Add sample type, diagnosis, subject id, and treatment group as metadata categories for plotting
amplicon_table_time_data_tax_relabund_temp$Sample_Type <- metadata_amplicon_time_data$`Sample type`
amplicon_table_time_data_tax_relabund_temp$Diagnosis <- metadata_amplicon_time_data$`Diagnosis`
amplicon_table_time_data_tax_relabund_temp$Subject_ID <- metadata_amplicon_time_data$`Subject_ID`
amplicon_table_time_data_tax_relabund_temp$Treatment_Group <- metadata_amplicon_time_data$`Treatment_Group`
amplicon_table_time_data_tax_relabund_temp$Timepoint <- metadata_amplicon_time_data$`Timepoint`
amplicon_table_time_data_tax_relabund_temp$NEC_Severity_Score <- metadata_amplicon_time_data$`NEC_Severity_Score`

view(amplicon_table_time_data_tax_relabund_temp)
# Name first column SampleID
amplicon_table_time_data_tax_relabund_temp <- rownames_to_column(amplicon_table_time_data_tax_relabund_temp,
                                                                   "ID")
# view(amplicon_table_time_data_tax_relabund_temp)

# Melt the dataframe for easier plotting with ggplot; retain the new metadata categories columns
amplicon_table_time_data_tax_relabund_temp_melt <- melt(amplicon_table_time_data_tax_relabund_temp,
                                                          id.vars = c("Sample_Type", "ID", "Diagnosis", "Subject_ID", "Treatment_Group", "NEC_Severity_Score", "Timepoint"))
view(amplicon_table_time_data_tax_relabund_temp_melt)


## More prep for plots

#Create new variable making treatment group a factor to control the order and color
amplicon_table_time_data_tax_relabund_temp_melt$Treatment_Group_f = factor(amplicon_table_time_data_tax_relabund_temp_melt$Treatment_Group, levels = c( "DHM", "Form", "Form3SL", "FormBI", "FormBoth"))


# view(amplicon_table_time_data_tax_relabund_temp_melt)


#Create vector for nec score to get the order right for regressions
amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score_f = factor(amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score, levels = c( "4", "7", "8", "9", "10", "11", "12", "14", "16", "17", "18", "19", "22", "23", "24"))

#Make timepoint a factor
amplicon_table_time_data_tax_relabund_temp_melt$Timepoint_f = factor(amplicon_table_time_data_tax_relabund_temp_melt$Timepoint, order = TRUE, levels = c( "1", "2", "3", "4", "5", "6", "7"))



#Make nec severity score a numeric vector by converting the vector/factor you already have, to numeric. As.character is needed when the variable is a number. 
amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score_f_n = as.numeric(as.character(amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score_f))
view(amplicon_table_time_data_tax_relabund_temp_melt)

class(amplicon_table_time_data_tax_relabund_temp_melt$Timepoint)

class(amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score_f_n)

class(amplicon_table_time_data_tax_relabund_temp_melt$NEC_Severity_Score)

