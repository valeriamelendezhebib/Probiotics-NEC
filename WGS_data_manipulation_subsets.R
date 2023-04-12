#Manipulation and subsets of WGS data
#Eliminate rectal swab samples from metadata 
metadata_wgs <- metadata_wgs[metadata_wgs$`Sample type` == "Colon Contents", ]


metaphlan2_relab <-metaphlan2_relab[, metadata_wgs$Sample_ID]


# Convert species rows into a column of the name Species
metaphlan2_relab_temp <- metaphlan2_relab %>%
  rownames_to_column("Species")


# Turn the column to rows and Finally, sort by descending sum relative abundance 
metaphlan2_relab_temp <- metaphlan2_relab_temp %>%
  column_to_rownames("Species") %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  dplyr::arrange(desc(Sum)) %>%
  slice(1:10)


view(metaphlan2_relab_temp)


# Remove the Sum column that was created
metaphlan2_relab_temp$Sum = NULL


#colsums
colSums (metaphlan2_relab_temp)

# Transpose dataframe and add an "other" column if you splice by top species abundance (not the case here)
metaphlan2_relab_temp <- as.data.frame(t(metaphlan2_relab_temp))
metaphlan2_relab_temp <- metaphlan2_relab_temp %>%
  dplyr::mutate(Other = 1 - rowSums(.))



#Add sample type, diagnosis, subject id, and treatment group as metadata categories for plotting
metaphlan2_relab_temp$Sample_Type <- metadata_wgs$`Sample type`
metaphlan2_relab_temp$Diagnosis <- metadata_wgs$`Diagnosis`
metaphlan2_relab_temp$Subject_ID <- metadata_wgs$`Subject_ID`
metaphlan2_relab_temp$Treatment_Group <- metadata_wgs$`Treatment_Group`
metaphlan2_relab_temp$NEC_Severity_Score <- metadata_wgs$`NEC_Severity_Score`

# Name first column SampleID
metaphlan2_relab_temp <- rownames_to_column(metaphlan2_relab_temp,
                                                               "SampleID")


# Melt the dataframe for easier plotting with ggplot; retain the new metadata categories columns
metaphlan2_relab_temp_melt <- melt(metaphlan2_relab_temp,
                                                       id.vars = c("Sample_Type", "SampleID", "Diagnosis", "Subject_ID", "Treatment_Group", "NEC_Severity_Score"))
view(metaphlan2_relab_temp_melt)

#Change group names
metaphlan2_relab_temp_melt$Treatment_Group <- sub("BI_HMO", "FormBoth", metaphlan2_relab_temp_melt$Treatment_Group,ignore.case = FALSE, perl = FALSE,
                                                                     fixed = TRUE, useBytes = FALSE)
metaphlan2_relab_temp_melt$Treatment_Group <- sub("BI", "FormBI", metaphlan2_relab_temp_melt$Treatment_Group,ignore.case = FALSE, perl = FALSE,
                                                                     fixed = TRUE, useBytes = FALSE)
metaphlan2_relab_temp_melt$Treatment_Group <- sub("HMO", "Form3SL", metaphlan2_relab_temp_melt$Treatment_Group,ignore.case = FALSE, perl = FALSE,
                                                                     fixed = TRUE, useBytes = FALSE)
metaphlan2_relab_temp_melt$Treatment_Group <- sub("EP", "Form", metaphlan2_relab_temp_melt$Treatment_Group,ignore.case = FALSE, perl = FALSE,
                                                                     fixed = TRUE, useBytes = FALSE)
metaphlan2_relab_temp_melt$Treatment_Group <- sub("HDM", "DHM", metaphlan2_relab_temp_melt$Treatment_Group,ignore.case = FALSE, perl = FALSE,
                                                  fixed = TRUE, useBytes = FALSE)
view(metaphlan2_relab_temp_melt)

#Create new variable making treatment group a factor to control the order and color
metaphlan2_relab_temp_melt$Treatment_Group_f = factor(metaphlan2_relab_temp_melt$Treatment_Group, levels = c( "DHM", "Form", "Form3SL", "FormBI", "FormBoth"))


# Make nec score a factor as a new variable to get the order right for regressions
metaphlan2_relab_temp_melt$NEC_Severity_Score_f = factor(metaphlan2_relab_temp_melt$NEC_Severity_Score, levels = c( "4", "7", "8", "9", "10", "11", "12", "14", "16", "17", "18", "19", "22", "23", "24"))

#Make nec severity score a numeric vector by converting the vector/factor you already have, to numeric. As.character is needed when the variable is a number. 
metaphlan2_relab_temp_melt$NEC_Severity_Score_f_n = as.numeric(as.character(metaphlan2_relab_temp_melt$NEC_Severity_Score_f))





#BA subset
metaphlan2_relab_temp_melt_BA <- metaphlan2_relab_temp_melt[grep("Bifidobacterium_animalis",metaphlan2_relab_temp_melt$`variable`), ]

#BL subset

metaphlan2_relab_temp_melt_BL <- metaphlan2_relab_temp_melt[grep("Bifidobacterium_longum",metaphlan2_relab_temp_melt$`variable`), ]

#CP subset
metaphlan2_relab_temp_melt_CP <- metaphlan2_relab_temp_melt[grep("Clostridium_perfringens", metaphlan2_relab_temp_melt$`variable`), ]

#E.coli subset
metaphlan2_relab_temp_melt_EC <- metaphlan2_relab_temp_melt[grep("Escherichia_coli", metaphlan2_relab_temp_melt$`variable`), ]
view(metaphlan2_relab_temp_melt_EC)
