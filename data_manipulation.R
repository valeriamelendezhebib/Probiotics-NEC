# Save copies of original dataframes to refer back to
humann2_path_cpm_og <- humann2_path_cpm
metaphlan2_relab_og <- metaphlan2_relab
amplicon_table_og <- amplicon_table
metadata_amplicon_og <- metadata_amplicon
metadata_wgs_og <- metadata_wgs
# view(metadata_amplicon)
# view(amplicon_table)
# Need to address sample duplication in Subjects 319A, 319C, 319E, 319I, 322A, and 322B at study endpoint
#Makes a subset of just the contents samples
metadata_amplicon_GI_data_tmp <- metadata_amplicon[grep("Contents", metadata_amplicon$`Sample type`), ]
# view(metadata_amplicon_GI_data_tmp)

# Uncomment and run the following code to view an arranged, subsetted version of this dataframe
#Created a subset in which there are only the samples listed below which are the duplicated ones
View(metadata_amplicon_GI_data_tmp %>%
       dplyr::filter(Subject_ID %in% c('319A', '319C', '319E', '319I', '322A', '322B')) %>%
       arrange(Subject_ID, `Sample type`))

# Need to combine the following Samples:
# 160 and 278 (319A SmallIntestine)
# 119 and 357 (319C SmallIntestine)
# 300, 421, 45, and 511 (319E Stomach)
# 242, 449, and 478 (319I Stomach)
# 506 and 65 (322A Colon) - This one is addressed later. It relates to time series data.
# 199 and 263 (322A SmallIntestine)
# 259 and 46 (Subject 322A Stomach)
# 314 and 77 (Subject 322B SmallIntestine)

# The following commands will create new samples by taking the mean counts of all features for the given
# replicate samples described above
amplicon_table <- amplicon_table %>%
  mutate(`160_278` = round(rowMeans(cbind(`160`, `278`)))) %>%
  dplyr::select(-c("160", "278"))
amplicon_table <- amplicon_table %>%
  mutate(`119_357` = round(rowMeans(cbind(`119`, `357`)))) %>%
  dplyr::select(-c("119", "357"))
amplicon_table <- amplicon_table %>%
  mutate(`300_421_45_511` = round(rowMeans(cbind(`300`, `421`, `45`, `511`)))) %>%
  dplyr::select(-c("300", "421", "45", "511"))
amplicon_table <- amplicon_table %>%
  mutate(`242_449_478` = round(rowMeans(cbind(`242`, `449`, `478`)))) %>%
  dplyr::select(-c("242", "449", "478"))
amplicon_table <- amplicon_table %>%
  mutate(`199_263` = round(rowMeans(cbind(`199`, `263`)))) %>%
  dplyr::select(-c("199", "263"))
amplicon_table <- amplicon_table %>%
  mutate(`259_46` = round(rowMeans(cbind(`259`, `46`)))) %>%
  dplyr::select(-c("259", "46"))
amplicon_table <- amplicon_table %>%
  mutate(`314_77` = round(rowMeans(cbind(`314`, `77`)))) %>%
  dplyr::select(-c("314", "77"))

# Make substitutions in metadata to match the above modifications to amplicon sequencing data

# Substitute sample 160 for 160_278 in amplicon sequencing metadata, then remove samples 160 and 278
metadata_amplicon$Sample_ID <- sub("160", "160_278", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "160" & Sample_ID != "278")

# Substitute sample 119 for 119_357 in amplicon sequencing metadata, then remove samples 119 and 357
metadata_amplicon$Sample_ID <- sub("119", "119_357", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "119" & Sample_ID != "357")

# Substitute sample 300 for 300_421_45_511 in amplicon sequencing metadata, then remove samples 300, 421, 45, and 511
metadata_amplicon$Sample_ID <- sub("300", "300_421_45_511", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "300" & Sample_ID != "421" & Sample_ID != "45" & Sample_ID != "511")

# Substitute sample 242 for 242_449_478 in amplicon sequencing metadata, then remove samples 242, 449, and 478
metadata_amplicon$Sample_ID <- sub("242", "242_449_478", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "242" & Sample_ID != "449" & Sample_ID != "478")

# Substitute sample 199 for 199_263 in amplicon sequencing metadata, then remove samples 199 and 263
metadata_amplicon$Sample_ID <- sub("199", "199_263", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "199" & Sample_ID != "263")

# substitute sample 259 for 259_46 in amplicon sequencing metadata, then remove samples 259 and 46
metadata_amplicon$Sample_ID <- sub("259", "259_46", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "259" & Sample_ID != "46")

# Substitute sample 314 for 314_77 in amplicon sequencing metadata, then remove samples 314 and 77
metadata_amplicon$Sample_ID <- sub("314", "314_77", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "314" & Sample_ID != "77")

# Next, combine 16S samples 506 and 65 into one sample called 506_65, and remove the original samples
amplicon_table <- amplicon_table %>%
  mutate(`506_65` = round(rowMeans(cbind(`506`, `65`)))) %>%
  dplyr::select(-c("506", "65"))

# Combine WGS samples DMJL006_4 and DMJL006_47, and remove the original samples
#Guess this was duplicated too
humann2_path_cpm <- humann2_path_cpm %>%
  mutate(`DMJL006_4_47_Abundance-CPM` = rowMeans(cbind(`DMJL006_4_Abundance-CPM`, `DMJL006_47_Abundance-CPM`))) %>%
  dplyr::select(-c(`DMJL006_4_Abundance-CPM`, `DMJL006_47_Abundance-CPM`))

metaphlan2_relab <- metaphlan2_relab %>%
  mutate(`DMJL006_4_47.metaphlan` = rowMeans(cbind(`DMJL006_4.metaphlan`, `DMJL006_47.metaphlan`))) %>%
  dplyr::select(-c(`DMJL006_4.metaphlan`, `DMJL006_47.metaphlan`))

# Modify metadata files so they have only one sample for the duplicates.
# Give it the same name as the combined samples created in the feature tables.

# Substitute sample 506 for 506_65 in amplicon sequencing data, then remove sample 65
#16S data
metadata_amplicon$Sample_ID <- sub("506", "506_65", metadata_amplicon$Sample_ID)
metadata_amplicon <- subset(metadata_amplicon, Sample_ID != "65" & Sample_ID != "506") #removal of original samples

# substitute DMJL006_4_Abundance-CPM for DMJL006_4_47_Abundance-CPM, then remove sample DMJL006_47_Abundance-CPM
metadata_wgs$Sample_ID <- sub("DMJL006_4$", "DMJL006_4_47", metadata_wgs$Sample_ID)
metadata_wgs <- subset(metadata_wgs, Sample_ID != "DMJL006_47" & Sample_ID != "DMJL006_4")

#These lines below deleted the .metaphlan from the name of every sample
# Rectify sample names in both tables of WGS data
names(metaphlan2_relab) <- sub("\\.m*.*", "", names(metaphlan2_relab))
names(humann2_path_cpm) <- sub("_Abundance*.*", "", names(humann2_path_cpm))


# Remove metaphlan2 and humann2 samples that are absent from WGS metadata
#Remove anything that is not in metadata_WGS
metaphlan2_relab <- metaphlan2_relab[, c("ID", metadata_wgs$Sample_ID)]
humann2_path_cpm <- humann2_path_cpm[, c("# Pathway", metadata_wgs$Sample_ID)]

# Convert the species and pathway columns to a rownames in their respective tables
#This takes a column name and eliminates it. Here there was ID naming the column so it was deleted. 
metaphlan2_relab <- column_to_rownames(metaphlan2_relab, "ID")
humann2_path_cpm <- column_to_rownames(humann2_path_cpm, "# Pathway")
# view(humann2_path_cpm)

# Remove unmapped and unintegrated pathways so they won't be considered in downstream analyses
rows_to_remove <- c(grep("UNMAPPED", row.names(humann2_path_cpm)), grep("UNINTEGRATED", row.names(humann2_path_cpm)))
humann2_path_cpm <- humann2_path_cpm[-rows_to_remove, ]
# view(humann2_path_cpm)
view(metaphlan2_relab)

#Eliminate rows of fungi, phages, and viruses
metaphlan2_relab <- metaphlan2_relab[-c(67, 68, 69, 70, 71, 72, 73, 74, 75, 76), ]
view(metaphlan2_relab)


# Convert humann2 and metaphlan2 outputs to relative abundances summing to 1 in each sample
humann2_path_cpm <- sweep(humann2_path_cpm, 2, colSums(humann2_path_cpm), "/")

metaphlan2_relab <- sweep(metaphlan2_relab, 2, colSums(metaphlan2_relab), "/")

# view(humann2_path_cpm)
# Run the following lines to code to check (sums should be 1)
table(colSums(humann2_path_cpm))
table(colSums(metaphlan2_relab))

# Limit amplicon metadata to samples that are associated with a time point

# First, subset the metadata to samples that have time point information
#These are all 16S that include GI content and rectal swab/stool samples
metadata_amplicon_time_data <- metadata_amplicon[metadata_amplicon$Timepoint != "NA", ]

# Uncomment and run the following code to see the sample types
table(metadata_amplicon_time_data$`Sample type`)
# view(metadata_amplicon_time_data)

# Remove the one "Colon Contents" sample
metadata_amplicon_time_data <- metadata_amplicon_time_data[metadata_amplicon_time_data$`Sample type` != "Colon Contents", ]

# Run the following code to verify it's now gone
table(metadata_amplicon_time_data$`Sample type`)

# view(metadata_amplicon_time_data)
# Subset the amplicon table based on the samples in the subset metadata
#This will make it so the samples in the amplicon table are only the fecal samples and no GI contents which matches the subset we created above of just "time data"
amplicon_table_time_data <- amplicon_table[, metadata_amplicon_time_data$Sample_ID]

# Next, make an amplicon sample set for comparing colon, stomach, and small intestine

# First subset the metadata to "Contents" samples
metadata_amplicon_GI_data <- metadata_amplicon[grep("Contents", metadata_amplicon$`Sample type`), ]

#These functions basically look at the IDs in one file and create a matching subset from the data file. 
# Then, subset the amplicon data to samples in the metadata subset
amplicon_table_GI_data <- amplicon_table[, metadata_amplicon_GI_data$Sample_ID]

#To this point you have a metadata file and amplicon table for each GI contents and fecal samples that were from the 16S sequencing (amplicon)

# Uncomment and run the following code to see the sample types in amplicon_table_GI_data
table(metadata_amplicon_GI_data$`Sample type`)

# Look for GI_data samples with technical replicates by looking for duplication in rows except for the Sample_ID
metadata_amplicon_GI_data_duplicates <- metadata_amplicon_GI_data[duplicated(metadata_amplicon_GI_data[, 2:ncol(metadata_amplicon_GI_data)]) | duplicated(metadata_amplicon_GI_data[, 2:ncol(metadata_amplicon_GI_data)], fromLast = T), ]

# Uncomment and run the following code to view potential duplicated samples
# Note: These have been eliminated in the beginning of this script, so this table is empty.
# If those first lines are skipped, there will be several duplicates.
table(metadata_amplicon_GI_data_duplicates$Subject_ID)
metadata_amplicon_GI_data_duplicates[metadata_amplicon_GI_data_duplicates$Subject_ID == '319E',]

# There are additional duplicated samples that have Diagnosis coded differently.
# Again, don't worry about this, because it was addressed at the beginning of the script.

metadata_amplicon_GI_data[metadata_amplicon_GI_data$Subject_ID == '319A',]
metadata_amplicon_GI_data[metadata_amplicon_GI_data$Subject_ID == '319C',]

# Limit to RectalSwab samples for time series data
metadata_amplicon_time_data <- metadata_amplicon_time_data[metadata_amplicon_time_data$`Sample type` == "RectalSwab", ]
#Now eliminate the stool samples you just eliminated from the metadata table but from the amplicon table
amplicon_table_time_data <- amplicon_table_time_data[, metadata_amplicon_time_data$Sample_ID]
dim(amplicon_table_time_data)


# Limit samples to those with at least 1,000 reads for both amplicon tables
amplicon_table_time_data <- amplicon_table_time_data[, colSums(amplicon_table_time_data) > 1000, ]
amplicon_table_GI_data <- amplicon_table_GI_data[, colSums(amplicon_table_GI_data) > 1000, ]

# Remove ASVs that are now 0 abundance across all samples in both amplicon tables
amplicon_table_time_data <- amplicon_table_time_data[rowSums(amplicon_table_time_data) != 0, ]
amplicon_table_GI_data <- amplicon_table_GI_data[rowSums(amplicon_table_GI_data) != 0, ]

# Rarefy amplicon sequencing data to the depth of sample with the fewest mapped reads for both tables separately

# Set rarefaction depth for time data table
#I guess min sets it to the minnumum depth found in the table
rarefaction_depth <- min(colSums(amplicon_table_time_data))

# Set random seed for reproducible rarefaction result, then rarefy all samples to this depth
#rarefy- 
set.seed(001)
amplicon_table_time_data_rare <- vegan::rrarefy(t(amplicon_table_time_data), rarefaction_depth)

# Set rarefaction depth for GI data table
rarefaction_depth <- min(colSums(amplicon_table_GI_data))

# Set random seed for reproducible rarefaction result, then rarefy all samples to this depth
set.seed(001)
amplicon_table_GI_data_rare <- vegan::rrarefy(t(amplicon_table_GI_data), rarefaction_depth)

# Remove columns (ASVs) that now sum to 0 for both tables separately
amplicon_table_time_data_rare <- amplicon_table_time_data_rare[, which(colSums(amplicon_table_time_data_rare) > 0)]
amplicon_table_GI_data_rare <- amplicon_table_GI_data_rare[, which(colSums(amplicon_table_GI_data_rare) > 0)]

# Transpose rarefied amplicon tables to make it consistent with humann2 and metaphlan2 tables
amplicon_table_time_data_rare <- as.data.frame(t(amplicon_table_time_data_rare))
amplicon_table_GI_data_rare <- as.data.frame(t(amplicon_table_GI_data_rare))
# view(amplicon_table_GI_data)
#Data manipulation done resulting in rarefied amplicon GI and time data



#Differential abundance table preparation
# Collapse amplicon tables by identical taxonomy for differential abundance analyses

# First, move rownames to a column called "Feature.ID" for both tables
#Name the first column feature ID. I think the name of the column was made into a row before.
 
amplicon_table_time_data_tmp <- rownames_to_column(amplicon_table_time_data, "Feature.ID")
amplicon_table_GI_data_tmp <- rownames_to_column(amplicon_table_GI_data, "Feature.ID")


# merge with the taxonomy map, which also has a column called "Feature.ID"
amplicon_table_time_data_tmp_tax <- merge(amplicon_table_time_data_tmp, amplicon_tax_map)
amplicon_table_GI_data_tmp_tax <- merge(amplicon_table_GI_data_tmp, amplicon_tax_map)
# view(amplicon_table_time_data_tmp_tax)
# Remove "D_7__;D_8__;D_9__;D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__" from taxonomies
amplicon_table_time_data_tmp_tax$Taxon <- sub("D_7__;D_8__;D_9__;D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__", "", amplicon_table_time_data_tmp_tax$Taxon)
amplicon_table_GI_data_tmp_tax$Taxon <- sub("D_7__;D_8__;D_9__;D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__", "", amplicon_table_GI_data_tmp_tax$Taxon)

# Remove "D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__" from taxonomies
amplicon_table_time_data_tmp_tax$Taxon <- sub("D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__", "", amplicon_table_time_data_tmp_tax$Taxon)
amplicon_table_GI_data_tmp_tax$Taxon <- sub("D_10__;D_11__;D_12__;D_13__;D_14__;D_15__;D_16__", "", amplicon_table_GI_data_tmp_tax$Taxon)

# Aggregate read counts by taxonomy strings
amplicon_table_time_data_agg <- aggregate(amplicon_table_time_data_tmp_tax[, 2:(ncol(amplicon_table_time_data_tmp_tax) - 2)],
                                          by = list(amplicon_table_time_data_tmp_tax$Taxon), FUN = "sum")

amplicon_table_GI_data_agg <- aggregate(amplicon_table_GI_data_tmp_tax[, 2:(ncol(amplicon_table_GI_data_tmp_tax) - 2)],
                                        by = list(amplicon_table_GI_data_tmp_tax$Taxon), FUN = "sum")

# Move taxonomy names to rownames
amplicon_table_time_data_tax <- column_to_rownames(amplicon_table_time_data_agg, "Group.1")
amplicon_table_GI_data_tax <- column_to_rownames(amplicon_table_GI_data_agg, "Group.1")

# Check that the column sums haven't changed; stop for troubleshooting if they have
stopifnot(all.equal(colSums(amplicon_table_time_data), colSums(amplicon_table_time_data_tax)))
stopifnot(all.equal(colSums(amplicon_table_GI_data), colSums(amplicon_table_GI_data_tax)))

# Remove taxa that now sum to 0 (there shouldn't be any)
amplicon_table_time_data_tax <- amplicon_table_time_data_tax[rowSums(amplicon_table_time_data_tax) != 0, ]
amplicon_table_GI_data_tax <- amplicon_table_GI_data_tax[rowSums(amplicon_table_GI_data_tax) != 0, ]

# Limit amplicon metadata to only those samples in the ASV tables
metadata_amplicon_time_data <- metadata_amplicon_time_data[match(names(amplicon_table_time_data), metadata_amplicon_time_data$Sample_ID, ), ]
metadata_amplicon_GI_data <- metadata_amplicon_GI_data[match(names(amplicon_table_GI_data), metadata_amplicon_GI_data$Sample_ID, ), ]

# Make sure all metadata sample IDs are in same order as OTU/ASV table sample IDs; stop for troubleshooting if not
stopifnot(all.equal(metadata_amplicon_time_data$Sample_ID, names(amplicon_table_time_data)))
stopifnot(all.equal(metadata_amplicon_GI_data$Sample_ID, names(amplicon_table_GI_data)))

stopifnot(all.equal(metadata_amplicon_time_data$Sample_ID, names(amplicon_table_time_data_rare)))
stopifnot(all.equal(metadata_amplicon_GI_data$Sample_ID, names(amplicon_table_GI_data_rare)))

stopifnot(all.equal(metadata_amplicon_time_data$Sample_ID, names(amplicon_table_time_data_tax)))
stopifnot(all.equal(metadata_amplicon_GI_data$Sample_ID, names(amplicon_table_GI_data_tax)))

stopifnot(all.equal(metadata_wgs$Sample_ID, names(humann2_path_cpm)))
stopifnot(all.equal(metadata_wgs$Sample_ID, names(metaphlan2_relab)))
# view(amplicon_table_GI_data_tax)
