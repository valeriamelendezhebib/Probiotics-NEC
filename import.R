# Read in data and metadata
#I removed data from these since I moved all files to one folder
# Read in biom-formatted 16S table
biom_table <- biomformat::read_biom("16S feature-table.biom")

# Convert to a matrix and then to a data.frame
amplicon_table <- as.data.frame(as.matrix(biom_data(biom_table)))

#why convert to matrix? 
#Why convert to data frame?
# Save in the data directory
write.csv(amplicon_table, "16S_feature_table.csv")

# Read in humann2 data
humann2_path_cpm <- vroom("all_pathabundance_cpm.tsv")

# Read in metaphlan data
metaphlan2_relab <- vroom("merged_abundance_table_species.txt")

# Read in amplicon metadata
metadata_amplicon <- read_excel("16S Metadata Burrin.Mills.xlsx", sheet = 1)

# Read in wgs metadata
metadata_wgs <- read_excel("UPDATED WGS Metadata Burrin.Mills.xlsx", sheet = 1)

#What is amplicon hash-tax? The big ID number gets assigned a taxa
# Read in amplicon hash-tax map
amplicon_tax_map <- read.delim("burring_pig_taxonomy.tsv")

# Remove row 1 from amplicon_tax_map
#Unnecesary first row
amplicon_tax_map <- subset(amplicon_tax_map, Feature.ID != "#q2:types")

#Ask what all these mean?
# Define default plotting theme for downstream plots
default_theme <- theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    axis.line = element_line(size = 1, lineend = "butt"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Set color list for use in all downstream analyses
cols <- c("#4c3069", "#47a196", "#FFC75C", "#028090", "#982a30", "#EF476F", "grey")

# Preview the color palette by uncommenting and running the following code
pie(rep(1,length(cols)), col = cols)

# Load custom get.clr() function for use in downstream analyses
get.clr <- function(x, denom = "all") {
  #' @title Apply aldex.clr function to a table of counts
  #' @description Applies the aldex.clr function to a count table, with samples in columns and taxa in rows, using the chosen denominator, and returns a sample (rows) x features (columns) table containing the median value of clr transformed counts from all Monte Carlo samples.
  #'
  #' @param x A taxatable with samples as columns and features as rows. From aldex.clr documentation: "A data.frame or RangedSummarizedExperiment object containing non-negative integers only and with unique names for all rows and columns, where each row is a different gene and each column represents a sequencing read-count. Rows with 0 reads in each sample are deleted prior to analysis."
  #' @param denom The type of denominator to use in calculating the clr. From aldex.clr documentation: "An any variable (all, iqlr, zero, lvha, median,user) indicating features to use as the denominator for the Geometric Mean calculation The default "all" uses the geometric mean abundance of all features. Using "median" returns the median abundance of all features. Using "iqlr" uses the features that are between the first and third quartile of the variance of the clr values across all samples. Using "zero" uses the non-zero features in each grop as the denominator. This approach is an extreme case where there are many nonzero features in one condition but many zeros in another. Using "lvha" uses features that have low variance (bottom quartile) and high relative abundance (top quartile in every sample). It is also possible to supply a vector of row indices to use as the denominator. Here, the experimentalist is determining a-priori which rows are thought to be invariant. In the case of RNA-seq, this could include ribosomal protein genes and and other house-keeping genes."
  #' @return A matrix of clr transformed counts with samples as rows and features as columns
  #' @seealso \code{ALDEx2::aldex.clr()} for more information on the aldex clr function
  #' @examples
  #' data("RAW_FILTERED_OTU")
  #' x <- get.clr(RAW_FILTERED_OTU)
  #' @export
  
  d.clr.list <- suppressMessages(ALDEx2::aldex.clr(x, denom = denom))
  d.clr <- data.frame(matrix(ncol = ncol(x), nrow = nrow(x)))
  colnames(d.clr) <- colnames(x)
  rownames(d.clr) <- rownames(x)
  
  for (i in 1:ncol(x)) {
    d.clr[,i] <- apply(d.clr.list@analysisData[i][[1]], 1, function(y) {return(stats::median(y))})
  }
  
  # Transpose -- samples are now ROWS and features are COLUMNS
  return(t(d.clr))
  
}

