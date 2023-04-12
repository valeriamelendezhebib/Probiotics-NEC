#Goal: Plot facetted barplots with amplicon data from script: amplicon_data_GI_Subsets_barplot_prep_Grp_comparisons.R

#Set colors for each segment based on the order of abundance of the taxa so that all taxa have the same colors in all barplots
  barplot_colors_Col <- c("orange", "purple", "coral3", "deeppink", "chartreuse3", "deepskyblue", "grey", "pink", "green",
                     "#b4426b", "#d395a5")
  
  barplot_colors_Stom <- c("purple", "orange", "chartreuse3", "pink", "coral3", "deeppink", "indianred2", "green", "#973a35",
                      "mediumorchid2", "#d395a5")
  
  barplot_colors_SI <- c("purple", "orange", "coral3", "chartreuse3", "deeppink", "pink", "#b4426b", "green", "deepskyblue",
                       "indianred2", "#d395a5")
  
#Barplots facet by treatment
  barplot_temp <- ggplot(amplicon_table_GI_data_tax_SI_relabund_temp_melt, aes(x = SampleID,
                                                                            y = value,
                                                                            fill= variable)) +
    ggtitle("Small Intestine Contents") +
    geom_bar(position = "stack", stat = "identity") + 
    theme_cowplot(12) +
    scale_fill_manual(values = c(barplot_colors_SI[(unique(amplicon_table_GI_data_tax_SI_relabund_temp_melt$variable))])) +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
          axis.text.x = element_blank(),
          axis.title.y.left = element_text(size = 12, vjust = 2),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, vjust = 2, size = 14),
          strip.background = element_rect(fill = "white", color = "black"),
          legend.position = "right") +
    ylab("Relative Abundance") +
    scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) +
    guides(fill = guide_legend(title = "Genus",
                               nrow = length(unique(amplicon_table_GI_data_tax_SI_relabund_temp_melt$variable)),
                               byrow = TRUE)) +
    facet_grid(.~Treatment_Group_f, scales = "free_x")

  
  ggsave(plot= barplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "SI_Treatment.png")
  
  print(barplot_temp)
  
#Barplots facet by diagnosis
  
  barplot_temp <- ggplot(amplicon_table_GI_data_tax_SI_relabund_temp_melt, aes(x = SampleID,
                                                                                y = value,
                                                                                fill= variable)) +
    ggtitle("Small Intestine Contents") +
    geom_bar(position = "stack", stat = "identity") + 
    theme_cowplot(12) +
    scale_fill_manual(values = c(barplot_colors_SI[(unique(amplicon_table_GI_data_tax_SI_relabund_temp_melt$variable))])) +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
          axis.text.x = element_blank(),
          axis.title.y.left = element_text(size = 12, vjust = 2),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, vjust = 2, size = 14),
          strip.background = element_rect(fill = "white", color = "black"),
          legend.position = "right") +
    ylab("Relative Abundance") +
    scale_y_continuous(limits = c(0, 1.0), expand = c(0, 0)) +
    guides(fill = guide_legend(title = "Genus",
                               nrow = length(unique(amplicon_table_GI_data_tax_SI_relabund_temp_melt$variable)),
                               byrow = TRUE)) +
    facet_grid(.~Diagnosis, scales = "free_x")
  
  
  ggsave(plot= barplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "SI_Diagnosis.png")
  
  print(barplot_temp)