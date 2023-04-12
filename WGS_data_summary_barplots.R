# Plot facet stacked barplots

#Set colors for taxa on plots

barplot_colors <- c("purple",  "indianred2", "chartreuse3","pink", "coral3", "orange" ,"deeppink", "green", "#973a35",
                         "mediumorchid2", "#d395a5")



#Diagnosis barplot
barplot_temp <- ggplot(metaphlan2_relab_temp_melt, aes(x = SampleID,
                                                                             y = value,
                                                                             fill= variable)) +
  ggtitle("Colon Contents") +
  geom_bar(position = "stack", stat = "identity") + 
  theme_cowplot(12) +
  scale_fill_manual(values = c(barplot_colors[(unique(metaphlan2_relab_temp_melt$variable))])) +
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
                             nrow = length(unique(metaphlan2_relab_temp_melt$variable)),
                             byrow = TRUE)) +
  facet_grid(.~Diagnosis, scales = "free_x")
print(barplot_temp)

ggsave(plot= barplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "WGS_Diag.png")


#Treatment barplot
barplot_temp <- ggplot(metaphlan2_relab_temp_melt, aes(x = SampleID,
                                                       y = value,
                                                       fill= variable)) +
  ggtitle("Colon Contents") +
  geom_bar(position = "stack", stat = "identity") + 
  theme_cowplot(12) +
  scale_fill_manual(values = c(barplot_colors[(unique(metaphlan2_relab_temp_melt$variable))])) +
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
                             nrow = length(unique(metaphlan2_relab_temp_melt$variable)),
                             byrow = TRUE)) +
  facet_grid(.~TreatmentGroup, scales = "free_x")
print(barplot_temp)

ggsave(plot= barplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "WGS_TRT.png")
