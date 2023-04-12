#Goal: Create box plots and Spearman correlation for WGS data using subsets created in WGS_data_manipulation_subsets.R

#Diagnosis

boxplot_temp <- ggplot(metaphlan2_relab_temp_melt_CP, aes(x= Diagnosis, y= value, fill= Diagnosis)) +
  geom_boxplot() +
  # geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = c("cadetblue3", "brown3", "chartreuse3", "chocolate2", "darkorchid3")) +
  geom_jitter(color="black", size=1.5, alpha=0.9)+
  theme(axis.title.y.left = element_text(size = 20, vjust = 2),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y = element_text(size=15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=20),
        plot.title = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black")) +
  ylab("Relative Abundance") +
  xlab("Clostridium perfringens") +
  ggtitle("Colon") +
  guides(fill = guide_legend(title = "Diagnosis"))  +
  ylim(0, 1)
print(boxplot_temp)

ggsave(plot= boxplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "WGS_CP_Diagnosis2.png") 

#Wilcoxon
res.EC <- wilcox.test(value~Diagnosis, data = metaphlan2_relab_temp_melt_EC)

res.EC

#Treatment Groups

boxplot_temp <- ggplot(metaphlan2_relab_temp_melt_CP, aes(x= Treatment_Group_f, y= value, fill= Treatment_Group_f)) +
  geom_boxplot() +
  # geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = c("#FFFF00", "#00003C", "#0000C4", "#8282FF", "#DBDBFF")) +
  geom_jitter(color="black", size=1.5, alpha=0.9)+
  theme(axis.title.y.left = element_text(size = 25, vjust = 2),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y = element_text(size=15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=25),
        plot.title = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black")) +
  ylab("Relative Abundance") +
  xlab("Clostridium perfringens") +
  ggtitle("Colon Contents") +
  guides(fill = guide_legend(title = "Treatment")) +
  ylim(0,1)
print(boxplot_temp)

#Save without significance symbols or values
ggsave(plot= boxplot_temp, height = 7, width= 9, dpi = 300, bg = "white", filename = "WGS_CP_Treatment2.png") 


#Krukal wallis multiple comparisons:
dunnTest(value~Treatment_Group, data = metaphlan2_relab_temp_melt_EC, method = "bh") 

ggsave(plot= boxplot_comparisons, height = 7, width= 9, dpi = 300, bg = "white", filename = ".png") 


#Spearman Correlations

ggpsp <- ggplot(metaphlan2_relab_temp_melt_EC, aes(x= NEC_Severity_Score_f_n, y=value)) +
  stat_cor(method = "spearman", label.x = 3, label.y = 1, size= 6) + geom_point() + geom_smooth() +  ylab("Relative Abundance") +
  xlab("NEC Severity Score") +
  ggtitle("Small Intestine Contents") + theme(plot.title = element_blank(), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15) )
ggpsp

ggsave(plot= ggpsp, height = 4, width= 5, dpi = 300, bg = "white", filename = "WGS_EC_SpCorr.png") 
