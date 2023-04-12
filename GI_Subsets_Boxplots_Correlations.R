#Goal: Generate box plots comparing relative abundance of certain taxa between groups and diagnosis


#Create new variable making treatment group a factor to control the order and color
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$Treatment_Group_f = factor(amplicon_table_GI_data_tax_Stom_relabund_temp_melt$TreatmentGroup, levels = c( "DHM", "Form", "Form3SL", "FormBI", "FormBoth"))
#Create new variable making treatment group a facor
amplicon_table_GI_data_tax_SI_relabund_temp_melt$Treatment_Group_f = factor(amplicon_table_GI_data_tax_SI_relabund_temp_melt$TreatmentGroup, levels = c( "DHM", "Form", "Form3SL", "FormBI", "FormBoth"))
#Create new variable making treatment group a facor
amplicon_table_GI_data_tax_Col_relabund_temp_melt$Treatment_Group_f = factor(amplicon_table_GI_data_tax_Col_relabund_temp_melt$TreatmentGroup, levels = c( "DHM", "Form", "Form3SL", "FormBI", "FormBoth"))

view(amplicon_table_GI_data_tax_SI_relabund_temp_melt)


#Create vector for nec score to get the order right for regressions
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$NEC_Severity_Score_f = factor(amplicon_table_GI_data_tax_Stom_relabund_temp_melt$NEC_Severity_Score, levels = c( "4", "7", "8", "9", "10", "11", "12", "14", "16", "17", "18", "19", "22", "23", "24"))
amplicon_table_GI_data_tax_SI_relabund_temp_melt$NEC_Severity_Score_f = factor(amplicon_table_GI_data_tax_SI_relabund_temp_melt$NEC_Severity_Score, levels = c( "4", "7", "8", "9", "10", "11", "12", "14", "16", "17", "18", "19", "22", "23", "24"))
amplicon_table_GI_data_tax_Col_relabund_temp_melt$NEC_Severity_Score_f = factor(amplicon_table_GI_data_tax_Col_relabund_temp_melt$NEC_Severity_Score, levels = c( "4", "7", "8", "9", "10", "11", "12", "14", "16", "17", "18", "19", "22", "23", "24"))
view(amplicon_table_GI_data_tax_Col_relabund_temp_melt)


#Make nec severity score a numeric vector by converting the vector/factor you already have, to numeric. As.character is needed when the variable is a number. 
amplicon_table_GI_data_tax_Stom_relabund_temp_melt$NEC_Severity_Score_f_n = as.numeric(as.character(amplicon_table_GI_data_tax_Stom_relabund_temp_melt$NEC_Severity_Score_f))
amplicon_table_GI_data_tax_SI_relabund_temp_melt$NEC_Severity_Score_f_n = as.numeric(as.character(amplicon_table_GI_data_tax_SI_relabund_temp_melt$NEC_Severity_Score_f))
amplicon_table_GI_data_tax_Col_relabund_temp_melt$NEC_Severity_Score_f_n = as.numeric(as.character(amplicon_table_GI_data_tax_Col_relabund_temp_melt$NEC_Severity_Score_f))
view(amplicon_table_GI_data_tax_Col_relabund_temp_melt)

#Subset for Clostridium sensu stricto
amplicon_table_GI_data_tax_Col_relabund_temp_melt_CSS1 <- amplicon_table_GI_data_tax_Col_relabund_temp_melt[grep("Clostridium sensu stricto 1$", amplicon_table_GI_data_tax_Col_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_Stom_relabund_temp_melt_CSS1 <- amplicon_table_GI_data_tax_Stom_relabund_temp_melt[grep("Clostridium sensu stricto 1$", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_SI_relabund_temp_melt_CSS1 <- amplicon_table_GI_data_tax_SI_relabund_temp_melt[grep("Clostridium sensu stricto 1$", amplicon_table_GI_data_tax_SI_relabund_temp_melt$`variable`), ]
view(amplicon_table_GI_data_tax_SI_relabund_temp_melt_CSS1)


#Subset for Escherichia-Shigella
amplicon_table_GI_data_tax_Col_relabund_temp_melt_ES <- amplicon_table_GI_data_tax_Col_relabund_temp_melt[grep("Escherichia-Shigella", amplicon_table_GI_data_tax_Col_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_Stom_relabund_temp_melt_ES <- amplicon_table_GI_data_tax_Stom_relabund_temp_melt[grep("Escherichia-Shigella", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_SI_relabund_temp_melt_ES <- amplicon_table_GI_data_tax_SI_relabund_temp_melt[grep("Escherichia-Shigella", amplicon_table_GI_data_tax_SI_relabund_temp_melt$`variable`), ]
view(amplicon_table_GI_data_tax_SI_relabund_temp_melt_ES)

#Subset for Enterococcus
amplicon_table_GI_data_tax_Col_relabund_temp_melt_EN <- amplicon_table_GI_data_tax_Col_relabund_temp_melt[grep("Enterococcus", amplicon_table_GI_data_tax_Col_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_Stom_relabund_temp_melt_EN <- amplicon_table_GI_data_tax_Stom_relabund_temp_melt[grep("Enterococcus", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_SI_relabund_temp_melt_EN <- amplicon_table_GI_data_tax_SI_relabund_temp_melt[grep("Enterococcus", amplicon_table_GI_data_tax_SI_relabund_temp_melt$`variable`), ]

#Subset for Bifidobacterium 
amplicon_table_GI_data_tax_Col_relabund_temp_melt_BI <- amplicon_table_GI_data_tax_Col_relabund_temp_melt[grep("Bifidobacterium", amplicon_table_GI_data_tax_Col_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_Stom_relabund_temp_melt_BI <- amplicon_table_GI_data_tax_Stom_relabund_temp_melt[grep("Bifidobacterium", amplicon_table_GI_data_tax_Stom_relabund_temp_melt$`variable`), ]
amplicon_table_GI_data_tax_SI_relabund_temp_melt_BI <- amplicon_table_GI_data_tax_SI_relabund_temp_melt[grep("Bifidobacterium", amplicon_table_GI_data_tax_SI_relabund_temp_melt$`variable`), ]
view(amplicon_table_GI_data_tax_Stom_relabund_temp_melt_BI)

#Diagnosis

boxplot_temp <- ggplot(amplicon_table_GI_data_tax_Col_relabund_temp_melt_EN, aes(x= Diagnosis, y= value, fill= Diagnosis)) +
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
  xlab("Enterococcus") +
  ggtitle("Colon Contents") +
  guides(fill = guide_legend(title = "Diagnosis")) +
  ylim(0, 1)
print(boxplot_temp)

ggsave(plot= boxplot_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "Col_EN_Diagnosis.png") 

#Wilcoxon to compare abundance by diagnosis
res.Stom <- wilcox.test(value~Diagnosis, data = amplicon_table_GI_data_tax_Stom_relabund_temp_melt_CSS1)
res.SI <- wilcox.test(value~Diagnosis, data = amplicon_table_GI_data_tax_SI_relabund_temp_melt_ES)
res.Col <- wilcox.test(value~Diagnosis, data = amplicon_table_GI_data_tax_Col_relabund_temp_melt_ES)
res.Stom
res.SI
res.Col

#Treatment Groups

boxplot_temp <- ggplot(amplicon_table_GI_data_tax_SI_relabund_temp_melt_EN, aes(x= Treatment_Group_f, y= value, fill= Treatment_Group_f)) +
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
  xlab("Enterococcus") +
  ggtitle("Colon Contents") +
  guides(fill = guide_legend(title = "Treatment")) +
  ylim(0,1)
print(boxplot_temp)

#Save without significance symbols or values
ggsave(plot= boxplot_temp, height = 7, width= 9, dpi = 300, bg = "white", filename = "SI_EN_Treatment.png") 

#Dunn Test / Kruskal for grouo comparisons with BH/FDR correction
dunnTest(value~Treatment_Group, data = amplicon_table_GI_data_tax_Stom_relabund_temp_melt_CSS1, method = "bh") 


#Spearman Correlations

ggpsp <- ggplot(amplicon_table_GI_data_tax_Stom_relabund_temp_melt_BI, aes(x= NEC_Severity_Score_f_n, y=value)) +
  stat_cor(method = "spearman", label.x = 3, label.y = 1, size= 6) + geom_point() + geom_smooth() +  ylab("Relative Abundance") +
  xlab("NEC Severity Score") +
  ggtitle("Small Intestine Contents") + theme(plot.title = element_blank(), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15) )
ggpsp

ggsave(plot= ggpsp, height = 4, width= 5, dpi = 300, bg = "white", filename = "Stom_BI_SpCorr.png") 

