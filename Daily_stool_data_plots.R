#Goals: Abundance of top 5 taxa by day in all groups

#Abundance of BI and CSS1 per day per group
#Subsets
amplicon_table_time_data_tax_relabund_temp_melt_BI <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Bifidobacterium", amplicon_table_time_data_tax_relabund_temp_melt$`variable`), ]
amplicon_table_time_data_tax_relabund_temp_melt_CSS1 <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Clostridium sensu stricto 1$", amplicon_table_time_data_tax_relabund_temp_melt$`variable`), ]
view(amplicon_table_time_data_tax_relabund_temp_melt_CSS1)


amplicon_table_time_data_tax_relabund_temp_melt_BI$Timepoint_f = factor(amplicon_table_time_data_tax_relabund_temp_melt_BI$Timepoint, order = TRUE, levels = c( "1", "2", "3", "4", "5", "6", "7"))

amplicon_table_time_data_tax_relabund_temp_melt_CSS1$Timepoint_f = factor(amplicon_table_time_data_tax_relabund_temp_melt_CSS1$Timepoint, order = TRUE, levels = c( "1", "2", "3", "4", "5", "6", "7"))



#Data manipulation for plotting #I want to plot a mean of all observations per day per group

Exp_dataset_time2 <- amplicon_table_time_data_tax_relabund_temp_melt_BI %>% 
  group_by(Timepoint_f, Treatment_Group_f) %>% 
  summarise(value_test = mean(value), value_test_se = se(value), value_test_sd = sd(value))

view(Exp_dataset_time2)

#Add an ID column 
# Name first column SampleID
Exp_dataset_time3 <- rownames_to_column(Exp_dataset_time2, "SampleID")

view(Exp_dataset_time3)

#Subset by day
#Day1
Exp_dataset_time3_1 <- subset(Exp_dataset_time3, Timepoint_f == "1", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))
view(Exp_dataset_time3_1)

#Stats
# anova_test(Exp_dataset_time3_1, dv= value_test, wid = SampleID, within = Treatment_Group_f) *need to not calculate the means before anova so im using a kruskal instead
#dunnTest is a kruskall wallis multiple comparisons that will display all comparisons and P values

dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_1, method = "bh") 

            
#Day2
Exp_dataset_time3_2 <- subset(Exp_dataset_time3, Timepoint_f == "2", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))
view(Exp_dataset_time3_2)

dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_2, method = "bonferroni") 

#Day3
Exp_dataset_time3_3 <- subset(Exp_dataset_time3, Timepoint_f == "3", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))
view(Exp_dataset_time3_3)

dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_3, method = "bonferroni") 

#Day4
Exp_dataset_time3_4 <- subset(Exp_dataset_time3, Timepoint_f == "4", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))


dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_4, method = "bonferroni") 

#Day5
Exp_dataset_time3_5 <- subset(Exp_dataset_time3, Timepoint_f == "5", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))


dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_5, method = "bonferroni") 

#Day6
Exp_dataset_time3_6 <- subset(Exp_dataset_time3, Timepoint_f == "6", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))
view(Exp_dataset_time3_6)

dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_6, method = "bonferroni") 

#Day7
Exp_dataset_time3_7 <- subset(Exp_dataset_time3, Timepoint_f == "7", select= c(Timepoint_f, SampleID, Treatment_Group_f, value_test, value_test_se, value_test_sd))
view(Exp_dataset_time3_7)

dunnTest(value_test~Treatment_Group_f, data = Exp_dataset_time3_6, method = "bonferroni") 



# view(amplicon_table_time_data_tax_relabund_temp_melt_BI)

Daily_temp <- ggplot(Exp_dataset_time2, aes(x= Timepoint_f, y=value_test, group=Treatment_Group_f)) + geom_line(aes(color=Treatment_Group_f), size=1.5) + geom_point()  +
  geom_ribbon(aes(ymin=value_test-value_test_se, ymax=value_test+value_test_se, fill= Treatment_Group_f), alpha=0.3 )+
scale_color_manual(values = c("#FFFF00", "#00003C", "#0000C4", "#8282FF", "#DBDBFF")) + scale_fill_manual(values = c("#FFFF00", "#00003C", "#0000C4", "#8282FF", "#DBDBFF")) +
  ylab("Relative Abundance") +  theme(axis.title.y.left = element_text(size = 20, vjust = 2),
legend.title = element_text(size = 15),
axis.text.y = element_text(size=15),
axis.text.x = element_text(size = 15),
legend.text = element_text(size = 15),
axis.title.x = element_text(size=20),
plot.title = element_text(hjust = 0.5, vjust = 2, size = 20)) +
  xlab("Days") +
  ggtitle("Clostridium sensu stricto 1 Daily Abundance") +
  guides(color = guide_legend(title = "Treatment")) + guides(fill = guide_legend(title = "Treatment")) + scale_y_continuous(limits = c( 0.0, 0.8), minor_breaks= seq(0, 0.8, by=0.05)) 
Daily_temp

ggsave(plot= Daily_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "Stool_CSS1_Daily.png") 




#Abundance of top 5 taxa by day in all groups
#dot plots
amplicon_table_time_data_tax_relabund_temp_melt_Staph <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Staphylococcus", amplicon_table_time_data_tax_relabund_temp_melt$`variable`), ]
amplicon_table_time_data_tax_relabund_temp_melt_E_s <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Escherichia-Shigella", amplicon_table_time_data_tax_relabund_temp_melt$`variable`), ]
amplicon_table_time_data_tax_relabund_temp_melt_Ent <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Enterococcus", amplicon_table_time_data_tax_relabund_temp_melt$`variable`), ]
#
Exp_dataset_time <- amplicon_table_time_data_tax_relabund_temp_melt_Ent %>% 
  group_by(Timepoint_f, Treatment_Group_f) %>% 
  summarise(value_test = mean(value), value_test_se = se(value), value_test_sd = sd(value))

Daily_temp <- ggplot(Exp_dataset_time, aes(x= Timepoint_f, y=value_test, group=Treatment_Group_f)) + geom_line(aes(color=Treatment_Group_f), size=1.5) + geom_point()  +
  geom_ribbon(aes(ymin=value_test-value_test_se, ymax=value_test+value_test_se, fill= Treatment_Group_f), alpha=0.3 )+
  scale_color_manual(values = c("#FFFF00", "#00003C", "#0000C4", "#8282FF", "#DBDBFF")) + scale_fill_manual(values = c("#FFFF00", "#00003C", "#0000C4", "#8282FF", "#DBDBFF")) +
  ylab("Relative Abundance") +  theme(axis.title.y.left = element_text(size = 20, vjust = 2),
                                      legend.title = element_text(size = 15),
                                      axis.text.y = element_text(size=15),
                                      axis.text.x = element_text(size = 15),
                                      legend.text = element_text(size = 15),
                                      axis.title.x = element_text(size=20),
                                      plot.title = element_text(hjust = 0.5, vjust = 2, size = 20)) +
  xlab("Days") +
  ggtitle("Enterococcus Daily Abundance") +
  guides(color = guide_legend(title = "Treatment")) + guides(fill = guide_legend(title = "Treatment")) + scale_y_continuous(limits = c( 0.0, 0.8), minor_breaks= seq(0, 0.8, by=0.05)) 
Daily_temp

ggsave(plot= Daily_temp, height = 4, width= 7, dpi = 300, bg = "white", filename = "Stool_Ent_Daily.png") 




#Boxplots

view(amplicon_table_time_data_tax_relabund_temp_melt)
boxplot_temp <- ggplot(amplicon_table_time_data_tax_relabund_temp_melt, aes(x= variable, y= value, fill= Timepoint_f)) +
  geom_boxplot() +
  # geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = c("orange", "purple", "coral3", "deeppink", "chartreuse3", "deepskyblue", "grey")) +
  theme(axis.title.y.left = element_text(size = 28, vjust = 2),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size = 25, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank(),
        plot.title = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black")) +
  ylab("Relative Abundance") +
  xlab("Top 5 Genera") +
  ggtitle("") +
  guides(fill = guide_legend(title = "Timepoint")) +
  ylim(0,1)
print(boxplot_temp)

#Save without significance symbols or values
ggsave(plot= boxplot_temp, height = 8, width= 13, dpi = 300, bg = "white", filename = "Top_5_Genera_Daily_axistext.png") 

# res.Stom <- kruskal.test(variable,value~Timepoint_f, data = amplicon_table_time_data_tax_relabund_temp_melt)

boxplot_comparisons <- boxplot_temp + stat_compare_means(data=amplicon_table_time_data_tax_relabund_temp_melt, method = "kruskal.test", label.y = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) 
print(boxplot_comparisons)

#Subset by group
view(amplicon_table_time_data_tax_relabund_temp_melt)
amplicon_table_time_data_tax_relabund_temp_melt_DHM <- amplicon_table_time_data_tax_relabund_temp_melt[grep("DHM", amplicon_table_time_data_tax_relabund_temp_melt$`Treatment_Group_f`), ]
amplicon_table_time_data_tax_relabund_temp_melt_Form <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Form", amplicon_table_time_data_tax_relabund_temp_melt$`Treatment_Group_f`), ]
amplicon_table_time_data_tax_relabund_temp_melt_FormBI <- amplicon_table_time_data_tax_relabund_temp_melt[grep("FormBI", amplicon_table_time_data_tax_relabund_temp_melt$`Treatment_Group_f`), ]
amplicon_table_time_data_tax_relabund_temp_melt_Form3SL <- amplicon_table_time_data_tax_relabund_temp_melt[grep("Form3SL", amplicon_table_time_data_tax_relabund_temp_melt$`Treatment_Group_f`), ]
amplicon_table_time_data_tax_relabund_temp_melt_FormBoth <- amplicon_table_time_data_tax_relabund_temp_melt[grep("FormBoth", amplicon_table_time_data_tax_relabund_temp_melt$`Treatment_Group_f`), ]
view(amplicon_table_time_data_tax_relabund_temp_melt_FormBoth)

#Abundance of top 5 taxa by day by group
view(amplicon_table_time_data_tax_relabund_temp_melt_Form)
boxplot_temp <- ggplot(amplicon_table_time_data_tax_relabund_temp_melt_FormBoth, aes(x= variable, y= value, fill= Timepoint_f)) +
  geom_boxplot() +
  # geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = c("orange", "purple", "coral3", "deeppink", "chartreuse3", "deepskyblue", "grey")) +
  theme(axis.title.y.left = element_text(size = 28, vjust = 2),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y = element_text(size=25),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black")) +
  ylab("Relative Abundance") +
  xlab("Top 5 Genera") +
  ggtitle("") +
  guides(fill = guide_legend(title = "Timepoint")) +
  ylim(0,1)
print(boxplot_temp)

#Save without significance symbols or values
ggsave(plot= boxplot_temp, height = 7, width= 9, dpi = 300, bg = "white", filename = "Top_5_Genera_Daily_Both.png") 

boxplot_comparisons <- boxplot_temp + stat_compare_means(data=amplicon_table_time_data_tax_relabund_temp_melt_DHM, method = "kruskal.test", label.y = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) 
print(boxplot_comparisons)

