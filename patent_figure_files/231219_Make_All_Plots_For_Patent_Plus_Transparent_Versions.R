
options(stringsAsFactors = F)
library(ggplot2)
#install.packages("ggpattern")
library(ggpattern)

output_dir = "/Users/annaminkina/Documents/Data/230918_ASO_analysis/Plots_for_leslie/"

combined_mean_dire_ov_ire_tbl_not_NA = read.table(paste0(output_dir, "all_samples_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points_TABLE_FOR_PLOT.txt"), header = T, sep = "\t", stringsAsFactors = F)
cols_to_use = c("white", "white", "white", "white")

combined_mean_dire_ov_ire_tbl_not_NA$sample_id = gsub("CR", "", combined_mean_dire_ov_ire_tbl_not_NA$sample_id)
  
all_sample_plot_BW_no_points = ggplot(combined_mean_dire_ov_ire_tbl_not_NA, aes(fill = factor(ASO_Conc), x = sample_id, y = sample_ov_pbs)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom = "bar_pattern", aes(pattern=ASO_Conc), pattern_density = .2, pattern_spacing = .03, pattern_key_scale_factor = .3, pattern_fill="black", colour="black", width=.5)+
  stat_summary(fun.data=mean_se,position=position_dodge(width = 0.80),geom="errorbar", width = .3) +
  #geom_point(position=position_dodge(width =0.80), size=1.5, shape = 21) +
  #geom_point(size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE (Norm. to PBS)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = cols_to_use, name = "ASO_Conc") +
  scale_pattern_manual(values=c("crosshatch", "stripe", "circle", "none"), name = "ASO_Conc")
#scale_pattern_density_manual(values = c("2"=.5, "20"=.5, "200" = 1, "2000" = 1))
#guides(fill=guide_legend(title="ASO Conc (nM)"))

all_sample_plot_BW_no_points_no_background = ggplot(combined_mean_dire_ov_ire_tbl_not_NA, aes(fill = factor(ASO_Conc), x = sample_id, y = sample_ov_pbs)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom = "bar_pattern", aes(pattern=ASO_Conc), pattern_density = .2, pattern_spacing = .03, pattern_key_scale_factor = .3, pattern_fill="black", colour="black", width=.5)+
  stat_summary(fun.data=mean_se,position=position_dodge(width = 0.80),geom="errorbar", width = .3) +
  #geom_point(position=position_dodge(width =0.80), size=1.5, shape = 21) +
  #geom_point(size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE (Norm. to PBS)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = cols_to_use, name = "ASO_Conc") +
  scale_pattern_manual(values=c("crosshatch", "stripe", "circle", "none"), name = "ASO_Conc") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

pdf(paste0(output_dir, "all_samples_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points.pdf"), height = 5, width = 14)
all_sample_plot_BW_no_points
dev.off() 

pdf(paste0(output_dir, "all_samples_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points_LARGER_writing.pdf"), height = 3, width = 10)
all_sample_plot_BW_no_points
dev.off() 

png(paste0(output_dir, "all_samples_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points_LARGER_writing_for_Valerie.png"), height = 3, width = 10, units = 'in', res = 300, bg="transparent",)
all_sample_plot_BW_no_points_no_background
dev.off()

samples_to_include = c("2327", "2331", "2332", "2333", "4148")

combined_mean_dire_ov_ire_tbl_not_NA_first_set = combined_mean_dire_ov_ire_tbl_not_NA[which(combined_mean_dire_ov_ire_tbl_not_NA$sample_id %in% samples_to_include),]

combined_mean_dire_ov_ire_tbl_not_NA_second_set = combined_mean_dire_ov_ire_tbl_not_NA[which(!(combined_mean_dire_ov_ire_tbl_not_NA$sample_id %in% samples_to_include)),]

all_sample_plot_BW_no_points_FIRST_SET = ggplot(combined_mean_dire_ov_ire_tbl_not_NA_first_set, aes(fill = factor(ASO_Conc), x = sample_id, y = sample_ov_pbs)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom = "bar_pattern", aes(pattern=ASO_Conc), pattern_density = .2, pattern_spacing = .03, pattern_key_scale_factor = .3, pattern_fill="black", colour="black", width=.5)+
  stat_summary(fun.data=mean_se,position=position_dodge(width = 0.80),geom="errorbar", width = .3) +
  #geom_point(position=position_dodge(width =0.80), size=1.5, shape = 21) +
  #geom_point(size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE (Norm. to PBS)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = cols_to_use, name = "ASO_Conc") +
  scale_pattern_manual(values=c("crosshatch", "stripe", "circle", "none"), name = "ASO_Conc")

all_sample_plot_BW_no_points_SECOND_SET = ggplot(combined_mean_dire_ov_ire_tbl_not_NA_second_set, aes(fill = factor(ASO_Conc), x = sample_id, y = sample_ov_pbs)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom = "bar_pattern", aes(pattern=ASO_Conc), pattern_density = .2, pattern_spacing = .03, pattern_key_scale_factor = .3, pattern_fill="black", colour="black", width=.5)+
  stat_summary(fun.data=mean_se,position=position_dodge(width = 0.80),geom="errorbar", width = .3) +
  #geom_point(position=position_dodge(width =0.80), size=1.5, shape = 21) +
  #geom_point(size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE (Norm. to PBS)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual(values = cols_to_use, name = "ASO_Conc") +
  scale_pattern_manual(values=c("crosshatch", "stripe", "circle", "none"), name = "ASO_Conc")

pdf(paste0(output_dir, "all_samples_SET1_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points_LARGER_writing.pdf"), height = 3, width = 6)
all_sample_plot_BW_no_points_FIRST_SET
dev.off() 

pdf(paste0(output_dir, "all_samples_SET2_dIRE1_ov_IRE_barplot_all_concentrations_BW_no_points_LARGER_writing.pdf"), height = 3, width = 6)
all_sample_plot_BW_no_points_SECOND_SET
dev.off() 


### protein:

pro_data_no_2327 = read.table(paste0(output_dir, "ONT_0026_in_vivo_protein_plot_BW_TABLE_FOR_PLOTTING.txt"), header = T, sep = "\t", stringsAsFactors = F)
pro_data_no_2327$treatment_3 = pro_data_no_2327$treatment_2
pro_data_no_2327[which(pro_data_no_2327$treatment_3 == "2332"),]$treatment_3 = "ASO-A"
pro_data_no_2327[which(pro_data_no_2327$treatment_3 == "2333"),]$treatment_3 = "ASO-B"


protein_plot = ggplot(pro_data_no_2327, aes(x=treatment_2, y=ave_fluor)) + 
  geom_bar(position='dodge', stat='summary', fun='mean', aes(fill = treatment), color="black", fill = "white",width = .7) +
  stat_summary(fun.data=mean_se, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2) +
  geom_point(size = .5) +
  theme_classic() +
  #scale_fill_manual(values = cols_to_use) +
  xlab("Treatment") +
  #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
  ylab(expression("Human"~alpha*"-Synuclein (ng/mL)")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

protein_plot_generic_aso_names = ggplot(pro_data_no_2327, aes(x=treatment_3, y=ave_fluor)) + 
  geom_bar(position='dodge', stat='summary', fun='mean', aes(fill = treatment), color="black", fill = "white",width = .7) +
  stat_summary(fun.data=mean_se, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2) +
  geom_point(size = .5) +
  theme_classic() +
  #scale_fill_manual(values = cols_to_use) +
  xlab("Treatment") +
  #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
  ylab(expression("Human"~alpha*"-Synuclein (ng/mL)")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

protein_plot_no_background = ggplot(pro_data_no_2327, aes(x=treatment_2, y=ave_fluor)) + 
  geom_bar(position='dodge', stat='summary', fun='mean', aes(fill = treatment), color="black", fill = "white",width = .7) +
  stat_summary(fun.data=mean_se, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2) +
  geom_point(size = .5) +
  theme_classic() +
  #scale_fill_manual(values = cols_to_use) +
  xlab("Treatment") +
  #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
  ylab(expression("Human"~alpha*"-Synuclein (ng/mL)")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

pdf(paste0(output_dir, "ONT_0026_in_vivo_protein_plot_BW.pdf"), height = 3, width = 3)
protein_plot
dev.off()

pdf(paste0(output_dir, "ONT_0026_in_vivo_protein_plot_generic_aso_names_BW.pdf"), height = 3, width = 3)
protein_plot_generic_aso_names
dev.off()

png(paste0(output_dir, "ONT_0026_in_vivo_protein_plot_BW_for_Valerie.png"), height = 3, width = 3, units = 'in', res = 300, bg="transparent",)
protein_plot_no_background
dev.off()

## isoform ratios (ONT)

in_vivo_tbl_no_2327 = read.table(paste0(output_dir, "ONT_0026_in_vivo_isoform_ratio_plot_BW_TABLE_FOR_PLOTTING.txt"), header = T, sep = "\t", stringsAsFactors = F)
in_vivo_tbl_no_2327$treatment_2 = in_vivo_tbl_no_2327$treatment
in_vivo_tbl_no_2327[which(in_vivo_tbl_no_2327$treatment_2 == "2332"),]$treatment_2 = "ASO-A"
in_vivo_tbl_no_2327[which(in_vivo_tbl_no_2327$treatment_2 == "2333"),]$treatment_2 = "ASO-B"

in_vivo_isoform_ratio_plot_BW = ggplot(in_vivo_tbl_no_2327, aes(x = treatment, y = dire1_ov_ire_ratio)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom="bar", fill = "white", color = "black")+
  stat_summary(fun.data=mean_se,position=position_dodge(0.80),geom="errorbar", width = .5) +
  geom_point(position=position_dodge(0.80), size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE Transcript Ratio") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

in_vivo_isoform_ratio_plot_BW_generic_aso_names = ggplot(in_vivo_tbl_no_2327, aes(x = treatment_2, y = dire1_ov_ire_ratio)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom="bar", fill = "white", color = "black")+
  stat_summary(fun.data=mean_se,position=position_dodge(0.80),geom="errorbar", width = .5) +
  geom_point(position=position_dodge(0.80), size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE Transcript Ratio") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


in_vivo_isoform_ratio_plot_BW_no_background = ggplot(in_vivo_tbl_no_2327, aes(x = treatment, y = dire1_ov_ire_ratio)) +
  stat_summary(fun=mean,position=position_dodge(width=0.80),geom="bar", fill = "white", color = "black")+
  stat_summary(fun.data=mean_se,position=position_dodge(0.80),geom="errorbar", width = .5) +
  geom_point(position=position_dodge(0.80), size = 1) +
  theme_classic() +
  xlab("Treatment") +
  ylab("dIRE1/IRE Transcript Ratio") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
  

pdf(paste0(output_dir, "ONT_0026_in_vivo_isoform_ratio_plot_BW.pdf"), height = 3, width = 3)
in_vivo_isoform_ratio_plot_BW
dev.off()

pdf(paste0(output_dir, "ONT_0026_in_vivo_isoform_ratio_plot_generic_aso_names_BW.pdf"), height = 3, width = 3)
in_vivo_isoform_ratio_plot_BW_generic_aso_names
dev.off()

png(paste0(output_dir, "ONT_0026_in_vivo_isoform_ratio_plot_BW_for_Valerie.png"), height = 3, width = 3, units = 'in', res = 300, bg="transparent",)
in_vivo_isoform_ratio_plot_BW_no_background
dev.off()

### Creyon style plots (IRE/Total & dIRE1/Total on the same plot)

tbl = read.table(paste0(output_dir, "creyon_style_plot_SecondSet_TABLE_FOR_PLOTTING.txt"), sep = "\t", stringsAsFactors = F, header =T)
#tbl = read.table("/Users/annaminkina/Downloads/1.13\ snca.longread_counts_new_names.csv", sep = ",", stringsAsFactors = F, header =T)

#tbl$ire_ov_total = tbl$IRE/tbl$total
#tbl$dire1_ov_total = tbl$dIRE1/tbl$total

#tbl$treatment = gsub("CR-AA-0", "", tbl$ASO)
#tbl$treatment = gsub("CR-", "", tbl$ASO)
#tbl$treatment = gsub("CR", "", tbl$ASO)

list_of_treatments = unique(tbl$treatment)

for(i in 1:length(list_of_treatments)){
  temp_treatment = list_of_treatments[i]
  tbl_sub = tbl[which(tbl$treatment == temp_treatment),]
  ire_tbl = tbl_sub[,c("Dose", "ire_ov_total")]
  ire_tbl$isoform = "IRE"
  colnames(ire_tbl) = c("Dose", "iso_ov_total", "Isoform")
  dire1_tbl = tbl_sub[,c("Dose", "dire1_ov_total")]
  dire1_tbl$isoform = "dIRE1"
  colnames(dire1_tbl) = c("Dose", "iso_ov_total", "Isoform")
  tbl_for_plotting = as.data.frame(rbind(ire_tbl, dire1_tbl))
  line_plot = ggplot(tbl_for_plotting, aes(x = as.factor(Dose), y = iso_ov_total, group = Isoform, linetype=Isoform)) + 
    geom_point(aes(shape=Isoform)) +
    stat_summary(fun.data=mean_se,geom="line") +
    #stat_summary(fun.data=mean_se,geom="errorbar", width = .2) +
    theme_classic() +
    #scale_fill_manual(values = cols_to_use) +
    xlab("Dose (nM)") +
    #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
    ylab("Isoform/Total SNCA ") +
    ggtitle(temp_treatment) +
    theme(legend.position="none") +
    ylim(0,.8)
  
  line_plot_no_background = ggplot(tbl_for_plotting, aes(x = as.factor(Dose), y = iso_ov_total, group = Isoform, linetype=Isoform)) + 
    geom_point(aes(shape=Isoform)) +
    stat_summary(fun.data=mean_se,geom="line") +
    #stat_summary(fun.data=mean_se,geom="errorbar", width = .2) +
    theme_classic() +
    #scale_fill_manual(values = cols_to_use) +
    xlab("Dose (nM)") +
    #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
    ylab("Isoform/Total SNCA ") +
    ggtitle(temp_treatment) +
    theme(legend.position="none") +
    ylim(0,.8) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  
  pdf(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_set2.pdf"), height = 3, width = 3)
  print(line_plot)
  dev.off()
  
  png(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_set2.png"), height = 3, width = 3, units = 'in', res = 300)
  print(line_plot)
  dev.off()
  
  png(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_for_Valerie_set2.png"), height = 3, width = 3, units = 'in', res = 300, bg="transparent")
  print(line_plot_no_background)
  dev.off()
}

#write.table(tbl, paste0(output_dir, "creyon_style_plot_SecondSet_TABLE_FOR_PLOTTING.txt"), sep = "\t", col.names = T, row.names = F, quote = F)


#### first set of ASOs, same plot as above

tbl = read.table(paste0(output_dir, "creyon_style_plot_FirstSet_TABLE_FOR_PLOTTING.txt"), sep = "\t", stringsAsFactors = F, header =T)
#tbl$ire_ov_total = tbl$IRE/tbl$total
#tbl$dire1_ov_total = tbl$dIRE1/tbl$total

#tbl$treatment = gsub("CR-AA-0", "", tbl$ASO)
#tbl$treatment = gsub("CR-", "", tbl$treatment)

list_of_treatments = unique(tbl$treatment)

for(i in 1:length(list_of_treatments)){
  temp_treatment = list_of_treatments[i]
  tbl_sub = tbl[which(tbl$treatment == temp_treatment),]
  ire_tbl = tbl_sub[,c("Dose", "ire_ov_total")]
  ire_tbl$isoform = "IRE"
  colnames(ire_tbl) = c("Dose", "iso_ov_total", "Isoform")
  dire1_tbl = tbl_sub[,c("Dose", "dire1_ov_total")]
  dire1_tbl$isoform = "dIRE1"
  colnames(dire1_tbl) = c("Dose", "iso_ov_total", "Isoform")
  tbl_for_plotting = as.data.frame(rbind(ire_tbl, dire1_tbl))
  line_plot = ggplot(tbl_for_plotting, aes(x = as.factor(Dose), y = iso_ov_total, group = Isoform, linetype=Isoform)) + 
    geom_point(aes(shape=Isoform)) +
    stat_summary(fun.data=mean_se,geom="line") +
    #stat_summary(fun.data=mean_se,geom="errorbar", width = .2) +
    theme_classic() +
    #scale_fill_manual(values = cols_to_use) +
    xlab("Dose (nM)") +
    #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
    ylab("Isoform/Total SNCA ") +
    ggtitle(temp_treatment) +
    theme(legend.position="none") +
    ylim(0,.8)
  
  line_plot_no_background = ggplot(tbl_for_plotting, aes(x = as.factor(Dose), y = iso_ov_total, group = Isoform, linetype=Isoform)) + 
    geom_point(aes(shape=Isoform)) +
    stat_summary(fun.data=mean_se,geom="line") +
    #stat_summary(fun.data=mean_se,geom="errorbar", width = .2) +
    theme_classic() +
    #scale_fill_manual(values = cols_to_use) +
    xlab("Dose (nM)") +
    #ylab(paste0("Human ", expression(alpha), "-Synuclein (ng/mL)")) +
    ylab("Isoform/Total SNCA ") +
    ggtitle(temp_treatment) +
    theme(legend.position="none") +
    ylim(0,.8) + 
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  
  pdf(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_set1.pdf"), height = 3, width = 3)
  print(line_plot)
  dev.off()
  
  png(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_set1.png"), height = 3, width = 3, units = 'in', res = 300)
  print(line_plot)
  dev.off()
  
  png(paste0(output_dir, "creyon_style_plot_", temp_treatment, "_for_Valerie_set1.png"), height = 3, width = 3, units = 'in', res = 300, bg="transparent")
  print(line_plot_no_background)
  dev.off()
}

#write.table(tbl, paste0(output_dir, "creyon_style_plot_FirstSet_TABLE_FOR_PLOTTING.txt"), sep = "\t", col.names = T, row.names = F, quote = F)





