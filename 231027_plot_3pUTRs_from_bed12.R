options(stringsAsFactors = F)
library(ggplot2)
library(ggpubr)
args = commandArgs(trailingOnly=TRUE)

make_3pUTR_plot = function(isoform_list, plot_start, plot_end, title, aso_start_c, aso_end_c){
  #isoform_list = isoform_list
  #plot_start = plot_start_coord
  #plot_end = plot_end_coord
  
  plot_title = title
  
  max_value = max(pileup_file[which(pileup_file$isoform %in% isoform_list & pileup_file$start > plot_start & pileup_file$start < plot_end),]$count)
  bar_height = .02*max_value
  space_size = .25*bar_height
  exon6_bar_midpoint = 0-space_size-(.5*bar_height)
  #dire1_bar_midpoint = 0-space_size-bar_height-space_size-(.5*bar_height)
  
  plot_to_return = ggplot() + xlim(plot_start, plot_end)
  
  for(i in 1:length(isoform_list)){
    temp_iso = isoform_list[i] 
    temp_col = iso_cols_tbl[which(iso_cols_tbl$isoforms == temp_iso),2]
    plot_to_return = plot_to_return + geom_bar(data = pileup_file[which(pileup_file$isoform == temp_iso),], aes(x = start, y = count), stat = "identity", fill = temp_col)
  }
  
  plot_to_return = plot_to_return + theme_bw() +
    xlab("position") + 
    ylab("coverage") + 
    ggtitle(plot_title) +
    geom_rect(aes_string(xmin = utr3p_start_coords, xmax = utr3p_end_coords, ymin = 0-space_size-bar_height, ymax = 0- space_size), fill = "purple", alpha = .25) +
    geom_rect(aes_string(xmin = enrichment_primer_start, xmax = enrichment_primer_end, ymin = 0-space_size-bar_height-space_size-bar_height, ymax = 0-space_size-bar_height-space_size), fill = "black", alpha = .75)
  #geom_rect(aes_string(xmin = aso_start_c, xmax = aso_end_c, ymin = 0-space_size-bar_height, ymax = 0- space_size), fill = "black", alpha = .25)
  
  
  # for(i in 1:nrow(annot_isoform_tbl)){
  #   temp_iso = annot_isoform_tbl[i,"isoform_names"]
  #   temp_col = annot_isoform_tbl[i,"cols"]
  #   temp_x = annot_isoform_tbl[i,"isoform_end_coords"]
  #   if(temp_iso == "IRE"){
  #     temp_y = ire_bar_midpoint
  #   } else if (temp_iso == "dIRE1"){
  #     temp_y = dire1_bar_midpoint
  #   }
  #   plot_to_return = plot_to_return + geom_point(aes_string(x = temp_x, y = temp_y), color = temp_col)
  # }
  return(plot_to_return)
}

prefix = "ONT-0035"
output_dir = paste0("/Users/annaminkina/Documents/Data/230918_ASO_analysis/", prefix, "/")

bed12_etc_file_name = paste0(output_dir, prefix, "_snca_slice_buffer12_bed12_plus_isoform_calls_etc.txt")
bed12_etc_file = read.table(bed12_etc_file_name, sep = "\t", header = F, stringsAsFactors = F)
bed12_etc_file_few_cols = bed12_etc_file[,c(4,1,2,3,10,13,14)]
colnames(bed12_etc_file_few_cols) = c("read_name", "chr", "start", "end", "num_exons", "isoform_code", "isoform")

system2("mkdir", args = c(paste0(output_dir, prefix, "_3pUTR_plots")))
UTR3p_plot_output_dir = paste0(output_dir, prefix, "_3pUTR_plots/")


### make pileup file:
isoform_list = unique(bed12_etc_file_few_cols$isoform)
pileup_file = data.frame()
pileup_file_top_10 = data.frame()
for(i in 1:length(isoform_list)){
  temp_iso = isoform_list[i]
  iso_bed12 = bed12_etc_file_few_cols[which(bed12_etc_file_few_cols$isoform == temp_iso),]
  iso_pileup = as.data.frame(table(iso_bed12$start))
  iso_pileup$isoform = temp_iso
  pileup_file = as.data.frame(rbind(pileup_file, iso_pileup))
  
  iso_pileup_ord = iso_pileup[order(-iso_pileup$Freq),]
  if(nrow(iso_pileup_ord) >= 10){
    iso_pileup_ord_top_10 = iso_pileup_ord[c(1:10),]
  } else {
    iso_pileup_ord_top_10 = iso_pileup_ord[c(1:nrow(iso_pileup_ord)),]
  }
  pileup_file_top_10 = as.data.frame(rbind(pileup_file_top_10, iso_pileup_ord_top_10))
}
colnames(pileup_file) = c("start", "count", "isoform")
pileup_file$start = as.numeric(as.character(pileup_file$start))
pileup_file$count = as.numeric(as.character(pileup_file$count))
#pileup_file$end = pileup_file$end + 1 

colnames(pileup_file_top_10) = c("start", "count", "isoform")
pileup_file_top_10$start = as.numeric(as.character(pileup_file_top_10$start))
pileup_file_top_10$count = as.numeric(as.character(pileup_file_top_10$count))
#pileup_file_top_10$end =pileup_file_top_10$end + 1

utr3p_start_coords = 89724099
utr3p_end_coords = 89726660

enrichment_primer_start = 89726466
enrichment_primer_end = 89726485

plot_start_coord = utr3p_start_coords - 100
plot_end_coord = utr3p_end_coords + 100

plot_title_prefix = prefix

#make_3pUTR_plot(c("IRE", "dIRE1"),plot_start, plot_end, title)
#make_3pUTR_plot(c("dIRE1"),plot_start, plot_end, title)


IRE_dIRE1_plot = make_3pUTR_plot(c("IRE", "dIRE1"), plot_start_coord, plot_end_coord, paste0(plot_title_prefix, ", IRE, dIRE1"))
IRE_plot = make_3pUTR_plot(c("IRE"), plot_start_coord, plot_end_coord, paste0(plot_title_prefix, ", IRE"))
dIRE1_plot = make_3pUTR_plot(c("dIRE1"), plot_start_coord, plot_end_coord, paste0(plot_title_prefix,", dIRE1"))

combined_TSS_plot_all_and_split <- ggarrange(IRE_dIRE1_plot, IRE_plot, dIRE1_plot, ncol = 1)

pdf(paste0(UTR3p_plot_output_dir, plot_title_prefix, "_IRE_dIRE1_all_and_split_",plot_start_coord,"to", plot_end_coord,".pdf"), width = 12, height = 3*3)
print(combined_TSS_plot_all_and_split)
dev.off()

#png(paste0(tss_plot_output_dir, plot_title_prefix, "_IRE_dIRE1_all_and_split_",plot_start_coord,"to", plot_end_coord,".png"), width = 12*1000, height = 3*3*1000, res = 1200)
#print(combined_TSS_plot_all_and_split)
#dev.off()

pdf(paste0(UTR3p_plot_output_dir, plot_title_prefix, "_dIRE1_",plot_start_coord,"to", plot_end_coord,".pdf"), width = 12, height = 3)
print(dIRE1_plot)
dev.off()

pdf(paste0(UTR3p_plot_output_dir, plot_title_prefix, "_IRE_",plot_start_coord,"to", plot_end_coord,".pdf"), width = 12, height = 3)
print(IRE_plot)
dev.off()

pdf(paste0(UTR3p_plot_output_dir, plot_title_prefix, "_IRE_dIRE1_",plot_start_coord,"to", plot_end_coord,".pdf"), width = 12, height = 3)
print(IRE_dIRE1_plot)
dev.off()

