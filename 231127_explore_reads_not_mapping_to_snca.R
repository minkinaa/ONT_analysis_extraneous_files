
## exploring unexpected ONT results

tbl = read.table("/Users/annaminkina/Downloads/LL1-231114-01_minimap2_sorted_grep_AAGAAAGTTGTCGGTGTCTTTGTG_few_cols_w_length", header =F, stringsAsFactors = F, sep = " ")
colnames(tbl) = c("read_name", "flag", "chr", "start", "seq_length")

#look at read length distribution
hist(tbl[which(tbl$seq_length < 2500),]$seq_length, breaks = 1000)

# just mapping
hist(tbl[which(tbl$seq_length < 2500 & tbl$flag %in% c(0,16)),]$seq_length, breaks = 1000)

# just not mapping
hist(tbl[which(tbl$seq_length < 2500 & tbl$flag %in% c(4)),]$seq_length, breaks = 1000)

read_length_dist = as.data.frame(table(tbl$seq_length))

# length with highest 
read_length_dist[which(read_length_dist$Freq == max(read_length_dist$Freq)), ]

table(tbl[which(tbl$seq_length == 583),]$chr)

#looks like majority on chr9
as.data.frame(table(tbl[which(tbl$chr == 9 & tbl$seq_length == 583),]$start))

#looks like chr9:128203431

#confirm locus on chr9 
hist(tbl[which(tbl$chr == 9),]$start,)

# now can look up that section in UCSC genome browser and/or make a bam slice

#if we remove chr9
hist(tbl[which(tbl$seq_length < 2500 & tbl$flag %in% c(0,16) & tbl$chr != 9),]$seq_length, breaks = 1000)

## what about the reads 







