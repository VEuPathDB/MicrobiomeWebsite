# setwd("~/Projects/microbiomedb/shiny_hover/alpha_diversity")
# library(phyloseq)
# library(reshape2)
# #
# df_abundance <-
#   read.csv(
#     "MicrobiomeSampleByMetadata_TaxaRelativeAbundance.txt",
#     sep = "\t",
#     col.names = c("Sample","Taxon", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Abundance", "Nada")
#   )
# 
# df_abundance$Abundance <- round(df_abundance$Abundance*100)
# # hist(subset(df_abundance, Sample=="EUSMPL0020-1-1")$Abundance)
# # sum(subset(df_abundance, Sample=="EUSMPL0020-1-1")$Abundance)
# 
# df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "Abundance")
# 
# OTU_MATRIX <- df_abundance.formatted[,8:ncol(df_abundance.formatted)]
# 
# OTU = otu_table(OTU_MATRIX, taxa_are_rows = T)
# #
# TAX_MATRIX <- df_abundance.formatted[,1:7]
# TAX_MATRIX <- as.matrix(TAX_MATRIX)
# 
# TAX <- tax_table(TAX_MATRIX)
# 
# #
# # colnames(TAX)<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
# #
# df_sample <-
#   read.csv(
#     "fran_updated_Characteristics_tabfile.txt",
#     sep = "\t",
#     col.names = c("SampleName", "Source", "Property", "Value", "Type", "Filter", "Nada"),
#     na.strings = c("NA", "<NA>")
#   )
# 
# 
# df_sample.formatted <- dcast(data = df_sample,formula = SampleName~Property, value.var = "Value")
# rownames(df_sample.formatted) <- df_sample.formatted[,1]
# c <- colnames(df_sample.formatted)
# d <- make.names(c)
# colnames(df_sample.formatted) <- d
# 
# hash_sample_names<-new.env()
# for (i in 1:length(d)) {
#   hash_sample_names[[c[i]]] <- d[i]
# }
# hash_sample_names[[c[7]]]
# c[7]
# hash_sample_names[[d[7]]]
# 
# SAMPLE <- sample_data(df_sample.formatted)
# physeq1 = phyloseq(OTU, TAX, SAMPLE)
# 
# sample_data(physeq1)

# df_sample.formatted[,"delivery"]
# cesarean <- subset(df_sample.formatted, delivery=="Cesarean")
# df_sample.formatted[df_sample.formatted[,'delivery'] == 'Cesarean',]
# df_sample.formatted[df_sample.formatted[['delivery']] == 'Cesarean',]
# 

# cesarean
# 
# p<-plot_richness(physeq1, x="delivery", measures = c("Chao1", "Simpson"))
# str(p)
# p$data
# sample_data(physeq1)[,hash_sample_names[["delivery"]]]
# df_sample.formatted
# str(p)
# estimate_richness(physeq1, split=T)
# rich
# p
# subset(rich, variable=="Chao1")
# str(p)
# class(p)
# subset(p$data, variable=="Chao1")
# 
# er <- estimate_richness(physeq1, measures = c("Chao1", "Shannon", "ACE"))
# er
# er[,a]
# er[,"se.ACE"]


# plot_richness(er)
# plot_bar(physeq1, fill="Phylum")
# category <- "biological sex"
# plot_richness(physeq1, x=hash_sample_names[[category]], measures = c("Chao1", "Shannon"))

# df_sample.formatted
# subset(df_sample.formatted,!is.na(df_sample.formatted$mom_prenatal_abx_trimester))
# df_sample.formatted
# subset(df_sample, df_sample$Property=="mom_prenatal_abx_trimester")
# df_sample.formatted
# df_sample.formatted


# plot_bar(physeq1, fill="Phylum")
# 
# 
# plot_richness(physeq1, x="biological.sex", measures = c("Chao1", "Shannon"))
# 
# plot_richness(physeq1, x="biological sex", measures = c("Chao1", "Shannon"))
# +theme(
# 	panel.grid.major.x = element_blank()
# )+geom_point(size = 4, alpha= 0.5)
# 
# plot_richness(physeq1, x="subjec", measures=c("Chao1", "Shannon"))

# 
# # Abundance
# plot_bar(physeq1, fill = "Phylum")
# plot_bar(physeq1, fill = "Family")
# plot_bar(physeq1, fill = "Genus")
# plot_bar(physeq1, fill = "Order")
# SAMPLE
# #Alpha
# # plot_richness(physeq1, x="day_of_life", color="delivery", measures = c("Chao1", "Shannon", "Simpson"))
# # p <- plot_richness(physeq1, measures = c("Chao1", "Shannon", "Simpson"))
# # ntaxa(physeq1)
# # otu_table(physeq1)
# samples <- sample_data(physeq1)
# # head(samples[,'biological.sex'])
# # levels(as.factor(samples[,'biological.sex']))
# # str(samples[,'biological.sex'])
# samples
# a <- lapply(samples[,'month_of_life'], as.character)
# # levels(as.factor(a[[1]]))
# #Beta
# # ordination = ordinate(physeq1, method = "PCoA", distance = "jsd")
# # plot_ordination(physeq1, ordination, color="delivery")
# # 
# # #Extra
# # plot_heatmap(physeq1, taxa.label="Family")
# # plot_heatmap(physeq1, taxa.label="Order")
# 
