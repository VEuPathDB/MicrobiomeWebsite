phyloseq2Deseq2 <- function (physeq, design, ...) 
{
  if (is.null(sample_data(physeq, FALSE))) {
    stop("There must be sample_data present, for specifying experimental design. See ?phyloseq_to_deseq2")
  }
  if (!taxa_are_rows(physeq)) {
    physeq <- t(physeq)
  }
  countData = as(otu_table(physeq), "matrix")
  colData = data.frame(sample_data(physeq))
  if (requireNamespace("DESeq2")) {
    dds <- DESeq2::DESeqDataSetFromMatrix(countData, colData, 
                                          design, ...)
    return(dds)
  }
}