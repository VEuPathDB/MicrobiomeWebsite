library(R6)
library(data.table)
library(tidyr)

source("../../lib/mbiome/mbiome-data.R")
source("../../lib/mbiome/mbiome-utils.R")
source("../../lib/mbiome/otu-table.R")
source("../../lib/mbiome/sample-table.R")

#' Function to read input files for MicrobiomeDB and returns a MicrobiomeData
#'   object.
#' @param taxa_abundance_path The path to the taxa abundance file.
#' @param sample_path The path to the sample characteristics file.
#' @param aggregate_by Taxonomic level to aggregate your data (Phylum, Class, Order,
#' Family, Genus, Species). Defaults to Phylum.
#' @param use_relative_abundance If it's true the relative abundance will be used to manipulate
#'   the data, otherwise the absolute abundance will be used. Defaults to TRUE.
#' @return Returns a MicrobiomeData object.
#' @keywords microbiome microbiomedb
#' @export
#' @example
#' import.eupath("~/Projects/microbiomedb/TaxaRelativeAbundance.txt",
#'     "~/Projects/microbiomedb/Characteristics.txt")
import.eupath <- function(taxa_abundance_path, sample_path, datasets_path, aggregate_by="Phylum", use_relative_abundance=T){
  df_abundance <-
    fread(
      taxa_abundance_path,
      col.names = c("Sample","Taxon", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "RelativeAbundance", "AbsoluteAbundance", "EmptyColumn"),
      colClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "integer", "character")
    )
  df_abundance[,EmptyColumn:=NULL]

  df_sample <-
    fread(
      sample_path,
      header = T,
      col.names = c("SampleName", "Source", "Property", "Value", "Type", "Filter", "EmptyColumn"),
      colClasses = c("character", "character", "character", "character", "character", "character", "character")
    )

  df_datasets <- 
    fread(
      datasets_path,
      header = T,
      col.names = c("Sample ID", "dataset_presenter_id", "display_name", "Dataset Summary", "EmptyColumn"),
      colClasses = c("character", "character", "character", "character", "character")
    )
 df_datasets_name <- data.frame (SampleName = df_datasets$"Sample ID", Source = "data set", Property = "Dataset name", Value = df_datasets$"display_name", Type = "string", Filter = "membership", EmptyColumn = "", stringsAsFactors=F)
 df_datasets_summary <- data.frame (SampleName = df_datasets$"Sample ID", Source = "data set", Property = "Dataset summary", Value = df_datasets$"Dataset Summary", Type = "string", Filter = "membership", EmptyColumn = "", stringsAsFactors = F)

  otu_object <- OTUClass$new(df_abundance, aggregate_by = aggregate_by, use_relative_abundance=use_relative_abundance)
  sample_object <- SampleClass$new(rbindlist(list(df_sample, df_datasets_name, df_datasets_summary)))

  MicrobiomeData$new(otu_object, sample_object)
}
