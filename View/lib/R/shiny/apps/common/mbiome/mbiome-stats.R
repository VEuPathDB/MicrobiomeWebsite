MicrobiomeStats <- R6Class("MicrobiomeStats",
    private = list(
      microbiome_study_obj = NULL
    ),
    active = list(
      microbiome_study = function(microbiome_study){
        if(missing(microbiome_study)) return(private$microbiome_study_obj)
        private$microbiome_study_obj = microbiome_study
      }
    ),
   public = list(
     initialize = function(microbiome_study) {
       private$microbiome_study_obj <- microbiome_study
     },

     calculate_correlation = function(taxonomy_level = "Phylum", cor_type = "tm"){

       if(identical(cor_type, "tm") |  identical(cor_type, "mm")){
         quantitative <- private$microbiome_study_obj$sample_table$get_quantitative_metadata()
         filtered_categories <- private$microbiome_study_obj$sample_table$get_filtered_categories()

         quantitative <- quantitative[ quantitative %chin% filtered_categories ]
         metadata_data <- private$microbiome_study_obj$get_metadata_as_column(quantitative)
         metadata_data[,SampleName:=NULL]

         cols <- colnames(metadata_data)

         metadata_data[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
         factor2<-metadata_data
         if(identical(cor_type, "mm")){
           factor1<-factor2
           column_factor1<-"metadata.1"
           column_factor2<-"metadata.2"
         }else{
           column_factor2<-"metadata"
         }
       }
       if(identical(cor_type, "tm") |  identical(cor_type, "tt")){
         abundance_data <- private$microbiome_study_obj$get_otu_as_column(taxonomy_level)
         abundance_data[,SampleName:=NULL]

         cols <- colnames(abundance_data)
         abundance_data[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
         factor1<-abundance_data

         if(identical(cor_type, "tt")){
           factor2<-factor1
           column_factor1<-paste0(taxonomy_level,".1")
           column_factor2<-paste0(taxonomy_level,".2")
         }else{
           column_factor1<-taxonomy_level
         }
       }
       number_rows <- ncol(factor1)*ncol(factor2)

       # pairwise.complete<-cor(data.cor,consumo, method="spearman", use="pairwise.complete.obs")



       result<-data.table("taxon"=character(),"metadata"=character(),"rho"=numeric(),"p"=numeric(),"psize"=numeric())

       columns_data<-colnames(factor1)
       columns_metadata<-colnames(factor2)
       for(i in 1:length(columns_data)) {
         for (j in 1:length(columns_metadata)) {
           NA_ = which(is.na(factor2[[j]]))
           if(length(NA_)>0){
             rho = cor(factor1[[i]][-NA_], factor2[[j]][-NA_], method = "spearman")
             p = cor.test(factor1[[i]][-NA_], factor2[[j]][-NA_], method = "spearman")[3]$p.value
           }else{
             rho = cor(factor1[[i]], factor2[[j]], method = "spearman")
             p = cor.test(factor1[[i]],factor2[[j]], method = "spearman")[3]$p.value
           }
           if(!is.na(rho)){
             line<-data.table(columns_data[i], columns_metadata[j], rho, p, log10(0.5+2/p))
             result<-rbindlist(list(result, line))
           }
         }
       }
       result<-subset(result, taxon!=metadata)
       colnames(result)<-c(column_factor1, column_factor2, "rho", "pvalue", "size")

       result
     }
   )
)
