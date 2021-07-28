MicrobiomeData <- R6Class("MicrobiomeData",
      private = list(
        common_samples = NULL,
        different_samples = NULL
      ),
       public = list(
         otu_table = NULL,
         sample_table = NULL,
         initialize = function(otu_table = NA, sample_table = NA) {
           self$otu_table <- otu_table
           self$sample_table <- sample_table
          
           sample_from_metadata <- self$sample_table$get_sample_names()
           sample_from_taxa <- self$otu_table$get_sample_names()
            
           
           if(isTRUE(all.equal(sample_from_metadata,sample_from_taxa))){
             private$common_samples <- sample_from_taxa
           }else{
             private$common_samples <-
               intersect(sample_from_metadata, sample_from_taxa)
             quant_s_otu <- self$otu_table$get_sample_count()
             quant_s_charac <- self$sample_table$get_sample_count()
             if(quant_s_otu>quant_s_charac){
               private$different_samples <- setdiff(
                 self$otu_table$get_sample_names(),
                 self$sample_table$get_sample_names()
               )
             }else{
               private$different_samples <- setdiff(
                 self$sample_table$get_sample_names(),
                 self$otu_table$get_sample_names()
               )
             }
           }

         },
         get_otu_by_sample = function(sample_name, taxonomy_level = NULL){
           self$otu_table$get_otu_by_sample(sample_name, taxonomy_level)
         },
         get_otu_as_column = function(taxonomy_level = NULL){
          if(is.null(private$different_samples)){
            self$otu_table$get_otu_as_column(taxonomy_level)
          }else{
            self$otu_table$get_otu_as_column(taxonomy_level, private$different_samples)
          }
         },
         get_metadata_as_column = function(metadata = NULL){
           if(!is.null(private$different_samples)){
            self$sample_table$get_metadata_as_column(metadata, private$different_samples)
           }else{
             self$sample_table$get_metadata_as_column(metadata)
           }
         },
         get_sample_names = function(){
           private$common_samples
         },
         get_sample_count = function(){
           length(self$get_sample_names())
         },
         get_filtered_categories = function(){
           self$sample_table$get_filtered_categories()
         },
         get_otus_by_level = function(taxonomy_level=NULL){
           self$otu_table$get_otus_by_level(taxonomy_level)
         },
         get_top_n_by_method = function(taxonomy_level=NULL, n=10, ranking_method = "Median", add_other=T, removeZeros=F){
            self$otu_table$get_top_n_by_method(taxonomy_level, n, ranking_method, add_other, removeZeros)
         },
         get_top_n_by_mean = function(taxonomy_level=NULL, n=10, add_other=T){
            self$otu_table$get_top_n_by_mean(taxonomy_level, n, add_other)
         },
         get_single_otu = function(taxonomy_level=NULL, otu_name, add_other=F, keep_all=F){
           self$otu_table$get_single_otu(taxonomy_level, otu_name, add_other, keep_all)
         }
       )
)
