SampleClass <- R6Class("SampleClass",
      private = list(
        dcast_sample_dt = NULL,
        quantitative_metadata = NULL,
        qualitative_metadata = NULL,
        sample_names = NULL
      ),
      public = list(
        sample_dt = NULL,
        initialize = function(sample_dt = NA) {
          self$sample_dt <- sample_dt
          private$sample_names <- unique(sample_dt$SampleName)
          setkey(self$sample_dt, Type)
          private$quantitative_metadata <-
            unique(subset(self$sample_dt, Type=="number")[["Property"]])
          private$qualitative_metadata <-
            unique(subset(self$sample_dt, Type!="number")[["Property"]])
          setkey(self$sample_dt, NULL)
        },
        get_sample_count = function(){
          length(private$sample_names)
        },
        get_qualitative_metadata = function(filter = NULL){
          private$qualitative_metadata
        },
        get_quantitative_metadata = function(filter = NULL){
          private$quantitative_metadata
        },
        get_metadata_as_column = function(metadata = NULL, filter_samples = NULL) {
          if(is.null(private$dcast_sample_dt)){
            if(is.null(filter_samples)){
              private$dcast_sample_dt <-dcast(data = self$sample_dt,formula = SampleName~Property, value.var = "Value")
            }else{
              private$dcast_sample_dt <-dcast(
                data = subset(self$sample_dt, SampleName != filter_samples),formula = SampleName~Property, value.var = "Value")
            }
          }
          if(is.null(metadata)){
            private$dcast_sample_dt
          }else{
            private$dcast_sample_dt[,c("SampleName", metadata), with=F]
          }

        },

        get_sample_names = function(){
          private$sample_names
        },

        get_filtered_categories = function(){
          setkey(self$sample_dt, Property)
          columns <- unique(self$sample_dt[,Property])
          filtered_categories<-NULL
          for(i in 1:length(columns)){
            sample_part <- subset(self$sample_dt, Property==columns[i])
            if(identical(sample_part[1,Type],"number")){
              unique_factors <- as.factor(as.numeric(sample_part$Value))
            }else{
              unique_factors <- as.factor(sample_part$Value)
            }
            if(length(levels(unique_factors)) > 1){
              new_columns <- paste0(columns[i], " (",length(levels(unique_factors)), ")")
              filtered_categories[[new_columns]] <- columns[i]
            }
          }
          setkey(self$sample_dt, NULL)
          filtered_categories
        }
      )
)
