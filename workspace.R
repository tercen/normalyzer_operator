library(tercen)
library(dplyr)

# creating an operator library 
# setRepositories() (use 1 9 10 11 12 13)
# packrat::init(options = list(use.cache = TRUE))
# options(repos = c(getOption("repos"), BiocInstaller::biocinstallRepos()))

select_type <- "none"
# select_type <- ctx$op.value('method')

noNorm <- function(rawData){rawData}

norm_func <- switch(select_type,
                    "global intensity" = NormalyzerDE::globalIntensityNormalization,
                    "median" = NormalyzerDE::medianNormalization,
                    "mean" = NormalyzerDE::meanNormalization,
                    "vsn" = NormalyzerDE::performVSNNormalization,
                    "quantile" =  NormalyzerDE::performQuantileNormalization,
                    "smad" = NormalyzerDE::performSMADNormalization,
                    "cyclicLoess" = NormalyzerDE::performCyclicLoessNormalization,
                    "global RLR" = NormalyzerDE::performGlobalRLRNormalization,
                    "none" = noNorm
                    )

options( "tercen.workflowId" = "09f761309ae90820aac2581211009bff")
options( "tercen.stepId" = "6-4" )

ctx = tercenCtx()

data = select(ctx, .y, .ci, .ri )
data = reshape2::acast(data, .ri ~ .ci, value.var='.y', fun.aggregate=mean)
data[is.nan( data )] <- NA
norm_data <- norm_func(data)


output_df = data.frame(
  norm = as.vector(norm_data),
  .ci  = rep(0:(ncol(norm_data)-1), each=nrow(norm_data)),
  .ri  = rep(seq.int(from=0,to=nrow(norm_data)-1), ncol(norm_data)))

# save it to tercen

output_df <- ctx$addNamespace(output_df )
ctx$save(output_df )

