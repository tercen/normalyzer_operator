library(tercen)
library(dplyr)
library(NormalyzerDE)

ctx <- tercenCtx()

select_type <- ctx$op.value('method', as.character, "mean")

noNorm <- function(rawData) {rawData}

norm_func <- switch(
  select_type,
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

norm_data <- ctx %>% 
  as.matrix(fill = NA) %>% 
  t() %>%
  norm_func()

df_out <- data.frame(
  norm = as.vector(norm_data),
  .ci  = rep(0:(ncol(norm_data) - 1L), each = nrow(norm_data)),
  .ri  = rep(seq.int(from = 0, to = nrow(norm_data) - 1L), ncol(norm_data))
) %>% ctx$addNamespace()

ctx$save(df_out)

