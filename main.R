library(tercen)
library(dplyr)
library(NormalyzerDE)

#library(tim)
#options("tercen.workflowId" = "6015a4dd34cef273755e1a1b1500427b")
#options("tercen.stepId"     = "d31241f6-173f-473a-9307-2b4b3c5c0882")

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

#ctx$save(df_out)

tim::build_test_data(res_table = df_out, ctx = ctx, test_name = "test1")
