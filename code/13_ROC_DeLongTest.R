library(dplyr)
library(pROC)
library(readr)

rm(list = ls())

df_art  <- read.delim("roc_article_inputs.tsv")
df_soft <- read.delim("roc_soft_inputs.tsv")

df_merge <- inner_join(
  df_art,
  df_soft,
  by = c("comparison", "phase_score", "idx"),
  suffix = c("_art", "_soft")
)

stopifnot(all(df_merge$y_true_art == df_merge$y_true_soft))

delong_res <- df_merge %>%
  group_by(comparison, phase_score) %>%
  group_modify(~{
    y_true <- .x$y_true_art
    roc1 <- roc(y_true, .x$y_score_art, quiet = TRUE)
    roc2 <- roc(y_true, .x$y_score_soft, quiet = TRUE)
    tst <- roc.test(roc1, roc2, paired = TRUE, method = "delong")
    
    tibble(
      auc_article = as.numeric(auc(roc1)),
      auc_soft = as.numeric(auc(roc2)),
      delta_auc = as.numeric(auc(roc2) - auc(roc1)),
      p_value = tst$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(p_adj_BH = p.adjust(p_value, method = "BH"))

delong_res

write_tsv(delong_res, "delong_results_SHvsZero.tsv")
