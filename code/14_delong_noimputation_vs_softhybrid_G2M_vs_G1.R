library(readr)
library(dplyr)
library(tidyr)
library(pROC)

# =========================================================
# 1. read
# =========================================================
df <- read_tsv(
  "ROC_G2M_vs_G1_NoImputation_vs_SoftHybrid_scanpyScore/roc_inputs_noimputation_vs_softhybrid_G2M_vs_G1_scanpyScore.tsv"
)

# =========================================================
# 2. basic check
# =========================================================
cell_check <- df %>%
  group_by(method) %>%
  summarise(
    n_rows = n(),
    n_cells = n_distinct(cell_id),
    .groups = "drop"
  )

print(cell_check)

# =========================================================
# 3. keep common cells only
# =========================================================
common_cells <- df %>%
  group_by(method) %>%
  summarise(cell_id = list(unique(cell_id)), .groups = "drop") %>%
  pull(cell_id) %>%
  Reduce(intersect, .)

df_common <- df %>%
  filter(cell_id %in% common_cells)

# =========================================================
# 4. truth consistency check
# =========================================================
truth_check <- df_common %>%
  dplyr::select(method, cell_id, y_true) %>%
  pivot_wider(names_from = method, values_from = y_true)

truth_check_flag <- truth_check %>%
  mutate(
    all_same = apply(dplyr::select(., -cell_id), 1, function(x) length(unique(x)) == 1)
  )

print(table(truth_check_flag$all_same))

# =========================================================
# 5. split and align
# =========================================================
df_noimp <- df_common %>%
  filter(method == "No_imputation") %>%
  arrange(cell_id)

df_soft <- df_common %>%
  filter(method == "SoftHybrid") %>%
  arrange(cell_id)

stopifnot(all(df_noimp$cell_id == df_soft$cell_id))
stopifnot(all(df_noimp$y_true == df_soft$y_true))

# =========================================================
# 6. ROC objects
# =========================================================
roc_noimp <- roc(
  response = df_noimp$y_true,
  predictor = df_noimp$y_score,
  quiet = TRUE,
  direction = "<"
)

roc_soft <- roc(
  response = df_soft$y_true,
  predictor = df_soft$y_score,
  quiet = TRUE,
  direction = "<"
)

auc_noimp <- as.numeric(auc(roc_noimp))
auc_soft  <- as.numeric(auc(roc_soft))

# =========================================================
# 7. paired DeLong test
# =========================================================
delong_res <- roc.test(
  roc_noimp,
  roc_soft,
  method = "delong",
  paired = TRUE
)

print(delong_res)

# =========================================================
# 8. summary table
# =========================================================
res_tbl <- tibble(
  method1 = "No_imputation",
  method2 = "SoftHybrid",
  auc1 = auc_noimp,
  auc2 = auc_soft,
  delta_auc = auc_soft - auc_noimp,
  statistic = as.numeric(delong_res$statistic),
  p_value = delong_res$p.value
)

print(res_tbl)

write_tsv(
  res_tbl,
  "delong_noimputation_vs_softhybrid_G2M_vs_G1.tsv"
)