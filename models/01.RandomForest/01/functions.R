source("models/01.RandomForest/functions_RandomForest.R", encoding = "utf-8")

add_pca_scores <- function(target_data, train_data, threshold = 0.9) {

  # 主成分分析
  pca.train <- train_data %>%

    # 対象データを選択
    dplyr::select(
      T_Bil,
      D_Bil,
      ALP,
      ALT_GPT,
      AST_GOT,
      TP,
      Alb,
      AG_ratio
    ) %>%

    # 主成分分析の実行
    # 標準化も実施
    prcomp(scale = T)

  # 累積寄与率が threshold を最初に超える主成分を特定
  idx.max_components <- which.max(summary(pca.train)$importance[3,] > threshold)

  target_data %>%

    # 主成分スコアの付与
    dplyr::bind_cols(
      tibble::as_tibble(predict(pca.train, target_data)[, 1:idx.max_components])
    )
}

# add_knn_features <- function(target_data, train_data, k = 1) {
#   # 関数のロード
#   fnc <- readr::read_file("https://raw.githubusercontent.com/you1025/knn_feature_extraction/master/knn_feature_extraction.R") %>%
#     parse(text = .)
#   eval(fnc)
# 
#   df.knn_d <- train_data %>%
# 
#     # 対象データを選択
#     dplyr::select(
#       T_Bil,
#       D_Bil,
#       ALP,
#       ALT_GPT,
#       AST_GOT,
#       TP,
#       Alb,
#       AG_ratio,
#       disease
#     ) %>%
# 
#     # 標準化
#     dplyr::mutate(
#       dplyr::across(-disease, function(x) { scale(x)[,1] })
#     ) %>%
# 
#     # knn feature extraction
#     add_knn_d_columns(col_class = disease, k = k) %>%
# 
#     dplyr::select(dplyr::matches("(TRUE|FALSE)"))
# 
#   dplyr::bind_cols(
#     target_data,
#     df.knn_d
#   )
# }
