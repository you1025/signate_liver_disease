# mtry: 3, trees: xxx, min_n: x, max.depth: x, train_roc_auc: xxxxxxxxx, test_roc_auc: xxxxxxxxx - xxx

# mtry: 5, trees: 250, min_n: 1, max.depth: 0, train_roc_auc: 1, test_roc_auc: 0.9405921
# mtry: 3, trees: 250, min_n: 1, max.depth: 0, train_roc_auc: 1, test_roc_auc: 0.9430425
# mtry: 3, trees: 250, min_n: 1, max.depth: 8, train_roc_auc: 0.9994865, test_roc_auc: 0.9425797
# mtry: 3, trees: 400, min_n: 1, max.depth: 8, train_roc_auc: 0.9994970, test_roc_auc: 0.9432885 - Baseline

# mtry: 3, trees: 500, min_n: 1, max.depth: 10, train_roc_auc: 1.0000000, test_roc_auc: 0.9436780 - PC1〜PC5 追加

# train_roc_auc: 1, test_roc_auc: 0.9443999 - Bil + TP_Alb_AG_ratio
# mtry: 1, trees: 500, min_n: 1, max.depth: 10, train_roc_auc: 0.9999175, test_roc_auc: 0.9448746 - 決定株w





library(tidyverse)
library(tidymodels)
library(furrr)

source("models/01.RandomForest/01/functions.R", encoding = "utf-8")

# Data Load ---------------------------------------------------------------

df.train_data <- load_train_data("data/01.input/train.csv") %>% clean()

df.cv <- create_cv(df.train_data)


# Feature Engineering -----------------------------------------------------

recipe <- create_recipe(df.train_data)


# Model Definition --------------------------------------------------------

model <- parsnip::rand_forest(
  mode = "classification",
  mtry  = parsnip::varying(),
  trees = parsnip::varying(),
  min_n = parsnip::varying()
) %>%
  parsnip::set_engine(
    engine = "ranger",
    num.threads = 8,
    seed = 1025
  )


# Hyper Parameter ---------------------------------------------------------

df.grid.params <- dials::grid_regular(
  dials::mtry(range = c(1, 1)),
  dials::trees(range = c(500, 500)),
  dials::min_n(range = c(10, 10)),
  levels = 1
) %>%
  tidyr::crossing(
    max.depth = 6
  )
df.grid.params


# Parametr Fitting --------------------------------------------------------

# 並列処理
future::plan(future::multisession(workers = 1))

system.time({
  set.seed(1025)
  
  df.results <-
    
    # ハイパーパラメータの組み合わせごとにループ
#    furrr::future_pmap_dfr(df.grid.params, function(...) {
    purrr::pmap_dfr(df.grid.params, function(...) {
      
      # パラメータの適用
      model.applied <- parsnip::set_args(model, ...)
      
      # モデル構築用の説明変数を指定
      formula <- (
        disease ~
          Age
        + Gender
        + T_Bil
        + D_Bil
        + ALP
        + ALT_GPT
        + AST_GOT
        + TP
        + Alb
        + AG_ratio

        + PC1
        + PC2
        + PC3
        + PC4
        + PC5

        + Bil
        + TP_Alb_AG_ratio
      )
      
      # クロスバリデーションの分割ごとにモデル構築&評価
#      purrr::map_dfr(
      furrr::future_map_dfr(
        df.cv$splits,
        train_and_eval,
        recipe = recipe,
        model = model.applied,
        formula = formula
        ,.options = furrr::future_options(seed = 1025L)
      ) %>%
        
        # CV 分割全体の平均値を評価スコアとする
        dplyr::summarise_all(mean)
#    }, .options = furrr::future_options(seed = 1025L)) %>%
    }) %>%
    
    # 評価結果とパラメータを結合
    dplyr::bind_cols(df.grid.params) %>%
    
    # 評価スコアの順にソート(昇順)
    dplyr::arrange(
      desc(test_roc_auc)
    ) %>%
    
    dplyr::select(
      colnames(df.grid.params),
      
      train_roc_auc,
      test_roc_auc
    )
})
