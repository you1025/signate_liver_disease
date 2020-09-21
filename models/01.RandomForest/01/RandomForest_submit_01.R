library(tidyverse)
library(tidymodels)

source("models/01.RandomForest/01/functions.R", encoding = "utf-8")

# Data Load ---------------------------------------------------------------

# 訓練データ
df.train_data <- load_train_data("data/01.input/train.csv") %>% clean()
df.test_data  <- load_test_data("data/01.input/test.csv")   %>% clean()

# 前処理レシピの作成
recipe <- create_recipe(df.train_data)


# 前処理済みデータ ---------------------------------------------------------------

trained_recipe <- recipes::prep(recipe, training = df.train_data)
df.train.raw <- recipes::juice(trained_recipe)
df.train <- df.train.raw %>%
  add_features_per_category(df.train.raw) %>%
  # 主成分スコアの追加
  add_pca_scores(., df.train.raw) %>%
  dplyr::mutate(disease = factor(disease, levels = c(TRUE, FALSE)))
df.test <- recipes::bake(trained_recipe, df.test_data) %>%
  add_features_per_category(df.train.raw) %>%
  # 主成分スコアの追加
  add_pca_scores(df.train.raw)


# Model -------------------------------------------------------------------

model <- parsnip::rand_forest(
  mode = "classification",
  mtry  = 3,
  trees = 500,
  min_n = 1
) %>%
  parsnip::set_engine(
    engine = "ranger",
    max.depth = 10,
    num.threads = 8
  )


# Predict by Test Data ----------------------------------------------------

# seed のサンプリング
sample(1:100000, size = 10, replace = F) %>%

  purrr::map_dfr(function(seed, model, train_data, test_data) {

    model.fitted <-

      # seed の設定
      parsnip::set_args(model, seed = seed) %>%

      # 学習
      parsnip::fit(
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
        ,
        train_data
      )

    # 予測結果データセット
    tibble(
      id = 0:(nrow(test_data)-1),
      predicted = predict(model.fitted, test_data, type = "prob")$.pred_TRUE
    )
  }, model = model, train_data = df.train, test_data = df.test) %>%

  # seed averaging
  dplyr::group_by(id) %>%
  dplyr::summarise(predicted = mean(predicted)) %>%

  # ファイルへの書き出し
  {
    df.result <- (.)
    
    # ファイル名
    filename <- stringr::str_c(
      "RandomForest",
      lubridate::now(tz = "Asia/Tokyo") %>% format("%Y%m%dT%H%M%S"),
      sep = "_"
    ) %>%
      stringr::str_c("csv", sep = ".")
    
    # 出力ファイルパス
    filepath <- stringr::str_c("models/01.RandomForest/01/data/", filename, sep = "/")
    
    # 書き出し
    readr::write_csv(df.result, filepath, col_names = F)
  }
