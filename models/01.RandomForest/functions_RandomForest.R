source("models/functions_global.R", encoding = "utf-8")

# Recipe ------------------------------------------------------------------

create_feature_engineerging_recipe <- function(data) {
  
  recipes::recipe(disease ~ ., data) %>%
    
    recipes::step_mutate(
      Bil = log(T_Bil + D_Bil),
      TP_Alb_AG_ratio = (TP + Alb) * AG_ratio,

      special_segment = dplyr::case_when(
        dplyr::between(Age,  0, 20) ~ "00_20",
        dplyr::between(Age, 21, 30) ~ "21_30",
        dplyr::between(Age, 31, 40) ~ "31_40",
        dplyr::between(Age, 41, 50) ~ "41_50",
        dplyr::between(Age, 51, 60) ~ "51_60",
        dplyr::between(Age, 61, 70) ~ "61_70",
        dplyr::between(Age, 71, 80) ~ "71_80"
      )
    )
}

# レシピの作成
create_recipe <- function(data) {

  recipe <- create_feature_engineerging_recipe(data)

  recipe %>%

    # 対数変換
    recipes::step_log(
      T_Bil,
      D_Bil,
      ALP,
      ALT_GPT,
      AST_GOT
    )
}


# Target Encoding ---------------------------------------------------------

# カテゴリ毎に target の代表値を算出
# カウントの極端に少ないカテゴリには補正あり
smoothed_categorical_value <- function(data, category, target, funs) {
  
  # smoothing parameters
  k <- 5
  f <- 1
  
  # for NSE
  category = dplyr::enquo(category)
  target   = dplyr::enquo(target)
  
  # category に依存しない集約値の算出
  # 補正のために使用
  df.outer_stat <- data %>%
    dplyr::group_by(special_segment) %>%
    dplyr::summarise(
      dplyr::across(!!target, .fns = funs, na.rm = T, .names = "outer_stat_{fn}_{col}")
    ) %>%
    dplyr::select(special_segment, dplyr::starts_with("outer_stat_"))
  
  
  # 指定 category 単位の集約値
  df.inner_stats <- data %>%
    dplyr::group_by(special_segment, !!category) %>%
    dplyr::summarise(
      n = n(),
      dplyr::across(!!target, .fns = funs, na.rm = T, .names = "inner_stat_{fn}_{col}")
    ) %>%
    dplyr::ungroup()
  
  
  # category 毎の代表値を補正付きに変換
  df.inner_stats %>%
    
    # to long-form
    tidyr::pivot_longer(
      cols = dplyr::starts_with("inner_stat_"),
      names_prefix = "inner_stat_",
      names_to = "variable",
      values_to = "inner_stat"
    ) %>%
    
    # category に依存しない集約値を結合
    dplyr::left_join(
      (
        df.outer_stat %>%
          tidyr::pivot_longer(
            cols = -special_segment,
            names_prefix = "outer_stat_",
            names_to = "variable",
            values_to = "outer_stat"
          ) 
      ),
      by = c("special_segment", "variable")
    ) %>%
    
    # 補正の実施
    dplyr::mutate(
      lambda = 1 / (1 + exp(-(n - k) / f)),
      smoothed_stat = lambda * inner_stat + (1 - lambda) * outer_stat
    ) %>%
    
    # to wide-form
    tidyr::pivot_wider(
      id_cols = c(special_segment, !!category),
      names_prefix = stringr::str_c(dplyr::quo_name(category), "_"),
      names_from = variable,
      values_from = smoothed_stat
    )
}

add_feature_per_category <- function(target_data, train_data, category, target, funs) {
  
  # for NSE
  category = dplyr::enquo(category)
  target   = dplyr::enquo(target)
  
  # category 毎の代表値を取得
  df.category_average <- smoothed_categorical_value(train_data, !!category, !!target, funs)
  
  # レコード全体での統計量
  # 補完に用いる
  df.total_summary <- train_data %>%
    dplyr::group_by(special_segment) %>%
    dplyr::summarise(
      dplyr::across(!!target, .fns = funs, na.rm = T, .names = "outer_stat_{fn}_{col}")
    ) %>%
    dplyr::select(special_segment, dplyr::starts_with("outer_stat_"))
  
  
  # 集約値の一覧
  df.aggregations <-
    
    # 集約キーの一覧
    (
      target_data %>%
        dplyr::select(special_segment, !!category) %>%
        dplyr::distinct()
    ) %>%
    
    # 集約値を結合
    dplyr::left_join(
      df.category_average,
      by = c("special_segment", dplyr::quo_name(category))
    ) %>%
    
    # to long-form
    # 直後に各集約値の名称を用いて補完用項目を結合するため
    tidyr::pivot_longer(
      cols = -c(special_segment, !!category),
      names_prefix = stringr::str_c(dplyr::quo_name(category), "_"),
      names_to = "variable",
      values_to = "inner_stat"
    ) %>%
    
    # レコード全体での統計量を結合
    dplyr::left_join(
      (
        df.total_summary %>%
          tidyr::pivot_longer(
            cols = -special_segment,
            names_prefix = "outer_stat_",
            names_to = "variable",
            values_to = "outer_stat"
          )
      ),
      by = c("special_segment", "variable")
    ) %>%
    
    # 補完
    dplyr::mutate(
      complemented_stat = dplyr::if_else(is.na(inner_stat), outer_stat, inner_stat)
    ) %>%
    
    # to wide-form
    tidyr::pivot_wider(
      id_cols = c(special_segment, !!category),
      names_prefix = stringr::str_c(dplyr::quo_name(category), "_"),
      names_from = variable,
      values_from = complemented_stat
    )
  
  # target_data に算出した代表値を結合
  target_data %>%
    dplyr::left_join(df.aggregations, by = c("special_segment", dplyr::quo_name(category)))
}

add_features_per_category <- function(target_data, train_data) {

  # 集約関数の一覧
  funs <- list(
    mean   = mean,
    median = median,
    min    = min,
    max    = max,
    sd     = sd
  )

  target_data %>%

    # Gender
    add_feature_per_category(train_data, Gender, disease, funs) %>%
    add_feature_per_category(train_data, Gender, T_Bil, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_T_Bil  = T_Bil - Gender_mean_T_Bil,
      ratio_Gender_mean_T_Bil = T_Bil / Gender_mean_T_Bil
    ) %>%
    add_feature_per_category(train_data, Gender, D_Bil, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_D_Bil  = D_Bil - Gender_mean_D_Bil,
      ratio_Gender_mean_D_Bil = D_Bil / Gender_mean_D_Bil
    ) %>%
    add_feature_per_category(train_data, Gender, ALP, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_ALP  = ALP - Gender_mean_ALP,
      ratio_Gender_mean_ALP = ALP / Gender_mean_ALP
    ) %>%
    add_feature_per_category(train_data, Gender, ALT_GPT, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_ALT_GPT  = ALT_GPT - Gender_mean_ALT_GPT,
      ratio_Gender_mean_ALT_GPT = ALT_GPT / Gender_mean_ALT_GPT
    ) %>%
    add_feature_per_category(train_data, Gender, AST_GOT, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_AST_GOT  = AST_GOT - Gender_mean_AST_GOT,
      ratio_Gender_mean_AST_GOT = AST_GOT / Gender_mean_AST_GOT
    ) %>%
    add_feature_per_category(train_data, Gender, TP, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_TP  = TP - Gender_mean_TP,
      ratio_Gender_mean_TP = TP / Gender_mean_TP
    ) %>%
    add_feature_per_category(train_data, Gender, Alb, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_Alb  = Alb - Gender_mean_Alb,
      ratio_Gender_mean_Alb = Alb / Gender_mean_Alb
    ) %>%
    add_feature_per_category(train_data, Gender, AG_ratio, funs) %>%
    dplyr::mutate(
      diff_Gender_mean_AG_ratio  = AG_ratio - Gender_mean_AG_ratio,
      ratio_Gender_mean_AG_ratio = AG_ratio / Gender_mean_AG_ratio
    )
}


# Train & Evaluate --------------------------------------------------------

# モデルの構築と評価
train_and_eval <- function(split, recipe, model, formula) {

  set.seed(1025)
  options("dplyr.summarise.inform" = F)

  # 前処理済データの作成
  trained_recipe <- recipes::prep(recipe, training = rsample::training(split))
  df.train.raw <- recipes::juice(trained_recipe)
  df.train <- df.train.raw %>%
    add_features_per_category(., .) %>%
    # 主成分スコアの追加
    add_pca_scores(df.train.raw) %>%
    dplyr::mutate(disease = factor(disease, levels = c(TRUE, FALSE)))
  df.test  <- recipes::bake(trained_recipe, new_data = rsample::testing(split)) %>%
    add_features_per_category(df.train.raw) %>%
    # 主成分スコアの追加
    add_pca_scores(df.train.raw) %>%
    dplyr::mutate(disease = factor(disease, levels = c(TRUE, FALSE)))


  # モデルの学習
  fit <- parsnip::fit(model, formula, data = df.train)


  # train データでモデルを評価
  df.result.train <- df.train %>%
    dplyr::mutate(
      predicted = predict(fit, df.train, type = "prob")$.pred_TRUE
    ) %>%
    yardstick::roc_auc(disease, predicted) %>%
    dplyr::select(-.estimator) %>%
    dplyr::mutate(
      .metric = stringr::str_c("train", .metric, sep = "_")
    ) %>%
    tidyr::spread(key = .metric, value = .estimate)

  # test データでモデルを評価
  df.result.test <- df.test %>%
    dplyr::mutate(
      predicted = predict(fit, df.test,  type = "prob")$.pred_TRUE
    ) %>%
    yardstick::roc_auc(disease, predicted) %>%
    dplyr::select(-.estimator) %>%
    dplyr::mutate(
      .metric = stringr::str_c("test", .metric, sep = "_")
    ) %>%
    tidyr::spread(key = .metric, value = .estimate)


  dplyr::bind_cols(
    df.result.train,
    df.result.test
  )
}