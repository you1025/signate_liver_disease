library(tidyverse)

# Data Load -------------------------------------------------------------

# Train Data
# TODO: パスのやつ後で対応したい(プロジェクトトップを指す方法が知りたい)
load_train_data <- function(path) {
  readr::read_csv(
    path,
    col_types = cols(
      Age = readr::col_integer(),
      Gender = readr::col_factor(levels = c("Male", "Female")),
      T_Bil = readr::col_double(),
      D_Bil = readr::col_double(),
      ALP = readr::col_double(),
      ALT_GPT = readr::col_double(),
      AST_GOT = readr::col_double(),
      TP = readr::col_double(),
      Alb = readr::col_double(),
      AG_ratio = readr::col_double(),
      disease = readr::col_logical()
    )
  )
}

load_test_data <- function(path) {
  readr::read_csv(
    path,
    col_types = cols(
      Age = readr::col_integer(),
      Gender = readr::col_factor(levels = c("Male", "Female")),
      T_Bil = readr::col_double(),
      D_Bil = readr::col_double(),
      ALP = readr::col_double(),
      ALT_GPT = readr::col_double(),
      AST_GOT = readr::col_double(),
      TP = readr::col_double(),
      Alb = readr::col_double(),
      AG_ratio = readr::col_double()
    )
  )
}


# データ変換 -------------------------------------------------------------------

# クレンジング
clean <- function(df) {
  df
}


# CV ----------------------------------------------------------------------

# CV 作成
create_cv <- function(df, v = 5, seed = 1) {

  set.seed(seed)

  df %>%

    rsample::vfold_cv(v = v, repeats = 1, strata = "Gender")
}
