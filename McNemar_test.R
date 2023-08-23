library(tidyverse)

# デモデータ作成 -------------------------------------------------------------------

A_success = data_frame(id = 1:7,  exposure = "A", outcome = 1, group = 1:7)
A_failure = data_frame(id = 8:20, exposure = 'A', outcome = 0, group = 8:20)
B_success = data_frame(id = 21:26,exposure = 'B', outcome = 1, group = 1:6)
B_failure = data_frame(id = 27,   exposure = 'B', outcome = 0, group = 7)
B_success_2 = data_frame(id = 28:35, exposure = 'B', outcome = 1, group = 8:15)
B_failure_2 = data_frame(id = 36:40, exposure = 'B', outcome =0, group = 16:20)

data <- A_success |> 
          bind_rows(A_failure) |> 
          bind_rows(B_success) |> 
          bind_rows(B_failure) |> 
          bind_rows(B_success_2) |> 
          bind_rows(B_failure_2) |> view()


# データ準備 ------------------------------------------------------------

data_for_McNemar <- data |> 
     #グループ化
     group_by(group) |> 
     
     # データフレームに他の列がある場合は、対象の列に絞り込む
     select(id,exposure,outcome,group) |> 

     # exposureで並べ替えてAとBの順番を揃える
     arrange(exposure) |>
     
     # outcomeの列を結合してMcNemar用の列を作る 
     mutate(McNemar = paste0(outcome,collapse = "")) |> 
     ungroup() |> 

     # A群だけ取り出す（ペアで考えるため）
     filter(exposure == "A") |> view()


# McNemar test ------------------------------------------------------------

  # McNemar用の文字列を数える 
data_for_McNemar <- data_for_McNemar |>     
     count(McNemar) |> 
     
     # McNemar n
     # 1 00          5
     # 2 01          8
     # 3 10          1
     # 4 11          6

     # n列をベクトルで取り出す
     pull(n) |> 
     
     # ベクトルをmatrixに変形
     matrix(nrow =2) 
     
     #      [,1] [,2]
     # [1,]    5    1
     # [2,]    8    6
     
     # 検定
     mcnemar.test(data_for_McNemar)

# McNemar's chi-squared = 4, df = 1, p-value = 0.0455


# 二項分布を用いる方法 --------------------------------------------------------------
library(exact2x2)
# 依存するtestthatがmatches,is_nullなどをマスクするので注意
     
mcnemar.exact(data_for_McNemar)

# Exact McNemar test (with central confidence intervals)
# 
# data:  data_for_McNemar
# b = 1, c = 8, p-value = 0.03906
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#      0.00281705 0.93235414
# sample estimates:
#      odds ratio 
# 0.125 
     