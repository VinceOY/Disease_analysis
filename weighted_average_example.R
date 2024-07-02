# 定義數值和權重
values <- c(10, 20, 30, 40, 50)
weights <- c(1, 2, 3, 4, 5)

# 使用weighted.mean函數計算加權平均
weighted_avg <- weighted.mean(values, weights)
print(weighted_avg)