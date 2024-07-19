# 定义时间和数值向量
t <- c(0, 20, 21, 23, 50, 70, 90, 200)
v <- c(5, 6, 6, 8, 10, 20, 5, 15)

# 1.  直接转换，计算 Spearman 秩相关系数 (不推荐)
cor(t, v, method = "spearman") 

# 2.  考虑时间间隔作为权重，计算加权 Spearman 秩相关系数
# 使用 diff() 函数计算时间间隔
time_diff <- diff(t)

# 使用 cor.test() 函数计算加权 Spearman 秩相关系数
cor.test(t[-1], v[-1], method = "spearman", weights = time_diff)