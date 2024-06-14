source("tool_function/tableone.R")

# test1: split.question()
dt <- data.table(
  id = c(1, 2, 3),
  items = c("apple;banana;cherry", "dog;elephant", "frog;giraffe;hippo")
)
result <- split.question(dt, "id", "items")
print(result)


# test2:combin.question
dt <- data.table(
  id = c(1, 1, 1, 2, 2, 3, 3, 3),
  items = c("apple", "banana", "cherry", "dog", "elephant", "frog", "giraffe", 
            "hippo"))
result <- combin.question(dt, "id", "items")
print(result)


# test3: category test()
dt <- data.table(
  category = c("A", "A", "B", "B", "C", "C"),
  outcome = c("success", "fail", "success", "fail", "success", "fail"),
  count = c(10, 5, 20, 10, 15, 5)
)
m.table <- table(dt$category, dt$outcome)

print(dt)
print(m.table)
result <- category.test(m.table)
print(result)

# test4: numeric test()
dt <- data.table(
  value = c(10, 15, 14, 20, 22, 18, 19, 30, 25, 35),
  group = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C")
)

print(dt)
result <- numeric.test.ind(dt, "value", "group")
print(result)


# test5: merge.list()
dt1 <- data.table(id = 1:5, value1 = c(10, 20, 30, 40, 50))
dt2 <- data.table(id = 3:7, value2 = c(30, 40, 50, 60, 70))
dt3 <- data.table(id = 5:9, value3 = c(50, 60, 70, 80, 90))
dt_list <- list(dt1, dt2, dt3)
print(dt1)
print(dt2)
print(dt3)
result_all <- merge.list(dt_list, "id", "all")
result_intersect <- merge.list(dt_list, "id", "intersect")
print(result_all)
print(result_intersect)


# test6: create.table1()
set.seed(123)
dt <- data.table(
  Group = sample(c("A", "B"), 100, replace = TRUE),
  Age = sample(18:65, 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Height = rnorm(100, mean = 165, sd = 10),
  Weight = rnorm(100, mean = 70, sd = 15),
  Outcome = sample(c(0, 1), 100, replace = TRUE)
)
result_table1 <- create.table1(
  dt = dt,
  need.col = c("Age", "Gender", "Height", "Weight", "Outcome"),
  group = "Group",
  overall = TRUE,
  percent_na = FALSE
)
print(result_table1)

# test7: my.lm()
formula_list <- list("Weight ~ Age + Gender + Height", 
                     "Outcome ~ Age + Gender + Height + Weight")
result_lm <- my.lm(
  dt = dt,
  formula.list = formula_list,
  round.n = 3,
  model = "lm",
  column.level = c("Age", "GenderMale", "Height")
)

print(result_lm$res[[1]])
print(result_lm$res[[2]])

# test8: create.lm.table()
lm_table <- create.lm.table(result_lm, "cbind")
print(lm_table)
#lm_table <- create.lm.table(result_lm, "rbind") #error 


# test9: corr_matrix()
set.seed(123)
n <- 100
df <- data.table(
  A = rnorm(n),
  B = rnorm(n),
  C = rnorm(n)
)
result <- corr_matrix(df, col_v = c("A", "B", "C"))
print(result)


# test10: bar_plot()
set.seed(123)
n <- 30
df <- data.table(
  Group = sample(c("A", "B", "C"), n, replace = TRUE),
  Value = rnorm(n, mean = 5, sd = 2)
)
dt_summary <- df[, .(mean = mean(Value), sd = sd(Value)), by = Group]
p <- bar_plot(dt_summary, x = "Group", y = "mean", t = "Group Comparison Bar Plot")
print(p)