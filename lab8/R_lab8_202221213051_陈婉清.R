# 加载数据集
data("cars", package = "datasets")  # 加载内置数据集 'cars'

# 查看数据概览
str(cars)  # 查看数据结构
head(cars) # 查看前几行数据

# 1. 定义目标函数：基于距离差平方的均值再开根号
measure_rmse_distance <- function(params, data) {
  intercept <- params[1]  # 截距
  slope <- params[2]      # 斜率
  
  # 回归线预测值
  predicted <- intercept + slope * data$speed
  
  # 距离差平方的均值开根号（RMSE）
  sqrt(mean((data$dist - predicted)^2))
}

# 2. 使用优化算法寻找最佳拟合线
# 初始参数设置为截距=0, 斜率=1
initial_params <- c(0, 1)

# 使用 optim 函数最小化目标函数
best_fit <- optim(
  par = initial_params,                      # 初始参数
  fn = measure_rmse_distance,               # 目标函数
  data = cars                                # 数据集
)

# 输出最优截距和斜率
cat("最优拟合线的截距：", best_fit$par[1], "\n")
cat("最优拟合线的斜率：", best_fit$par[2], "\n")

# 3. 绘图展示拟合结果
library(ggplot2)
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +  # 绘制散点图
  geom_abline(intercept = best_fit$par[1], slope = best_fit$par[2], color = "blue", size = 1) +
  labs(title = "最佳拟合回归线", x = "速度 (speed)", y = "制动距离 (dist)") +
  theme_minimal()

#--------------------------------------------------定义新方法
# 加载必要的包
library(tidyverse)

house <- read.csv("ch6_house.csv")
# 定义模型函数，以sqft_living为自变量，price为因变量
model1 <- function(a, data) {
  a[1] + data$sqft_living * a[2]
}

# （1）距离差平方的均值再开根号，作为平均距离的衡量标准，拟合最佳回归线
# 定义计算平均距离的函数（RMSE）
measure_distance <- function(mod, data) {
  diff <- data$price - model1(mod, data)
  sqrt(mean(diff^2))
}

# 从(0, 0)出发，寻找distance最小
best_rmse <- optim(c(0, 0), measure_distance, data = house)

# （2）设计一种新的平均距离的测度方法，拟合最佳回归线
# 采用平均绝对误差（MAE）作为新的测度方法
measure_distance_new <- function(mod, data) {
  diff <- data$price - model1(mod, data)
  mean(abs(diff))
}

# 从(0, 0)出发，寻找distance最小
best_mae <- optim(c(0, 0), measure_distance_new, data = house)

# （3）用optim计算拟合结果的截距和斜率值，与measure_distance的结果进行对比分析
# 提取两种方法的截距和斜率
result <- data.frame(
  Measure = c("RMSE", "MAE"),
  Intercept = c(best_rmse$par[1], best_mae$par[1]),
  Slope = c(best_rmse$par[2], best_mae$par[2])
)

# 输出对比结果
print(result)

# 绘制基于RMSE和MAE的拟合线在同一张图中
ggplot(house, aes(x = sqft_living, y = price)) +
  geom_point() +
  geom_abline(intercept = best_rmse$par[1], slope = best_rmse$par[2], color = "red", 
              linetype = "solid", size = 0.8, alpha = 0.8, 
              aes(label = "RMSE拟合线")) +
  geom_abline(intercept = best_mae$par[1], slope = best_mae$par[2], color = "blue", 
              linetype = "solid", size = 0.8, alpha = 0.8, 
              aes(label = "MAE拟合线")) +
  labs(title = "RMSE和MAE拟合线对比",
       x = "房屋居住面积（sqft_living）",
       y = "房屋价格（price）") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "拟合方法"))
