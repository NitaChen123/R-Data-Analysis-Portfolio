# 设置随机种子以确保结果可重复
set.seed(12345)

# 读取数据
insurance <- read.csv("insurance.csv", 
                      stringsAsFactors = FALSE)  # 防止字符变量自动转换为因子

# 声明各变量类型
insurance$sex <- as.character(insurance$sex)         # 将sex转换为字符型
insurance$smoker <- as.character(insurance$smoker)   # 将smoker转换为字符型
insurance$region <- as.character(insurance$region)   # 将region转换为字符型

insurance$age <- as.numeric(insurance$age)           # 将age转换为数值型
insurance$bmi <- as.numeric(insurance$bmi)           # 将bmi转换为数值型
insurance$children <- as.numeric(insurance$children) # 将children转换为数值型
insurance$charges <- as.numeric(insurance$charges)   # 将charges转换为数值型

# 查看分类变量的频数表
table(insurance$sex)     # 性别频数表
table(insurance$smoker)  # 吸烟状态频数表
table(insurance$region)  # 地区频数表

# 对因变量charges进行对数转换
insurance$log_charges <- log(insurance$charges)  # 添加log_charges列，保存转换后的数据

# 检查数据结构以确认变量类型是否正确
str(insurance)

# 确定总样本数
total_rows <- nrow(insurance)

# 随机抽取70%的观测作为学习数据集的索引
set.seed(12345)  # 保持可重复性
train_indices <- sample(1:total_rows, size = 0.7 * total_rows, replace = FALSE)

# 创建学习数据集和测试数据集
learning_data <- insurance[train_indices, ]      # 提取学习数据集
testing_data <- insurance[-train_indices, ]      # 剩余的数据作为测试数据集

# 将学习数据集存储为CSV文件
write.csv(learning_data, "insurance_learning.csv", row.names = FALSE)

# 将测试数据集存储为CSV文件
write.csv(testing_data, "insurance_test.csv", row.names = FALSE)

# 查看存储结果的摘要
cat("学习数据集保存为 'insurance_learning.csv'，测试数据集保存为 'insurance_test.csv'。\n")
cat("学习数据集包含", nrow(learning_data), "条观测，测试数据集包含", nrow(testing_data), "条观测。\n")

# 使用学习数据集建立线性模型
lm_model <- lm(log_charges ~ age + sex + bmi + children + smoker + region, data = learning_data)

# 查看模型摘要
summary(lm_model)

# 绘制线性模型的诊断图
par(mfrow = c(2, 2))  # 设置2x2的布局
plot(lm_model)        # 生成诊断图

# 计算模型在测试数据集上的预测值
# 对测试数据集进行预测
predicted_log_charges <- predict(lm_model, newdata = testing_data)

# 将对数转换回原始的保险费用值
predicted_charges <- exp(predicted_log_charges)

# 计算均方根误差（RMSE）
actual_charges <- testing_data$charges
rmse <- sqrt(mean((actual_charges - predicted_charges)^2))

# 输出RMSE
cat("线性模型对测试数据集保险费用预测的均方根误差（RMSE）：", rmse, "\n")


