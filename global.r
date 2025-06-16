# 載入必要的套件
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# 設定工作目錄
setwd(".")

# 讀取人口資料
read_population_data <- function(file_path) {
  data <- read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  # 確保總計欄位為數值型態，移除千分位符號
  if("總計" %in% colnames(data)) {
    data$總計 <- as.numeric(gsub(",", "", data$總計))
  }
  return(data)
}

# 讀取初始人口資料
population_init <- read_population_data("Data/people_cleanedData/A_台北市/A_init.csv")

# 讀取並處理每月遷入遷出資料
population_files <- list.files("Data/people_cleanedData/A_台北市", pattern = "^A_\\d{3}\\.csv$", full.names = TRUE)
population_data <- lapply(population_files, read_population_data)

# 將每月資料轉換為長格式，支援X1月~X12月欄位
population_long <- lapply(seq_along(population_data), function(i) {
  df <- population_data[[i]]
  # 尋找月份欄位
  month_cols <- grep("^X\\d+月$", colnames(df), value = TRUE)
  if(length(month_cols) == 0) {
    cat(sprintf("[DEBUG] population_data第%d個檔案找不到月份欄位，實際欄位：%s\n", i, paste(colnames(df), collapse=",")))
    return(NULL)
  }
  # 先將所有月份欄位轉成字串，避免型態不一致
  df[month_cols] <- lapply(df[month_cols], as.character)
  # 寬轉長
  df_long <- tidyr::pivot_longer(
    df,
    cols = all_of(month_cols),
    names_to = "月份",
    values_to = "人口數"
  )
  # 處理月份欄位
  df_long$月份 <- as.numeric(gsub("X|月", "", df_long$月份))
  # 組合成標準日期
  df_long$日期 <- as.Date(paste0("2015-", sprintf("%02d", df_long$月份), "-01"))
  # 人口數轉數值
  df_long$人口數 <- as.numeric(gsub(",", "", df_long$人口數))
  # 保留區域、日期、人口數
  df_long <- df_long[, c("區域別", "日期", "人口數")]
  return(df_long)
})

# 過濾掉NULL
population_long_valid <- population_long[!sapply(population_long, is.null)]
cat(sprintf("[DEBUG] 有效人口資料data frame數量：%d\n", length(population_long_valid)))
if(length(population_long_valid) == 0) {
  stop("[ERROR] 沒有任何有效的人口資料，請檢查原始檔案格式！")
}

population_combined <- do.call(rbind, population_long_valid)

# 讀取房價資料
read_house_price_data <- function(file_path) {
  data <- read.csv(file_path, fileEncoding = "UTF-8")
  # 轉換日期格式（從民國年轉換為西元年）
  if("交易年月日" %in% colnames(data)) {
    data$交易年月日 <- as.character(data$交易年月日)
    # 將民國年轉換為西元年（民國年 + 1911）
    year <- as.numeric(substr(data$交易年月日, 1, 3)) + 1911
    month <- substr(data$交易年月日, 4, 5)
    day <- substr(data$交易年月日, 6, 7)
    data$交易年月日 <- as.Date(paste(year, month, day, sep = "-"))
  }
  return(data)
}

# 讀取所有房價資料檔案
house_price_files <- list.files("Data/MergedHousePricingData", pattern = ".*_lvr_land_a\\.csv$", recursive = TRUE, full.names = TRUE)
house_price_data <- lapply(house_price_files, read_house_price_data)

# 合併所有房價資料
house_price_combined <- do.call(rbind, house_price_data)

# 計算每個月的平均房價
monthly_house_price <- house_price_combined %>%
  group_by(日期 = as.Date(format(交易年月日, "%Y-%m-01"))) %>%
  summarise(平均單價 = mean(單價元平方公尺, na.rm = TRUE))

# 合併人口和房價資料（用日期欄位）
combined_data <- merge(
  population_combined,
  monthly_house_price,
  by = "日期",
  all.x = TRUE
)

# 移除包含 NA 的列
combined_data <- combined_data %>%
  filter(!is.na(人口數) & !is.na(平均單價))

# 計算相關係數
correlation <- cor(combined_data$人口數, combined_data$平均單價)

# 建立迴歸模型
model <- lm(平均單價 ~ 人口數, data = combined_data)

# 計算基本統計量
city_list <- unique(population_combined$區域別)
date_range <- range(population_combined$日期, na.rm = TRUE)
