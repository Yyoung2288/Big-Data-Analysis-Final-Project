# 載入必要的套件
library(shiny)
library(dplyr)
library(ggplot2)

# 城市代碼對照表（中文名稱=英文代號）
city_codes <- c(
  "台北市" = "A", "新北市" = "B", "台中市" = "C", "台南市" = "D",
  "高雄市" = "E", "桃園市" = "F", "新竹縣" = "G", "苗栗縣" = "H",
  "彰化縣" = "I", "南投縣" = "J", "雲林縣" = "K", "嘉義縣" = "L",
  "屏東縣" = "M", "宜蘭縣" = "N", "花蓮縣" = "O", "台東縣" = "P",
  "基隆市" = "Q", "新竹市" = "R", "嘉義市" = "S", "澎湖縣" = "T",
  "金門縣" = "U", "連江縣" = "V"
)

# 讀取人口資料的函數
read_population_data <- function(city_name, year) {
  city_code <- city_codes[[city_name]]
  folder <- paste0(city_code, "_", city_name)
  init_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_init.csv"))
  migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_", year, ".csv"))

  # 讀取初始人口數（自動抓總計那一列，處理千分位逗號）
  init_pop <- read.csv(init_file, header = TRUE, stringsAsFactors = FALSE)
  row_idx <- which(init_pop[,1] == "總計")
  if (length(row_idx) == 0) stop("找不到總計那一列，請檢查檔案內容。")
  total_population <- gsub(",", "", init_pop[row_idx[1], 2])
  total_population <- suppressWarnings(as.numeric(total_population))
  if (is.na(total_population)) stop("初始人口數轉換失敗，請檢查檔案內容格式。")

  # 讀取每月遷移資料，轉為長格式
  migration_data <- read.csv(migration_file, header = TRUE, stringsAsFactors = FALSE)
  net_mig <- suppressWarnings(as.numeric(migration_data[1, 2:13]))
  if (any(is.na(net_mig))) warning("遷移資料有NA，請檢查檔案內容格式。")
  migration_long <- data.frame(
    month = 1:12,
    net_migration = net_mig
  )
  migration_long$date <- sprintf("%d-%02d", 1911 + as.numeric(year), migration_long$month)

  # 計算每月人口
  migration_long$population <- total_population + cumsum(c(0, migration_long$net_migration[-length(migration_long$net_migration)]))

  # 輸出欄位：date, population, net_migration
  result <- migration_long[, c("date", "population", "net_migration")]
  return(result)
}

# 讀取房價資料的函數
read_house_price_data <- function(city_name, year) {
  city_code <- city_codes[[city_name]]
  # 先找a檔，找不到再找b檔
  price_file_a <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_a.csv"))
  price_file_b <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_b.csv"))
  if (file.exists(price_file_a)) {
    price_file <- price_file_a
  } else if (file.exists(price_file_b)) {
    price_file <- price_file_b
  } else {
    stop("找不到房價資料檔案，請檢查檔名是否為 [年分]/[城市代碼]_lvr_land_[a或b].csv")
  }
  price_data <- read.csv(price_file, stringsAsFactors = FALSE)
  # 只保留區與單價與交易年月日
  price_data <- price_data[, c("鄉鎮市區_The.villages.and.towns.urban.district", 
                               "單價元平方公尺_the.unit.price..NTD...square.meter.", 
                               "交易年月日_transaction.year.month.and.day")]
  # 處理單價欄位，移除逗號並轉為數字
  price_data$單價元平方公尺_the.unit.price..NTD...square.meter. <- as.numeric(gsub(",", "", price_data$單價元平方公尺_the.unit.price..NTD...square.meter.))
  # 取年月
  price_data$year <- as.integer(substr(price_data$交易年月日_transaction.year.month.and.day, 1, 3)) + 1911
  price_data$month <- as.integer(substr(price_data$交易年月日_transaction.year.month.and.day, 4, 5))
  price_data$date <- sprintf("%d-%02d", price_data$year, price_data$month)
  # 依月份計算平均單價
  price_monthly <- price_data %>%
    group_by(date) %>%
    summarise(price_per_sqm = mean(單價元平方公尺_the.unit.price..NTD...square.meter., na.rm = TRUE)) %>%
    ungroup()
  return(as.data.frame(price_monthly))
}

# 計算相關係數的函數
calculate_correlation <- function(pop_data, price_data) {
  merged_data <- pop_data %>%
    left_join(price_data, by = "date")
  merged_data <- merged_data[complete.cases(merged_data), ]
  correlation <- cor(merged_data$population, merged_data$price_per_sqm)
  return(correlation)
}
