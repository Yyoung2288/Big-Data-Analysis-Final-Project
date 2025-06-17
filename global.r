# 載入必要的套件
library(shiny)
library(dplyr)
library(ggplot2)
library(zoo)  # 用於處理時間序列資料
library(scales)  # 用於顏色調色盤

# 城市代碼對照表（中文名稱=英文代號）
city_codes <- c(
  "台北市" = "A", "新北市" = "B", "台中市" = "C", "台南市" = "D",
  "高雄市" = "E", "桃園市" = "F", "新竹縣" = "G", "苗栗縣" = "H",
  "彰化縣" = "I", "南投縣" = "J", "雲林縣" = "K", "嘉義縣" = "L",
  "屏東縣" = "M", "宜蘭縣" = "N", "花蓮縣" = "O", "台東縣" = "P",
  "基隆市" = "Q", "新竹市" = "R", "嘉義市" = "S", "澎湖縣" = "T",
  "金門縣" = "U", "連江縣" = "V"
)

# 獲取區域列表的函數
get_districts <- function(city_name, year) {
  city_code <- city_codes[[city_name]]
  
  # 從人口資料獲取區域
  pop_districts <- c()
  folder <- paste0(city_code, "_", city_name)
  migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_", year, ".csv"))
  if (file.exists(migration_file)) {
    migration_data <- read.csv(migration_file, stringsAsFactors = FALSE)
    pop_districts <- migration_data$區域別[migration_data$區域別 != "總計"]
  }
  
  # 從房價資料獲取區域
  price_districts <- c()
  price_file_a <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_a.csv"))
  price_file_b <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_b.csv"))
  
  if (file.exists(price_file_a)) {
    price_file <- price_file_a
  } else if (file.exists(price_file_b)) {
    price_file <- price_file_b
  }
  
  if (exists("price_file") && file.exists(price_file)) {
    price_data <- read.csv(price_file, stringsAsFactors = FALSE)
    price_districts <- unique(price_data$鄉鎮市區_The.villages.and.towns.urban.district)
  }
  
  # 合併並去重
  all_districts <- unique(c(pop_districts, price_districts))
  return(c("全部", sort(all_districts)))
}

# 讀取人口資料的函數
read_population_data <- function(city_name, year, district = "全部") {
  city_code <- city_codes[[city_name]]
  folder <- paste0(city_code, "_", city_name)
  init_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_init.csv"))
  migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_", year, ".csv"))

  # 讀取初始人口數和每月遷移資料
  init_pop <- read.csv(init_file, header = TRUE, stringsAsFactors = FALSE)
  migration_data <- read.csv(migration_file, header = TRUE, stringsAsFactors = FALSE)
  
  # 生成完整的月份序列
  all_months <- data.frame(
    month = 1:12,
    date = sprintf("%d-%02d", 1911 + as.numeric(year), 1:12)
  )
  
  # 移除總計列，只保留各區域資料
  districts_to_process <- migration_data$區域別[migration_data$區域別 != "總計"]
  
  # 如果選擇特定區域，則只處理該區域
  if (district != "全部") {
    if (!district %in% districts_to_process) {
      stop(paste("找不到區域：", district))
    }
    districts_to_process <- district
  }
  
  # 處理每個區域的資料
  all_results <- data.frame()
  
  for (current_district in districts_to_process) {
    # 從初始人口數中找到對應區域
    district_row <- which(init_pop[,1] == current_district)
    if (length(district_row) == 0) {
      warning(paste("在初始人口資料中找不到區域：", current_district))
      next
    }
    total_population <- gsub(",", "", init_pop[district_row[1], 2])
    total_population <- suppressWarnings(as.numeric(total_population))
    if (is.na(total_population)) {
      warning(paste("區域", current_district, "的初始人口數轉換失敗"))
      next
    }
    
    # 從遷移資料中找到對應區域
    migration_row <- which(migration_data[,1] == current_district)
    if (length(migration_row) == 0) {
      warning(paste("在遷移資料中找不到區域：", current_district))
      next
    }
    net_mig <- suppressWarnings(as.numeric(gsub(",", "", as.character(migration_data[migration_row[1], 2:13]))))
    
    # 處理NA值：使用0替代NA
    net_mig[is.na(net_mig)] <- 0
    
    migration_long <- data.frame(
      month = 1:12,
      net_migration = net_mig
    )
    
    # 合併資料並確保所有月份都有
    migration_long <- merge(all_months, migration_long, by = "month", all.x = TRUE)
    migration_long$net_migration[is.na(migration_long$net_migration)] <- 0
    
    # 計算累計人口：使用累積和
    migration_long$population <- total_population + cumsum(migration_long$net_migration)
    
    # 添加區域資訊
    migration_long$district <- current_district
    
    # 合併到總結果中
    result <- migration_long[, c("date", "population", "net_migration", "district")]
    all_results <- rbind(all_results, result)
  }
  
  # 確保按日期和區域排序
  all_results <- all_results[order(all_results$date, all_results$district), ]
  return(all_results)
}

# 讀取房價資料的函數
read_house_price_data <- function(city_name, year, district = "全部") {
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
  
  # 讀取並處理房價資料
  price_data <- read.csv(price_file, stringsAsFactors = FALSE)
  
  # 只保留區與單價與交易年月日
  price_data <- price_data[, c("鄉鎮市區_The.villages.and.towns.urban.district", 
                              "單價元平方公尺_the.unit.price..NTD...square.meter.", 
                              "交易年月日_transaction.year.month.and.day")]
  
  # 生成完整的月份序列
  all_months <- data.frame(
    date = sprintf("%d-%02d", 1911 + as.numeric(year), 1:12)
  )
  
  # 如果選擇特定區域，則過濾資料
  if (district != "全部") {
    price_data <- price_data[price_data$鄉鎮市區_The.villages.and.towns.urban.district == district, ]
  }
  
  # 處理單價欄位，移除逗號並轉為數字
  price_data$單價元平方公尺_the.unit.price..NTD...square.meter. <- 
    suppressWarnings(as.numeric(gsub(",", "", price_data$單價元平方公尺_the.unit.price..NTD...square.meter.)))
  
  # 移除NA值
  price_data <- price_data[!is.na(price_data$單價元平方公尺_the.unit.price..NTD...square.meter.), ]
  
  # 取年月
  price_data$year <- as.integer(substr(price_data$交易年月日_transaction.year.month.and.day, 1, 3)) + 1911
  price_data$month <- as.integer(substr(price_data$交易年月日_transaction.year.month.and.day, 4, 5))
  price_data$date <- sprintf("%d-%02d", price_data$year, price_data$month)
  
  # 依月份和區域計算平均單價
  price_monthly <- price_data %>%
    group_by(date, 鄉鎮市區_The.villages.and.towns.urban.district, .groups = "drop") %>%
    summarise(price_per_sqm = mean(單價元平方公尺_the.unit.price..NTD...square.meter., na.rm = TRUE)) %>%
    ungroup()
  
  # 確保所有月份和區域都有資料
  all_combinations <- expand.grid(
    date = all_months$date,
    district = unique(price_data$鄉鎮市區_The.villages.and.towns.urban.district),
    stringsAsFactors = FALSE
  )
  names(all_combinations)[2] <- "鄉鎮市區_The.villages.and.towns.urban.district"
  
  price_monthly <- merge(all_combinations, price_monthly, 
                        by = c("date", "鄉鎮市區_The.villages.and.towns.urban.district"), 
                        all.x = TRUE)
  
  # 使用線性插值填補缺失值
  price_monthly <- price_monthly %>%
    group_by(鄉鎮市區_The.villages.and.towns.urban.district) %>%
    arrange(date) %>%
    mutate(price_per_sqm = na.approx(price_per_sqm, na.rm = FALSE)) %>%
    ungroup()
  
  # 移除仍然有NA的列
  price_monthly <- price_monthly[complete.cases(price_monthly), ]
  
  return(as.data.frame(price_monthly))
}

# 計算相關係數的函數
calculate_correlation <- function(pop_data, price_data) {
  merged_data <- pop_data %>%
    inner_join(price_data, by = "date") %>%
    na.omit()
  
  if(nrow(merged_data) < 3) return(NA)
  
  correlation <- cor(merged_data$population, merged_data$price_per_sqm)
  return(correlation)
}