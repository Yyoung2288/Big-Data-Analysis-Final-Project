# 載入必要的套件
library(shiny)
library(dplyr)
library(ggplot2)
library(zoo)
library(scales)
library(plotly)

# 城市代碼對照表
CITY_CODES <- c(
  "台北市" = "A", "新北市" = "B", "台中市" = "C", "台南市" = "D",
  "高雄市" = "E", "桃園市" = "F", "新竹縣" = "G", "苗栗縣" = "H",
  "彰化縣" = "I", "南投縣" = "J", "雲林縣" = "K", "嘉義縣" = "L",
  "屏東縣" = "M", "宜蘭縣" = "N", "花蓮縣" = "O", "台東縣" = "P",
  "基隆市" = "Q", "新竹市" = "R", "嘉義市" = "S", "澎湖縣" = "T",
  "金門縣" = "U", "連江縣" = "V"
)

# 預處理：建立城市區域對照表
CITY_DISTRICTS <- lapply(names(CITY_CODES), function(city_name) {
  city_code <- CITY_CODES[city_name]
  
  # 從人口資料獲取區域
  districts <- c()
  folder <- paste0(city_code, "_", city_name)
  migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_113.csv"))
  if (file.exists(migration_file)) {
    migration_data <- read.csv(migration_file, stringsAsFactors = FALSE)
    districts <- migration_data$區域別[migration_data$區域別 != "總計"]
    districts <- districts[districts != ""]
    districts <- sort(districts)
  }
  
  return(districts)
})
names(CITY_DISTRICTS) <- names(CITY_CODES)

# 獲取區域列表的函數（使用預處理的資料）
get_districts <- function(city_name, year = NULL) {
  if (is.null(city_name) || city_name == "") return(c("全部"))
  if (!(city_name %in% names(CITY_DISTRICTS))) return(c("全部"))
  
  districts <- CITY_DISTRICTS[[city_name]]
  if (length(districts) == 0) return(c("全部"))
  return(c("全部", districts))
}

# 計算累積人口的輔助函數
calculate_cumulative_population <- function(city_code, city_name, target_year, district_name) {
  folder <- paste0(city_code, "_", city_name)
  init_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_init.csv"))
  
  init_pop <- read.csv(init_file, header = TRUE, stringsAsFactors = FALSE)
  district_row <- which(init_pop[,1] == district_name)
  if (length(district_row) == 0) return(NULL)
  
  base_population <- suppressWarnings(as.numeric(gsub(",", "", init_pop[district_row[1], 2])))
  if (is.na(base_population)) return(NULL)
  
  cumulative_migration <- 0
  for (year in 104:(target_year-1)) {
    migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_", year, ".csv"))
    if (file.exists(migration_file)) {
      migration_data <- read.csv(migration_file, header = TRUE, stringsAsFactors = FALSE)
      migration_row <- which(migration_data[,1] == district_name)
      if (length(migration_row) > 0) {
        year_migration <- suppressWarnings(as.numeric(gsub(",", "", as.character(migration_data[migration_row[1], 2:13]))))
        year_migration[is.na(year_migration)] <- 0
        cumulative_migration <- cumulative_migration + sum(year_migration)
      }
    }
  }
  
  return(base_population + cumulative_migration)
}

# 讀取人口資料的函數
read_population_data <- function(city_name, year, district = "全部") {
  if (is.null(city_name) || is.null(year)) return(NULL)
  
  city_code <- CITY_CODES[city_name]
  if (is.null(city_code)) return(NULL)
  
  folder <- paste0(city_code, "_", city_name)
  init_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_init.csv"))
  migration_file <- file.path("Data", "people_cleanedData", folder, paste0(city_code, "_", year, ".csv"))
  
  if (!file.exists(init_file) || !file.exists(migration_file)) return(NULL)
  
  migration_data <- read.csv(migration_file, header = TRUE, stringsAsFactors = FALSE)
  all_months <- data.frame(
    month = 1:12,
    date = sprintf("%03d%02d", as.numeric(year), 1:12)
  )
  
  districts_to_process <- migration_data$區域別[migration_data$區域別 != "總計"]
  if (district != "全部") {
    if (!district %in% districts_to_process) return(NULL)
    districts_to_process <- district
  }
  
  all_results <- data.frame()
  for (current_district in districts_to_process) {
    base_population <- calculate_cumulative_population(city_code, city_name, as.numeric(year), current_district)
    if (is.null(base_population)) next
    
    migration_row <- which(migration_data[,1] == current_district)
    if (length(migration_row) == 0) next
    
    net_mig <- suppressWarnings(as.numeric(gsub(",", "", as.character(migration_data[migration_row[1], 2:13]))))
    net_mig[is.na(net_mig)] <- 0
    
    migration_long <- data.frame(
      month = 1:12,
      net_migration = net_mig
    )
    
    migration_long <- merge(all_months, migration_long, by = "month", all.x = TRUE)
    migration_long$net_migration[is.na(migration_long$net_migration)] <- 0
    migration_long$population <- base_population + cumsum(migration_long$net_migration)
    migration_long$district <- current_district
    
    result <- migration_long[, c("date", "population", "net_migration", "district")]
    all_results <- rbind(all_results, result)
  }
  
  all_results <- all_results[order(all_results$date, all_results$district), ]
  return(all_results)
}

# 讀取房價資料的函數
read_house_price_data <- function(city_name, year, district = "全部") {
  if (is.null(city_name) || is.null(year)) return(NULL)
  
  city_code <- CITY_CODES[city_name]
  if (is.null(city_code)) return(NULL)
  
  price_file_a <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_a.csv"))
  price_file_b <- file.path("Data", "MergedHousePricingData", as.character(year), paste0(city_code, "_lvr_land_b.csv"))
  
  if (file.exists(price_file_a)) {
    price_file <- price_file_a
  } else if (file.exists(price_file_b)) {
    price_file <- price_file_b
  } else {
    return(NULL)
  }
  
  price_data <- read.csv(price_file, stringsAsFactors = FALSE)
  price_data <- price_data[, c("鄉鎮市區_The.villages.and.towns.urban.district", 
                              "單價元平方公尺_the.unit.price..NTD...square.meter.", 
                              "交易年月日_transaction.year.month.and.day")]
  
  price_data$date <- substr(price_data$交易年月日_transaction.year.month.and.day, 1, 5)
  price_data$date <- sprintf("%03d%02d", 
                            as.numeric(substr(price_data$date, 1, 3)),
                            as.numeric(substr(price_data$date, 4, 5)))
  
  price_data$price_per_sqm <- suppressWarnings(as.numeric(gsub(",", "", price_data$單價元平方公尺_the.unit.price..NTD...square.meter.)))
  price_data <- price_data[!is.na(price_data$price_per_sqm), ]
  
  if (district != "全部") {
    price_data <- price_data[price_data$鄉鎮市區_The.villages.and.towns.urban.district == district, ]
  }
  
  price_summary <- aggregate(price_per_sqm ~ date + 鄉鎮市區_The.villages.and.towns.urban.district, 
                           data = price_data, 
                           FUN = mean)
  
  all_months <- data.frame(
    date = sprintf("%03d%02d", as.numeric(year), 1:12)
  )
  
  districts <- unique(price_summary$鄉鎮市區_The.villages.and.towns.urban.district)
  result <- data.frame()
  
  for (dist in districts) {
    dist_data <- price_summary[price_summary$鄉鎮市區_The.villages.and.towns.urban.district == dist, ]
    dist_months <- merge(all_months, dist_data, by = "date", all.x = TRUE)
    dist_months$鄉鎮市區_The.villages.and.towns.urban.district <- dist
    result <- rbind(result, dist_months)
  }
  
  result <- result[order(result$date, result$鄉鎮市區_The.villages.and.towns.urban.district), ]
  return(result)
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