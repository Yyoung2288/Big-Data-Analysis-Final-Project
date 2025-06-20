from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
import time
import os
import glob

city_code = {
    "A": "臺北市", "B": "新北市", "C": "台中市", "D": "台南市", "E": "高雄市",
    "F": "桃園市", "G": "新竹縣", "H": "苗栗縣", "I": "彰化縣", "J": "南投縣",
    "K": "雲林縣", "L": "嘉義縣", "M": "屏東縣", "N": "宜蘭縣", "O": "花蓮縣",
    "P": "台東縣", "Q": "基隆市", "R": "新竹市", "S": "嘉義市", "T": "澎湖縣",
    "U": "金門縣", "V": "連江縣"
}

download_path = "C:\\Users\\斯煒嵐\\OneDrive\\桌面\\DownloadedData"
if not os.path.exists(download_path):
    os.makedirs(download_path)

options = Options()
options.add_experimental_option("prefs", {
    "download.default_directory": download_path,
    "download.prompt_for_download": False,
    "safebrowsing.enabled": True
})
driver = webdriver.Chrome(options=options)

def find_element_by_multiple_selectors(driver, selectors, timeout=10):
    """使用多種選擇器尋找元素"""
    wait = WebDriverWait(driver, timeout)
    for selector_type, selector_value in selectors:
        try:
            if selector_type == "id":
                return wait.until(EC.element_to_be_clickable((By.ID, selector_value)))
            elif selector_type == "class":
                return wait.until(EC.element_to_be_clickable((By.CLASS_NAME, selector_value)))
            elif selector_type == "xpath":
                return wait.until(EC.element_to_be_clickable((By.XPATH, selector_value)))
            elif selector_type == "css":
                return wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, selector_value)))
        except:
            continue
    return None

def wait_for_download(download_dir, timeout=30):
    """等待下載完成"""
    start_time = time.time()
    initial_files = set(glob.glob(os.path.join(download_dir, "*")))
    
    time.sleep(3)
    
    while time.time() - start_time < timeout:
        downloading_files = glob.glob(os.path.join(download_dir, "*.crdownload"))
        current_files = set(glob.glob(os.path.join(download_dir, "*")))
        
        if not downloading_files:
            new_files = current_files - initial_files
            if new_files:
                return True
            
            if len(current_files) > len(initial_files):
                return True
            
            recent_files = []
            for f in current_files:
                if os.path.isfile(f) and os.path.getmtime(f) > start_time - 5:
                    recent_files.append(f)
            
            if recent_files and time.time() - start_time > 8:
                return True
        
        time.sleep(2)
    
    final_files = set(glob.glob(os.path.join(download_dir, "*")))
    if len(final_files) > len(initial_files) or final_files != initial_files:
        return True
    
    return False

def rename_downloaded_file(download_dir, city_code, year, month):
    """重命名下載的檔案"""
    import time
    
    time.sleep(3)
    
    all_files = glob.glob(os.path.join(download_dir, "*"))
    files = [f for f in all_files if not f.endswith('.crdownload') and os.path.isfile(f)]
    
    if files:
        target_file = None
        
        keywords = ["人口", "統計", "地區", "全國", "縣市"]
        for f in files:
            filename = os.path.basename(f)
            if any(keyword in filename for keyword in keywords):
                if not target_file or os.path.getmtime(f) > os.path.getmtime(target_file):
                    target_file = f
        
        if not target_file:
            target_file = max(files, key=os.path.getmtime)
        
        original_filename = os.path.basename(target_file)
        file_extension = os.path.splitext(target_file)[1]
        
        new_filename = f"{city_code}_{year:02d}{month:02d}{file_extension}"
        new_filepath = os.path.join(download_dir, new_filename)
        
        if target_file == new_filepath:
            return True
        
        if os.path.exists(new_filepath):
            try:
                os.remove(new_filepath)
            except Exception:
                return False
        
        try:
            os.rename(target_file, new_filepath)
            return True
        except Exception:
            return False
    else:
        return False

try:
    driver.get("https://gis.ris.gov.tw/dashboard.html?key=E01")
    wait = WebDriverWait(driver, 15)
    
    print("等待頁面載入...")
    time.sleep(8)
    
    print("\n開始下載各縣市資料...")
    print("注意：這將下載 22個縣市 × 11年 × 12個月 = 2904個檔案")
    
    confirmation = input("\n確定要開始大量下載嗎？輸入 'yes' 繼續，或任意鍵取消: ")
    if confirmation.lower() != 'yes':
        print("取消下載")
        driver.quit()
        exit()
    
    start_year = 103
    end_year = 113
    
    total_downloads = 0
    successful_downloads = 0
    
    for code, city_name in city_code.items():
        print(f"\n正在處理 {city_name} (代碼: {code})")
        
        for year in range(start_year, end_year + 1):
            for month in range(1, 13):
                try:
                    print(f"  下載 {year}年{month}月 資料...")
                    total_downloads += 1
                    
                    driver.get("https://gis.ris.gov.tw/dashboard.html?key=E01")
                    time.sleep(5)
                    
                    # 選擇淨遷徙人數
                    net_migration_selectors = [
                        ("xpath", "//input[@type='radio' and @name='option0' and @value='0_3']"),
                        ("css", "input[name='option0'][value='0_3']"),
                        ("xpath", "//input[@type='radio' and @value='0_3']"),
                        ("xpath", "//input[@type='radio'][3]"),
                        ("xpath", "//label[contains(text(), '淨遷徙人數')]/preceding-sibling::input[@type='radio']"),
                        ("xpath", "//label[contains(text(), '淨遷徙人數')]/../input[@type='radio']")
                    ]
                    
                    net_migration_radio = find_element_by_multiple_selectors(driver, net_migration_selectors)
                    if net_migration_radio:
                        try:
                            net_migration_radio.click()
                            time.sleep(2)
                        except Exception:
                            pass
                    
                    # 選擇年份
                    year_selectors = [
                        ("xpath", "//select[@class='input-sm mb-md'][1]"),
                        ("xpath", "//select[option[contains(@value, '105') or contains(@value, '113')]]"),
                        ("css", "select:nth-of-type(1)"),
                        ("xpath", "//select[contains(@class, 'input-sm')][1]"),
                        ("id", "yearSelect"),
                        ("name", "year")
                    ]
                    
                    year_dropdown = find_element_by_multiple_selectors(driver, year_selectors)
                    if year_dropdown:
                        try:
                            year_select = Select(year_dropdown)
                            year_select.select_by_value(str(year))
                            time.sleep(2)
                        except Exception:
                            continue
                    else:
                        continue
                    
                    # 選擇月份
                    month_selectors = [
                        ("xpath", "//select[@class='input-sm mb-md'][2]"),
                        ("xpath", "//select[option[@value='1'] and option[@value='12']]"),
                        ("css", "select:nth-of-type(2)"),
                        ("xpath", "//select[contains(@class, 'input-sm')][2]"),
                        ("xpath", "//span[contains(text(), '月')]/preceding-sibling::select"),
                        ("id", "monthSelect"),
                        ("name", "month")
                    ]
                    
                    month_dropdown = find_element_by_multiple_selectors(driver, month_selectors)
                    if month_dropdown:
                        try:
                            month_select = Select(month_dropdown)
                            month_select.select_by_value(str(month))
                            time.sleep(2)
                        except Exception:
                            continue
                    else:
                        continue
                    
                    # 選擇縣市
                    city_selectors = [
                        ("xpath", "//select[contains(@class, 'form-control')]"),
                        ("css", "select.form-control"),
                        ("xpath", "//div[@class='form-group']//select"),
                        ("xpath", "//select[option[contains(text(), '雲林縣') or contains(text(), '臺北市')]]"),
                        ("xpath", "//button[contains(@class, 'dropdown-toggle')]/following-sibling::ul//a"),
                        ("css", ".dropdown-menu a"),
                        ("id", "countySelect"),
                        ("name", "county")
                    ]
                    
                    multiselect_button_selectors = [
                        ("xpath", "//button[contains(@class, 'multiselect') and contains(@class, 'dropdown-toggle')]"),
                        ("css", "button.multiselect.dropdown-toggle"),
                        ("xpath", "//button[@class='multiselect dropdown-toggle btn btn-default']")
                    ]
                    
                    multiselect_button = find_element_by_multiple_selectors(driver, multiselect_button_selectors)
                    if multiselect_button:
                        try:
                            multiselect_button.click()
                            time.sleep(2)
                            
                            city_option_selectors = [
                                ("xpath", f"//ul[contains(@class, 'multiselect-container')]//label[contains(text(), '{city_name}')]"),
                                ("xpath", f"//ul[@class='multiselect-container dropdown-menu']//label[contains(text(), '{city_name}')]"),
                                ("xpath", f"//li//label[contains(text(), '{city_name}')]"),
                                ("xpath", f"//input[@type='checkbox']/following-sibling::text()[contains(., '{city_name}')]/../input"),
                                ("css", f".multiselect-container label:contains('{city_name}')")
                            ]
                            
                            city_option = find_element_by_multiple_selectors(driver, city_option_selectors, timeout=5)
                            if city_option:
                                city_option.click()
                                time.sleep(2)
                                
                                try:
                                    multiselect_button.click()
                                    time.sleep(1)
                                except:
                                    pass
                            else:
                                continue
                        except Exception:
                            pass
                    
                    if not multiselect_button:
                        city_dropdown = find_element_by_multiple_selectors(driver, city_selectors)
                        if city_dropdown:
                            try:
                                city_select = Select(city_dropdown)
                                try:
                                    city_select.select_by_visible_text(city_name)
                                except:
                                    try:
                                        city_select.select_by_value(code)
                                    except:
                                        continue
                                time.sleep(2)
                            except Exception:
                                continue
                        else:
                            continue
                    
                    # 點擊查詢
                    query_selectors = [
                        ("xpath", "//button[contains(text(), '查詢')]"),
                        ("css", "button.btn-primary"),
                        ("xpath", "//button[@type='submit']"),
                        ("id", "queryBtn"),
                        ("class", "btn-query")
                    ]
                    
                    query_button = find_element_by_multiple_selectors(driver, query_selectors)
                    if query_button:
                        try:
                            query_button.click()
                            time.sleep(5)
                        except Exception:
                            continue
                    else:
                        continue
                    
                    # 點擊下載
                    download_selectors = [
                        ("xpath", "//div[@class='col-md-6'][2]//i[@class='fa fa-download']"),
                        ("xpath", "//div[@class='col-md-6'][position()=2]//a[contains(@class, 'fa-download')]"),
                        ("xpath", "(//div[@class='col-md-6'])[2]//i[@class='fa fa-download']"),
                        ("xpath", "//section[contains(@class, 'panel') and .//text()[contains(., '統計表')]]//i[@class='fa fa-download']"),
                        ("xpath", "//header[contains(text(), '統計表')]/following-sibling::*//i[@class='fa fa-download']"),
                        ("css", ".col-md-6:nth-child(2) .fa-download"),
                        ("xpath", "//i[@class='fa fa-download'][2]")
                    ]
                    
                    download_button = find_element_by_multiple_selectors(driver, download_selectors)
                    if download_button:
                        try:
                            download_button.click()
                            time.sleep(3)
                        except Exception:
                            continue
                    else:
                        continue
                    
                    if wait_for_download(download_path):
                        if rename_downloaded_file(download_path, code, year, month):
                            successful_downloads += 1
                            print(f"    ✓ 成功下載: {code}_{year:02d}{month:02d}")
                        else:
                            print(f"    ✗ 下載成功但重命名失敗: {year}年{month}月")
                    else:
                        print(f"    ✗ 下載超時: {year}年{month}月")
                    
                    time.sleep(3)
                    
                except Exception:
                    print(f"    ✗ 處理 {year}年{month}月 時發生錯誤")
                    continue
    
    print(f"\n下載完成！")
    print(f"統計結果:")
    print(f"總計嘗試下載: {total_downloads} 個檔案")
    print(f"成功下載: {successful_downloads} 個檔案")
    print(f"失敗數量: {total_downloads - successful_downloads} 個檔案")
    if total_downloads > 0:
        success_rate = successful_downloads/total_downloads*100
        print(f"成功率: {success_rate:.1f}%")
        
    print(f"\n檔案儲存位置: {download_path}")

except Exception:
    print(f"程式執行時發生錯誤")

finally:
    input("按 Enter 鍵關閉瀏覽器...")
    driver.quit()
    print("瀏覽器已關閉") 