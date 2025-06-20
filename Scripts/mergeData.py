import os
import pandas as pd
from pathlib import Path

def merge_year_data(source_dirs, output_dir, year):
    """合併單一年份的資料"""
    
    all_files = set()
    for dir_name in source_dirs:
        if os.path.exists(dir_name):
            files = os.listdir(dir_name)
            csv_files = [f for f in files if f.endswith('.csv')]
            all_files.update(csv_files)
    
    if not all_files:
        print("  找不到任何CSV檔案，跳過...")
        return

    print(f"找到 {len(all_files)} 個不重複的檔案名稱")
    
    for filename in sorted(all_files):
        print(f"正在處理: {filename}")
        
        dataframes = []
        first_columns = None
        
        for idx, dir_name in enumerate(source_dirs):
            file_path = Path(dir_name) / filename
            if not file_path.exists():
                continue

            df = None
            try:
                read_args = {'on_bad_lines': 'skip'}
                if idx == 0:
                    read_args['header'] = [0, 1]
                else:
                    read_args['header'] = 0
                    read_args['skiprows'] = 1

                try:
                    df = pd.read_csv(file_path, encoding='utf-8', **read_args)
                    encoding_used = 'utf-8'
                except UnicodeDecodeError:
                    df = pd.read_csv(file_path, encoding='big5', **read_args)
                    encoding_used = 'big5'
                
                print(f"  從 {dir_name} ({encoding_used}) 讀取 {len(df)} 筆資料")
                
                if isinstance(df.columns, pd.MultiIndex):
                    df.columns = ['_'.join(col).strip() for col in df.columns.values]

                df['來源資料夾'] = dir_name
                
                if first_columns is None:
                    first_columns = df.columns.tolist()
                
                dataframes.append(df)
                
            except Exception as e:
                print(f"  讀取或處理 {file_path} 時發生錯誤: {e}")

        if dataframes:
            try:
                if len(dataframes) > 1:
                    for i in range(1, len(dataframes)):
                        if len(dataframes[i].columns) == len(first_columns):
                            dataframes[i].columns = first_columns
                        elif len(dataframes[i].columns) == len(first_columns) - 1:
                            dataframes[i].columns = [col for col in first_columns if col != '來源資料夾']

                merged_df = pd.concat(dataframes, ignore_index=True)
                
                output_path = output_dir / filename
                merged_df.to_csv(output_path, index=False, encoding='utf-8-sig')
                
                print(f"  合併完成，共 {len(merged_df)} 筆資料，儲存至 {output_path}")
                
            except Exception as e:
                print(f"  合併 {filename} 時發生錯誤: {e}")
        else:
            print(f"  沒有讀取到任何 {filename} 的有效資料")
        
        print()

def merge_all_years(start_year=104, end_year=113):
    """合併所有年份的資料"""
    
    print("CSV檔案合併工具 - 批次處理版本")
    print("=" * 50)
    print(f"將處理 {start_year}年 到 {end_year}年 的所有資料")
    print("=" * 50)
    
    for year in range(start_year, end_year + 1):
        print(f"\n{'='*30}")
        print(f"開始處理 {year} 年資料...")
        print(f"{'='*30}")
        
        output_dir = Path(str(year))
        output_dir.mkdir(exist_ok=True)
        
        source_dirs = [f"{year}_02", f"{year}_03", f"{year}_04", f"{year}_05"]
        
        existing_dirs = [d for d in source_dirs if os.path.exists(d)]
        if not existing_dirs:
            print(f"  找不到 {year} 年的任何季度資料夾，跳過...")
            continue
        elif len(existing_dirs) < 4:
            print(f"  {year} 年只找到 {len(existing_dirs)} 個季度資料夾: {existing_dirs}")
        
        merge_year_data(existing_dirs, output_dir, year)
        print(f" {year} 年資料處理完成")

if __name__ == "__main__":
    merge_all_years(104, 113)
    print("\n 所有年份合併完成") 