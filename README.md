# Disease_analysis

## Overview
`Disease_analysis` is a diabetes analysis project in s, w, t:

## Structure

```
Disease_analysis/
├── tool_function/
|   |── tableone.R
|   |── standard_function.R  
|   |── fetch_function.R
|   └── documentation.md
├── get_valid_data_step1.R 
├── get_valid_data_step2.R 
├── get_valid_data_step3.R 
├── lab_result.R
├── create_table.R
├── cox_model.R
├── find_test_item_code.R
├── density_plot.R
└── README.md
```

## File Descriptions:
- get_valid_data_step1.R: 
  - 目的: 原始資料撈取疾病病例資料
    - input_files: ICD92ICD10.csv、i(o)pd_basic.csv
    - output_table: 
      - disease_code_list: 所有的related disease code 對應的 ICD code
      - disease_i(o)pd_hospital: 從original 撈取疾病病例資料
      
- get_valid_data_step2.R: 
  - 目的: 合併病人基本資料、目標疾病相關的病歷紀錄(同時篩選目標病患)
    - input_files: chr_basic.csv、disease_i(o)pd_hospital.csv
    - output_table: 
      - dt_target: 合併(disease_iopd_hospital) 並篩選目標疾病的病人病歷資料
      - dt_merge: 合併(dt_target & chr_basic) 
    
- get_valid_data_step3.R: 
  - 目的: 清整資料(exclude 條件)
    - input_files: dt_merge.csv
    - output: 
      - diabete_exclude1: 從dt_merge移除 ID, AGE, Index_date
      - outcome_exclude2: 各outcome皆有一筆資料 * 12
        - 從diabete_exclude1移除index date 前有發生outcome、
        - index date後一年內發生outcome

- lab_result.R: 
  - 目的: 雙和、北醫、萬芳檢驗資料整合
    - input_files: v_labresult_t.csv、v_labresult_s.csv.csv、v_exper_sign_w.csv
    - output_table: 三院資料整合與重新命名欄位(lab_result.csv) * 1

- create_table.R:  output * 20 
  - 目的: 輸出資料表、以及最後要放入模型分析的病患檔案
    - table1: dt_exclude1下基本資料欄位分布, table1_basic.csv*1
    - table2: 有效患者各季test item的檢驗值、次數
      - step1:# find disease lab + add interval_col
        - input: 3院lab檔、dt_exclude1
        - output: test_lab.csv(每個患者的test值有算interval) *2
        
      - step2: # get valid id data
        - input: dt_exclude1下lab檔
        - cutpoint2: test_valid_id.csv (定期做檢驗的病人ID) *2
        - output: test_item_table2.csv *2
        
    - table3: exclude2下4季皆有追蹤的患者: 各outcome (event、總數量、總追蹤時間) 
        - input: test_item_valid_id、dt_exclude2
        - cutpoint: test_outcome_dtf.csv * 12
        - output: table3_all_outcome_summary.csv *1

- cox_model_result.R: 
  - 目的: 從6 outcomes, 6 test items, 4 variable selections, 整理48個cox model的result
    - input_table: test_outcome_dtf.csv
    - output_table: 
      -  test_outcome_dtf_all.csv
      -  model_summary.csv

- find_test_item_code.R
  - 目的: 找出相對應的Test item code and Test item name
    - input_table: v_exp_item_t.csv, v_exp_item_s.csv
    - 連回create_table.csv
    
- density_plot.R
  - 目的: 畫出三院各檢測項目的分佈圖(同時要壓縮離群值), 同時輸出各檢驗結果的分佈值
    - input_table: lab_result_swt.csv
    - output: test_item_density_plot_all.png, test_value_density_diabete.csv
      