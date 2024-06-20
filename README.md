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
    - output_table: 三院資料整合與重新命名欄位(lab_result.csv) * 2(test_items)

- create_table.R:  (todo)
  - 目的: 資料摘要表(包括全體、有檢驗項目、及條件篩選過後的摘要表)
    - input: outcome_clean.csv、lab_result.csv
    - output: 
    - output_table1: 所有糖尿病患者的基本資料摘要
    - output_table2: index date一年內4季皆有定期檢驗test_item的糖尿病患者中test_item檢驗值的統計值*2
    - output_table3: 目標病患在各test_items對應到各outcome總數量、發病數量以及總追蹤時間
    - output_table 4~15: 最後分析人選的檢驗資料(含基本資料) (test_outcome.csv) * 12 

- cox_model_result.R: 
  - 目的: 從6 outcomes, 2 test items, 4 variable selections, 整理48個cox model的result
    - input_table: test_lab.csv、outcome_test_valid_id.csv
    - output_table: 
      -  test_outcome_dtf_all.csv
      -  model_summary.csv