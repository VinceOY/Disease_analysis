# Disease_analysis

## Overview
`Disease_analysis` is a diabetes analysis project in s, w, t:

## Structure

```
Disease_analysis/
|
├── get.R 
├── lab_result_combine.R
├── create_table.R
├── cox_model_result.R
└── README.md
```

## File Descriptions:

- get_data.R: done
  - 目的: 整合分析相關疾病碼、基本資料、看診檔(門診,住院)
    - input_files: ICD92ICD10.csv、basic.csv、opd_basic.csv、ipd_basic.csv、
    - output_table 1~6: 篩選出各outcome下的有效病人(outcome_clean.csv)

- lab_result_combine.R: done
  - 目的: 雙和、北醫、萬芳檢驗資料整合
    - input_files: v_labresult_t.csv、v_labresult_s.csv.csv、v_exper_sign_w.csv
    - output_table: 三院資料整合與重新命名欄位(lab_result.csv) * 2test_items

- create_table.R: done
  - 目的: 資料摘要表(包括全體、有檢驗項目、及條件篩選過後的摘要表)
    - input: outcome_clean.csv、lab_result.csv
    - output: 
    - output_table1: 所有糖尿病患者的基本資料摘要
    - output_table2: index date一年內4季皆有定期檢驗test_item的糖尿病患者中test_item檢驗值的統計值*2
    - output_table3: 目標病患在各test_items對應到各outcome總數量、發病數量以及總追蹤時間
    - output_table 4~15: 最後分析人選的檢驗資料(含基本資料) (test_outcome.csv) * 12 

- cox_model_result.R: ToDo
  - 目的: 2個檢驗項目, 3種變數組合, 6個併發症用cox model進行建模&分析 
    - input_table: test_outcome.csv
    - output_table: 36種模型結果整合 (model_result.csv)
