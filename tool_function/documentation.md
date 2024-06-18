### 函數: fetch_data()

這個函數的目的是從一個資料表中提取目標 ID 列和符合特定疾病碼的資料。

**參數:**
- `dt` (`data.table`): 包含需要處理的資料。
- `target_ID_cols` (`vector`): 要標準化的目標 ID 列表，如 (ID, DATE)。
- `disease_ID_cols` (`vector`): 疾病 ID 所在的欄位 (如疾病col1, 疾病col2, ...)。
- `disease_codes` (`vector`): 要配對的疾病碼。

**返回值:**
- `data.table`: 包含符合條件的資料表。

**流程:**
1. 檢查資料類型，確保參數類型正確。
2. 選擇需要的欄位。
3. 將`disease_ID_cols`資料轉換為字符類型。
4. 將資料表進行 `melt` 操作，使`disease_ID_cols`成為單一欄位。
5. 過濾符合疾病碼的資料。
6. 返回包含`target_ID_cols`的資料表。

---

### 函數: get_valid_data()

這個函數的目的是從資料表中提取在一年內看診次數超過 k 次的資料。

**參數:**
- `dt` (`data.table`): 門診資料。
- `group_id_col` (`character`): 病人 ID 欄位名稱。
- `date_col` (`character`): 日期欄位名稱。
- `k` (`numeric`): 一年內看診次數。

**返回值:**
- `data.table`: 包含符合條件的資料表。

**流程:**
1. 檢查資料類型，確保參數類型正確。
2. 重新命名資料欄位。
3. 移除重複值。
4. 按 (ID, DATE) 排序。
5. 創建移動欄位以計算日期差。
6. 過濾一年內看診次數超過 k 次的資料。
7. 返回符合條件的資料表。

---

### 函數: find_earliest_date()

這個函數的目的是從多個資料表中找到各確診病人的第一次確診時間。

**參數:**
- `P_list` (`list`): 包含多個參數列表，每個列表包括:
  - `df` (`data.table`): 資料表。
  - `idcol` (`character`): 病人 ID 欄位名稱。
  - `datecol` (`character`): 日期欄位名稱。
  - `k` (`numeric`): 一年內看診次數。

**返回值:**
- `data.table`: 各確診病人的第一次確診時間 (ID, DATE)。

**流程:**
1. 初始化一個空的 `data.table`。
2. 遍歷參數列表，對每個資料表應用 `get_valid_data` 函數。
3. 合併所有符合條件的資料。
4. 找到每個病人的最早確診日期。
5. 返回結果。

---

### 函數: standardized_date()

這個函數的目的是將日期欄位標準化為統一的日期格式。

**參數:**
- `dt` (`data.table`): 包含日期欄位的資料框。
- `date_col` (`character`): 日期欄位的名稱。

**返回值:**
- `data.frame`: 標準化後的資料框，日期欄位被轉換為統一的日期格式。

**流程:**
1. 檢查日期欄位的格式，如果不符合 "YYYYMMDD" 格式，則進行轉換。
2. 將日期欄位轉換為統一的日期格式 "%Y-%m-%d"。
3. 返回結果。

