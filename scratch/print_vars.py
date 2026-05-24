import pandas as pd

df = pd.read_csv("scratch/biz_variables.csv")
print("=== Matches for Nationality words ===")
matches = df[df['label'].str.contains("印尼|越南|泰國|菲律賓|國籍|印|泰|菲|越", na=False)]
for idx, row in matches.iterrows():
    print(f"{row['variable']}: {row['label']}")
