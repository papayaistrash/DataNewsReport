from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/103年針對移工問卷/提要分析103.pdf")
print("Total pages:", len(reader.pages))

# Let's extract pages 25 to 35 (which correspond to household caregiver results)
with open("c:/R4CSS-master/報告用資料/scripts/household_summary.txt", "w", encoding="utf-8") as f:
    for idx in range(24, 35): # 0-indexed, so 25 to 35
        if idx < len(reader.pages):
            text = reader.pages[idx].extract_text()
            f.write(f"--- PAGE {idx+1} ---\n")
            f.write(text)
            f.write("\n\n")

print("Wrote text to household_summary.txt")
