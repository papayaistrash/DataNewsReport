from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/103年針對移工問卷/提要分析103.pdf")
print("Total pages in summary analysis:", len(reader.pages))

keywords = ["費用", "仲介", "介紹", "負擔"]
results = []

for idx, page in enumerate(reader.pages):
    text = page.extract_text()
    lines = text.split("\n")
    for line_idx, line in enumerate(lines):
        if any(kw in line for kw in keywords):
            results.append((idx + 1, line))

print("\n--- Search Results in 提要分析103.pdf ---")
for page_num, line in results[:100]:
    print(f"Page {page_num}: {line.strip()}")
