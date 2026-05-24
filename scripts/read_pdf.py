from pypdf import PdfReader
import re

reader = PdfReader("data/103年針對移工問卷/code103.pdf")
print("Total pages:", len(reader.pages))

keywords = ["仲介", "費用", "服務", "介紹", "規費", "代辦"]
results = []

for idx, page in enumerate(reader.pages):
    text = page.extract_text()
    for line in text.split("\n"):
        if any(kw in line for kw in keywords):
            results.append((idx + 1, line))

print("Search results:")
for page_num, line in results[:100]: # Print first 100 lines containing keywords
    print(f"Page {page_num}: {line.strip()}")
