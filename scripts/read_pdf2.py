from pypdf import PdfReader
import sys

# Set output encoding to utf-8
sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/103年針對移工問卷/code103.pdf")
print("Total pages:", len(reader.pages))

# Let's write the first 30 pages to a file
with open("c:/R4CSS-master/報告用資料/scripts/code103_text.txt", "w", encoding="utf-8") as f:
    for idx in range(len(reader.pages)):
        text = reader.pages[idx].extract_text()
        f.write(f"--- PAGE {idx+1} ---\n")
        f.write(text)
        f.write("\n\n")

print("Wrote text to code103_text.txt")
