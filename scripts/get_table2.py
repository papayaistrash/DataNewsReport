from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/103年針對移工問卷/提要分析103.pdf")
print("Extracting page 3 text:")
print(reader.pages[2].extract_text()) # 0-indexed page 3 is index 2

print("\nExtracting page 4 text:")
print(reader.pages[3].extract_text())
