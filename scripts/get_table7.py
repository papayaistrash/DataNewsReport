from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/103年針對移工問卷/提要分析103.pdf")
print("Extracting page 8 text:")
print(reader.pages[7].extract_text()) # 0-indexed page 8 is index 7
