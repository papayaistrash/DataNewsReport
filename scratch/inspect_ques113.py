import sys
from pypdf import PdfReader

sys.stdout.reconfigure(encoding='utf-8')

reader = PdfReader("data/事業面問卷/ques113.pdf")
print("Total pages:", len(reader.pages))

# Write all text to a file in scratch
full_text = ""
for i, page in enumerate(reader.pages):
    full_text += f"\n--- PAGE {i+1} ---\n"
    full_text += page.extract_text() or ""

with open("scratch/biz_ques_extracted.txt", "w", encoding="utf-8") as f:
    f.write(full_text)

print("Extracted questionnaire text written to scratch/biz_ques_extracted.txt")
