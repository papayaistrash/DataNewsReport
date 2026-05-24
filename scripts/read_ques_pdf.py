from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

for name in ["ques103_1", "ques103_2", "提要分析103", "實施計畫103"]:
    path = f"data/103年針對移工問卷/{name}.pdf"
    reader = PdfReader(path)
    print(f"\n=== {name} (pages: {len(reader.pages)}) ===")
    
    # Check if text is readable or garbled
    sample_text = ""
    for p in reader.pages[:2]:
        sample_text += p.extract_text()
    
    readable_chars = sum(1 for c in sample_text if '\u4e00' <= c <= '\u9fff')
    print(f"Readable Han characters in first 2 pages: {readable_chars}")
    if readable_chars > 20:
        print("Sample readable text:")
        print(sample_text[:300])
    else:
        print("Seems garbled or scanned.")
