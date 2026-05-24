from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

for name in ["ques103_1", "ques103_2"]:
    path = f"data/103年針對移工問卷/{name}.pdf"
    reader = PdfReader(path)
    print(f"\n=== {name} (pages: {len(reader.pages)}) ===")
    for idx, page in enumerate(reader.pages):
        text = page.extract_text()
        lines = text.split("\n")
        # Find where "貳、" is located
        for l_idx, line in enumerate(lines):
            if "貳、" in line or "仲介" in line:
                # print 20 lines from here
                print(f"Page {idx+1}, Line {l_idx+1}:")
                for i in range(max(0, l_idx - 1), min(len(lines), l_idx + 25)):
                    print(f"  {lines[i]}")
                print("-" * 50)
                break # Only print first occurrence in page or break to avoid too much output
