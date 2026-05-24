from pypdf import PdfReader
import sys

sys.stdout.reconfigure(encoding='utf-8')

def find_keywords_in_pdf(path, keywords):
    reader = PdfReader(path)
    print(f"\n=========================================\nFILE: {path}\n=========================================")
    for idx, page in enumerate(reader.pages):
        text = page.extract_text()
        lines = text.split("\n")
        for line_idx, line in enumerate(lines):
            if any(kw in line for kw in keywords):
                # print the line, and maybe previous/next lines for context
                start = max(0, line_idx - 1)
                end = min(len(lines), line_idx + 3)
                context = "\n  ".join(lines[start:end])
                print(f"Page {idx+1}, Line {line_idx+1}:")
                print(f"  {context}")
                print("-" * 40)

keywords = ["仲介", "費用", "服務", "登記", "介紹"]
find_keywords_in_pdf("data/103年針對移工問卷/ques103_1.pdf", keywords)
find_keywords_in_pdf("data/103年針對移工問卷/ques103_2.pdf", keywords)
