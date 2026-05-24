import zipfile
import xml.etree.ElementTree as ET
import re
import sys

sys.stdout.reconfigure(encoding='utf-8')

def get_docx_text(path):
    try:
        with zipfile.ZipFile(path) as docx:
            xml_content = docx.read('word/document.xml')
            root = ET.fromstring(xml_content)
            
            # Find all text elements
            ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
            texts = []
            for elem in root.findall('.//w:t', ns):
                texts.append(elem.text or '')
            return "".join(texts)
    except Exception as e:
        return str(e)

path = "data/113年移工/資料使用說明113.docx"
text = get_docx_text(path)
print("Text length:", len(text))
print("First 1000 characters:")
print(text[:1000])

# Search for "國籍"
print("\n=== Search results for '國籍' ===")
for match in re.finditer(r".{0,40}國籍.{0,40}", text):
    print("MATCH:", match.group())
