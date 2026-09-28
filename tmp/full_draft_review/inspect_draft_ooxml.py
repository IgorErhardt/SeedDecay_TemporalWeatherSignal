from pathlib import Path
from zipfile import ZipFile
from lxml import etree

docx = Path(r"C:\Users\Usuario\Downloads\Modelo_GA_SOJA.docx")
ns = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "m": "http://schemas.openxmlformats.org/officeDocument/2006/math",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "pr": "http://schemas.openxmlformats.org/package/2006/relationships",
}
with ZipFile(docx) as z:
    root = etree.fromstring(z.read("word/document.xml"))
    rels = etree.fromstring(z.read("word/_rels/document.xml.rels"))
    relmap = {
        el.get("Id"): el.get("Target")
        for el in rels.findall("pr:Relationship", ns)
    }
    paras = root.xpath("//w:body/w:p", namespaces=ns)
    for i, p in enumerate(paras):
        text = "".join(p.xpath(".//w:t/text() | .//m:t/text()", namespaces=ns))
        embeds = p.xpath(".//a:blip/@r:embed", namespaces=ns)
        if text.strip() or embeds:
            print(f"P{i:03d}\t{text}")
            for rid in embeds:
                print(f"  IMAGE {rid} -> {relmap.get(rid)}")
