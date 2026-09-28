from pathlib import Path
from PIL import Image, ImageOps, ImageDraw

root = Path(r"D:\Igor_Masters\Proj_PodAndGrainRot\tmp\full_draft_review\rendered_revised")
pages = sorted(root.glob("check-*.png"), key=lambda p: int(p.stem.split("-")[-1]))
sheet = Image.new("RGB", (542*3, 772*3), "#d9d9d9")
for j, path in enumerate(pages):
    n = int(path.stem.split("-")[-1])
    im = Image.open(path).convert("RGB")
    im.thumbnail((520, 735))
    canvas = Image.new("RGB", (540, 770), "white")
    canvas.paste(im, ((540-im.width)//2, 28))
    ImageDraw.Draw(canvas).text((10, 6), f"Page {n}", fill="black")
    sheet.paste(ImageOps.expand(canvas, border=1, fill="grey"), ((j%3)*542, (j//3)*772))
sheet.save(root / "table_layout_check.png")
