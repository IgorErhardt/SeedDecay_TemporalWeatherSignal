from pathlib import Path
from PIL import Image, ImageOps, ImageDraw

root = Path(r"D:\Igor_Masters\Proj_PodAndGrainRot\tmp\full_draft_review\rendered_revised")
pages = sorted(root.glob("page-*.png"), key=lambda p: int(p.stem.split("-")[-1]))
for group_no, start in enumerate(range(0, len(pages), 12), 1):
    group = pages[start:start+12]
    thumbs = []
    for idx, path in enumerate(group, start+1):
        im = Image.open(path).convert("RGB")
        im.thumbnail((420, 594))
        canvas = Image.new("RGB", (440, 630), "white")
        canvas.paste(im, ((440-im.width)//2, 24))
        d = ImageDraw.Draw(canvas)
        d.text((10, 5), f"Page {idx}", fill="black")
        thumbs.append(ImageOps.expand(canvas, border=1, fill="grey"))
    sheet = Image.new("RGB", (442*3, 632*4), "#d9d9d9")
    for j, im in enumerate(thumbs):
        sheet.paste(im, ((j % 3)*442, (j // 3)*632))
    sheet.save(root / f"contact_sheet_{group_no}.png")
print(len(pages))
