from pathlib import Path
from PIL import Image, ImageOps, ImageDraw

root = Path(r"D:\Igor_Masters\Proj_PodAndGrainRot\tmp\full_draft_review\rendered_revised")
pages = sorted(root.glob("final-*.png"), key=lambda p: int(p.stem.split("-")[-1]))
for group_no, start in enumerate(range(0, len(pages), 12), 1):
    group = pages[start:start+12]
    sheet = Image.new("RGB", (382*3, 547*4), "#d9d9d9")
    for j, path in enumerate(group):
        n = int(path.stem.split("-")[-1])
        im = Image.open(path).convert("RGB")
        im.thumbnail((360, 510))
        canvas = Image.new("RGB", (380, 545), "white")
        canvas.paste(im, ((380-im.width)//2, 28))
        ImageDraw.Draw(canvas).text((8, 5), f"Page {n}", fill="black")
        sheet.paste(ImageOps.expand(canvas, border=1, fill="grey"), ((j%3)*382, (j//3)*547))
    sheet.save(root / f"final_contact_sheet_{group_no}.png")
print(len(pages))
