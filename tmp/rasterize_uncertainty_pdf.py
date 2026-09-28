from pathlib import Path
import pypdfium2 as pdfium

qa = Path(r"D:\Igor_Masters\Proj_PodAndGrainRot\tmp\scalar_on_function_uncertainty_equations_qa")
pdf = pdfium.PdfDocument(qa / "scalar_on_function_uncertainty_equations.pdf")
for index in range(len(pdf)):
    page = pdf[index]
    bitmap = page.render(scale=2.0)
    bitmap.to_pil().save(qa / f"page-{index + 1}.png")
print(len(pdf))
