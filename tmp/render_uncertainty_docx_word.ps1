$inputDoc = (Resolve-Path -LiteralPath 'D:\Igor_Masters\Proj_PodAndGrainRot\tmp\scalar_on_function_uncertainty_equations.docx').Path
$qaDir = 'D:\Igor_Masters\Proj_PodAndGrainRot\tmp\scalar_on_function_uncertainty_equations_qa'
New-Item -ItemType Directory -Path $qaDir -Force | Out-Null
$outputPdf = Join-Path $qaDir 'scalar_on_function_uncertainty_equations.pdf'

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$word.DisplayAlerts = 0
try {
    $doc = $word.Documents.Open($inputDoc, $false, $true)
    $doc.ExportAsFixedFormat($outputPdf, 17)
    $doc.Close($false)
}
finally {
    $word.Quit()
}

Write-Output $outputPdf
