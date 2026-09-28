$ErrorActionPreference = 'Stop'
$docxPath = 'D:\Igor_Masters\Proj_PodAndGrainRot\tmp\full_draft_review\Modelo_GA_SOJA_reviewed_current_analysis.docx'
$pdfPath = 'D:\Igor_Masters\Proj_PodAndGrainRot\tmp\full_draft_review\Modelo_GA_SOJA_reviewed_current_analysis.pdf'
$word = New-Object -ComObject Word.Application
$word.Visible = $false
$word.DisplayAlerts = 0
try {
    $doc = $word.Documents.Open($docxPath, $false, $true)
    $doc.ExportAsFixedFormat($pdfPath, 17)
    $doc.Close($false)
}
finally {
    $word.Quit()
    [System.Runtime.InteropServices.Marshal]::FinalReleaseComObject($word) | Out-Null
}
Write-Output $pdfPath
