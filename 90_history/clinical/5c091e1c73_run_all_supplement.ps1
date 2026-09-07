param(
    [string]$SourceRoot = 'F:\文章_大论文\0830\结果\第五章完整重算_0831',
    [string]$SupplementRoot = 'F:\文章_大论文\0830\补充0831',
    [int]$RsflcWorkerCores = 2
)

$ErrorActionPreference = 'Stop'
$scriptRoot = Join-Path $SupplementRoot '代码'
$runtimeRoot = Join-Path $SupplementRoot 'runtime_links'
New-Item -ItemType Directory -Path $runtimeRoot -Force | Out-Null

$linkTargets = @{
    source_figures = Join-Path $SourceRoot '输出\figures'
    source_tables  = Join-Path $SourceRoot '输出\tables'
    source_models  = Join-Path $SourceRoot '输出\models'
    source_data    = Join-Path $SourceRoot '数据'
    source_code    = Join-Path $SourceRoot '代码'
    source_output  = Join-Path $SourceRoot '输出'
    self_code      = $scriptRoot
    target_root    = $SupplementRoot
}

foreach ($name in $linkTargets.Keys) {
    $linkPath = Join-Path $runtimeRoot $name
    if (-not (Test-Path -LiteralPath $linkPath)) {
        New-Item -ItemType Junction -Path $linkPath -Target $linkTargets[$name] | Out-Null
    }
}

$env:CH5_SUPPLEMENT_ROOT = (Join-Path $runtimeRoot 'target_root').Replace('\', '/')
$env:CH5_SOURCE_FIGURES = (Join-Path $runtimeRoot 'source_figures').Replace('\', '/')
$env:CH5_SOURCE_TABLES = (Join-Path $runtimeRoot 'source_tables').Replace('\', '/')
$env:CH5_SOURCE_MODELS = (Join-Path $runtimeRoot 'source_models').Replace('\', '/')
$env:CH5_SOURCE_DATA = (Join-Path $runtimeRoot 'source_data').Replace('\', '/')
$env:CH5_SOURCE_CODE = (Join-Path $runtimeRoot 'source_code').Replace('\', '/')
$env:CH5_SOURCE_OUTPUT = (Join-Path $runtimeRoot 'source_output').Replace('\', '/')
$env:RSFLC_WORKER_CORES = [string]$RsflcWorkerCores

$rscript = (Get-Command Rscript -ErrorAction Stop).Source
$launcher = Join-Path $runtimeRoot 'self_code\utf8_launcher.R'

function Invoke-Utf8R([string]$TargetScript, [string[]]$TargetArgs = @()) {
    $target = Join-Path $runtimeRoot ('self_code\' + $TargetScript)
    & $rscript $launcher $target @TargetArgs
    if ($LASTEXITCODE -ne 0) { throw "R script failed: $TargetScript" }
}

Invoke-Utf8R '00_export_existing_missing.R'
Invoke-Utf8R '01_static_fixed_5fold.R'

$logRoot = Join-Path $SupplementRoot 'logs'
New-Item -ItemType Directory -Path $logRoot -Force | Out-Null
$worker = Join-Path $runtimeRoot 'self_code\02_dynamic_fixed_5fold_worker.R'
$jobs = foreach ($fold in 1..5) {
    Start-Process -FilePath $rscript `
        -ArgumentList @($launcher, $worker, [string]$fold) `
        -PassThru -WindowStyle Hidden `
        -RedirectStandardOutput (Join-Path $logRoot "rsflc_fold_${fold}.out.log") `
        -RedirectStandardError (Join-Path $logRoot "rsflc_fold_${fold}.err.log")
}
$jobs | ForEach-Object { $_.WaitForExit(); if ($_.ExitCode -ne 0) { throw "RSFLC fold worker failed: PID $($_.Id)" } }

Invoke-Utf8R '03_combine_dynamic_fixed_5fold.R'

$pythonCandidates = @(
    'C:\Users\A\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe',
    (Get-Command python -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1)
) | Where-Object { $_ -and (Test-Path -LiteralPath $_) }
if ($pythonCandidates.Count -gt 0) {
    & $pythonCandidates[0] (Join-Path $runtimeRoot 'self_code\00_fix_utf8_manifests.py')
    if ($LASTEXITCODE -ne 0) { throw 'UTF-8 manifest post-processing failed' }
}

Write-Host 'SUPPLEMENT_0831_ALL_OK'

