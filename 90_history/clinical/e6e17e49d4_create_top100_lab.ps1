<#
从summary_lab.csv中筛选admission_count最多的前100个项目
创建summary_lab_100.csv文件
#>

Write-Host "开始筛选admission_count最多的前100个实验室检查项目..."

# 定义文件路径
$script_dir = Split-Path -Parent $MyInvocation.MyCommand.Path
$summary_lab_file = Join-Path -Path $script_dir -ChildPath "summary_lab.csv"
$output_file = Join-Path -Path $script_dir -ChildPath "summary_lab_100.csv"

# 检查输入文件是否存在
if (-not (Test-Path $summary_lab_file)) {
    Write-Host "错误：summary_lab.csv文件不存在！" -ForegroundColor Red
    Write-Host "请先运行run_lab_summary.bat生成该文件" -ForegroundColor Yellow
    pause
    exit 1
}

# 读取summary_lab.csv文件
Write-Host "读取summary_lab.csv文件..."
$summary_lab_data = Import-Csv $summary_lab_file

# 按admission_count降序排序，并选择前100个记录
Write-Host "筛选前100个admission_count最多的项目..."
$top100_lab_data = $summary_lab_data | Sort-Object -Property @{Expression={[int]$_.admission_count}; Descending=$true} | Select-Object -First 100

# 导出到CSV文件
Write-Host "导出到summary_lab_100.csv文件..."
$top100_lab_data | Export-Csv -Path $output_file -NoTypeInformation -Encoding UTF8

# 验证导出结果
if (Test-Path $output_file) {
    $exported_count = (Import-Csv $output_file).Count
    Write-Host "筛选完成！" -ForegroundColor Green
    Write-Host "已将admission_count最多的前$exported_count个项目保存到$output_file" -ForegroundColor Green
    
    # 显示前5条记录
    Write-Host "\n前5条记录：" -ForegroundColor Cyan
    $top100_lab_data | Select-Object -First 5
    
    # 显示统计信息
    Write-Host "\n统计信息：" -ForegroundColor Cyan
    
    # 将数据转换为整数类型后再计算
    $total_examinations = ($top100_lab_data | ForEach-Object { [int]$_.total_examinations } | Measure-Object -Sum).Sum
    $total_patients = ($top100_lab_data | ForEach-Object { [int]$_.patient_count } | Measure-Object -Sum).Sum
    $total_admissions = ($top100_lab_data | ForEach-Object { [int]$_.admission_count } | Measure-Object -Sum).Sum
    
    $avg_examinations = [math]::Round(($top100_lab_data | ForEach-Object { [int]$_.total_examinations } | Measure-Object -Average).Average, 2)
    $avg_patients = [math]::Round(($top100_lab_data | ForEach-Object { [int]$_.patient_count } | Measure-Object -Average).Average, 2)
    $avg_admissions = [math]::Round(($top100_lab_data | ForEach-Object { [int]$_.admission_count } | Measure-Object -Average).Average, 2)
    
    Write-Host "前100个项目的总检查次数：$total_examinations"
    Write-Host "前100个项目的总患者数：$total_patients"
    Write-Host "前100个项目的总住院次数：$total_admissions"
    Write-Host "平均每个项目的检查次数：$avg_examinations"
    Write-Host "平均每个项目的患者数量：$avg_patients"
    Write-Host "平均每个项目的住院次数：$avg_admissions"
} else {
    Write-Host "导出失败！" -ForegroundColor Red
}

Write-Host "\n任务完成！" -ForegroundColor Blue
