devtools::load_all()

devtools::document()

# 2. 定义文件路径
# 请确保这两个文件在你的工作目录，或者填写绝对路径
expr_file <- "./test/plasmix_met_test_expr.txt"
meta_file <- "./test/plasmix_met_test_meta.txt"

template <- system.file("extdata", "Plasmix_template.docx", package = "PlasmixMetQC")

template

output_dir <- './test/'

tryCatch({
  qc_res <- qc_conclusion(
    exp_path = expr_file,
    meta_path = meta_file,
    output_dir = output_dir
  )
  
  # 4. 查看结果
  message("\n=== 分析完成 ===")
  print("结论表:")
  print(qc_res$conclusion)
  
  # 如果想直接在 RStudio 里看图，可以运行：
  # print(qc_res$results$snr_results$snr_plot)  # 查看 SNR PCA 图
  # print(qc_res$results$cor_results$cor_plot)  # 查看 RC 相关性图
  
}, error = function(e) {
  message("运行报错: ", e$message)
})

qc_res

generate_metabo_report(
  qc_result = qc_res,
  report_template = template, 
  report_dir = output_dir
  # report_name = "我的测试报告.docx"
  # batch_name = "20260108测试批次" # 可选，指定表格里的批次名
)
