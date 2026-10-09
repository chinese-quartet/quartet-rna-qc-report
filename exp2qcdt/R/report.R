# ---------------------------------------------------------------------------- #
#' @title Generate Quartet RNA report
#'
#' @description Use calculated RNA result to generate report
#'
#' @param qc_result list
#' @param report_template character
#' @param report_dir character
#' @param report_name character
#'
#' @importFrom dplyr %>%
#' @importFrom flextable flextable
#' @importFrom flextable theme_vanilla
#' @importFrom flextable color
#' @importFrom flextable set_caption
#' @importFrom flextable align
#' @importFrom flextable width
#' @importFrom flextable bold
#' @importFrom flextable bg
#' @importFrom flextable theme_box
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom officer body_add_par
#' @importFrom flextable body_add_flextable
#' @importFrom officer body_add_gg
#' @importFrom officer body_add_break
#' @importFrom officer read_docx
#' @importFrom officer fp_text
#' @importFrom officer fpar
#' @importFrom officer ftext
#' @importFrom officer body_add_fpar
#'
#' @examples
#' # 加载示例 RNA_qc_result 对象
#' rna_qc_result <- system.file("extdata", "rna_qc_result_example.RData", package = "exp2qcdt")
#' load(rna_qc_result)
#'
#' # 指定包内文档的路径
#' report_template <- system.file("extdata", "quartet_template.docx", package = "exp2qcdt")
#'
#' # 运行函数
#' generate_rna_report(qc_result = RNA_result, report_template = report_template)
#'
#' @export
generate_rna_report <- function(qc_result,
                                report_template,
                                report_dir = NULL,
                                report_name = NULL) {
  if (is.null(qc_result) || is.null(report_template)) {
    stop("All arguments (qc_result, report_template) are required.")
  }

  if (is.null(report_dir)) {
    path <- getwd()
    sub_dir <- "output"
    dir.create(file.path(path, sub_dir), showWarnings = FALSE)
    report_dir <- file.path(path, "output")
  }

  ### 读取quarter报告模板并生成报告
  if (is.null(report_name)) {
    report_name <- "Quartet_RNA_report.docx"
  }
  output_file <- file.path(report_dir, report_name)

  # --- 1. 定义中文文本内容 (基于 ShenKang-Quartet-RNA-Report_v0.1.docx) ---
  
  # 摘要
  text_sum_intro <- "本报告基于多项组学关键质量控制指标，总结了 Quartet RNA 参考物质所生成数据的质量情况。质量控制流程从用户输入基因表达矩阵开始，分别计算外部质控品的信噪比（Signal-to-Noise Ratio, SNR）、与参考数据集的Pearson相关系数（Pearson correlation coefficient, PCC）及整体质量判断。"
  
  # 定义解释文本
  # 信噪比定义
  text_snr_title <- "信噪比（Signal-to-Noise Ratio, SNR）"
  text_snr_desc <- "用于表征某一检测平台、实验室或批次区分不同生物样本组之间内在生物学差异（“信号”）与同一样本组技术重复之间变异（“噪声”）的能力。SNR 越高，说明区分生物学差异的能力越强。"
  
  # 相对相关性定义
  text_rc_title <- "与参考数据集的Pearson相关系数（Pearson correlation coefficient, PCC）"
  text_rc_desc <- "定义为在给定样本对之间，测试数据集中比值型表达水平与对应比值型参考数据集之间的 Pearson 相关系数，用于表征比值表达谱在数值层面的整体一致性趋势。为提高分析可靠性，在进行比值表达分析前，首先对每个样本组的技术重复取均值。差异倍数（fold change）采用 log2 转换。"
  
  # 参考文献
  text_ref_title <- "参考文献"
  text_ref_1 <- "1. Zheng Y, et al. Multi-omics data integration using ratio-based quantitative profiling with Quartet reference materials. Nature Biotechnology, 2024."
  text_ref_2 <- "2. Yu, Y. et al. Quartet RNA reference materials improve the quality of transcriptomic data through ratio-based profiling. Nature biotechnology, 2024."
  text_ref_3 <- "3. GB/T 45214-2025《人全基因组高通量测序数据质量评价方法》"
  text_ref_4 <- "4. 上海临床队列组学检测工作指引（征求意见稿）, 2025/11/26."
  
  # 免责声明
  text_disclaimer_title <- "免责声明"
  text_disclaimer_content <- "本数据质量报告仅针对所评估的特定数据集提供分析结果，仅供信息参考之用。尽管已尽最大努力确保分析结果的准确性和可靠性，但本报告按“现状（AS IS）”提供，不附带任何形式的明示或暗示担保。报告作者及发布方不对基于本报告内容所采取的任何行动承担责任。本报告中的结论不应被视为对任何产品或流程质量的最终判定，也不应用于关键应用场景、商业决策或法规合规用途，除非经过专业核查和独立验证。对于分析结果的正确性、准确性、可靠性或适用性，不作任何明示或暗示的保证。"

  # --- 创建符合新格式的表格 ---
  # 1. 从原始 qc_metrics_table 中提取数据
  # 使用 grepl 模糊匹配，防止因为空格或大小写导致取不到值
  raw_table <- qc_result$qc_metrics_table
  
  # 提取 SNR 和 RC 的数值 (转为 numeric 以便比较)
  snr_row <- raw_table[grep("Signal-to-Noise Ratio", raw_table$QC_metrics), ]
  rc_row  <- raw_table[grep("Relative Correlation", raw_table$QC_metrics), ]
  
  snr_val <- as.numeric(snr_row$Value)
  rc_val  <- as.numeric(rc_row$Value)
  
  # 2. 定义判断逻辑 (根据 DOCX 推荐标准: SNR>=10, RC>=0.8)
  # 批次名称 (如果 qc_result 里没有 batch_name，这里暂时用 QC_test，或者你可以改为动态获取)
  batch_name_str <- "Query_data" 
  
  # 格式化显示字符串 (如果未达标，添加向下箭头 ↓)
  snr_str <- sprintf("%.2f", snr_val)
  if (!is.na(snr_val) && snr_val < 10) {
    snr_str <- paste0(snr_str, " ↓")
  }
  
  rc_str <- sprintf("%.2f", rc_val)
  if (!is.na(rc_val) && rc_val < 0.80) {
    rc_str <- paste0(rc_str, " ↓")
  }
  
  # 整体质量判断
  is_pass <- (!is.na(snr_val) && snr_val >= 10) && (!is.na(rc_val) && rc_val >= 0.80)
  # is_pass <- (!is.na(snr_val) && snr_val >= 10)
  quality_str <- ifelse(is_pass, "Yes", "No")
  
  # 3. 手动构建符合 DOCX 格式的新数据框
  # 第一行是推荐标准，第二行是实际数据
  new_df <- data.frame(
    "样本组" = c("推荐质量标准", batch_name_str),
    "信噪比" = c("≥10", snr_str),
    "Pearson相关系数" = c("≥0.80", rc_str),
    "是否通过" = c("-", quality_str),
    check.names = FALSE # 防止列名被自动修改
  )
  
  # 4. 生成 Flextable 样式
  ft1 <- flextable(new_df) %>%
    # 设置基础边框主题
    theme_box() %>%
    flextable::font(part = "all", fontname = "Times New Roman") %>%
    # 全局居中
    align(align = "center", part = "all") %>%
    # 调整列宽 (根据 Word 页面宽度适当调整)
    width(width = 1.5) %>%
    # 表头加粗
    bold(part = "header") %>%
    # 第一行(推荐标准)加粗
    bold(i = 1, part = "body") %>%
    # 设置表头背景色 (浅灰色)
    bg(part = "header", bg = "#EFEFEF") %>%
    # 动态上色：如果整体质量是 No，标红
    color(i = 2, j = "是否通过", color = ifelse(quality_str == "No", "#B80D0D", "black")) %>%
    color(i = 2, j = "信噪比", color = ifelse(snr_val < 10, "#B80D0D", "black"))
    color(i = 2, j = "Pearson相关系数", color = ifelse(rc_val < 0.80, "#B80D0D", "black"))



  # --- 3. 生成中文报告流程 ---
  
  read_docx(report_template) %>%
    # 1. 标题：Quartet转录组质量报告
    body_add_par(value = "Quartet转录组质量报告", style = "heading 1") %>%
    
    # 2. 摘要 (Abstract)
    body_add_par(value = "摘要", style = "heading 2") %>%
    body_add_par(value = text_sum_intro, style = "Normal") %>%
    body_add_par(value = " ", style = "Normal") %>% # 空行
    
    # 3. 插入表格 (The Table)
    body_add_flextable(ft1) %>%
    # body_add_break() %>% # 分页
    
    # 4. 质量控制指标说明 (Definitions)
    body_add_par(value = "质量控制指标", style = "heading 2") %>%
    
    # SNR 定义
    body_add_par(value = text_snr_title, style = "heading 3") %>%
    body_add_par(value = text_snr_desc, style = "Normal") %>%
    
    # RC 定义
    body_add_par(value = text_rc_title, style = "heading 3") %>%
    body_add_par(value = text_rc_desc, style = "Normal") %>%
    # body_add_break() %>% # 分页
    
    
    # 5. 参考文献 (References)
    body_add_par(value = text_ref_title, style = "heading 2") %>%
    body_add_par(value = text_ref_1, style = "Normal") %>%
    body_add_par(value = text_ref_2, style = "Normal") %>%
    body_add_par(value = text_ref_3, style = "Normal") %>%
    body_add_par(value = text_ref_4, style = "Normal") %>%
    body_add_par(value = " ", style = "Normal") %>%
    
    # 6. 免责声明 (Disclaimer)
    body_add_par(value = text_disclaimer_title, style = "heading 3") %>% # 通常免责声明用小一点的标题或粗体
    body_add_par(value = text_disclaimer_content, style = "Normal") %>%
    body_add_break() %>%
    
    # 7. 插入图片 (Plots)
    # 注意：新报告似乎主要关注 SNR 和 Correlation 两个图
    # 请确保 qc_result$snr_plot 和 qc_result$logfc_plot (或 correlation plot) 是存在的
    
    # 插入 SNR 图片
    body_add_par(value = "Signal-to-Noise Ratio", style = "heading 2") %>%
    body_add_gg(value = qc_result$snr_plot, style = "centered") %>% # [cite: 8]
    
    # 插入 Correlation 图片
    body_add_break() %>%
    body_add_par(value = "Pearson Correlation Coefficient", style = "heading 2") %>%
    body_add_gg(value = qc_result$logfc_plot, style = "centered") %>% # [cite: 9]

    # 输出文件
    print(target = output_file)
}
