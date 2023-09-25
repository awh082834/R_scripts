## Included Scripts

|Scripts|Description|Args|
|:---   |:---       |:---|
|Isolate_report_builder.Rmd|R Markdown script to build a pdf report on a per isolate basis for the Ashe long read pipeline||
|snp_tree_builder.R|R script used to create phylogenetic trees based on SNPs rather than evolutionary distance||
|tree_builder.R|Base R script that snp_tree_builder is based off of||
|metrics_maker.R|R script used to help automate the SARS-COV2 submission process|Rscript metrics_maker.R Masterlist Start_ID End_ID monroe_summary output_filename|