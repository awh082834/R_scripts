library(readxl)
library(writexl)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
data <- read_excel(args[1]) # nolint: line_length_linter.

names(data) <- c("ID", "RunNum", "date", "loc",
 "district", "region", "vax", "ob", "facility",
 "lims", "runDate", "runName", "notes") # nolint: line_length_linter.

metrics_frame <- subset(data, ID >= args[2] & ID <= args[3])
metrics_frame <- select(metrics_frame, -c(4, 5, 6, 9, 10, 11, 13))
metrics_frame <- metrics_frame %>% mutate(purpose =
 case_when(vax == "vaccinated" | ob != "NA" ~ "targetted_sequencing", # nolint: line_length_linter.
           vax == "vaccinated" & ob != "NA" ~ "targetted_sequencing")) # nolint: line_length_linter.
metrics_frame["purpose"][is.na(metrics_frame["purpose"])] <- "baselinesurveillance" # nolint: line_length_linter.

monroe_depths <- read.csv(args[4], header = TRUE)
monroe_depths <- select(monroe_depths, c("sample", "mean_depth"))
monroe_depths <- monroe_depths[order(monroe_depths$sample,
 decreasing = FALSE), ]
monroe_depths <- subset(monroe_depths,
 sample >= args[2] & sample <= args[3])

metrics_frame <- cbind(metrics_frame, monroe_depths$mean_depth)
metrics_frame[, "date"] = as.character(metrics_frame[["date"]])

names(metrics_frame) <- c("DCLS Sequencing ID", "DCLS Specimen ID", "Date of Collection", # nolint: line_length_linter.
 "Vaccination Status", "Outbreak ID", "Sequence Run Name", "Purpose of Sampling", "Mean Depth") # nolint: line_length_linter.

print(monroe_depths)
print(metrics_frame)

write_xlsx(metrics_frame, args[5])
