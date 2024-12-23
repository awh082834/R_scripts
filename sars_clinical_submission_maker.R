# v1.1.0
# 2024-08-09

library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)

args <- commandArgs(trailingOnly = TRUE)
samples <- read.csv(args[1])
masterlist <- read.csv(args[2])
sub_name <- args[3]
submitter <- args[4]

#testing args
#samples <- read.csv("/opt/efs/AWH_dev/Submissions/script_verification/samplesheet.csv")
#masterlist <- read.csv("/opt/efs/AWH_dev/Submissions/templates/SARS-CoV-2_NGS_Masterlist_2024.csv")
#masterlist_2 <- read.csv("/opt/efs/AWH_dev/Submissions/templates/SARS-CoV-2_NGS_Masterlist_2024_071124.csv", skip = 1, header = FALSE)
#sub_name <- "sub_463"
#submitter <- "awh0828"

passing_samples = masterlist %>%
  filter(DCLS.Sequencing.ID %in% samples$sample) %>%
  mutate(coverage = samples$coverage) %>%
  filter(Pass.Fail == "Pass") %>%
  mutate(Date.of.Collection = strptime(as.character(Date.of.Collection), "%m/%d/%Y")) %>%
  mutate(Date.of.Collection = format(as.Date(Date.of.Collection, format = "%Y-%mm-%dd"))) %>%
  select(DCLS.Sequencing.ID, Date.of.Collection, Outbreak.ID, coverage)

removed_samples = masterlist %>%
  select(DCLS.Sequencing.ID, Pass.Fail) %>%
  filter(DCLS.Sequencing.ID %in% samples$sample) %>%
  filter(Pass.Fail == "Fail") %>%
  select(DCLS.Sequencing.ID) 

gis_headers <- c("submitter","fn","covv_virus_name","covv_type","covv_passage","covv_collection_date","covv_location","covv_add_location","covv_host","covv_add_host_info",
                 "covv_sampling_strategy","covv_gender","covv_patient_age","covv_patient_status","covv_specimen","covv_outbreak","covv_last_vaccinated","covv_treatment",
                 "covv_seq_technology","covv_assembly_method","covv_coverage","covv_orig_lab","covv_orig_lab_addr","covv_provider_sample_id","covv_subm_lab",
                 "covv_subm_lab_addr","covv_subm_sample_id","covv_authors","covv_comment","comment_type")

gisaid_sub = passing_samples %>%
  select(DCLS.Sequencing.ID) %>%
  mutate(Submitter = submitter, "FASTA filename" = paste0(sub_name,".fasta"), "Virus name" = paste0("hCoV-19/USA/",
    DCLS.Sequencing.ID,"/",year(passing_samples$Date.of.Collection)), Type = "betacoronavirus", "Passage details/history" = "origin",
    "Collection date" = passing_samples$Date.of.Collection, Location = "North America / USA / Virginia", "Additional location information"="",
    Host = "Human", "Additional host information"="",
    "Sampling Strategy" = case_when(passing_samples$Outbreak.ID != "" & passing_samples$Outbreak.ID != "RES989FluSentinel" ~ 'Targeted sequencing',
                                  passing_samples$Outbreak.ID == "" ~ 'Baseline surveillance', passing_samples$Outbreak.ID == "RES989FluSentinel" ~ 'Baseline surveillance'),
    Gender = "unknown", "Patient age" = "unknown", "Patient status" = "unknown", "Specimen source" = "unknown", Outbreak = "", "Last vaccinated" = "", Treatment = "",
    "Sequencing technology" = "Illumina Miseq", "Assembly method" = "minimap2/ivar", Coverage = paste0(passing_samples$coverage,"x"),
    "Originating lab" = "Virginia Division of Consolidated Laboratory Services", Address = "600 N 5th Street, Richmond VA 23219",
    "sample ID given by the sample provider" = "", "Submitting lab" = "Virginia Division of Consolidated Laboratory Services",
    Address_2 = "600 N 5th Street, Richmond VA 23219", "Sample ID given by the submitting laboratory" = passing_samples$DCLS.Sequencing.ID,
    Authors = "Virginia Division of Consolidated Laboratory Services", Comment = "", "Comment Icon" = "") %>% 
  select(-1)

gisaid_sub <- rbind(gis_headers,gisaid_sub)

sra_metadata = passing_samples %>%
  select(DCLS.Sequencing.ID) %>%
  rename(sample_name = DCLS.Sequencing.ID) %>%
  mutate(library_ID = passing_samples$DCLS.Sequencing.ID, title = paste0("Amplicon-based sequencing of SARS-CoV-2: ", 
                                                                         passing_samples$DCLS.Sequencing.ID), 
    library_strategy = "AMPLICON", library_source = "VIRAL RNA", library_selection = "cDNA", library_layout = "paired",
    platform = "Illumina", instrument_model = "Illumina Miseq", design_description = "sequencing protocol description is ARTIC PCR-tiling of viral cDNA (V4.1), sequenced by Illumina MiSeq with DNA Flex library prep-kit. Only reads aligned to SARS-CoV-2 reference (NC_045512.2) retained. ",
    filetype = "fastq", filename = paste0(passing_samples$DCLS.Sequencing.ID,"_SC2_R1.fastq.gz"), filename2 = paste0(passing_samples$DCLS.Sequencing.ID, "_SC2_R2.fastq.gz"))

sra_attributes = passing_samples %>%
  select(DCLS.Sequencing.ID) %>%
  rename("*sample_name" = DCLS.Sequencing.ID) %>%
  mutate(sample_title = "", bioproject_accession = "PRJNA625551", 
         "*organism" = "Severe acute respiratory syndrome coronavirus 2", 
         strain = paste0("SARS-CoV-2/human/USA/",passing_samples$DCLS.Sequencing.ID,"/",year(passing_samples$Date.of.Collection)),
         isolate = "Missing", "*collected_by" = "Virginia Division of Consolidated Laboratory Services", 
         "*collection_date" = passing_samples$Date.of.Collection, "*geo_loc_name" = "USA: Virginia",
         "*host" = "Homo sapiens", "*host_disease" = "COVID-19", "*isolation_source" = "human", 
         "*lat_lon" = "Missing", purpose_of_sequencing = case_when(passing_samples$Outbreak.ID != "" & passing_samples$Outbreak.ID != "RES989FluSentinel" ~ 'Targeted sequencing',
                                                                  passing_samples$Outbreak.ID == "" ~ 'Baseline surveillance (random sampling)', passing_samples$Outbreak.ID == "RES989FluSentinel" ~ 'Baseline surveillance (random sampling)'),
         genotype = "", host_age = "", host_description = "", host_disease_outcome = "", host_disease_stage = "", host_health_state = "", host_sex = "",
         host_subject_id = "", host_tissue_sampled = "", passage_history = "", pathotype = "", serotype = "", serovar = "", 
         specimen_voucher = "", subgroup = "", subtype = "", description = "")

genbank_headers = passing_samples %>%
  select(DCLS.Sequencing.ID) %>%
  mutate(isolate = paste0(">",DCLS.Sequencing.ID), 
  "fasta header" = paste0(">Seq",row_number(), " [organism=Severe acute respiratory syndrome coronavirus 2]",
                          " [collection_date=",passing_samples$Date.of.Collection,"]"," [keyword=purposeofsampling:",
                          case_when(passing_samples$Outbreak.ID != "" & passing_samples$Outbreak.ID != "RES989FluSentinel" ~ 'targeted_sequencing',
                                    passing_samples$Outbreak.ID == "" ~ 'baselinesurveillance', passing_samples$Outbreak.ID == "RES989FluSentinel" ~ 'baselinesurveillance'),"]"," [host=Homo sapiens]",
                          " [country=USA:Virgina]", " [isolate=",passing_samples$DCLS.Sequencing.ID,"]")) %>%
  select(-1)

gisaid_headers = genbank_headers %>%
  select(isolate) %>%
  mutate(fasta_header = paste0(">hCoV-19/USA/",passing_samples$DCLS.Sequencing.ID,"/",year(passing_samples$Date.of.Collection)))

write_tsv(removed_samples, "removed.txt")
write_tsv(sra_metadata, "sra_meta.tsv")
write_tsv(sra_attributes, "sra_attr.tsv")
write_tsv(genbank_headers, "gen_headers.tsv")
write_tsv(gisaid_headers, "gis_headers.tsv")
write_tsv(gisaid_sub,paste0(sub_name,".tsv"))