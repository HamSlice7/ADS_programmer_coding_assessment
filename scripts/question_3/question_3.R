#Implementing sink() with tryCatch() to write to a log file and implement error handling
sink("question_3_logfile.txt", split = TRUE)

cat("===== SCRIPT START =====\n")
cat("Time:", Sys.time(), "\n\n")

tryCatch({
  
  print("Load required packages")
  
  library(pharmaverseadam)
  library(gtsummary)
  library(gt)
  library(dplyr)
  library(ggplot2)
  library(binom)
  
  print("Loadin in ADAE and ADSL data sets")
  #Loading in data sets
  adae <- pharmaverseadam::adae
  adsl <- pharmaverseadam::adsl
  
  ##1.Summary Table using {gtsummary}---------------------
  
  print("Creating a summary table of treatment-emergent adverse events (TEAEs)")
  
  
  #Filter for treatment emergent AEs per subject
  adae_unique_AE_per_subject <- adae %>% 
    filter(TRTEMFL == "Y") %>% 
    distinct(USUBJID, ACTARM, AETERM)
  
  
  #Create a summary table of treatment emergent AE's
  teae_table <- adae_unique_AE_per_subject %>% 
    tbl_summary(
      by = ACTARM,
      include = AETERM,
      statistic = all_categorical() ~ "{n} ({p}%)",
      sort = all_categorical() ~ "frequency"
    ) %>% 
    add_overall(last = TRUE)
  
  teae_table <- teae_table %>% 
    as_gt()
  
  print("Saving summary table to ../../output/question_3/teae_table.docx")
  
  gtsave(teae_table, "../../output/question_3/teae_table.docx")
  
  ##2.1 AE severity distribution by treatment plot-------------
  
  print("Creating AE severity distribution by treatment plot ")
  
  #Generating plot of AE severity by treatment
  ae_plot <- ggplot(adae, aes(x = ACTARM, fill = AESEV)) +
    geom_bar() +
    labs(title = "AE severity distribution by treatment",
         x = "Treatment Arms",
         y = "Count of AEs")
  
  print("Saving plot to ../../output/question_3/AE_plot.png ")
  
  ggsave("../../output/question_3/AE_plot.png", plot = ae_plot, width = 6.5, height = 5)
  
  ##2.2 Top 10 most frequent AEs Plot--------------
  
  print("Creating a plot of the top 10 most frequent AEs")
  
  #Getting total number of subjects from ADSL
  total_subjects <- adsl %>% 
    distinct(USUBJID) %>% 
    nrow()
  
  #Generating new table of the top 10 most frequent AE events with with 95% CI for incidence rates
  ae_pct <- adae %>% 
    distinct(USUBJID, AETERM) %>% 
    count(AETERM, name = "ae_n_subjects") %>% 
    mutate(pct = (ae_n_subjects / total_subjects) * 100) %>% 
    arrange(desc(pct)) %>% 
    head(10) %>% 
    rowwise() %>% 
    mutate(
      ci_lower = list(binom.test(ae_n_subjects, total_subjects)$conf.int)[[1]][1] * 100,
      ci_upper = list(binom.test(ae_n_subjects, total_subjects)$conf.int)[[1]][2] * 100
    ) 
  
  #Generating plot for the top 10 most frequent AE events
  top_10_ae_plot <- ggplot(ae_pct, aes(x = pct, y = reorder(AETERM, pct))) +
    geom_point() +
    geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), orientation = "y", width = 0.2) +
    labs(title = "Top 10 Most Frequent Adverse Events",
         subtitle = "n = 306, 95% Clopper-Pearson CIs",
         y = "",
         x = "Percentage of Patients (%)")
  
  
  print("Saving plot to ../../output/question_3/top_10_AE_plot.png ")
  
  ggsave("../../output/question_3/top_10_AE_plot.png", plot = top_10_ae_plot, width = 6.5, height = 5)
  
  cat("\nTime:", Sys.time(), "\n")
  cat("===== SCRIPT COMPLETED SUCCESSFULLY =====\n")
  sink()
  
},error = function(e) {
  cat("\nSTATUS: FAILED\n")
  cat("ERROR MESSAGE:\n")
  cat(e$message, "\n")
  sink()
  })


 
 


  
