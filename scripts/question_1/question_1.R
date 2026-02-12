#Implementing sink() with tryCatch() to write to a log file and implement error handling
sink("question_1_logfile.txt", split = TRUE)

cat("===== SCRIPT START =====\n")
cat("Time:", Sys.time(), "\n\n")

tryCatch({
  
  print("Load required packages")
  
  library(readr)
  library(sdtm.oak)
  library(pharmaverseraw)
  library(dplyr)
  library(haven)
  
  print("Loading in raw clinical data and DM domain dataset")
  
  # Loading in Disposition Raw dataset
  ds_raw <- pharmaverseraw::ds_raw
  
  # Loading in DM domain to calculate DSSTDY 
  dm <- pharmaversesdtm::dm
  
  print("Generating Oak ID variables")
  
  # Create Oak ID variables
  ds_raw <- generate_oak_id_vars(
    ds_raw,
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )
  
  print("Loading in study control terms")
  
  # Loading in study control terms from Github repo
  study_ct <- read_csv("https://raw.githubusercontent.com/pharmaverse/examples/refs/heads/main/metadata/sdtm_ct.csv")
  
  print("Mapping the topic variable")
  
  #Map the topic variable
  ds <- assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  )
  
  print("Mapping the rest of the variables")
  
  #Mapping the rest of the variables for the SDTM DS Domain 
  ds <- ds %>%
    #Create STUDYID, DOMAIN, and USUBJID variables
    mutate(
      STUDYID = ds_raw$STUDY,
      DOMAIN = "DS",
      USUBJID = paste0("01-", ds_raw$PATNUM)
    )%>% 
    
    #Create DSSEQ variable
    group_by(USUBJID) %>%
    mutate(DSSEQ = row_number()) %>%
    ungroup() %>% 
    
    #Create DSDECOD variable from study_ct
    assign_ct(
      raw_dat = ds_raw,
      raw_var = "IT.DSDECOD",
      tgt_var = "DSDECOD",
      ct_spec = study_ct,
      ct_clst = "C66727",
      id_vars = oak_id_vars()
    ) %>% 
    
    #Create DSCAT variable
    mutate(
      DSCAT = case_when(
        # Protocol Milestones - key study events
        DSDECOD %in% c("RANDOMIZED") ~ "PROTOCOL MILESTONE",
        
        # Disposition Events - study completion/discontinuation
        DSDECOD %in% c("COMPLETED", 
                       "ADVERSE EVENT",
                       "DEATH",
                       "LACK OF EFFICACY",
                       "LOST TO FOLLOW-UP",
                       "PHYSICIAN DECISION",
                       "PROTOCOL VIOLATION",
                       "SCREEN FAILURE",
                       "STUDY TERMINATED BY SPONSOR",
                       "WITHDRAWAL BY SUBJECT") ~ "DISPOSITION EVENT",
        
        # Default to Other Event if none of above
        !is.na(DSDECOD) ~ "OTHER EVENT",
        
        # Keep NA if DSDECOD is NA
        TRUE ~ NA_character_
      )
    ) %>% 
    
    #Create DSDTC from DSDTCOL and DSTMCOL in ds_raw
    assign_datetime(
      raw_dat = ds_raw,
      raw_var = c("DSDTCOL","DSTMCOL"),
      tgt_var = "DSDTC",
      raw_fmt = c("m-d-y", "H:M"),
      id_vars = oak_id_vars()
    ) %>% 
    
    #Create DSSTDTC 
    assign_datetime(
      raw_dat = ds_raw,
      raw_var = "IT.DSSTDAT",
      tgt_var = "DSSTDTC",
      raw_fmt = c("m-d-y"),
      id_vars = oak_id_vars()
      
    ) %>%
    derive_study_day(
      sdtm_in = .,
      dm_domain = dm,
      tgdt = "DSSTDTC",
      refdt = "RFXSTDTC",
      study_day_var = "DSSTDY"
    ) %>% 
    
    # Map VISIT from INSTANCE using assign_ct
    assign_ct(
      raw_dat = ds_raw,
      raw_var = "INSTANCE",
      tgt_var = "VISIT",
      ct_spec = study_ct,
      ct_clst = "VISIT",
      id_vars = oak_id_vars()
    ) %>%
    # Map VISITNUM from INSTANCE using assign_ct
    assign_ct(
      raw_dat = ds_raw,
      raw_var = "INSTANCE",
      tgt_var = "VISITNUM",
      ct_spec = study_ct,
      ct_clst = "VISITNUM",
      id_vars = oak_id_vars()
    ) %>% 
    select(
      "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", 
      "VISITNUM", "VISIT","DSDTC","DSSTDTC", "DSSTDY"
    )
  
  print("Saving final SDTM DS Domain dataset to: ../../output/question_1/ds.xpt")
  
  #Export SDTM DS Domain to output folder.
  write_xpt(ds, "../../output/question_1/ds.xpt", version = 5)
  
  cat("\nTime:", Sys.time(), "\n")
  cat("===== SCRIPT COMPLETED SUCCESSFULLY =====\n")
  sink()
  
}, error = function(e) {
  cat("\nSTATUS: FAILED\n")
  cat("ERROR MESSAGE:\n")
  cat(e$message, "\n")
  sink()
  
})






