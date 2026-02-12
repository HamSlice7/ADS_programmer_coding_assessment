#Implementing sink() with tryCatch() to write to a log file and implement error handling
sink("question_2_logfile.txt", split = TRUE)

cat("===== SCRIPT START =====\n")
cat("Time:", Sys.time(), "\n\n")


tryCatch({
  print("Load required packages")
  
  library(admiral)
  library(dplyr)
  library(pharmaversesdtm)
  library(lubridate)
  library(stringr)
  
  print("Read in SDTM data")
  # Read in input SDTM data
  dm <- pharmaversesdtm::dm #demographics
  vs <- pharmaversesdtm::vs #vital signs
  ex <- pharmaversesdtm::ex #exposure
  ds <- pharmaversesdtm::ds #disposition
  ae <- pharmaversesdtm::ae #adverse events
  
  dm <- convert_blanks_to_na(dm) 
  vs <- convert_blanks_to_na(vs)
  ex <- convert_blanks_to_na(ex)
  ds <- convert_blanks_to_na(ds)
  ae <- convert_blanks_to_na(ae)
  
  print("Initializing ADSL with STDM DM")
  
  # initializing adsl object with dm
  adsl <- dm %>%
    select(-DOMAIN)
  
  print("Deriving AGEGR9 and AGEGR9N")
  
  ##Derive AGEGR9 and AGEGR9N
  
  # Create lookup table
  agegr9_lookup <- exprs(
    ~condition, ~AGEGR9, ~AGEGR9N,
    AGE < 18, "<18", 1,
    between(AGE, 18, 50), "18-50", 2,
    AGE > 50, ">50", 3,
    is.na(AGE), "Missing", 4
  )
  
  adsl <- adsl %>% 
    derive_vars_cat(
      definition = agegr9_lookup
    )
  
  print("Deriving TRTSDTM and TRTSTMF")
  
  ## Derive TRTSDTM and TRTSTMF
  
  #Preprocess ex
  
  ex_ext <- ex %>% 
    derive_vars_dtm(
      dtc = EXSTDTC,
      new_vars_prefix = "EXST"
    ) %>% 
    derive_vars_dtm(
      dtc = EXENDTC,
      new_vars_prefix = "EXEN",
      time_imputation = "last"
    )
  
  #Merging ex into ADSL and deriving TRTSDTM and TRTSTMF
  adsl <- adsl %>% 
    derive_vars_merged(
      dataset_add = ex_ext,
      filter_add = (EXDOSE > 0 | 
                      (EXDOSE == 0 &
                         str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
      new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
      order = exprs(EXSTDTM, EXSEQ),
      mode = "first",
      by_vars = exprs(STUDYID, USUBJID)
    )
  
  print("Deriving ITTFL")
  
  #Adding ITTFL from ARM
  adsl <- adsl %>%
    mutate(
      ITTFL = if_else(!is.na(ARM), "Y", "N")
    )
  
  print("Deriving LSTAVLDT")
  
  ## Deriving LSTAVLDT
  
  #Preprocesses VS to include only valid test results
  vs_last <- vs %>% 
    filter(!is.na(VSSTRESN) & !is.na(VSSTRESC)) %>%
    filter(!is.na(as.Date(VSDTC))) %>% 
    group_by(STUDYID, USUBJID)  %>% 
    summarise(LSTVSDT = max(as.Date(VSDTC), na.rm = TRUE), .groups = "drop")
  
  #Getting last AESTDTC per patient
  ae_last <- ae %>% 
    filter(!is.na(as.Date(AESTDTC))) %>% 
    group_by(STUDYID, USUBJID) %>% 
    summarise(LSTAEDT = max(as.Date(AESTDTC), na.rm = TRUE), .groups = "drop" )
  
  
  #Getting last complete disposition date per patient 
  ds_last <- ds %>% 
    filter(!is.na(as.Date(DSSTDTC))) %>% 
    group_by(STUDYID, USUBJID) %>% 
    summarise(LSTDSDT = max(as.Date(DSSTDTC), na.rm = TRUE), .groups = "drop")
  
  #Getting last date of treatment administration where patient received a valid dose
  trt_last <- adsl %>% 
    filter(!is.na(TRTSDTM)) %>% 
    select(STUDYID, USUBJID, TRTSDTM) %>% 
    mutate(LSTTRTDT = as.Date(TRTSDTM)) %>% 
    select(-TRTSDTM)
  
  #Merge last-date variables into ADSL and take the max 
  
  adsl <- adsl %>% 
    left_join(vs_last, by = c("STUDYID", "USUBJID")) %>%
    left_join(ae_last, by = c("STUDYID", "USUBJID")) %>%
    left_join(ds_last,by = c("STUDYID", "USUBJID")) %>%
    left_join(trt_last, by = c("STUDYID", "USUBJID")) %>% 
    rowwise() %>% 
    mutate(
      LSTAVLDT = max(c(LSTVSDT, LSTAEDT, LSTDSDT, LSTTRTDT), na.rm = TRUE)
    ) %>% 
    ungroup()
  
  print("Saving final ADSL dataset to: ../../output/question_2/adsl.xpt")
  
  write_xpt(adsl, "../../output/question_2/adsl.xpt", version = 5)
  
  
  cat("\nTime:", Sys.time(), "\n")
  cat("===== SCRIPT COMPLETED SUCCESSFULLY =====\n")
  sink()
  
}, error = function(e) {
  cat("\nSTATUS: FAILED\n")
  cat("ERROR MESSAGE:\n")
  cat(e$message, "\n")
  sink()
})

