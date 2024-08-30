organize_data <- function(flag, data_FOURD, data_CARTBIND) {
  # select variables corresponding to different time points, baseline, t30, followup 1, etc.,
  # and rename these variables to meet the following model fitness code requirements
  # input variables
  # flag: the labels for different time points, 1: baseline, 2: t30, 3: followup 1, 4: followup 2, 5: followup 3, 6: delta (t30 - baseline)
  # data_FOURD: raw FOURD data
  # data_CARTBIND: raw CARTBIND data
  #
  # Written by Xiao Chen 240603
  # Modified by Xiao Chen 240604
  # also extract madrs item scores
  # Modified by Xiao Chen 240729
  # Use the screen data from the FOURD dataset, for its lower missing rate
  # Modified by Xiao Chen 240819
  # Check if the baseline data is NA, if yes, carry the screen data over
  # chenxiaophd@gmail.com
  
  # baseline time points
  if (flag == 1){
    # rename variables in FOURD and CARTBIND into same variable names, and select the
    # interested variables
    # use FOURD baseline data
    data_FOURD_renamed <- data_FOURD %>%
      select(
        subj_id, hrsd_total_b, total_madrs_b,
        hrsd1s_b, hrsd2s_b, hrsd3s_b, hrsd4s_b, hrsd5s_b, hrsd6s_b, hrsd7s_b, hrsd8s_b, hrsd9s_b, hrsd10s_b, hrsd11s_b, hrsd12s_b,
        hrsd13s_b, hrsd14s_b, hrsd15s_b, hrsd16s_b, hrsd17s_b,
        madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno,current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_b, madrs_total = total_madrs_b,
        age = AGE, sex = SEX, educ = YRS_EDUC, benzo = BENZO, antidep = ANTIDEP,
        antidep.combintn = ANTIDEP_COMB,
        hrsd1s = hrsd1s_b, hrsd2s = hrsd2s_b, hrsd3s = hrsd3s_b, hrsd4s = hrsd4s_b,
        hrsd5s = hrsd5s_b, hrsd6s = hrsd6s_b, hrsd7s = hrsd7s_b, hrsd8s = hrsd8s_b,
        hrsd9s = hrsd9s_b, hrsd10s = hrsd10s_b, hrsd11s = hrsd11s_b, hrsd12s = hrsd12s_b,
        hrsd13s = hrsd13s_b, hrsd14s = hrsd14s_b, hrsd15s = hrsd15s_b, hrsd16s = hrsd16s_b, hrsd17s = hrsd17s_b,
        madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b,
        madrs5 = madrs5_b, madrs6 = madrs6_b, madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
      )
    
    # Use the FOURD screen data
    # data_FOURD_renamed <- data_FOURD %>% 
    #   select(
    #     subj_id, hrsd_total_s, total_madrs_b,
    #     hrsd1s_s, hrsd2s_s, hrsd3s_s, hrsd4s_s, hrsd5s_s, hrsd6s_s, hrsd7s_s, hrsd8s_s, hrsd9s_s, hrsd10s_s, hrsd11s_s, hrsd12s_s, 
    #     hrsd13s_s, hrsd14s_s, hrsd15s_s, hrsd16s_s, hrsd17s_s,
    #     madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
    #     AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno,current_psyther_yesno
    #   ) %>% rename(
    #     hrsd_total = hrsd_total_s, madrs_total = total_madrs_b,
    #     age = AGE, sex = SEX, educ = YRS_EDUC, benzo = BENZO, antidep = ANTIDEP,
    #     antidep.combintn = ANTIDEP_COMB,
    #     hrsd1s = hrsd1s_s, hrsd2s = hrsd2s_s, hrsd3s = hrsd3s_s, hrsd4s = hrsd4s_s, 
    #     hrsd5s = hrsd5s_s, hrsd6s = hrsd6s_s, hrsd7s = hrsd7s_s, hrsd8s = hrsd8s_s, 
    #     hrsd9s = hrsd9s_s, hrsd10s = hrsd10s_s, hrsd11s = hrsd11s_s, hrsd12s = hrsd12s_s, 
    #     hrsd13s = hrsd13s_s, hrsd14s = hrsd14s_s, hrsd15s = hrsd15s_s, hrsd16s = hrsd16s_s, hrsd17s = hrsd17s_s,
    #     madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b, 
    #     madrs5 = madrs5_b, madrs6 = madrs6_b, madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
    #   )
    
    # This subject's baseline data seems to be missing, after imputed NAs, could be problematic
    data_FOURD_renamed$hrsd_total[data_FOURD_renamed$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_renamed$hrsd4s[data_FOURD_renamed$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_renamed$hrsd8s[data_FOURD_renamed$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_renamed$hrsd9s[data_FOURD_renamed$subj_id == "CAMH_LL_155"] <- NA
    
    data_CARTBIND_renamed <- data_CARTBIND %>% 
      select(
        subj_id, hrsd_total_b2, total_madrs_b,
        hrsd1s_b2, hrsd2s_b2, hrsd3s_b2, hrsd4s_b2, hrsd5s_b2, hrsd6s_b2, hrsd7s_b2, 
        hrsd8s_b2, hrsd9s_b2, hrsd10s_b2, hrsd11s_b2, hrsd12s_b2, hrsd13s_b2, hrsd14s_b2,  hrsd15s_b2, hrsd16s_b2, hrsd17s_b2,
        madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
        age,sex,educ,benzo,antidep,antidep.combintn,past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_b2, madrs_total = total_madrs_b,
        hrsd1s = hrsd1s_b2, hrsd2s = hrsd2s_b2, hrsd3s = hrsd3s_b2, hrsd4s = hrsd4s_b2, hrsd5s = hrsd5s_b2, hrsd6s = hrsd6s_b2, 
        hrsd7s = hrsd7s_b2, hrsd8s = hrsd8s_b2, hrsd9s = hrsd9s_b2, hrsd10s = hrsd10s_b2, hrsd11s = hrsd11s_b2, 
        hrsd12s = hrsd12s_b2, hrsd13s = hrsd13s_b2, hrsd14s = hrsd14s_b2, hrsd15s = hrsd15s_b2, hrsd16s = hrsd16s_b2, hrsd17s = hrsd17s_b2,
        madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b, madrs5 = madrs5_b, madrs6 = madrs6_b, 
        madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
      )
  }
  
  
  # T30 time point
  if (flag == 2){
    # rename variables in FOURD and CARTBIND into same variable names, and select the interested variables
    data_FOURD_renamed <- data_FOURD %>% 
      select(
        subj_id, hrsd_total_t30, total_madrs_t30,
        hrsd1s_t30, hrsd2s_t30, hrsd3s_t30, hrsd4s_t30, hrsd5s_t30, hrsd6s_t30, hrsd7s_t30, hrsd8s_t30, hrsd9s_t30, hrsd10s_t30, 
        hrsd11s_t30, hrsd12s_t30, hrsd13s_t30, hrsd14s_t30, hrsd15s_t30, hrsd16s_t30, hrsd17s_t30,
        madrs1_t30, madrs2_t30, madrs3_t30, madrs4_t30, madrs5_t30, madrs6_t30, madrs7_t30, madrs8_t30, madrs9_t30, madrs10_t30, 
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>% 
      rename(
        age = AGE, sex = SEX, educ = YRS_EDUC,
        benzo = BENZO, antidep = ANTIDEP, antidep.combintn = ANTIDEP_COMB,
        hrsd_total = hrsd_total_t30, madrs_total = total_madrs_t30,
        hrsd1s = hrsd1s_t30, hrsd2s = hrsd2s_t30, hrsd3s = hrsd3s_t30, hrsd4s = hrsd4s_t30, hrsd5s = hrsd5s_t30, hrsd6s = hrsd6s_t30, 
        hrsd7s = hrsd7s_t30, hrsd8s = hrsd8s_t30, hrsd9s = hrsd9s_t30, hrsd10s = hrsd10s_t30, hrsd11s = hrsd11s_t30, hrsd12s = hrsd12s_t30, 
        hrsd13s = hrsd13s_t30, hrsd14s = hrsd14s_t30, hrsd15s = hrsd15s_t30, hrsd16s = hrsd16s_t30, hrsd17s = hrsd17s_t30,
        madrs1 = madrs1_t30, madrs2 = madrs2_t30, madrs3 = madrs3_t30, madrs4 = madrs4_t30, madrs5 = madrs5_t30, madrs6 = madrs6_t30, 
        madrs7 = madrs7_t30, madrs8 = madrs8_t30, madrs9 = madrs9_t30, madrs10 = madrs10_t30
      ) 
    
    
    data_CARTBIND_renamed <- data_CARTBIND %>%
      select(
        subj_id, hrsd_total_t30, total_madrs_t30,
        hrsd1s_t30, hrsd2s_t30, hrsd3s_t30, hrsd4s_t30, hrsd5s_t30, hrsd6s_t30, hrsd7s_t30, hrsd8s_t30, hrsd9s_t30, 
        hrsd10s_t30, hrsd11s_t30, hrsd12s_t30, hrsd13s_t30, hrsd14s_t30, hrsd15s_t30, hrsd16s_t30, hrsd17s_t30,
        madrs1_t30, madrs2_t30, madrs3_t30, madrs4_t30, madrs5_t30, madrs6_t30, madrs7_t30, madrs8_t30, madrs9_t30, madrs10_t30,
        age,sex,educ,benzo,antidep,antidep.combintn,past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_t30 , madrs_total = total_madrs_t30,
        hrsd1s = hrsd1s_t30, hrsd2s = hrsd2s_t30, hrsd3s = hrsd3s_t30, hrsd4s = hrsd4s_t30, hrsd5s = hrsd5s_t30, hrsd6s = hrsd6s_t30, 
        hrsd7s = hrsd7s_t30, hrsd8s = hrsd8s_t30, hrsd9s = hrsd9s_t30, hrsd10s = hrsd10s_t30, hrsd11s = hrsd11s_t30, hrsd12s = hrsd12s_t30, 
        hrsd13s = hrsd13s_t30, hrsd14s = hrsd14s_t30, hrsd15s = hrsd15s_t30, hrsd16s = hrsd16s_t30, hrsd17s = hrsd17s_t30,
        madrs1 = madrs1_t30, madrs2 = madrs2_t30, madrs3 = madrs3_t30, madrs4 = madrs4_t30, madrs5 = madrs5_t30, madrs6 = madrs6_t30, 
        madrs7 = madrs7_t30, madrs8 = madrs8_t30, madrs9 = madrs9_t30, madrs10 = madrs10_t30
      ) 
  }
  
  
  # follow up 1
  if (flag == 3){
    data_FOURD_renamed <- data_FOURD %>% 
      select(
        subj_id, hrsd_total_f1, total_madrs_f1,
        hrsd1s_f1, hrsd2s_f1, hrsd3s_f1, hrsd4s_f1, hrsd5s_f1, hrsd6s_f1, hrsd7s_f1, hrsd8s_f1, hrsd9s_f1, hrsd10s_f1, hrsd11s_f1, hrsd12s_f1, 
        hrsd13s_f1, hrsd14s_f1, hrsd15s_f1, hrsd16s_f1, hrsd17s_f1,
        madrs1_f1, madrs2_f1, madrs3_f1, madrs4_f1, madrs5_f1, madrs6_f1, madrs7_f1, madrs8_f1, madrs9_f1, madrs10_f1,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        age = AGE, sex = SEX, educ = YRS_EDUC, benzo = BENZO,
        antidep = ANTIDEP, antidep.combintn = ANTIDEP_COMB,
        hrsd_total = hrsd_total_f1, madrs_total = total_madrs_f1,
        hrsd1s = hrsd1s_f1, hrsd2s = hrsd2s_f1, hrsd3s = hrsd3s_f1, hrsd4s = hrsd4s_f1, hrsd5s = hrsd5s_f1, hrsd6s = hrsd6s_f1, 
        hrsd7s = hrsd7s_f1, hrsd8s = hrsd8s_f1, hrsd9s = hrsd9s_f1, hrsd10s = hrsd10s_f1, hrsd11s = hrsd11s_f1, hrsd12s = hrsd12s_f1, 
        hrsd13s = hrsd13s_f1, hrsd14s = hrsd14s_f1, hrsd15s = hrsd15s_f1, hrsd16s = hrsd16s_f1, hrsd17s = hrsd17s_f1,
        madrs1 = madrs1_f1, madrs2 = madrs2_f1, madrs3 = madrs3_f1, madrs4 = madrs4_f1, madrs5 = madrs5_f1, madrs6 = madrs6_f1, 
        madrs7 = madrs7_f1, madrs8 = madrs8_f1, madrs9 = madrs9_f1, madrs10 = madrs10_f1
      ) 
    
    data_CARTBIND_renamed <- data_CARTBIND %>%
      select(
        subj_id, hrsd_total_f1, total_madrs_f1,
        hrsd1s_f1, hrsd2s_f1, hrsd3s_f1, hrsd4s_f1, hrsd5s_f1, hrsd6s_f1, hrsd7s_f1, hrsd8s_f1, hrsd9s_f1, hrsd10s_f1, hrsd11s_f1, hrsd12s_f1, 
        hrsd13s_f1, hrsd14s_f1, hrsd15s_f1, hrsd16s_f1, hrsd17s_f1,
        madrs1_f1, madrs2_f1, madrs3_f1, madrs4_f1, madrs5_f1, madrs6_f1, madrs7_f1, madrs8_f1, madrs9_f1, madrs10_f1,
        age, sex, educ, benzo, antidep, antidep.combintn, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_f1,
        madrs_total = total_madrs_f1,
        hrsd1s = hrsd1s_f1, hrsd2s = hrsd2s_f1, hrsd3s = hrsd3s_f1, hrsd4s = hrsd4s_f1, hrsd5s = hrsd5s_f1, hrsd6s = hrsd6s_f1, 
        hrsd7s = hrsd7s_f1, hrsd8s = hrsd8s_f1, hrsd9s = hrsd9s_f1, hrsd10s = hrsd10s_f1, hrsd11s = hrsd11s_f1, 
        hrsd12s = hrsd12s_f1, hrsd13s = hrsd13s_f1, hrsd14s = hrsd14s_f1, hrsd15s = hrsd15s_f1, hrsd16s = hrsd16s_f1, hrsd17s = hrsd17s_f1,
        madrs1 = madrs1_f1, madrs2 = madrs2_f1, madrs3 = madrs3_f1, madrs4 = madrs4_f1, madrs5 = madrs5_f1, madrs6 = madrs6_f1, 
        madrs7 = madrs7_f1, madrs8 = madrs8_f1, madrs9 = madrs9_f1, madrs10 = madrs10_f1
      ) 
  }
  
  
  # follow up 2
  if (flag == 4){
    data_FOURD_renamed <- data_FOURD %>% 
      select(
        subj_id, hrsd_total_f2, total_madrs_f2,
        hrsd1s_f2, hrsd2s_f2, hrsd3s_f2, hrsd4s_f2, hrsd5s_f2, hrsd6s_f2, hrsd7s_f2, hrsd8s_f2, hrsd9s_f2, hrsd10s_f2, hrsd11s_f2, hrsd12s_f2, 
        hrsd13s_f2, hrsd14s_f2, hrsd15s_f2, hrsd16s_f2, hrsd17s_f2,
        madrs1_f2, madrs2_f2, madrs3_f2, madrs4_f2, madrs5_f2, madrs6_f2, madrs7_f2, madrs8_f2, madrs9_f2, madrs10_f2,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>% 
      rename(
        age = AGE, sex = SEX, educ = YRS_EDUC,
        benzo = BENZO, antidep = ANTIDEP,
        antidep.combintn = ANTIDEP_COMB,
        hrsd_total = hrsd_total_f2, madrs_total = total_madrs_f2,
        hrsd1s = hrsd1s_f2, hrsd2s = hrsd2s_f2, hrsd3s = hrsd3s_f2, hrsd4s = hrsd4s_f2, hrsd5s = hrsd5s_f2, hrsd6s = hrsd6s_f2, 
        hrsd7s = hrsd7s_f2, hrsd8s = hrsd8s_f2, hrsd9s = hrsd9s_f2, hrsd10s = hrsd10s_f2, hrsd11s = hrsd11s_f2, hrsd12s = hrsd12s_f2, 
        hrsd13s = hrsd13s_f2, hrsd14s = hrsd14s_f2, hrsd15s = hrsd15s_f2, hrsd16s = hrsd16s_f2, hrsd17s = hrsd17s_f2,
        madrs1 = madrs1_f2, madrs2 = madrs2_f2, madrs3 = madrs3_f2, madrs4 = madrs4_f2, madrs5 = madrs5_f2, madrs6 = madrs6_f2, 
        madrs7 = madrs7_f2, madrs8 = madrs8_f2, madrs9 = madrs9_f2, madrs10 = madrs10_f2
      ) 
    
    data_CARTBIND_renamed <- data_CARTBIND %>%
      select(
        subj_id, hrsd_total_f2, total_madrs_f2,
        hrsd1s_f2, hrsd2s_f2, hrsd3s_f2, hrsd4s_f2, hrsd5s_f2, hrsd6s_f2, hrsd7s_f2, 
        hrsd8s_f2, hrsd9s_f2, hrsd10s_f2, hrsd11s_f2, hrsd12s_f2, 
        hrsd13s_f2, hrsd14s_f2, hrsd15s_f2, hrsd16s_f2, hrsd17s_f2,
        madrs1_f2, madrs2_f2, madrs3_f2, madrs4_f2, madrs5_f2, madrs6_f2, madrs7_f2, madrs8_f2, madrs9_f2, madrs10_f2,
        age, sex, educ, benzo, antidep, antidep.combintn, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_f2, madrs_total = total_madrs_f2,
        hrsd1s = hrsd1s_f2, hrsd2s = hrsd2s_f2, hrsd3s = hrsd3s_f2, hrsd4s = hrsd4s_f2, hrsd5s = hrsd5s_f2, hrsd6s = hrsd6s_f2, 
        hrsd7s = hrsd7s_f2, hrsd8s = hrsd8s_f2, hrsd9s = hrsd9s_f2, hrsd10s = hrsd10s_f2, hrsd11s = hrsd11s_f2, hrsd12s = hrsd12s_f2, 
        hrsd13s = hrsd13s_f2, hrsd14s = hrsd14s_f2, hrsd15s = hrsd15s_f2, hrsd16s = hrsd16s_f2, hrsd17s = hrsd17s_f2,
        madrs1 = madrs1_f2, madrs2 = madrs2_f2, madrs3 = madrs3_f2, madrs4 = madrs4_f2, madrs5 = madrs5_f2, madrs6 = madrs6_f2, 
        madrs7 = madrs7_f2, madrs8 = madrs8_f2, madrs9 = madrs9_f2, madrs10 = madrs10_f2
      ) 
  }
  
  
  # follow up 3
  if (flag == 5){
    data_FOURD_renamed <- data_FOURD %>% 
      select(
        subj_id,
        hrsd_total_f3,
        total_madrs_f3,
        hrsd1s_f3, hrsd2s_f3, hrsd3s_f3, hrsd4s_f3, hrsd5s_f3, hrsd6s_f3, hrsd7s_f3, hrsd8s_f3, 
        hrsd9s_f3, hrsd10s_f3, hrsd11s_f3, hrsd12s_f3, 
        hrsd13s_f3, hrsd14s_f3, hrsd15s_f3, hrsd16s_f3, hrsd17s_f3,
        madrs1_f3, madrs2_f3, madrs3_f3, madrs4_f3, madrs5_f3, madrs6_f3, madrs7_f3, madrs8_f3, madrs9_f3, madrs10_f3,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        age = AGE, sex = SEX, educ = YRS_EDUC,
        benzo = BENZO, antidep = ANTIDEP,
        antidep.combintn = ANTIDEP_COMB,
        hrsd_total = hrsd_total_f3, madrs_total = total_madrs_f3,
        hrsd1s = hrsd1s_f3, hrsd2s = hrsd2s_f3, hrsd3s = hrsd3s_f3, hrsd4s = hrsd4s_f3, hrsd5s = hrsd5s_f3, hrsd6s = hrsd6s_f3, 
        hrsd7s = hrsd7s_f3, hrsd8s = hrsd8s_f3, hrsd9s = hrsd9s_f3, hrsd10s = hrsd10s_f3, hrsd11s = hrsd11s_f3, hrsd12s = hrsd12s_f3, 
        hrsd13s = hrsd13s_f3, hrsd14s = hrsd14s_f3, hrsd15s = hrsd15s_f3, hrsd16s = hrsd16s_f3, hrsd17s = hrsd17s_f3,
        madrs1 = madrs1_f3, madrs2 = madrs2_f3, madrs3 = madrs3_f3, madrs4 = madrs4_f3, madrs5 = madrs5_f3, madrs6 = madrs6_f3, 
        madrs7 = madrs7_f3, madrs8 = madrs8_f3, madrs9 = madrs9_f3, madrs10 = madrs10_f3
      ) 
    
    data_CARTBIND_renamed <- data_CARTBIND %>%
      select(
        subj_id, hrsd_total_f3, total_madrs_f3,
        hrsd1s_f3, hrsd2s_f3, hrsd3s_f3, hrsd4s_f3, hrsd5s_f3, hrsd6s_f3, hrsd7s_f3, hrsd8s_f3, 
        hrsd9s_f3, hrsd10s_f3, hrsd11s_f3, hrsd12s_f3, 
        hrsd13s_f3, hrsd14s_f3, hrsd15s_f3, hrsd16s_f3, hrsd17s_f3,
        madrs1_f3, madrs2_f3, madrs3_f3, madrs4_f3, madrs5_f3, madrs6_f3, madrs7_f3, madrs8_f3, madrs9_f3, madrs10_f3,
        age, sex, educ, benzo, antidep, antidep.combintn, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_f3, madrs_total = total_madrs_f3,
        hrsd1s = hrsd1s_f3, hrsd2s = hrsd2s_f3, hrsd3s = hrsd3s_f3, hrsd4s = hrsd4s_f3, hrsd5s = hrsd5s_f3, hrsd6s = hrsd6s_f3, 
        hrsd7s = hrsd7s_f3, hrsd8s = hrsd8s_f3, hrsd9s = hrsd9s_f3, hrsd10s = hrsd10s_f3, hrsd11s = hrsd11s_f3, hrsd12s = hrsd12s_f3, 
        hrsd13s = hrsd13s_f3, hrsd14s = hrsd14s_f3, hrsd15s = hrsd15s_f3, hrsd16s = hrsd16s_f3, hrsd17s = hrsd17s_f3,
        madrs1 = madrs1_f3, madrs2 = madrs2_f3, madrs3 = madrs3_f3, madrs4 = madrs4_f3, madrs5 = madrs5_f3, madrs6 = madrs6_f3, 
        madrs7 = madrs7_f3, madrs8 = madrs8_f3, madrs9 = madrs9_f3, madrs10 = madrs10_f3
      ) 
  }
  
  
  # delta data
  if (flag == 6){
    # use baseline data, high missing rate
    data_FOURD_baseline <- data_FOURD %>%
      select(
        subj_id, hrsd_total_b, total_madrs_b,
        hrsd1s_b, hrsd2s_b, hrsd3s_b, hrsd4s_b, hrsd5s_b, hrsd6s_b, hrsd7s_b, hrsd8s_b, hrsd9s_b, hrsd10s_b, hrsd11s_b, hrsd12s_b,
        hrsd13s_b, hrsd14s_b, hrsd15s_b, hrsd16s_b, hrsd17s_b,
        madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>%
      rename(
        hrsd_total = hrsd_total_b, madrs_total = total_madrs_b,
        age = AGE, sex = SEX, educ = YRS_EDUC,
        benzo = BENZO, antidep = ANTIDEP,
        antidep.combintn = ANTIDEP_COMB,
        hrsd1s = hrsd1s_b, hrsd2s = hrsd2s_b, hrsd3s = hrsd3s_b, hrsd4s = hrsd4s_b, hrsd5s = hrsd5s_b, hrsd6s = hrsd6s_b,
        hrsd7s = hrsd7s_b, hrsd8s = hrsd8s_b, hrsd9s = hrsd9s_b, hrsd10s = hrsd10s_b, hrsd11s = hrsd11s_b, hrsd12s = hrsd12s_b,
        hrsd13s = hrsd13s_b, hrsd14s = hrsd14s_b, hrsd15s = hrsd15s_b, hrsd16s = hrsd16s_b, hrsd17s = hrsd17s_b,
        madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b, madrs5 = madrs5_b, madrs6 = madrs6_b,
        madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
      )

    # # Use the screen data
    # data_FOURD_baseline <- data_FOURD %>% 
    #   select(
    #     subj_id, hrsd_total_s, total_madrs_b,
    #     hrsd1s_s, hrsd2s_s, hrsd3s_s, hrsd4s_s, hrsd5s_s, hrsd6s_s, hrsd7s_s, hrsd8s_s, hrsd9s_s, hrsd10s_s, hrsd11s_s, hrsd12s_s, 
    #     hrsd13s_s, hrsd14s_s, hrsd15s_s, hrsd16s_s, hrsd17s_s,
    #     madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
    #     AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
    #   ) %>% 
    #   rename(
    #     hrsd_total = hrsd_total_s, madrs_total = total_madrs_b,
    #     age = AGE, sex = SEX, educ = YRS_EDUC,
    #     benzo = BENZO, antidep = ANTIDEP,
    #     antidep.combintn = ANTIDEP_COMB,
    #     hrsd1s = hrsd1s_s, hrsd2s = hrsd2s_s, hrsd3s = hrsd3s_s, hrsd4s = hrsd4s_s, hrsd5s = hrsd5s_s, hrsd6s = hrsd6s_s, 
    #     hrsd7s = hrsd7s_s, hrsd8s = hrsd8s_s, hrsd9s = hrsd9s_s, hrsd10s = hrsd10s_s, hrsd11s = hrsd11s_s, hrsd12s = hrsd12s_s, 
    #     hrsd13s = hrsd13s_s, hrsd14s = hrsd14s_s, hrsd15s = hrsd15s_s, hrsd16s = hrsd16s_s, hrsd17s = hrsd17s_s,
    #     madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b, madrs5 = madrs5_b, madrs6 = madrs6_b, 
    #     madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
    #   )
    
    # This subject's baseline data seems to be missing, after imputed NAs, could be problematic
    data_FOURD_baseline$hrsd_total[data_FOURD_baseline$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_baseline$hrsd4s[data_FOURD_baseline$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_baseline$hrsd8s[data_FOURD_baseline$subj_id == "CAMH_LL_155"] <- NA
    data_FOURD_baseline$hrsd9s[data_FOURD_baseline$subj_id == "CAMH_LL_155"] <- NA
    
    data_CARTBIND_baseline <- data_CARTBIND %>% 
      select(
        subj_id,
        hrsd_total_b2, total_madrs_b,
        hrsd1s_b2, hrsd2s_b2, hrsd3s_b2, hrsd4s_b2, hrsd5s_b2, hrsd6s_b2, hrsd7s_b2, 
        hrsd8s_b2, hrsd9s_b2, hrsd10s_b2, hrsd11s_b2, hrsd12s_b2, hrsd13s_b2, hrsd14s_b2,  hrsd15s_b2, hrsd16s_b2, hrsd17s_b2, 
        madrs1_b, madrs2_b, madrs3_b, madrs4_b, madrs5_b, madrs6_b, madrs7_b, madrs8_b, madrs9_b, madrs10_b,
        age,sex,educ,benzo,antidep,antidep.combintn,past_ect_yesno, current_psyther_yesno
      ) %>% 
      rename(
        hrsd_total = hrsd_total_b2, madrs_total = total_madrs_b,
        hrsd1s = hrsd1s_b2, hrsd2s = hrsd2s_b2, hrsd3s = hrsd3s_b2, hrsd4s = hrsd4s_b2, hrsd5s = hrsd5s_b2, hrsd6s = hrsd6s_b2, 
        hrsd7s = hrsd7s_b2, hrsd8s = hrsd8s_b2, hrsd9s = hrsd9s_b2, hrsd10s = hrsd10s_b2, hrsd11s = hrsd11s_b2, 
        hrsd12s = hrsd12s_b2, hrsd13s = hrsd13s_b2, hrsd14s = hrsd14s_b2, hrsd15s = hrsd15s_b2, hrsd16s = hrsd16s_b2, hrsd17s = hrsd17s_b2,
        madrs1 = madrs1_b, madrs2 = madrs2_b, madrs3 = madrs3_b, madrs4 = madrs4_b, madrs5 = madrs5_b, madrs6 = madrs6_b, 
        madrs7 = madrs7_b, madrs8 = madrs8_b, madrs9 = madrs9_b, madrs10 = madrs10_b
      )
    
    data_FOURD_t30 <- data_FOURD %>% 
      select(
        subj_id, hrsd_total_t30, total_madrs_t30,
        hrsd1s_t30, hrsd2s_t30, hrsd3s_t30, hrsd4s_t30, hrsd5s_t30, hrsd6s_t30, hrsd7s_t30, hrsd8s_t30, hrsd9s_t30, hrsd10s_t30, hrsd11s_t30, hrsd12s_t30, 
        hrsd13s_t30, hrsd14s_t30, hrsd15s_t30, hrsd16s_t30, hrsd17s_t30,
        madrs1_t30, madrs2_t30, madrs3_t30, madrs4_t30, madrs5_t30, madrs6_t30, madrs7_t30, madrs8_t30, madrs9_t30, madrs10_t30,
        AGE, SEX, YRS_EDUC, BENZO, ANTIDEP, ANTIDEP_COMB, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        age = AGE, sex = SEX, educ = YRS_EDUC,
        benzo = BENZO, antidep = ANTIDEP, antidep.combintn = ANTIDEP_COMB,
        hrsd_total = hrsd_total_t30, madrs_total = total_madrs_t30,
        hrsd1s = hrsd1s_t30, hrsd2s = hrsd2s_t30, hrsd3s = hrsd3s_t30, hrsd4s = hrsd4s_t30, hrsd5s = hrsd5s_t30, hrsd6s = hrsd6s_t30, 
        hrsd7s = hrsd7s_t30, hrsd8s = hrsd8s_t30, hrsd9s = hrsd9s_t30, hrsd10s = hrsd10s_t30, hrsd11s = hrsd11s_t30, hrsd12s = hrsd12s_t30, 
        hrsd13s = hrsd13s_t30, hrsd14s = hrsd14s_t30, hrsd15s = hrsd15s_t30, hrsd16s = hrsd16s_t30, hrsd17s = hrsd17s_t30,
        madrs1 = madrs1_t30, madrs2 = madrs2_t30, madrs3 = madrs3_t30, madrs4 = madrs4_t30, madrs5 = madrs5_t30, madrs6 = madrs6_t30, 
        madrs7 = madrs7_t30, madrs8 = madrs8_t30, madrs9 = madrs9_t30, madrs10 = madrs10_t30
      ) 
    
    data_CARTBIND_t30 <- data_CARTBIND %>%
      select(
        subj_id, hrsd_total_t30, total_madrs_t30,
        hrsd1s_t30, hrsd2s_t30, hrsd3s_t30, hrsd4s_t30, hrsd5s_t30, hrsd6s_t30, hrsd7s_t30, hrsd8s_t30, hrsd9s_t30, hrsd10s_t30, 
        hrsd11s_t30, hrsd12s_t30, hrsd13s_t30, hrsd14s_t30, hrsd15s_t30, hrsd16s_t30, hrsd17s_t30,
        madrs1_t30, madrs2_t30, madrs3_t30, madrs4_t30, madrs5_t30, madrs6_t30, madrs7_t30, madrs8_t30, madrs9_t30, madrs10_t30,
        age, sex, educ, benzo, antidep, antidep.combintn, past_ect_yesno, current_psyther_yesno
      ) %>% rename(
        hrsd_total = hrsd_total_t30, madrs_total = total_madrs_t30,
        hrsd1s = hrsd1s_t30, hrsd2s = hrsd2s_t30, hrsd3s = hrsd3s_t30, hrsd4s = hrsd4s_t30, hrsd5s = hrsd5s_t30, hrsd6s = hrsd6s_t30, 
        hrsd7s = hrsd7s_t30, hrsd8s = hrsd8s_t30, hrsd9s = hrsd9s_t30, hrsd10s = hrsd10s_t30, hrsd11s = hrsd11s_t30, hrsd12s = hrsd12s_t30, 
        hrsd13s = hrsd13s_t30, hrsd14s = hrsd14s_t30, hrsd15s = hrsd15s_t30, hrsd16s = hrsd16s_t30, hrsd17s = hrsd17s_t30,
        madrs1 = madrs1_t30, madrs2 = madrs2_t30, madrs3 = madrs3_t30, madrs4 = madrs4_t30, madrs5 = madrs5_t30, madrs6 = madrs6_t30, 
        madrs7 = madrs7_t30, madrs8 = madrs8_t30, madrs9 = madrs9_t30, madrs10 = madrs10_t30
      ) 
    
    data_FOURD_renamed = data.frame(
      subj_id = data_FOURD_baseline$subj_id,
      hrsd_total = data_FOURD_baseline$hrsd_total - data_FOURD_t30$hrsd_total,
      madrs_total = data_FOURD_baseline$madrs_total - data_FOURD_t30$madrs_total,
      age = data_FOURD_baseline$age,
      sex = data_FOURD_baseline$sex,
      educ = data_FOURD_baseline$educ,
      benzo = data_FOURD_baseline$benzo,
      antidep = data_FOURD_baseline$antidep,
      antidep.combintn = data_FOURD_baseline$antidep.combintn,
      past_ect_yesno = data_FOURD_baseline$past_ect_yesno,
      current_psyther_yesno = data_FOURD_baseline$current_psyther_yesno,
      hrsd1s = data_FOURD_baseline$hrsd1s - data_FOURD_t30$hrsd1s,
      hrsd2s = data_FOURD_baseline$hrsd2s - data_FOURD_t30$hrsd2s,
      hrsd3s = data_FOURD_baseline$hrsd3s - data_FOURD_t30$hrsd3s,
      hrsd4s = data_FOURD_baseline$hrsd4s - data_FOURD_t30$hrsd4s,
      hrsd5s = data_FOURD_baseline$hrsd5s - data_FOURD_t30$hrsd5s,
      hrsd6s = data_FOURD_baseline$hrsd6s - data_FOURD_t30$hrsd6s,
      hrsd7s = data_FOURD_baseline$hrsd7s - data_FOURD_t30$hrsd7s,
      hrsd8s = data_FOURD_baseline$hrsd8s - data_FOURD_t30$hrsd8s,
      hrsd9s = data_FOURD_baseline$hrsd9s - data_FOURD_t30$hrsd9s,
      hrsd10s = data_FOURD_baseline$hrsd10s - data_FOURD_t30$hrsd10s,
      hrsd11s = data_FOURD_baseline$hrsd11s - data_FOURD_t30$hrsd11s,
      hrsd12s = data_FOURD_baseline$hrsd12s - data_FOURD_t30$hrsd12s,
      hrsd13s = data_FOURD_baseline$hrsd13s - data_FOURD_t30$hrsd13s,
      hrsd14s = data_FOURD_baseline$hrsd14s - data_FOURD_t30$hrsd14s,
      hrsd15s = data_FOURD_baseline$hrsd15s - data_FOURD_t30$hrsd15s,
      hrsd16s = data_FOURD_baseline$hrsd16s - data_FOURD_t30$hrsd16s,
      hrsd17s = data_FOURD_baseline$hrsd17s - data_FOURD_t30$hrsd17s,
      madrs1 = data_FOURD_baseline$madrs1 - data_FOURD_t30$madrs1,
      madrs2 = data_FOURD_baseline$madrs2 - data_FOURD_t30$madrs2,
      madrs3 = data_FOURD_baseline$madrs3 - data_FOURD_t30$madrs3,
      madrs4 = data_FOURD_baseline$madrs4 - data_FOURD_t30$madrs4,
      madrs5 = data_FOURD_baseline$madrs5 - data_FOURD_t30$madrs5,
      madrs6 = data_FOURD_baseline$madrs6 - data_FOURD_t30$madrs6,
      madrs7 = data_FOURD_baseline$madrs7 - data_FOURD_t30$madrs7,
      madrs8 = data_FOURD_baseline$madrs8 - data_FOURD_t30$madrs8,
      madrs9 = data_FOURD_baseline$madrs9 - data_FOURD_t30$madrs9,
      madrs10 = data_FOURD_baseline$madrs10 - data_FOURD_t30$madrs10
    )
    
    data_CARTBIND_renamed = data.frame(
      subj_id = data_CARTBIND_baseline$subj_id,
      hrsd_total = data_CARTBIND_baseline$hrsd_total - data_CARTBIND_t30$hrsd_total,
      madrs_total = data_CARTBIND_baseline$madrs_total - data_CARTBIND_t30$madrs_total,
      age = data_CARTBIND_baseline$age,
      sex = data_CARTBIND_baseline$sex,
      educ = data_CARTBIND_baseline$educ,
      benzo = data_CARTBIND_baseline$benzo,
      antidep = data_CARTBIND_baseline$antidep,
      antidep.combintn = data_CARTBIND_baseline$antidep.combintn,
      past_ect_yesno = data_CARTBIND_baseline$past_ect_yesno,
      current_psyther_yesno = data_CARTBIND_baseline$current_psyther_yesno,
      hrsd1s = data_CARTBIND_baseline$hrsd1s - data_CARTBIND_t30$hrsd1s,
      hrsd2s = data_CARTBIND_baseline$hrsd2s - data_CARTBIND_t30$hrsd2s,
      hrsd3s = data_CARTBIND_baseline$hrsd3s - data_CARTBIND_t30$hrsd3s,
      hrsd4s = data_CARTBIND_baseline$hrsd4s - data_CARTBIND_t30$hrsd4s,
      hrsd5s = data_CARTBIND_baseline$hrsd5s - data_CARTBIND_t30$hrsd5s,
      hrsd6s = data_CARTBIND_baseline$hrsd6s - data_CARTBIND_t30$hrsd6s,
      hrsd7s = data_CARTBIND_baseline$hrsd7s - data_CARTBIND_t30$hrsd7s,
      hrsd8s = data_CARTBIND_baseline$hrsd8s - data_CARTBIND_t30$hrsd8s,
      hrsd9s = data_CARTBIND_baseline$hrsd9s - data_CARTBIND_t30$hrsd9s,
      hrsd10s = data_CARTBIND_baseline$hrsd10s - data_CARTBIND_t30$hrsd10s,
      hrsd11s = data_CARTBIND_baseline$hrsd11s - data_CARTBIND_t30$hrsd11s,
      hrsd12s = data_CARTBIND_baseline$hrsd12s - data_CARTBIND_t30$hrsd12s,
      hrsd13s = data_CARTBIND_baseline$hrsd13s - data_CARTBIND_t30$hrsd13s,
      hrsd14s = data_CARTBIND_baseline$hrsd14s - data_CARTBIND_t30$hrsd14s,
      hrsd15s = data_CARTBIND_baseline$hrsd15s - data_CARTBIND_t30$hrsd15s,
      hrsd16s = data_CARTBIND_baseline$hrsd16s - data_CARTBIND_t30$hrsd16s,
      hrsd17s = data_CARTBIND_baseline$hrsd17s - data_CARTBIND_t30$hrsd17s,
      madrs1 = data_CARTBIND_baseline$madrs1 - data_CARTBIND_t30$madrs1,
      madrs2 = data_CARTBIND_baseline$madrs2 - data_CARTBIND_t30$madrs2,
      madrs3 = data_CARTBIND_baseline$madrs3 - data_CARTBIND_t30$madrs3,
      madrs4 = data_CARTBIND_baseline$madrs4 - data_CARTBIND_t30$madrs4,
      madrs5 = data_CARTBIND_baseline$madrs5 - data_CARTBIND_t30$madrs5,
      madrs6 = data_CARTBIND_baseline$madrs6 - data_CARTBIND_t30$madrs6,
      madrs7 = data_CARTBIND_baseline$madrs7 - data_CARTBIND_t30$madrs7,
      madrs8 = data_CARTBIND_baseline$madrs8 - data_CARTBIND_t30$madrs8,
      madrs9 = data_CARTBIND_baseline$madrs9 - data_CARTBIND_t30$madrs9,
      madrs10 = data_CARTBIND_baseline$madrs10 - data_CARTBIND_t30$madrs10
    )
  }
  return(list(data_FOURD_renamed = data_FOURD_renamed, data_CARTBIND_renamed = data_CARTBIND_renamed))
}
