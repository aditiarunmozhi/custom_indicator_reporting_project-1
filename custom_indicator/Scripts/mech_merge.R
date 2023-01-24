#merge with mech ref file
mech_merge <- left_join(kp_disaggs_counts_clean, mech_id_ref_table_fy23, by = c("Country" ="country"))
