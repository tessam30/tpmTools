# Datasets


# p4p indicators ----------------------------------------------------------


p4p_indicators <- tibble::tribble(
  ~no.,                                                                                                  ~indicator,            ~indicator_short,
  1L,                                                                            "Total number of Antenatal Visits",                      "ANC",
  2L,                                                                            "Total number of Postnatal Visits",                      "PNC",
  3L,                                                                "Institutional deliveries excluding C-Section", "Institutional Deliveries",
  4L,                                            "Total number of Family Planning couple years of protection (CYP)",                      "CYP",
  5L,                                                                        "Total number of Penta3 immunizations",                    "Penta3",
  6L,                                                                          "TT2+ for women of reproductive age",                     "TT2+",
  7L,                                                    "Number of sputum smear (+) TB cases successfully treated",                       "TB",
  8L, "Growth monitoring of under 2-year children and IYCF counselling for pregnant and lactating women (GMP/IYCF)",                      "GMP",
  9L,                                                "Under five children’s morbidities (HMIS-MIAR-A1-morbidities)",           "U5 Morbidities",
  10L,                                                                                                   "C-Section",                "C-Section",
  11L,                                                                         "Major Surgeries excluding C-Section",                       "MS"
) %>%
  dplyr::mutate(indicator = forcats::fct_inorder(indicator),
                indicator_short = forcats::fct_inorder(indicator_short))

usethis::use_data(p4p_indicators, overwrite = TRUE)


# hmis verification summary -----------------------------------------------

# Verification of HMIS
hmis_verif_summary <- tibble::tribble(
  ~Indicator,   ~RH,   ~PH,   ~DH,  ~CHC,  ~BHC,  ~SHC,
  "ANC", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "PNC", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Inst. Deliveries", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Penta3", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "CYP", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "TT+", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "TB smear+", "Yes", "Yes", "Yes", "Yes", "Yes",    NA,
  "Growth Monitoring", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "U5 Morbidities", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "C-section", "Yes", "Yes", "Yes", "Yes",    NA,    NA,
  "Major Surgery", "Yes", "Yes", "Yes", "Yes",    NA,    NA
)

usethis::use_data(hmis_verif_summary, overwrite = TRUE)


# QQC structural ----------------------------------------------------------

qqc_structural <- tibble::tribble(
  ~row,                   ~domain, ~points,
  1L,         "General Management", 14L,
  2L,                    "Hygiene", 34L,
  3L,                        "OPD", 44L,
  4L,            "Family Planning", 23L,
  5L,                 "Laboratory", 15L,
  6L, "Essential Drugs Management", 52L,
  7L,               "Tracer Drugs", 90L,
  8L,                  "Maternity", 29L,
  9L,                        "EPI", 27L,
  10L,                       "ANC", 13L
) %>% dplyr::mutate(total_points = sum(points),
             total_points_bhc = sum(points[domain != "Laboratory"]),
             domain_label = paste0("Domain ", dplyr::row_number(), ": ", domain)
)

usethis::use_data(qqc_structural, overwrite = TRUE)


# ESS list ----------------------------------------------------------------

ess_list <- tibble::tribble(
              ~ESS.Number,                                                                                    ~Description, ~HER,
                       1L,                       "Assessment and Management of Environmental and Social Risks and Impacts",   1L,
                       2L,                                                                  "Labor and Working Conditions",   1L,
                       3L,                                       "Resource Efficiency and Pollution Prevention Management",   1L,
                       4L,                                                                   "Community and Health Safety",   1L,
                       5L,                       "Land Acquisition, Restrictions on Land Use and Inovluntary Resettlement",   0L,
                       6L,              "Biodiversity Conservation and Sustainable Management of Living Natural Resources",   0L,
                       7L, "Indigenous Peoples/Sub-Saharan African Historically Underserved Traditional Local Communities",   0L,
                       8L,                                                                             "Cultural Heritage",   0L,
                       9L,                                                                      "Financial Intermediaries",   0L,
                      10L,                                               "Stakeholder Engement and Information Disclosure",   1L
              )

usethis::use_data(ess_list, overwrite = TRUE)


# qqm content -------------------------------------------------------------

qqm_content_short <- tibble::tribble(
                                       ~qqm_content, ~max_score, ~bonus,
                                        "Pneumonia",       100L,     0L,
                                        "Diarrhoea",       100L,     0L,
                  "Growth Monitoring and Promotion",       100L,     0L,
                            "TB Patient Management",       100L,     0L,
                                            "Labor",       100L,     0L,
                                              "PNC",       100L,     1L,
                                  "First ANC visit",       100L,     1L,
                    "Birth spacing family planning",       100L,     0L,
                 "Changing FP method to injectable",       100L,     0L,
                                              "PPH",       100L,     0L
                 )
usethis::use_data(qqm_content_short, overwrite = TRUE)


# GT data frame of p4p indicators
p4p_gt <-
  p4p_indicators %>%
  dplyr::select(no., indicator_short, indicator) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "P4P indicators for BPHS and EPHS verification"
  ) %>%
  gt::cols_label(
    `no.` = "#",
    indicator = "Indicator description",
    indicator_short = "Short name"
  ) %>%
  gt::cols_align(
    align = "left",
    columns = 2:3
  ) %>%
  gt::cols_width(
    `no.` ~ gt::px(4),
    indicator ~ gt::px(80),
    indicator_short ~ gt::px(17)
  ) %>%
  # Uncomment this line if any column uses markdown formatting
  # gt::fmt_markdown(columns = dplyr::everything()) %>%
  gt::tab_options(
    table.font.size = gt::px(13),
    table.width = gt::pct(80)
  ) %>%
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_title()
  ) %>%
  gtayblr::si_gt_base() %>%
  adjust_row_spacing(padding_setting = 2)
usethis::use_data(p4p_gt, overwrite = TRUE)



# Create domain table -----------------------------------------------------

qqm <- tibble::tribble(
  ~`Structural.Quality.(Facility.checklist)`,    ~`Content.of.Care.(Vignettes)`,                                    ~`Quality.of.Care.(Exit.interviews)`,
  "General Management",                       "Pneumonia",                                "Outpatient care for sick child under 5",
  "Hygiene",                       "Diarrhoea", "Family planning services for women (incl. Satisfaction with services)",
  "Out-Patient Department", "Growth Monitoring and Promotion",                                                                      NA,
  "Family Planning",                    "Tuberculosis",                                                                      NA,
  "Laboratory (NA for BHC)",                           "Labor",                                                                      NA,
  "Essential Drugs Management",                  "Postnatal Care",                                                                      NA,
  "Availability of Tracer Drugs",                  "Antenatal Care",                                                                      NA,
  "Maternity",     "Family Planning Counselling",                                                                      NA,
  "ANC",       "Change of Family Planning",                                                                      NA,
  "EPI",         "Post-partum Haemorrhage",                                                                      NA
)

usethis::use_data(qqm, overwrite = TRUE)


qqm_gt <-
  tpmTools::qqm %>%
  gt::gt() %>%
  gt::sub_missing(missing_text = "") %>%
  gt::tab_header(
    title = "QQM Components and Domains"
  ) %>%
  gt::cols_label(
    `Structural.Quality.(Facility.checklist)` = gt::md("Structural Quality<br>(Facility Checklist)"),
    `Content.of.Care.(Vignettes)` = gt::md("Content of Care<br>(Vignettes)"),
    `Quality.of.Care.(Exit.interviews)` = gt::md("Quality of Care<br>(Exit Interviews)")
  ) %>%
  gt::cols_align(
    align = "left",
    columns = gt::everything()
  ) %>%
  gt::cols_width(
    gt::everything() ~ gt::px(200)
  ) %>%
  gtayblr::si_gt_base() %>%
  gtayblr::adjust_row_padding(padding_setting = "condensed") %>%
  gt::tab_style(
    style = gt::cell_borders(
      sides = "top",
      color = "#E6E7E8",
      weight = gt::px(1)
    ),
    locations = gt::cells_body(rows = TRUE)
  )

usethis::use_data(qqm_gt, overwrite = TRUE)


# Create the data
qqm_calc <- tibble::tibble(
  Domain = c(
    "Structural Indicators",
    "Content of Care",
    "Quality of Care",
    "Overall QQM Score"
  ),
  `Max Points` = c(
    "341 (CHC/DH), 326 (BHC)",
    "400",
    "200",
    "Out of 100% (weighted score)"
  ),
  Description = c(
    "Checklist-based assessment across 10 domains (e.g. hygiene, drugs, lab, OPD, etc.)",
    "4 clinical vignettes (100 pts each), testing provider knowledge & response to case scenarios",
    "2 exit interviews: one with caregiver of child <5, one with FP client (100 pts each)",
    "Weighted composite score based on structural, content and quality scores (40% structural, 40% content, 20% quality)"
  ),
  `Scoring Method` = c(
    "(Points Scored / Max Points) × 100",
    "Sum of vignette scores / 400 × 100",
    "Sum of interview scores / 200 × 100",
    "(0.4 × Structural) + (0.4 × Content) + (0.2 × Quality)"
  ),
  `Example Score` = c(
    "214 / 341 = 63%",
    "(23 + 46 + 78 + 61)/400 = 52%",
    "(53 + 48)/200 = 51%",
    "(0.4×63) + (0.4×52) + (0.2×51) = 56.2%"
  )
)
usethis::use_data(qqm_calc, overwrite = TRUE)


# Build the gt table
qqm_calc_gt <-
  tpmTools::qqm_calc %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Summary of QQM Scoring Process at Facility Level**")
  ) %>%
  gt::cols_label(
    Domain = "Domain",
    `Max Points` = "Max Points",
    Description = "Description",
    `Scoring Method` = "Scoring Method",
    `Example Score` = "Example Score"
  ) %>%
  gt::cols_align(
    align = "left",
    columns = gt::everything()
  ) %>%
  gt::cols_width(
    Domain ~ gt::px(100),
    `Max Points` ~ gt::px(120),
    Description ~ gt::px(300),
    `Scoring Method` ~ gt::px(200),
    `Example Score` ~ gt::px(150)
  ) %>%
  gtayblr::si_gt_base() %>%
  gt::tab_style(
    style = gt::cell_borders(
      sides = "top",
      color = "#E6E7E8",
      weight = gt::px(1)
    ),
    locations = gt::cells_body(rows = TRUE)
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = 600),
    locations = gt::cells_body(rows = nrow(tpmTools::qqm_calc))
  ) %>%
  gt::tab_source_note(
    source_note = gt::md("The same analysis is also performed on provincial level, by aggregating the points achieved by
                         all health facilities in a province and dividing that by the maximum points multiplied by the
                         total number of health facilities in that province.")
  )

usethis::use_data(qqm_calc_gt, overwrite = TRUE)


# Content of care domains
qqm_content_scores <-
  tibble::tribble(
  ~`Content of Care Domain`,                                                     ~`Domain Name`,                    ~`Max Score`,
  "1. Respiratory Infection Pneumonia",                                        "Pneumonia content of care quality", "100",
  "2. Diarrhea with Cat B dehydration",                                         "Diarrhoea content of care quality",  "100",
  "3. Growth monitoring and promotion",                  "Growth Monitoring and Promotion content of care quality",  "100",
  "4. Management of presumptive tuberculosis",                     "TB Patient Management content of care quality",  "100",
  "5. Labor",                                                                      "Labor content of care quality",  "100",
  "6. Postnatal care pre-discharge",                                                 "PNC content of care quality", "100+",
  "7. First antenatal care visit",                                       "First ANC visit content of care quality", "100+",
  "8. FP counselling",    "                                 Birth spacing family planning content of care quality",  "100",
  "9. Birth spacing; changing FP method to injectable", "Changing FP method to injectable content of care quality",  "100",
  "10. Post partum haemorrhage – atonic uterus",                                     "PPH content of care quality",  "100"
)
usethis::use_data(qqm_content_scores, overwrite = TRUE)


# HW assessment -----------------------------------------------------------
# Health worker interviews and payroll inspection
health_wf <- tibble::tribble(
                                         ~component,                                                                                                         ~indicators,
  "Health worker interviews and payroll inspection",                                    "Received salary for the past 3 months within 10 days at the end of each month.",
  "Health worker interviews and payroll inspection",                                                                   "Means of salary payment (bank transfer or cash)",
                         "Health worker interviews", "Received training on Gender-Based Violence (DH, PH, and RH only) in the past 3 months, or more than 3 months ago.",
                         "Health worker interviews",                             "Level of satisfaction (1 = very dissatisfied; 4 = very satisfied) with 18 statements.",
                  "Facility checklist / timesheets",               "Current and retrospective availability of health workers according to minimum standards of services",
                  "Facility checklist / timesheets",                       "Availability of at least one female health worker in each facility on the day of the survey",
                  "Facility checklist / timesheets",                                   "Availability of at least one female community health worker in each health post"
  )
usethis::use_data(health_wf, overwrite = TRUE)


health_wf_gt <- tpmTools::health_wf %>%
  dplyr::mutate(
    indic = stringr::str_c(dplyr::row_number(), "."),
    .by = component,
    .before = component
  ) %>%
  gt::gt(groupname_col = "component") %>%
  gt::tab_header(
    title = "Indicators included in the non QQM-Assessment"
  ) %>%
  gt::cols_label(
    indicators = "COMPONENT / Indicators",
    indic = ""
  ) %>%
  gt::cols_align(
    align = "left",
    columns = gt::everything()
  ) %>%
  gt::tab_style(
    style = gt::cell_text(v_align = "top"),
    locations = gt::cells_body(columns = gt::everything())
  ) %>%
  gtayblr::si_gt_base()

usethis::use_data(health_wf_gt, overwrite = TRUE)


# Weights for HW assessments
hw_assessment <- tibble::tribble(
                                     ~Indicator,                   ~SHC, ~`PH/RH`,
         "Availability of functional equipment",                 ">90%",   ">90%",
  "Pharmaceuticals (current and retrospective)",                 ">90%",   ">90%",
                      "Other than P4P services",                 ">90%",   "100%",
                          "Hospital governance",                     NA,   ">95%",
         "Provincial key staff (retrospective)", "70% (each key staff)",       "70% (each key staff)"
  )
usethis::use_data(hw_assessment, overwrite = TRUE)

hw_assessment_gt <-
  hw_assessment %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Minimum standards of services cut-offs for SHC and PH/RH"
  ) %>%
  gt::cols_label(
    Indicator = "Indicator",
    SHC = "SHC",
    `PH/RH` = "PH/RH"
  ) %>%
  gt::sub_missing(missing_text = "not applicable") %>%
  gtayblr::si_gt_base()
usethis::use_data(hw_assessment_gt, overwrite = TRUE)



# RED Flags Verification --------------------------------------------------


red_flags_verif<- tibble::tibble(
  Category = c(
    "Data Management",
    "Human Resources", "Human Resources", "Human Resources",
    "Infrastructure",
    "Protection & Ethics",
    "Security",
    "Service Disruption",
    "Supply Chain / Pharmacy", "Supply Chain / Pharmacy"
  ),
  Red_Flag = c(
    "Registers are not available for verification of HMIS data.",
    "Staff are absent and did not notify the health facility management.",
    "Staff report salaries not paid, payments delayed, payment shortfalls.",
    "Minimum staffing is not met.",
    "Health facility has non-functional sections.",
    "Clients report abuse, exploitation, or mistreatment by health facility staff.",
    "Theft incident at the health facility.",
    "Health facility is closed during normal expected working hours.",
    "Stock-out of essential medicine and nutrition supply (RUTF), selling medicines, patients given prescription to procure medication out of the facility.",
    "Major violation of conditions affecting drug quality including drug storage."
  )
)

usethis::use_data(red_flags_verif, overwrite = TRUE)

red_flag_gt <-
  red_flags_verif %>%
  dplyr::group_by(Category) %>%
  dplyr::mutate(
    Display_Category = dplyr::if_else(dplyr::row_number() == 1, Category, "")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(Display_Category, Red_Flag) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Red Flags for Verification"
  ) %>%
  gt::cols_label(
    Display_Category = "Category",
    Red_Flag = "Red Flag"
  ) %>%
  gt::cols_align(
    align = "left",
    columns = gt::everything()
  ) %>%
  gtayblr::si_gt_base() %>%
  gtayblr::adjust_row_padding(padding_setting = "condensed") %>%
  gt::tab_style(
    style = gt::cell_borders(
      sides = "top",
      color = "#E6E7E8",
      weight = gt::px(1)
    ),
    locations = gt::cells_body(rows = TRUE)
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(columns = Display_Category)
  )
usethis::use_data(red_flag_gt, overwrite = TRUE)
