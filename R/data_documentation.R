#' Environmental and Social Standards (ESS) Applied to the HER Project
#'
#' This dataset provides a summary of the ten Environmental and Social Standards (ESS)
#' as defined by the World Bank, including a brief description and an indicator showing
#' whether each ESS applies to the Health Emergency Response (HER) project.
#'
#' @format A data frame with 10 rows and 3 columns:
#' \describe{
#'   \item{ESS.Number}{An integer indicating the ESS number (1 through 10).}
#'   \item{Description}{A character string describing the focus of each ESS.}
#'   \item{HER}{A binary indicator (1 = applies to HER, 0 = does not apply).}
#' }
#'
#' @source Adapted from World Bank Environmental and Social Framework documentation and HER project materials.
#'
#' @examples
#' data(ess_list)
#' head(ess_list)
"ess_list"

#' HMIS Verification Coverage by Facility Type
#'
#' A summary table showing which health indicators are included in Health Management Information System (HMIS) verification across different types of health facilities. This is used to determine which indicators are verified at each level of the health system, from Regional Hospitals (RH) to Sub-Health Centers (SHC).
#'
#' @format A data frame with 11 rows and 7 variables:
#' \describe{
#'   \item{Indicator}{Character. The name of the health indicator subject to HMIS verification.}
#'   \item{RH}{Character. Whether the indicator is verified at Regional Hospitals (Yes/NA).}
#'   \item{PH}{Character. Whether the indicator is verified at Provincial Hospitals (Yes/NA).}
#'   \item{DH}{Character. Whether the indicator is verified at District Hospitals (Yes/NA).}
#'   \item{CHC}{Character. Whether the indicator is verified at Comprehensive Health Centers (Yes/NA).}
#'   \item{BHC}{Character. Whether the indicator is verified at Basic Health Centers (Yes/NA).}
#'   \item{SHC}{Character. Whether the indicator is verified at Sub-Health Centers (Yes/NA).}
#' }
#'
#' @details An entry of "Yes" indicates that the given indicator is verified at the specified facility type. "NA" indicates that the indicator is not applicable or not verified at that facility level.
#'
#' @source HMIS Verification Framework, Health Emergency Response Project documentation.
#'
#' @examples
#' data(hmis_verif_summary)
#' head(hmis_verif_summary)
"hmis_verif_summary"

#' Pay-for-Performance (P4P) Indicators for Verification
#'
#' A dataset listing the Pay-for-Performance (P4P) indicators that are subject to verification under the Health Emergency Response (HER) project. Each indicator represents a service delivery output tied to performance-based payments, along with a short label for reference in reporting or analysis.
#'
#' @format A data frame with 11 rows and 3 variables:
#' \describe{
#'   \item{no.}{Integer. The sequential number assigned to each indicator.}
#'   \item{indicator}{Factor. Full description of the health service or output used for performance-based verification.}
#'   \item{indicator_short}{Character. Short label for the indicator used in visualizations or summaries.}
#' }
#'
#' @details These indicators are used in the verification of service delivery under the Pay-for-Performance (P4P) scheme, with financial implications based on the quantity and quality of services reported through the Health Management Information System (HMIS).
#'
#' @source HER project documentation and HMIS reporting guidance.
#'
#' @examples
#' data(p4p_indicators)
#' head(p4p_indicators)
"p4p_indicators"

#' Structural Quality Domains in the QQM Process
#'
#' A dataset listing the structural quality domains assessed under the Quantified Quality Metrics (QQM) process, along with the maximum points allocated to each domain. These domains are part of the facility-level structural assessment used in calculating composite quality scores for the Health Emergency Response (HER) project.
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{row}{Integer. Row number or sequential order of domains.}
#'   \item{domain}{Character. The structural quality domain assessed (e.g., Hygiene, OPD, Maternity).}
#'   \item{points}{Integer. Maximum points available for the domain within the QQM structural score.}
#'   \item{total_points}{Integer. Total max points avaiable across all structural domains for non BHCs facilities.}
#'   \item{total_points_bhc}{Integer. Total max points available across all structural domains for BHCs facilities.}
#'   \item{domain_label}{Character. A full label of each domain with Domain number pre-pended.}
#' }
#'
#' @details The structural domains cover a wide range of facility readiness and service delivery areas, including infection prevention, drug management, and reproductive health services. Each domain contributes to the overall facility quality score used in the performance-based payment mechanism.
#'
#' @source Quantified Quality Metrics (QQM) guidelines, HER project documentation.
#'
#' @examples
#' data(qqm_structural)
#' head(qqm_structural)
"qqm_structural"

#' QQM Quality of Care Indicators
#'
#' A dataset summarizing the content of care quality indicators used in the Quantified Quality Metrics (QQM) process. Each indicator represents a clinical vignette or service area evaluated as part of the QQM quality scoring framework. The dataset includes the maximum score assignable to each indicator and whether a performance-based bonus applies.
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{qqm_content}{Character. The name of the quality of care indicator (e.g., Pneumonia, First ANC visit, TB Patient Management).}
#'   \item{max_score}{Integer. The maximum score assignable to the indicator. Typically 100, with some exceptions (e.g., ANC = 120).}
#'   \item{bonus}{Integer. Binary indicator (1 = eligible for a performance-based bonus, 0 = not eligible).}
#' }
#'
#' @details These indicators are typically assessed using clinical vignettes or direct observation of provider performance, and form part of the facility’s overall content of care quality score under the QQM system.
#'
#' @source HER project documentation, Quantified Quality Metrics (QQM) clinical assessment tools.
#'
#' @examples
#' data(qqm_content_short)
#' head(qqm_content_short)
"qqm_content_short"


#' P4P Indicators GT Table
#'
#' A pre-formatted `gt` table displaying P4P indicators used in BPHS and EPHS verification.
#'
#' This object includes a stylized summary table with left-aligned columns, customized labels,
#' font sizing, and column widths. It is generated from the `p4p_indicators` dataset.
#'
#' @format A `gt_tbl` object with 3 columns and as many rows as `p4p_indicators`.
#' @source Derived from internal dataset `p4p_indicators`.
#' @seealso [p4p_indicators], [gt::gt()]
#' @examples
#' \dontrun{
#' p4p_gt  # View the stylized GT table
#' }
"p4p_gt"


#' Summary Table of QQM Scoring Process at Facility Level
#'
#' This dataset summarizes the scoring methodology for the Quantified Quality Metrics (QQM) used in third-party monitoring
#' of health facility performance. The table outlines the key domains assessed (Structural Indicators, Content of Care,
#' Quality of Care), maximum points per domain, scoring methods, and example scores from a sample facility. It also
#' includes the weighted formula used to calculate the overall QQM score.
#'
#' @format A data frame with 4 rows and 5 columns:
#' \describe{
#'   \item{Domain}{Name of the QQM scoring domain (e.g., Structural Indicators, Content of Care)}
#'   \item{Max Points}{Maximum number of points assigned to the domain. Varies by facility type for structural indicators}
#'   \item{Description}{Narrative description of how the domain is assessed}
#'   \item{Scoring Method}{Formula used to compute the domain-level percentage score}
#'   \item{Example Score}{Illustrative example based on a sample facility's results}
#' }
#'
#' @details
#' The overall QQM score is a weighted combination of three domain scores:
#' - 40% Structural Indicators
#' - 40% Content of Care
#' - 20% Quality of Care
#'
#' The dataset supports the generation of a formatted `gt` table for inclusion in reporting outputs.
#'
"qqm_calc"

#' GT Table: QQM Scoring Process Summary at Facility Level
#'
#' A styled `gt` table object that presents a summary of the facility-level scoring methodology
#' used in the Quantified Quality Metrics (QQM) system for third-party monitoring of health services.
#'
#' This table includes:
#' - Domain definitions (Structural, Content of Care, Quality of Care)
#' - Maximum possible scores
#' - Scoring formulas
#' - Example calculations
#' - Weighted composite formula for the overall QQM score
#'
#' The table is formatted with `gt()` and styled with fixed column widths, bolding of the final
#' row, soft borders between rows, and an explanatory source note.
#'
#' @format A `gt_tbl` object with 4 rows and 5 columns.
#'
#' @details
#' This object is built from the `qqm_calc` dataset and intended for direct use in R Markdown or Quarto documents,
#' dashboards, or rendered reports. It includes custom styling via `si_gt_base()` and consistent alignment and spacing
#' for professional presentation.
#'
#' The final row ("Overall QQM Score") is bolded to emphasize the composite nature of the scoring system:
#' \deqn{Score = (0.4 × Structural) + (0.4 × Content) + (0.2 × Quality)}
#'
#'
#' @seealso \code{\link[gt]{gt}} for table creation and \code{qqm_calc} for the raw input data
"qqm_calc_gt"


#' QQM Components and Domains Table
#'
#' A styled `gt` table summarizing the Quality of Care Measurement (QQM) components
#' and domains. This table presents structural, content, and quality measures drawn from
#' facility checklists, vignettes, and exit interviews, respectively.
#'
#' The table is styled for consistent presentation with condensed row spacing,
#' missing values replaced by empty strings, and column labels formatted with line breaks.
#'
#' @format A `gt_tbl` object (created using the `gt` package).
#'
#' @source Built from the `qqm` data object using the `gt` and `gtayblr` packages.
#'
#' @seealso [gtayblr::si_gt_base()]
#'
#' @examples
#' qqm_gt  # View the styled table
"qqm_gt"

#' Quality of Care Measurement (QQM) Components
#'
#' A `tibble` listing the specific components of structural quality, content of care, and
#' quality of care used in health facility assessments. These are organized by data source:
#'
#' - **Structural Quality:** from facility checklists
#' - **Content of Care:** from clinical vignettes
#' - **Quality of Care:** from exit interviews with service users
#'
#' Each row links a structural domain with a content or quality domain where applicable.
#' `NA` indicates that a given structural component does not have a corresponding measure in that domain.
#'
#' @format A `tibble` with 11 rows and 3 variables:
#' \describe{
#'   \item{Structural.Quality.(Facility.checklist)}{Facility-level structural domain}
#'   \item{Content.of.Care.(Vignettes)}{Clinical domain assessed using vignettes}
#'   \item{Quality.of.Care.(Exit.interviews)}{Service experience measures from exit interviews}
#' }
#'
#' @examples
#' qqm
#'
#' # Filter only rows with exit interview data
#' dplyr::filter(qqm, !is.na(`Quality.of.Care.(Exit.interviews)`))
"qqm"


#' Content of Care Vignette Domains and Maximum Scores
#'
#' A dataset describing the clinical vignette domains included in the Content of Care component of the Quantified Quality Metric (QQM) scoring system.
#' Each vignette is designed to assess the knowledge and clinical decision-making of health workers in specific maternal, newborn, child health, and
#' family planning scenarios.
#'
#' @format A tibble with 10 rows and 3 columns:
#' \describe{
#'   \item{Content of Care Domain}{Numbered label and short title of the vignette domain as it appears in training and data tools.}
#'   \item{Domain Name}{Full descriptive title used in reports and dashboards to describe each vignette domain.}
#'   \item{Max Score}{Maximum points achievable for the vignette. Most domains are scored out of 100; some may exceed 100, indicated as "100+".}
#' }
#'
#' @details
#' These content domains are assessed via case-based vignettes administered to health workers. Each vignette is scored against a checklist
#' reflecting essential steps and quality of care standards. Scores from all vignettes contribute equally to the overall Content of Care score
#' (40% weight in the final QQM composite).
#'
#' The domains include both curative child health conditions (e.g., pneumonia, diarrhea), preventive services (e.g., growth monitoring),
#' maternal care (e.g., labor, postnatal care, postpartum hemorrhage), and family planning counseling.
#'
"qqm_content_scores"


#' Health Workforce Monitoring Indicators
#'
#' A dataset summarizing health workforce-related indicators used for monitoring service delivery and staff satisfaction.
#' This data was compiled from health worker interviews, payroll inspections, and facility checklists.
#'
#' @format A tibble with 7 rows and 2 variables:
#' \describe{
#'   \item{component}{The data collection component or source (e.g., interviews, payroll, checklist)}
#'   \item{indicators}{Indicator text describing the measured concept or question}
#' }
#'
#' @examples
#' health_wf
"health_wf"

#' Formatted Table: Health Workforce Indicators (Non-QQM)
#'
#' A pre-formatted `gt` table showing indicators used in the non-QQM Health Workforce Assessment.
#' Indicators are grouped by data collection component (e.g., interviews, payroll inspection, facility checklist).
#' Each group is preceded by a row number prefix for easier referencing.
#'
#' Styling includes grouped rows, vertical alignment for readability, and application of the `si_gt_base()` style.
#'
#' @format A `gt_tbl` object with grouped rows, custom header, aligned text, and styled for presentation.
#'
#' @examples
#' health_wf_gt
"health_wf_gt"


#' Health Workforce Service Standard Cutoffs
#'
#' This dataset contains minimum service standard cutoff values used to assess health facilities by type.
#' Values are defined separately for Sub-Health Centers (SHC) and Primary/Referral Hospitals (PH/RH).
#'
#' @format A tibble with 5 rows and 3 variables:
#' \describe{
#'   \item{Indicator}{Description of the indicator used for workforce or service assessment.}
#'   \item{SHC}{Minimum standard for Sub-Health Centers (e.g., ">90\%").}
#'   \item{PH/RH}{Minimum standard for Primary/Referral Hospitals. \code{NA} if not applicable.}
#' }
#'
#' @examples
#' hw_assessment
"hw_assessment"




#' Formatted Table: HW Service Standard Cutoffs
#'
#' A pre-formatted `gt` table displaying the minimum service standard cutoffs for
#' health workforce indicators across facility types. Useful for direct display in
#' reports or dashboards.
#'
#' Missing values are shown as `"not applicable"` and styling follows the `si_gt_base()` theme.
#'
#' @format A `gt_tbl` object with custom header, missing text formatting, and styling applied.
#'
#' @examples
#' hw_assessment_gt
"hw_assessment_gt"


#' Red Flags for Facility Monitoring
#'
#' This dataset contains a list of predefined "red flags" identified during health facility monitoring visits
#' under the Health Emergency Response (HER) project. These red flags represent serious operational or
#' compliance issues that require follow-up or escalation.
#'
#' @format A tibble with 10 rows and 1 column:
#' \describe{
#'   \item{red_flags_verif}{Character. Description of the red flag observed or reported at a health facility.}
#' }
#'
#' @details
#' Each row represents a specific type of red flag that may be encountered during verification visits. These include:
#' \itemize{
#'   \item Facility closures during expected working hours
#'   \item Unreported staff absences
#'   \item Missing HMIS registers
#'   \item Client reports of abuse or mistreatment
#'   \item Stockouts or mismanagement of medicines and therapeutic foods
#'   \item Salary payment issues for health workers
#'   \item Staffing shortfalls
#'   \item Non-functional facility sections
#'   \item Drug storage violations
#'   \item Theft incidents at the facility
#' }
#'
#' These red flags are used to flag facilities for additional supervision, documentation, or corrective actions.
#'
#' @source TPM HER Facility Monitoring Guidelines, Afghanistan, 2025.
#'
#' @examples
#' data(red_flags_verif)
#' print(red_flags_verif)
"red_flags_verif"


#' GT Table: Red Flags for Verification
#'
#' A pre-formatted `gt` table object displaying red flags encountered during health facility monitoring and verification
#' exercises. The table organizes red flags into thematic categories such as Human Resources, Infrastructure,
#' Protection & Ethics, and more.
#'
#' This table is intended for inclusion in analytical reports, dashboards, and stakeholder presentations as a
#' clear and readable summary of key verification concerns.
#'
#' @format A `gt_tbl` object with 2 columns:
#' \describe{
#'   \item{Category}{Thematic category of the red flag. Displayed once per group.}
#'   \item{Red Flag}{Description of the issue or anomaly encountered during monitoring.}
#' }
#'
#' @details
#' The `gt` table groups red flags by category using a non-repeating `Category` column (`Display_Category` in the raw data)
#' and applies bold styling to improve legibility. Each category group is separated by a soft top border for visual clarity.
#' Padding is adjusted using the custom `gtayblr::adjust_row_padding()` function to condense row height for compact display.
#'
#' This object is generated from the `red_flags_verif` dataset, which contains a full list of predefined verification red flags
#' used in the Health Emergency Response (HER) project in Afghanistan.
#'
#' @source TPM HER Facility Monitoring Verification Framework, Afghanistan, 2025.
#'
#' @seealso \code{\link[gt]{gt}} for table generation, \code{red_flags_verif} for the input dataset.
#'
#' @examples
#' red_flag_gt  # View the stylized red flag summary table
"red_flag_gt"
