# Suppress R CMD check NOTES for non-standard evaluation (NSE) functions and operators
utils::globalVariables(c(
  "%>%",
  ":=",
  "case_when",
  "enquo",
  "mutate",
  "sym",
  "fill",
  "par",
  "rect",
  "text",
  "barplot",
  "title"
))
