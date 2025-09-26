# tpmTools

`tpmTools` is a lightweight R package designed to support **Third-Party Monitoring (TPM)** projects — particularly for health systems verification and performance monitoring. It provides helper functions, clean data templates, and preformatted tables to speed up analysis, review, and reporting.

> ⚠️ This is a personal project and is under active development. Use at your own risk!

---

## 📦 Installation

You can install the development version of `tpmTools` from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("tessam30/tpmTools")
```

---

## 🚀 Getting Started

Load the package and explore its contents:

```r
library(tpmTools)

# List available data and utility functions
list_tpm_tools()
```

---

## 🔧 Features

- ✅ Clean metadata and pre-structured indicator reference tables  
- 📊 Preformatted `gt` tables for quick presentation-ready outputs  
- 📋 Helper functions for checking structure, process, and outcome metrics  

---

## 🧪 Example

Here’s a quick look at one of the preformatted tables:

```r
p4p_gt  # A gt table of P4P indicators
```

You can also access internal data objects like:

```r
ess_list
hmis_verif_summary
qqm_content_scores
```

---

## 📁 Included Data

- **`ess_list`** – Environmental and Social Standards list for HER projects  
- **`p4p_indicators`** – P4P indicator names and short codes  
- **`hmis_verif_summary`** – Summary of verification data across facility types  
- **`qqm`**, **`qqm_structural`**, and more...

---

## 📚 Documentation

Each dataset and function is documented with examples. Type `?function_name` or `?data_object` in your R console for more info.

---

## 🛠 Development Notes

This project is in early stages. Current priorities include:

- Expanding function coverage for analysis
- Improving metadata documentation
- Streamlining `gt` output formatting

Feel free to submit issues, ideas, or pull requests.

---

## 📄 License

This project is licensed under the MIT License.

---

## 🤝 Acknowledgements

This package was developed to support TPM work in global health monitoring, including data systems like HMIS and performance verification indicators for health facilities.

---
