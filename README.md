# CRL-Deficiency-Analysis
Analysis of FDA Complete Response Letter (CRL) deficiency patterns using SAS and R Shiny

Live Dashboard: https://hannahweber-crl-deficiency-analysis.share.connect.posit.cloud/

## Overview
Complete Response Letters (CRLs) issued by the U.S. Food and Drug Administration (FDA) provide detailed feedback when drug and biologic applications cannot be approved; however, their unstructured format makes it difficult to perform systematic analysis.

This project develops an end-to-end SAS-based workflow to transform CRLs into structured datasets suitable for statistical and relational analysis. Each CRL is manually curated and assigned a unique identifier, capturing application-level information and associated deficiencies and subtypes.

The structured data are used to evaluate deficiency patterns across domains and subtypes, examine co-occurrence of deficiencies, and support interactive exploration through an R Shiny dashboard.

---

## Methods

### Data Structure
CRLs were manually reviewed and converted into a relational dataset consisting of:

- **CRL_EVENTS**
  - EVENT_ID: ID number used to link datasets
  - APPLICATION_NUMBER: FDA application number (can have multiple applications)
  - APPLICATION_TYPE:
    - NDA: New Drug Application
    - BLA: Biologics License Application
  - CRL_DATE: Date the CRL was issued
  - YEAR: Year of issuance

- **CRL_DEFICIENCIES**
  - EVENT_ID
  - DEFICIENCY_DOMAIN: Clinical, Nonclinical, CMC, Statistics, Regulatory, Patent Certifications

Deficiencies were coded at the event level, allowing for analysis at both the deficiency level and the CRL (application) level. Multiple deficiencies within a single CRL were recorded independently rather than assigning a single primary issue.

### Deficiency Classification

#### Clinical Subtypes
- Risk-Benefit: FDA determines benefits do not outweigh risks
- Efficacy: Failure to demonstrate substantial evidence of effectiveness
- Safety: Insufficient or unacceptable safety data
- Pharmacology: Inadequate PK/PD or exposure data
- Study Design: Flaws limiting interpretability
- Data Integrity: Issues with reliability or verification
- Human Factors: Risks related to product use or usability
- Redacted: Insufficient detail available

#### Nonclinical Subtypes
- Impurity Qualification: Inadequate safety data for impurities
- Insufficient Nonclinical Support: Missing required studies
- Toxicology: Safety concerns requiring additional studies
- Redacted

#### CMC Subtypes
- Facility: Manufacturing site or inspection issues
- Microbiology: Sterility or contamination concerns
- Stability: Inadequate stability data
- Specifications: Missing or inadequate quality criteria
- Manufacturing Controls: Process or validation issues
- Biopharmaceutics: Inadequate comparability or bioequivalence
- Redacted

#### Statistics Subtypes
- SAP Deficiencies: Issues with statistical analysis plan
- Missing/Inadequate Analyses: Required analyses not performed
- Insufficient Evidence: Results not persuasive
- Redacted

#### Regulatory Subtypes
- Administrative/Filing: Submission or documentation issues
- Labeling: Prescribing information issues
- REMS/PMRs: Risk management or post-marketing requirements
- PREA: Pediatric study requirements not met
- Redacted

#### Patent Certifications
- Issues related to patent certification requirements

---

## Tools Used
- SAS (data processing and statistical analysis)
- R (Shiny dashboard development)
- Excel (initial data organization)

---

## Analysis

- SAS was used to construct datasets, perform frequency analyses, and evaluate relationships between variables
- Domain-level datasets were created to prevent double-counting within CRLs
- Co-occurrence datasets were developed to identify multi-domain deficiency patterns
- Indicator variables were generated to support modeling and pattern analysis

---

## Key Findings

- CMC (Chemistry, Manufacturing, and Controls) deficiencies account for the majority of CRLs (~51%)
- Biologic applications (BLAs) show a higher proportion of CMC deficiencies compared to NDAs
- Facility-related issues are the most common CMC subtype
- Many CRLs involve multiple deficiency domains, indicating systemic issues rather than isolated failures

---

## Dashboard

An interactive R Shiny dashboard was developed to explore:
- deficiency domains
- subtype distributions
- application type differences
- co-occurrence patterns

---

## Data

A sample dataset is included to demonstrate structure and analysis.

The full dataset is not publicly shared.
