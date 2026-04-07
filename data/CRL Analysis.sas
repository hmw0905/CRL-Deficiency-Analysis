LIBNAME CRLProj XLSX "C:\Users\Hannah Weber\Desktop\CRL Project\CRL_Project Local.xlsx";
LIBNAME sasdata "C:\Users\Hannah Weber\Desktop\CRL Project"; /* where you save SAS datasets */

ods listing close;

/*=======================
  1. SETUP AND IMPORT
=======================*/
DATA sasdata.crl_header;
    SET CRLProj.CRL_Events;
    app_num_char = PUT(application_number, 8. -L);
    DROP application_number source_link;
    RENAME app_num_char = application_number;
    IF office = 'Not Available' OR
       regulatory_pathway = 'Not Available' THEN data_limitation = 1;
    ELSE data_limitation = 0;
RUN;

DATA sasdata.crl_deficiency;
    SET CRLProj.CRL_DEF;
    IF deficiency_subtype = 'Redacted'  THEN subtype_redacted = 1;
    ELSE IF MISSING(deficiency_subtype) THEN subtype_redacted = .;
    ELSE subtype_redacted = 0;
RUN;

/* Close the Excel libname once copied */
LIBNAME CRLProj CLEAR;

/*=======================
  2. DATA PREP
=======================*/
PROC SORT DATA=sasdata.crl_header;
    BY event_id;
RUN;

PROC SORT DATA=sasdata.crl_deficiency;
    BY event_id;
RUN;

/*Each CRL can contain multiple independent deficiencies (one-to-many relationship).
The merged dataset (crl_long) retains all deficiency-level observations so that 
subtype and co-occurence analyses can be performed without collapsing information.*/
DATA sasdata.crl_long;
    MERGE sasdata.crl_deficiency (IN=a)
          sasdata.crl_header     (IN=b);
    BY event_id;
    IF a;
RUN;

proc export data = sasdata.crl_long
	outfile = "C:\Users\Hannah Weber\Desktop\CRL Project\crl_long.csv"
	dbms = csv
	replace;
run;

/*Domain-level analyses require one observation per CRL_domain combination.
Multiple deficiencies within the same domain for a given CRL are not double-counted.

This prevents inflation of domain frequencies while preserving the presence of each domain
within a CRL. This explains why the number of domain observations is smaller than the total
number of deficiencies.*/
PROC SORT DATA=sasdata.crl_long(KEEP=event_id deficiency_domain application_type regulatory_pathway year office)
          OUT=sasdata.domain_unique
          NODUPKEY;
    BY event_id deficiency_domain;
RUN;

proc export data = sasdata.domain_unique
	outfile = "C:\Users\Hannah Weber\Desktop\CRL Project\domain_unique.csv"
	dbms = csv
	replace;
run;

/*========================================
  SAMPLE DATASETS FOR GITHUB
  Sample whole CRLs (preserves structure)
========================================*/

/* Step 1: Get unique CRL IDs */
proc sort data=sasdata.crl_long(keep=event_id)
          out=event_ids nodupkey;
    by event_id;
run;

/* Step 2: Randomly select ~25 CRLs (you can change this) */
proc surveyselect data=event_ids
    out=sample_event_ids
    method=srs
    sampsize=25
    seed=430;
run;

/* Step 3: Keep ALL deficiency rows for those CRLs */
proc sql;
    create table sample_crl_long as
    select a.*
    from sasdata.crl_long as a
    inner join sample_event_ids as b
        on a.event_id = b.event_id;
quit;

/* Optional: sort for readability */
proc sort data=sample_crl_long;
    by event_id deficiency_domain deficiency_subtype;
run;

/* Step 4: Export long dataset */
proc export data=sample_crl_long
    outfile="C:\Users\Hannah Weber\Desktop\CRL Project\sample_crl_long.csv"
    dbms=csv
    replace;
run;


/* Step 5: Keep matching domain-level rows */
proc sql;
    create table sample_domain_unique as
    select a.*
    from sasdata.domain_unique as a
    inner join sample_event_ids as b
        on a.event_id = b.event_id;
quit;

/* Optional: sort */
proc sort data=sample_domain_unique;
    by event_id deficiency_domain;
run;

/* Step 6: Export domain dataset */
proc export data=sample_domain_unique
    outfile="C:\Users\Hannah Weber\Desktop\CRL Project\sample_domain_unique.csv"
    dbms=csv
    replace;
run;

/*========================
  3. Cooccurence Prep
=======================*/
ods pdf file = "CRL Co-Occurrence Supplement.pdf";
*Domain CoOccurence;
data domain_flags; 
	set sasdata.domain_unique; 

	cmc = (deficiency_domain = "CMC"); 
	clinical = (deficiency_domain = "Clinical"); 
	regulatory = (deficiency_domain = "Regulatory"); 
	nonclinical = (deficiency_domain = "Nonclinical"); 
	stats = (deficiency_domain = "Statistics"); 
	patent = (deficiency_domain = "Patent Certifications"); 
run; 

proc sort data = domain_flags; 
	by event_id; 
run; 

data domain_event; 
	set domain_flags; 
	by event_id; 
	retain cmc_f clinical_f regulatory_f nonclinical_f stats_f patent_f; 

	if first.event_id then do; 
		cmc_f = 0; 
		clinical_f = 0; 
		regulatory_f = 0; 
		nonclinical_f = 0; 
		stats_f = 0; 
		patent_f = 0; 
	end; 

	if cmc then cmc_f = 1; 
	if clinical then clinical_f = 1; 
	if nonclinical then nonclinical_f = 1; 
	if regulatory then regulatory_f = 1; 
	if stats then stats_f = 1; 
	if patent then patent_f = 1; 
	if last.event_id then output; 

	keep event_id cmc_f clinical_f regulatory_f nonclinical_f stats_f patent_f; 
run; *might not be all of the possible pairs; 

title "Pairwise Domain Co-Occurrence";
proc freq data = domain_event; 
	tables cmc_f*clinical_f 
		   cmc_f*regulatory_f 
		   clinical_f*regulatory_f
		   cmc_f*nonclinical_f / nopercent norow nocol; 
run; 
title;
data domain_combo; 
	set domain_event; 
	length pattern $100; 
	pattern = catx(', ', 
		ifc(cmc_f, 'CMC',''), 
		ifc(Clinical_f, 'Clinical',''), 
		ifc(Nonclinical_f, 'Nonclinical',''), 
		ifc(regulatory_f, 'Regulatory',''), 
		ifc(stats_f, 'Statistical',''), 
		ifc(patent_f, 'Patent','') ); 
run;

title "Full Domain Co-Occurrence Patterns";
proc freq data = domain_combo order=freq;
    tables pattern / out  = domain_pattern_counts;
run;
title;
data sasdata.domain_top;
    set domain_pattern_counts;
    where count >= 5;
run;
*CMC COOCCURENCE;
data cmc_only;
    set sasdata.crl_long;
    where deficiency_domain = "CMC";
run;

data cmc_flags;
    set cmc_only;

    facility_f      = (deficiency_subtype = "Facility");
    specs_f         = (deficiency_subtype = "Specifications");
    mfgctrl_f       = (deficiency_subtype = "Manufacturing Controls");
    redacted_f      = (deficiency_subtype = "Redacted");
    stability_f     = (deficiency_subtype = "Stability");
    microbio_f      = (deficiency_subtype = "Microbiology");
    biopharm_f      = (deficiency_subtype = "Biopharmaceutics");
run;

proc sort data=cmc_flags;
    by event_id;
run;

data cmc_event;
    set cmc_flags;
    by event_id;

    retain facility specs mfgctrl redacted stability microbio biopharm;

    if first.event_id then do;
        facility  = 0;
        specs     = 0;
        mfgctrl   = 0;
        redacted  = 0;
        stability = 0;
        microbio  = 0;
        biopharm  = 0;
    end;

    if facility_f  then facility  = 1;
    if specs_f     then specs     = 1;
    if mfgctrl_f   then mfgctrl   = 1;
    if redacted_f  then redacted  = 1;
    if stability_f then stability = 1;
    if microbio_f  then microbio  = 1;
    if biopharm_f  then biopharm  = 1;
    if last.event_id then output;

    keep event_id facility specs mfgctrl redacted stability microbio biopharm;
run;
data cmc_combo;
    set cmc_event;
    length cmc_pattern $150;

    cmc_pattern = catx(', ',
        ifc(facility=1,  'Facility', ''),
        ifc(specs=1,     'Specifications', ''),
        ifc(mfgctrl=1,   'Manufacturing Controls', ''),
        ifc(redacted=1,  'Redacted', ''),
        ifc(stability=1, 'Stability', ''),
        ifc(microbio=1,  'Microbiology', ''),
        ifc(biopharm=1,  'Biopharmaceutics', '')
    );
run;

title "Pairwise CMC Subtype Co-Occurrence";
proc freq data=cmc_event;
    tables facility*specs
           facility*mfgctrl
           specs*mfgctrl
           facility*stability
           mfgctrl*stability
           facility*microbio / nopercent norow nocol;
run;
title;

title "Full CMC Subtype Patterns";
proc freq data=cmc_combo order=freq;
    tables cmc_pattern / out = cmc_pattern_counts;
run;
title;


data sasdata.cmc_top;
    set cmc_pattern_counts;
    where count >= 5;   /* keep only meaningful patterns */
run;
ods pdf close;
*Regression Model;
proc sort data=sasdata.crl_header out=header_unique nodupkey;
    by event_id;
run;

data sasdata.domain_event_model;
    merge domain_event (in=a)
          header_unique (keep=event_id application_type);
    by event_id;
    if a;
run;

/*========================
  4. FINAL PDF RESULTS
=======================*/
ods noproctitle;
ODS PDF FILE="CRL Primary Results.pdf";

title "Primary Drivers of Complete Response Letters";
/* Overall primary driver */
title2 "Distribution of Deficiency Domains";
PROC FREQ DATA=sasdata.domain_unique ORDER=FREQ;
    TABLES deficiency_domain / MISSING;
RUN;
title2;

/* By application type */
title2 "Deficiency Domains by Application Type (NDA vs BLA)";
PROC FREQ DATA=sasdata.domain_unique ORDER=FREQ;
    TABLES application_type*deficiency_domain / CHISQ nocol nopercent;
RUN;
title2;

* Clinical Domain by Subtype;
ods pdf startpage = now;
title2 "Clinical Deficiency Subtypes";
PROC FREQ DATA=sasdata.crl_long ORDER=FREQ;
	where deficiency_domain = 'Clinical';
    TABLES deficiency_domain*deficiency_subtype / nocol nopercent;
RUN;
title2;

* CMC Domain by Subtype;
ods pdf startpage = no;
title2 "CMC Deficiency Subtypes";
PROC FREQ DATA=sasdata.crl_long ORDER=FREQ;
	where deficiency_domain = 'CMC';
    TABLES deficiency_domain*deficiency_subtype / nocol nopercent;
RUN;
title2;

* Nonclinical Domain by Subtype;
title2 "Nonclinical Deficiency Subtypes";
PROC FREQ DATA=sasdata.crl_long ORDER=FREQ;
	where deficiency_domain = 'Nonclinical';
    TABLES deficiency_domain*deficiency_subtype / nocol nopercent;
RUN;
title2;

* Regulatory Domain by Subtype;
title2 "Regulatory Deficiency Subtypes";
PROC FREQ DATA=sasdata.crl_long ORDER=FREQ;
	where deficiency_domain = 'Regulatory';
    TABLES deficiency_domain*deficiency_subtype / nocol nopercent;
RUN;
title2;

* Domain Co-Occurence Pattern;
ods pdf startpage = now;
title2 "Co-occurrence of Deficiency Domains";
proc print data=sasdata.domain_top noobs;
    var pattern count percent;
run;
title2;

* Subtype Co-Occurence Pattern;
title2 "Top CMC Subtype Co-occurrence Patterns";
proc print data = sasdata.cmc_top noobs;
run;
title;
title2;

title2 "Logistic Model of Domain Event";
proc logistic data=sasdata.domain_event_model;
    class application_type (ref='NDA');
    model cmc_f(event='1') = application_type;
    title "Logistic Regression: Predicting CMC Deficiencies";
run;
title2;

/*Summary:
This analysis identifies the most common deficiency domains and subtypes cited in CRLs,
examines how deficiencies co-occur within applications, and evaluates patterns across
application types.

Findings emphasize the dominant role of CMC deficiencies and highlight that failures
often reflect systemic issues rather than isolated deficiencies.*/

ods pdf close;

ods pdf file="CRL Domain Supplement.pdf";
/*Exploratory analyses: regulatory pathway, office, and year.
These were evaluated for potential patterns but are not included in the final PDF
due to sparse cell counts, limited interpretability, and lack of contribution to the 
primary research question (drivers of CRLs).*/
/* By Reg Pathway:*/
title "Deficiency Domain by Regulatory Pathway";
PROC FREQ DATA=sasdata.domain_unique ORDER=FREQ;
    TABLES regulatory_pathway*deficiency_domain / CHISQ;
RUN;
title;

/*by office*/
title "Deficiency Domain by Office";
PROC FREQ DATA=sasdata.domain_unique ORDER=FREQ;
    TABLES office*deficiency_domain / CHISQ;
RUN;

/* By Year */
title "Deficiency Domain by Year";
PROC FREQ DATA=sasdata.domain_unique ORDER=FREQ;
    TABLES year*deficiency_domain / CHISQ;
RUN;
title;
ods pdf close;

ods pdf file = "CRL Subtype Supplement.pdf";
/*They are not included in the final analysis because subtypes are domain-specific,
and aggregating them across domains reduces interpretability and obscures the 
hierarchical structure of deficiencies.*/
/*SUBTYPES*/
title "Deficiency Subtype Overview";
PROC FREQ DATA=sasdata.crl_long ORDER=FREQ;
    TABLES deficiency_subtype / MISSING;
RUN;
title;

/*This cross-tabulation is not included in the final results because it mixes subtypes
across domains, leading to difficult interpretation. Subtype analyses are instead
performed within each domain to preserve meaningful structure.*/
title "Deficiency Subtype by Application Type";
PROC FREQ DATA=sasdata.crl_long;
    TABLES application_type*deficiency_subtype / nocol nopercent;
RUN;
title;
ods pdf close;
ods listing;
quit;
