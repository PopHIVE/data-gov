# Search Notes

The goal is to find nationwide data sets from unique sources (not republications of the same originating data set) that can be stratified by race, gender (or sex), and age with reports that span several years. These sources are then harmonized with one another for reasonable opportunities of cross-source comparisons in time-series plots.

Opioids constitute natural, semi-synthetic, and synthetic chemical materials and are referred to by several clinical or slang terms; opioid(s), heroin, fentanyl, oxycontin, oxycodone, codeine, hydrocodone, and morphine being some of the most common. Clinical incidences of opioid overdoses can arise from legal or illicit opioid consumption but differentiating them is not always possible. The most official sources will reference the opioid type by its predetermined International Classification of Diseases, Tenth Revision (ICD-10) code: 
- T40: Poisoning by adverse effect of and underdosing of narcotics and psychodysleptics hallucinogens ([ICD-10 T40 Descriptions](https://www.icd10data.com/ICD10CM/Codes/S00-T88/T36-T50/T40-))
- F11: Opioid related disorders with "intoxication” ([ICD-10 F11 Descriptions](https://www.icd10data.com/ICD10CM/Codes/F01-F99/F10-F19/F11-))

In 2015 the clinical ICD codes were changed from the Ninth Revision. This change in nomenclature was found to result in opioid use, abuse, and overdose trend differences that are not otherwise expected to be clinically significant even for broad, aggregated classes ([Heslin and Owens et al 2017](https://hcup-us.ahrq.gov/datainnovations/Opioid_trends_ICD_Med_Care.pdf)). This analysis is focused only on ICD-10 codes unless otherwise noted.

Opioids taken in conjunction with central nervous system depressants increases the risk of life-threatening overdose. Benzodiazepines in particular have constituted nearly 14% of overdose deaths involving opioids in 2021 ([Benzodiazepines and Opioids - National Institute of Drug Abuse](https://nida.nih.gov/research-topics/opioids/benzodiazepines-opioids)). Therefore the ICD-10 code T42.2 was included: Poisoning by, adverse effect of and underdosing of benzodiazepines ([ICD-10 T42 Descriptions](https://www.icd10data.com/ICD10CM/Codes/S00-T88/T36-T50/T42-)).

The Centers for Disease Control and Prevention (CDC) State Unintentional Drug Overdose Reporting System (SUDORS), Drug Overdose Surveillance and Epidemiology (DOSE), and Agency for Healthcare Research and Quality (AHRQ) sources have restricted export options that are preset. Because of this the CDC's Wide-ranging ONline Data for Epidemiologic Research (WONDER) export was set to provide as similar of information as was available through the API. There were limitations to how well the export could align with the other sources, as only ICD-10 codes were available where the other sources noted using ICD-9 (where applicable), ICD-10, Systematized Nomenclature of Medicine (SNOMED), and chief complaint free text fields to determine opioid related overdose events.

# Data Sources

## CDC SUDORS and DOSE

49 participating states and the District of Columbia collect and abstract data on drug overdose deaths for entry that is shared with the National Violent Death Reporting System (NVDRS). These are compiled by SUDORS and DOSE to support an understanding of the circumstances surrounding overdoses (fatal and non-fatal) and point to emerging polysubstance overdose trends.

SUDORS only includes overdoses that resulted in an unintentional or unknown intent overdose (ICD-10 codes X40–X44 and Y10–Y14). The location where the death occurred is not noted, and so all possible [places of deaths](https://wonder.cdc.gov/wonder/help/mcd-expanded.html#Place%20of%20Death) (as defined by the CDC WONDER webtool) is expected to be included. Drug overdoses are determined by death certificates and/or coroner or medical examiner reports that indicate opioid related drug toxicity. ICD-10 codes are not required as the deaths could have occured outside of an emergency department or inpatient care system ([APPENDIX D: SUDORS/NVDRS comparison pg. 143](https://www.cdc.gov/overdose-prevention/media/pdfs/SUDORS_Coding_Manual_OD2A_v6.3.pdf)).

DOSE reports non-fatal opioid overdoses. While it would be ideal to include these rates for easier comparison with the AHRQ data set, which includes fatal and non-fatal overdose events, the dashboard only provides percent changes. Because of this, it has been excluded from the analysis. Further description about SUDORS and DOSE can be found on their respective about pages: [About SUDORS]( https://www.cdc.gov/overdose-prevention/data-research/facts-stats/about-sudors.html) and [About DOSE]( https://www.cdc.gov/overdose-prevention/data-research/facts-stats/about-dose-system.html).

## AHRQ

AHRQ seeks to improve healthcare by facilitating evidence healthcare decisions through higher quality studies and data sharing. It produces its resources through collaboration with the U.S. Department of Health and Human Services and with the states, territories, Tribal nations, and private partners. Its Healthcare Cost and Utilization Project (HCUP) Fast Stats page includes an opioid overdose report: Quarterly and annual rates for Opioid-Related Hospital Use for all available States and settings of care ([Healthcare Cost and Utilization Project (HCUP) Fast Stats](https://datatools.ahrq.gov/hcup-fast-stats/#downloads)).

Starting October 1, 2015 key opioid ICD-10 codes included are: T40.0 (Opium), T40.1 (Heroin), T40.2 (Other opioids), T40.3 (Methadone), T40.4 (Other synthetic narcotics), T40.6 (Other and unspecified narcotics), and F11.0 (Mental and behavioural disorders due to use of opioids, acute intoxication). Note that F11.11, F11.21, and F11.91 are excluded. Opioid overdoses reported prioir to that date are noted using ICD-9 codes. These date ranges have been excluded due to challenges comparing rates between the coding systems (refer above).

## CDC WONDER

The CDC's WONDER online tool facilitates searching and access of public health information. The downloaded data includes stratification by state, age, gender, race, and year of observation from 1999-2020 and from 2018-2022. For easier comparison with race stratification with the SUDORS data set

Key opioid ICD-10 codes included are: T40.0 (Opium), T40.1 (Heroin), T40.2 (Other opioids), T40.3 (Methadone), T40.4 (Other synthetic narcotics), T40.6 (Other and unspecified narcotics), and F11.0 (Mental and behavioural disorders due to use of opioids, acute intoxication).



Two Multiple Cause of Death Data (Final) data sets are available for download: 1999-2020 and 2018-2022 ([Mortality Data on CDC WONDER](https://wonder.cdc.gov/mcd.html)). The former only includes bridged race categories (4 groups) while the later allows for single race categories (6 groups, 15 groups, or single/multiple 31 groups). For easier matching between the two, only 6 groups were used for the 2018-2022 data pull. Sub date ranges were downloaded at a time for manual joining due to data size limitations for downloading.

- Saved data search: [Multiple Cause of Death, 2018-2022](https://wonder.cdc.gov/controller/saved/D157/D419F705)
