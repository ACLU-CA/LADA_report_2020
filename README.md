# LADA_report_2020

This repository provides all the information necessary to replicate all the analyses performed for the ACLU of Northern California's paper [(In)Justice In LA: An Analysis of the Los Angeles County District Attorney’s Office  & Recommendations for Justice Reform](https://meetyourda.org/reports/lada/5573/). Inquiries should be sent to jwilliams@aclunc.org

On May 13, 2019, the ACLU of Northern California sent a Public Records Act request to the Los Angeles District Attorney’s Office. The request asked for, among other things:

> Records of prosecution data within your possession for calendar year 2017 and 2018, including but not limited to:
>   * Unique identifiers for each person, charges, and outcomes for all minors (any persons under the age of 18) prosecuted directly in adult court in Los Angeles County (adult court is defined as a court of criminal jurisdiction) (otherwise known as “pipeline” or “direct file” cases) under Welfare and Institutions Code section 707.
>     * Unique identifiers for each person, charges, and outcomes for all minors prosecuted in adult court in Los Angeles County after any one of the following:
>       * a judicial certification to adult court following a juvenile transfer hearing under the newly amended Welfare and Institutions Code section 707 subsection (a);
>       * a juvenile defendant’s waiver of transfer hearing or stipulation to adult court following the District Attorney’s motion to transfer to adult court.
>   * Unique case identifiers, charges, and outcomes for all minors prosecuted in juvenile court in Los Angeles county, including, but not limited to demographic data, charges filed, and case outcomes during the calendar year of 2017 and 2018.
>   * Unique case identifiers, charges, and outcomes (including diversion) of all misdemeanor charges for minors and adults in Los Angeles county.
>   * Unique case identifiers, charges, enhancements and outcomes (including diversion) of all felony charges for minors and adults in Los Angeles county.

In response to this portion of the request, the Los Angeles District Attorney's office provided an Excel file, `20191114-ACLU_New.xlsx`.

la_data.R takes that Excel spreadsheet, cleans it and adds information necessary to conduct the analyses described in the paper. It produces a dataframe called `data`.

la_paper_script.R uses the dataframe `data` which is created by la_data.R and carries out all the analyses from the paper. To make finding the different analyses from the paper easier, a snippet of quote from the paper is provided in a comment before the analysis code, and the script is broken down into the different sections in the same way the paper is organized.

