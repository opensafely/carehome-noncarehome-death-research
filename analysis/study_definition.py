
# STUDY DEFINITION FOR BASELINE CHARACTERISTICS 

# Import necessary functions

from cohortextractor import (
    StudyDefinition,
    patients,
    codelist_from_csv,
    codelist,
    filter_codes_by_category,
    combine_codelists,
    Measure
)

# Import all codelists

from codelists import *

# Specifiy study definition

study = StudyDefinition(
    # configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "exponential_increase",
        "incidence" : 0.2
    },
    # select the study population
    index_date="2019-02-01",
    
    population=patients.satisfying(
        """
        (age >= 65 AND age < 120) AND 
        is_registered_with_tpp AND 
        (sex = "M" OR sex = "F") AND 
        (care_home_type = "PC" OR care_home_type = "PN" OR care_home_type = "PS" OR care_home_type = "U") 
        """,
        is_registered_with_tpp=patients.registered_as_of(
          "index_date"
        ),
    ),
    # define and select variables 
    # ADMINISTRATIVE 

    ## deregistration (censor) date
    dereg_date=patients.date_deregistered_from_all_supported_practices(
        on_or_after="index_date", date_format="YYYY-MM",
    ),

    # HOUSEHOLD INFORMATION
    ## care home status 
    care_home_type=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "PC": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "PN": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "PS": "IsPotentialCareHome",
            "U": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"PC": 0.30, "PN": 0.10, "PS": 0.10, "U":0.5},},
        },
    ),

    care_home_prior=patients.care_home_status_as_of(
        "index_date - 1 months",
        categorised_as={
            "PC": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "PN": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "PS": "IsPotentialCareHome",
            "U": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"PC": 0.30, "PN": 0.10, "PS": 0.10, "U":0.5},},
        },
    ),

    # DEMOGRAPHICS  
    ## age 
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    ## age groups 
    ageband_narrow = patients.categorised_as(
        {   
            "0": "DEFAULT",
            "65-74": """ age >=  65 AND age < 75""",
            "75-79": """ age >=  75 AND age < 80""",
            "80-84": """ age >=  80 AND age < 85""",
            "85-89": """ age >=  85 AND age < 90""",
            "90+": """ age >=  90 AND age < 120""",
        },
        return_expectations={
            "rate":"universal",
            "category": {"ratios": {"65-74": 0.4, "75-79": 0.2, "80-84":0.2, "85-89":0.1, "90+":0.1 }}
        },
    ),
    ageband_broad = patients.categorised_as(
        {
            "0": "DEFAULT",
            "65-74": """ age >=  65 AND age < 75""",
            "75-84": """ age >=  75 AND age < 85""",
            "85+": """ age >=  85 AND age < 120""",
        },
        return_expectations={
            "rate":"universal",
            "category": {"ratios": {"65-74": 0.5, "75-84": 0.2, "85+": 0.3 }}
        },
    ),
    ## sex 
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    ## self-reported ethnicity 
    ethnicity=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=True,
        return_expectations={
            "category": {"ratios": {"1": 0.5, "2": 0.2, "3": 0.1, "4": 0.1, "5": 0.1}},
            "incidence": 0.75,
        },
    ),  
    # GEOGRAPHICAL VARIABLES 
    ## grouped region of the practice
    region=patients.registered_practice_as_of(
        "index_date",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2,
                },
            },
        },
    ),
    ## middle layer super output area (msoa) - nhs administrative region 
    msoa=patients.registered_practice_as_of(
        "index_date",
        returning="msoa_code",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}},
        },
    ), 
    ## patient living in rural or urban area
    rural_urban=patients.address_as_of(
        "index_date",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"5": 0.1, "8": 0.3, "1": 0.55, "-1": 0.05}},
        },
    ),
    ## index of multiple deprivation, estimate of SES based on patient post code 
	imd=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
        },
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19,
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19,
                }
            },
        },
    ),

  # CLINICAL COMORBIDITIES  
    ## cancer 
    # cancer
    cancer=patients.satisfying(
        "lung_cancer OR haem_cancer OR other_cancer",
        lung_cancer=patients.with_these_clinical_events(
        lung_cancer_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence": 0.10},
        ),
        haem_cancer=patients.with_these_clinical_events(
        haem_cancer_codes,
        on_or_before="index_date",
        return_expectations={"incidence": 0.05},
        ),
        other_cancer=patients.with_these_clinical_events(
        other_cancer_codes,
        on_or_before="index_date",
        return_expectations={"incidence": 0.10},
        ),

    ),

    ## variabels to define ckd 
    ### creatinine 
    creatinine=patients.with_these_clinical_events(
        creatinine_codes,
        find_last_match_in_period=True,
        between=["index_date - 1 year", "index_date"],
        returning="numeric_value",
        include_date_of_match=True,
        include_month=True,
        return_expectations={
            "float": {"distribution": "normal", "mean": 60.0, "stddev": 15},
            "date": {"earliest": "index_date - 1 year", "latest": "index_date"},
            "incidence": 0.95,
        },
    ),
    ### end stage renal disease codes incl. dialysis / transplant
    esrf=patients.with_these_clinical_events(
        esrf_codes,
        on_or_before="index_date",
        return_last_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    ## diabetes
    diabetes=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": "index_date"}},
    ),
    ## liver disease 
    chronic_liver_disease=patients.with_these_clinical_events(
        chronic_liver_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## chronic heart disease
    chronic_cardiac_disease=patients.with_these_clinical_events(
        chronic_cardiac_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## chronic respiratory disease (excl asthma)
    chronic_respiratory_disease=patients.with_these_clinical_events(
        chronic_respiratory_disease_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## stroke
    stroke=patients.with_these_clinical_events(
        stroke_codes,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),
    ## dementia 
    dementia=patients.with_these_clinical_events(
        dementia,
        on_or_before="index_date",
        return_first_date_in_period=True,
        include_month=True,
    ),

    ## varaibles to define flu vaccination status 
    ### flu vaccine in tpp
    flu_vaccine_tpp_table=patients.with_tpp_vaccination_record(
        target_disease_matches="INFLUENZA",
        between=["index_date - 6 months", "index_date"],  # current flu season
        find_first_match_in_period=True,
        returning="date",
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine entered as a medication 
    flu_vaccine_med=patients.with_these_medications(
        flu_med_codes,
        between=["index_date - 6 months", "index_date"],  # current flu season
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine as a read code 
    flu_vaccine_clinical=patients.with_these_clinical_events(
        flu_clinical_given_codes,
        ignore_days_where_these_codes_occur=flu_clinical_not_given_codes,
        between=["index_date - 6 months", "index_date"],  # current flu season
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),
    ### flu vaccine any of the above 
    flu_vaccine=patients.satisfying(
        """
        flu_vaccine_tpp_table OR
        flu_vaccine_med OR
        flu_vaccine_clinical
        """,
    ),

    # OUTCOMES 

    ## tpp death
    tpp_death_date=patients.with_death_recorded_in_primary_care(
        on_or_after="index_date",
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest": "2019-02-01"},
                             "rate" : "exponential_increase"
                            }, 
    ), 

    ## ons covid death 
    ons_covid_death_date=patients.with_these_codes_on_death_certificate(
       covid_codelist,
       on_or_after="index_date",
       match_only_underlying_cause=False,
       returning="date_of_death",
       date_format="YYYY-MM-DD",
       return_expectations={"date": {"earliest": "2019-02-01"},
                            "rate" : "exponential_increase"
                            }, 
    ),  

    ## ons death 
    ons_any_death_date=patients.died_from_any_cause(
       on_or_after="index_date",
       returning="date_of_death",
       date_format="YYYY-MM-DD",
       return_expectations={"date": {"earliest": "2019-02-01"},
                            "rate" : "exponential_increase"
                            }, 
    ),
) 