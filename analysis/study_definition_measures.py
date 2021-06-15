
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

# Specify study definition 

study = StudyDefinition(
    # configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "exponential_increase",
        "incidence" : 0.2
    },
    # set an index date (as starting point)
    index_date="2019-02-01",

    # extract source population from which the numerator and denominator for deaths will be constructed 
    # this should be all deaths during an interval, divided by those at midpoint, so need to extract both these quantities separately 
    population=patients.satisfying(
        """
        (age >= 65 AND age < 120) AND 
        (sex = "M" OR sex = "F") AND 
        (care_home_type = "Y" OR care_home_type = "N") AND 
        (registered_at_start)
        """,
    ),
    
    # create registration date at start of interval
    registered_at_start=patients.registered_as_of(
          "index_date"
        ),

    # create registration date at mid-point of interval 
    registered_at_midpoint=patients.registered_as_of(
          "index_date + 14 days"
        ), 

    # define all outcomes (numerators)
    ons_any_death=patients.died_from_any_cause(
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       returning="binary_flag",
       return_expectations={"incidence" : 0.3},
    ), 
    ons_covid_death=patients.with_these_codes_on_death_certificate(
       covid_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=False,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ), 
    ons_noncovid_death=patients.satisfying(
        """(NOT ons_covid_death) AND ons_any_death""",
        return_expectations={"incidence": 0.15},
    ),
    tpp_death=patients.with_death_recorded_in_primary_care(
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ), 

    # define cause of death
    died_cause_ons=patients.died_from_any_cause(
        returning="underlying_cause_of_death",
        return_expectations={"category": {"ratios": {"U071":0.2, "I21":0.2, "C34":0.1, "C83":0.1 , "J09":0.05 , "J45.1":0.05 ,"G30":0.05, "A01.2":0.25}},},
    ),

    # causes of death as outcomes (sensitivity)
    ons_dementia_death=patients.with_these_codes_on_death_certificate(
       dementia_death_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=True,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ), 
    ons_cv_death=patients.with_these_codes_on_death_certificate(
       circulatory_death_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=True,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ),
    ons_respiratory_death=patients.with_these_codes_on_death_certificate(
       respiratory_death_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=True,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ),  
    ons_cancer_death=patients.with_these_codes_on_death_certificate(
       cancer_death_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=True,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ),
    # sensitivity outcomes
    tested_covid=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="any",
        between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
        returning="binary_flag", 
        return_expectations={"incidence" : 0.1},
    ),
    admitted_covid=patients.admitted_to_hospital(
        returning="binary_flag",
        with_these_diagnoses=covid_codelist,
        between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
        return_expectations={"incidence" : 0.1},
    ),
    admitted_any=patients.admitted_to_hospital(
        returning="binary_flag",
        between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
        return_expectations={"incidence" : 0.1},
    ),

    # define age (needed for population and stratification group)
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    # define stratification groups 
        
    # define an indicator variable for all patients within this cohort (needed as input for measures framework )
    allpatients=patients.satisfying("""age>=0""", return_expectations={"incidence": 1}
    ),
    # age groups 
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
    ageband_five = patients.categorised_as(
        {   
            "0": "DEFAULT",
            "65-69": """ age >=  65 AND age < 70""",
            "70-74": """ age >=  70 AND age < 75""",
            "75-79": """ age >=  75 AND age < 80""",
            "80-84": """ age >=  80 AND age < 85""",
            "85-89": """ age >=  85 AND age < 90""",
            "90+": """ age >=  90 AND age < 120""",
        },
        return_expectations={
            "rate":"universal",
            "category": {"ratios": {"65-69": 0.2,"70-74": 0.2, "75-79": 0.2, "80-84":0.2, "85-89":0.1, "90+":0.1 }}
        },
    ),
    #gender groups 
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    #care_home_type - binary 
    care_home_type=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "Y": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "Y": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "Y": "IsPotentialCareHome",
            "N": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"Y": 0.30, "N": 0.70},},
        },
    ),
    #care_home_type - specific institution 
    care_home_detail=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "Care_Home": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "Nursing_Home": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "Care_Or_Nursing": "IsPotentialCareHome",
            "Private_Home": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"Care_Home": 0.10, "Nursing_Home": 0.10, "Care_Or_Nursing": 0.20, "Private_Home": 0.60},},
        },
    ),
)


measures = [

    ## PRIMARY analysis: grouped care or nursing home 

    # covid death
    Measure(
        id="covid_death_age",
        numerator="ons_covid_death",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    # all-cause death
    Measure(
        id="allcause_death_age",
        numerator="ons_any_death",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    # Non covid death
    Measure(
        id="noncovid_death_age",
        numerator="ons_noncovid_death",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),

    ## 5-YEAR AGE BANDS FOR STANDARDISATION 
    Measure(
        id="covid_death_sex_age_five",
        numerator="ons_covid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),
    Measure(
        id="allcause_death_sex_age_five",
        numerator="ons_any_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),
    Measure(
        id="noncovid_death_sex_age_five",
        numerator="ons_noncovid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),

    ## SENSITIVITY: stratified by care or nursing home for completion 

    # covid death
    Measure(
        id="covid_death_age_chdetail",
        numerator="ons_covid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),
    # all-cause death
    Measure(
        id="allcause_death_age_chdetail",
        numerator="ons_any_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),
    # Non covid death
    Measure(
        id="noncovid_death_age_chdetail",
        numerator="ons_noncovid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),

    ## SENSITIVITY: Display cause-specific deaths over time 
    Measure(
        id="dementia",
        numerator="ons_dementia_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),
    Measure(
        id="respiratory",
        numerator="ons_respiratory_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),
    Measure(
        id="cv",
        numerator="ons_cv_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),
    Measure(
        id="cancer",
        numerator="ons_cancer_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),

    ## SENSITIVITY: Hospital admissions and COVID-19 testing in care home compared to non care home residents 

    ### for standardisation 
    Measure(
        id="tested_covid",
        numerator="tested_covid",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),
    Measure(
        id="admitted_covid",
        numerator="admitted_covid",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),
    Measure(
        id="admitted_any",
        numerator="admitted_any",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "care_home_type"],
    ),

    ### for stratification by age 
    Measure(
        id="tested_covid_age",
        numerator="tested_covid",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    Measure(
        id="admitted_covid_age",
        numerator="admitted_covid",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    Measure(
        id="admitted_any_age",
        numerator="admitted_any",
        denominator="registered_at_start",
        group_by = ["ageband_narrow", "care_home_type"],
    ),

]
