
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

    # define denominator for rates 
    population=patients.satisfying(
        """
        (age >= 65 AND age < 120) AND 
        is_registered_with_tpp AND 
        (sex = "M" OR sex = "F") AND 
        (care_home_type = "Y" OR care_home_type = "N")
        """,
        is_registered_with_tpp=patients.registered_as_of(
          "index_date"
        ),
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
        denominator="population",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    # all-cause death
    Measure(
        id="allcause_death_age",
        numerator="ons_any_death",
        denominator="population",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
    # Non covid death
    Measure(
        id="noncovid_death_age",
        numerator="ons_noncovid_death",
        denominator="population",
        group_by = ["ageband_narrow", "care_home_type"],
    ),

    ## 5-YEAR AGE BANDS FOR STANDARDISATION 
    Measure(
        id="covid_death_sex_age_five",
        numerator="ons_covid_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),
    Measure(
        id="allcause_death_sex_age_five",
        numerator="ons_any_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),
    Measure(
        id="noncovid_death_sex_age_five",
        numerator="ons_noncovid_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_type"],  
    ),

    ## SENSITIVITY 1: stratified by care or nursing home for completion 

    # covid death
    Measure(
        id="covid_death_age_chdetail",
        numerator="ons_covid_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),
    # all-cause death
    Measure(
        id="allcause_death_age_chdetail",
        numerator="ons_any_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),
    # Non covid death
    Measure(
        id="noncovid_death_age_chdetail",
        numerator="ons_noncovid_death",
        denominator="population",
        group_by = ["sex", "ageband_five", "care_home_detail"],
    ),

    ## SENSITIVITY 2: tpp death as the outcome for comparison (all cause only)
    Measure(
        id="tpp_death_age",
        numerator="tpp_death",
        denominator="population",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
]
