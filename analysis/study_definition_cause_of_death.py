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
        (care_home_type = "Y" OR care_home_type = "N") AND 
        ons_any_death 
        """,
        is_registered_with_tpp=patients.registered_as_of(
          "index_date"
        ),
    ),

    # identify those who died 
    ons_any_death=patients.died_from_any_cause(
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       returning="binary_flag",
       return_expectations={"incidence" : 1.0},
    ), 

    # define cause of death
    died_cause_ons=patients.died_from_any_cause(
        returning="underlying_cause_of_death",
        return_expectations={"category": {"ratios": {"U071":0.2, "I21":0.2, "C34":0.1, "C83":0.1 , "J09":0.05 , "J45":0.05 ,"G30":0.05, "A01":0.25}},},
    ),

    # define age (needed for defining the study population) 
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
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
)