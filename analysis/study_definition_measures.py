
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
    # select the study population
    index_date="2019-02-01",
    population=patients.satisfying(
        """
        (age >= 65) AND 
        is_registered_with_tpp 
        """,
        is_registered_with_tpp=patients.registered_with_one_practice_between(
          "first_day_of_month(index_date)", "last_day_of_month(index_date)"
        ),
    ),
    care_home=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "Y": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              OR LocationRequiresNursing='N'
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
            "category": {"ratios": {"Y": 0.20, "N": 0.80},},
        },
    ),
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
            "category": {"ratios": {"PC": 0.40, "PN": 0.30, "PS": 0.30},},
        },
    ),
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    ons_covid_death=patients.with_these_codes_on_death_certificate(
       covid_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=False,
       returning="date_of_death",
       date_format="YYYY-MM-DD",
       return_expectations={"date": {"earliest": "2020-02-01"},
                            "rate" : "exponential_increase"
                            }, 
    ),  
)

# Specify Measures to be calculated 

measures = [
    Measure(
        id="ons_covid_death",
        numerator="ons_covid_death",
        denominator="population",
        group_by="care_home",
    ),
]