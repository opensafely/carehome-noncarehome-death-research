
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
    ons_any_death=patients.died_from_any_cause(
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
    ), 
    ons_covid_death=patients.with_these_codes_on_death_certificate(
       covid_codelist,
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
       match_only_underlying_cause=False,
       returning="binary_flag",
       return_expectations={"incidence" : 0.1},
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
)

# Specify measures to be calculated 

measures = [
    Measure(
        id="ons_covid_death",
        numerator="ons_covid_death",
        denominator="population",
    ),
]