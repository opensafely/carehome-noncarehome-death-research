
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

    # define denominator for rates (mid point of time interval of interest - month initially)
    population=patients.satisfying(
        """
        (age >= 65) AND 
        is_registered_with_tpp 
        """,
        is_registered_with_tpp=patients.registered_as_of(
          "index_date + 14 days"
        ),
    ),

    # define all outcomes (numerators)
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

    # define age (needed for population and stratification group)
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    # define stratification groups 
        
    # define an indicator variable for all patients (needed as input for measures framework )
    allpatients=patients.satisfying("""age>=0""", return_expectations={"incidence": 1}
    ),
    # age groups 
    ageband = patients.categorised_as(
        {
            "0": "DEFAULT",
            "18-49": """ age >= 18 AND age < 50""",
            "50-59": """ age >=  50 AND age < 60""",
            "60-69": """ age >=  60 AND age < 70""",
            "70-79": """ age >=  70 AND age < 80""",
            "80+": """ age >=  80 AND age < 120""",
        },
        return_expectations={
            "rate":"universal",
            "category": {"ratios": {"18-49": 0.5, "50-59": 0.2, "60-69": 0.1, "70-79":0.1, "80+":0.1 }}
        },
    ),
    #gender groups 
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),

)

# Specify mortality rates to be calculated 

measures = [
    Measure(
        id="covid_death_all",
        numerator="ons_covid_death",
        denominator="population",
        group_by = "allpatients", 
    ),
    Measure(
        id="covid_death_sex",
        numerator="ons_covid_death",
        denominator="population",
        group_by = "sex", 
    ),
    Measure(
        id="covid_death_age",
        numerator="ons_covid_death",
        denominator="population",
        group_by = "ageband", 
    ),
    Measure(
        id="allcause_death_all",
        numerator="ons_any_death",
        denominator="population",
        group_by = "allpatients", 
    ),
    Measure(
        id="allcause_death_sex",
        numerator="ons_any_death",
        denominator="population",
        group_by = "sex", 
    ),
    Measure(
        id="allcause_death_age",
        numerator="ons_any_death",
        denominator="population",
        group_by = "ageband", 
    ),
]