
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

# Specify study definition 

# Import all codelists

from codelists import *

study = StudyDefinition(
    # configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "exponential_increase",
        "incidence" : 0.2
    },
    # set an index date (as starting point)
    index_date="2016-02-01",

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

    # define age (needed for population and stratification group)
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    # define stratification groups 
        
    # age groups 
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

    ## sex 
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
    primis_carehome_ever=patients.with_these_clinical_events(
        primis_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence": 0.1},
    ),

    #### has any value for the above 
    any_care_home=patients.satisfying(
        """
        care_home_type = "Y" OR 
        primis_carehome_ever
        """,
    ),
)

measures = [

    ## any care or nursing home, using any method

    ## 5-YEAR AGE BANDS FOR STANDARDISATION 
    Measure(
        id="SENS_covid_death_sex_age_five",
        numerator="ons_covid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "any_care_home"],  
    ),
    Measure(
        id="SENS_allcause_death_sex_age_five",
        numerator="ons_any_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "any_care_home"],  
    ),
    Measure(
        id="SENS_noncovid_death_sex_age_five",
        numerator="ons_noncovid_death",
        denominator="registered_at_start",
        group_by = ["sex", "ageband_five", "any_care_home"],  
    ),
]
