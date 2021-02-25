
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

    # define all outcomes (numerators)
    tpp_death=patients.with_death_recorded_in_primary_care(
       between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"], 
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

)

measures = [

    ## SENSITIVITY 2: tpp death as the outcome for comparison (all cause only)
    Measure(
        id="tpp_death_age",
        numerator="tpp_death",
        denominator="population",
        group_by = ["ageband_narrow", "care_home_type"],
    ),
]
