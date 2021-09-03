
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
        (care_home_type = "Yes" OR care_home_type = "No") AND 
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


    # define age (needed for population)
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

    # define all outcomes (numerators)
    diabetes=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
    ## liver disease 
    chronic_liver_disease=patients.with_these_clinical_events(
        chronic_liver_disease_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
    ## chronic heart disease
    chronic_cardiac_disease=patients.with_these_clinical_events(
        chronic_cardiac_disease_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
    ## chronic respiratory disease (excl asthma)
    chronic_respiratory_disease=patients.with_these_clinical_events(
        chronic_respiratory_disease_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
    ## stroke
    stroke=patients.with_these_clinical_events(
        stroke_codes,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
    ## dementia 
    dementia=patients.with_these_clinical_events(
        dementia,
        on_or_before="index_date",
        returning="binary_flag",
        return_expectations={"incidence" : 0.1},
    ),
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

    # define stratification groups 
        
    #care_home_type - binary 
    care_home_type=patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "Yes": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "Yes": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "Yes": "IsPotentialCareHome",
            "No": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"Yes": 0.30, "No": 0.70},},
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

    Measure(
        id="COMORB_cancer",
        numerator="cancer",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_dementia",
        numerator="dementia",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_stroke",
        numerator="stroke",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_chronic_liver_disease",
        numerator="chronic_liver_disease",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_chronic_cardiac_disease",
        numerator="chronic_cardiac_disease",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_chronic_respiratory_disease",
        numerator="chronic_respiratory_disease",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),
    Measure(
        id="COMORB_diabetes",
        numerator="diabetes",
        denominator="registered_at_start",
        group_by = ["care_home_type"],
    ),        
]
