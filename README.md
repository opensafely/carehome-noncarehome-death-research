# Mortality among Care Home Residents in England during the first and second waves of the COVID-19 pandemic: an analysis of 4.3 million adults over the age of 65

This is the code and configuration for our paper, "Mortality among Care Home Residents in England during the first and second waves of the COVID-19 pandemic: an analysis of 4.3 million adults over the age of 65". 

The paper is pre-printed and under review with a journal. Open an issue or email me at anna.schultze@lshtm.ac.uk for comments, suggestions or questions.  

* A link to the paper will be put here once our pre-print has gone live. 
* Raw model outputs, including charts, crosstabs, etc, are in `released_analysis_results/`
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the code should review
[DEVELOPERS.md](./docs/DEVELOPERS.md).

# About the OpenSAFELY framework

The OpenSAFELY framework is a new secure analytics platform for
electronic health records research in the NHS.

Instead of requesting access for slices of patient data and
transporting them elsewhere for analysis, the framework supports
developing analytics against dummy data, and then running against the
real data *within the same infrastructure that the data is stored*.
Read more at [OpenSAFELY.org](https://opensafely.org).

The framework is under fast, active development to support rapid
analytics relating to COVID19; we're currently seeking funding to make
it easier for outside collaborators to work with our system.  You can
read our current roadmap [here](ROADMAP.md).
