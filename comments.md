1. I show an alternative approach to identify the blunt multisystem cohort just under the current for loop. This approach uses the more R:ish apply function and is substantially faster than the for loop but produces exactly the same result. You can consider replacing the other for loops using a similar approach.
2. If I read it correctly the current code to identify severe TBI identifies severe isolated TBI, i.e. where the TBI is the only injury. Is that intentional?
3. You probably want an "all" column in table 1. To achieve this you could add an additional "cohort" that you call "all" before merging the cohorts. Also, without having tested, wouldn't you achieve the same thing by stacking the cohorts on top of each other, i.e by using for example `merged.cohorts <- do.call(rbind, list(geriatric_cohort, penetrating_cohort, blunt_multisystem_cohort, shock_cohort, severe_tbi_cohort))`? If you also include a complete cohort in that list you could then easily get an "All" column in your table 1.
4. Date can be converted to string to a date and time class using `fewer_variables$date <- as.Date(strptime(fewer_variables$Ankomst_te, format = "%Y%m%d %H:%M"))`
5. There are many ways to achieve the incidence calculation that you conduct per cohort, without having to repeat the code. This is one way that uses your code:

```{r}
cohorts <- list(geriatric_cohort = geriatric_cohort,
                penetrating_cohort = penetrating_cohort,
                blunt_multisystem_cohort = blunt_multisystem_cohort,
                shock_cohort = shock_cohort,
                severe_tbi_cohort = severe_tbi_cohort)
calculate_incidence <- function(cohort) {
    cohort$date <- as.Date(strptime(cohort$Ankomst_te, format = "%Y%m%d %H:%M"))
    cohort.ofi <- incidence(cohort$date, interval = "year", groups = cohort$OFI) %>% as.data.frame()
    cohort.ofi$incidence <- 0
    if (is.element("NO", names(cohort.ofi)))
        cohort.ofi$incidence <- cohort.ofi$YES / (cohort.ofi$YES + cohort.ofi$NO) * 100
    return (cohort.ofi)
}
incidence.data <- lapply(cohorts, calculate_incidence)
```

As you can see, for example by using `str(incidence.data)`, `incidence.data` is a list of 5 data.frames with the incidence data.
