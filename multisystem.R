#Need to load datasets first. The code works on the dataset swetrau.
# change to combined set to include OFI status etc.


####
# Identify serious cases
####

is_serious <- function(code) {
  severity <- substr(as.character(code), 8, 8)
  as.numeric(severity) >= 3  # if severity level is 3 or higher, the injury is considered serious
}

####
# Identify distinct regions
####

# Expects a vector of codes
number_of_regions <- function(code) {
  region <- substr(as.character(code), 1, 1)
  length(unique(region))
}

###
# Putting them together
###

has_more_than_one_serious_injury <- function(code) {
  code <- code[!is.na(code)]
  serious_injury <- is_serious(code)
  number_of_regions(code[serious_injury]) >= 2
}

###
# Apply on dataset
###

# Would be better to use the apply family but couldent get it to work
# Creates column blunt_multisystem with TRUE/FALSE 

swetrau$blunt_multisystem <- NA
swetrau2 <- swetrau[!is.na(swetrau$inj_dominant), ] #Need to remove not known in inj_dominant

for (i in 1:nrow(swetrau2)) {
  v = 0+i
  if (has_more_than_one_serious_injury(swetrau2[v,grepl( "AISCode" , names(swetrau))]) == TRUE && swetrau2[v,"inj_dominant"] == 1) {
    swetrau2[v,"blunt_multisystem"] <- TRUE
  } else {
    swetrau2[v,"blunt_multisystem"] <- FALSE
  }
}
