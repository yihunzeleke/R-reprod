coin_sides <- c("head", "tail")
n_flips <- 10
# Sample from coin_sides n_flips times with replacement
sample(coin_sides,n_flips, replace = T)

# Update the function to return n coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, n_flips, replace = T)
}
toss_coin(10)

coin_sides <- c("head", "tail")
n_flips <- 10
p_head <- 0.8

# Define a vector of weights
weights <- 0.2

# Update so that heads are sampled with prob p_head
sample(coin_sides, n_flips, replace = TRUE, prob=c(p_head, weights))

toss_coin_prob <- function(n_flips){
  coin_sides <- c("head", "tail")
  n_flips <- 10
  p_head <- 0.8
  
  # Define a vector of weights
  weights <- 0.2
  
  # Update so that heads are sampled with prob p_head
  sample(coin_sides, n_flips, replace = TRUE, prob=c(p_head, weights))
}
toss_coin_prob(10)


# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1 - p_head)
  # Modify the sampling to be weighted 
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, p_head = 0.8)

# writing custom glm model
# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}


# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  arrange(desc(predicted_n_visits))


# Set the default for na.rm to FALSE
cut_by_quantile <- function(x, n = 5, na.rm=FALSE, labels, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the na.rm argument from the call
cut_by_quantile(
  n_visits, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)

library(devtools)
install_bitbucket("richierocks/assertive")
library(assertive)
library(assertive.code)
library(assertive.base)

calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  # Check if any values of x are non-positive
  if(any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it negative numbers
calc_harmonic_mean(mtcars$mpg - 20)


# Update the function definition to fix the na.rm argument
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)) {
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  # Use the first value of na.rm, and coerce to logical
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it malformed na.rm
calc_harmonic_mean(mtcars$mpg, na.rm = 1:5)



is_leap_year <- function(year,n) {
  # If year is div. by 400 return TRUE
  if(is_divisible_by(year,400)) {
    return(TRUE)
  }
  # If year is div. by 100 return FALSE
  if(is_divisible_by(year,100) ) {
    return(FALSE)
  }  
  # If year is div. by 4 return TRUE
  if(is_divisible_by(year,4))
    return(TRUE)
  
  # Otherwise return FALSE
  
  return(FALSE)
}

is_leap_year(year = 2004)


# ----------------------- -------------------
kcmo_death <- kcmo_death %>% 
mutate(causeOfDeath_ICD10 = case_when(
  dthcause %in%c("A040","A041", "A042", "A043", "A044", "A045", "A046", "A047", "A048","A049") ~ "Other Bacterial Intestinal Infection",
  dthcause%in%c("A080","A081", "A082", "A083", "A084", "A085") & !(dthcause %in%c("J108", "J118"))~ "Viral and other specified intestinal infections",
  dthcause%in%c("A090", "A099") & !(dthcause%in%c("A000","A001", "A009", "A010", "A011", "A012", "A013", "A014", "A020", "A021", "A022", "A028","A029","A030", "A031", "A032", "A033", "A034","A038","A039","A040","A041","A042", "A043", "A044", "A045", "A046", "A047", "A048", "A049", "A050","A051", "A052", "A053", "A054", "A055", "A056", "A057", "A058","A059", "A060","A061", "A062", "A063", "A064", "A065", "A066","A071","A072", "A073", "A074","A067","A068","A069","A070","A075", "A076", "A077", "A078", "A079", "K529", "P783")) ~ "Diarrhoea and gastroenteritis of presumed infectious origin",
  dthcause%in%c("A160", "A161", "A162", "A163", "A164", "A165", "A166", "A167", "A168", "A169") ~ "Respiratory tuberculosis, not confirmed bacteriologically",
  dthcause %in%c("A310", "A311","A318","A319")& !(dthcause%in%c("A160", "A161", "A162", "A163", "A164", "A165", "A166", "A167", "A168", "A169", "A150", "A151", "A152", "A153", "A154", "A155", "A156", "A157", "A158", "A159","A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179","A180", "A181", "A182", "A183", "A184", "A185", "A186", "A187", "A188", "A189","A190", "A191", "A192", "A193", "A194", "A195", "A196", "A197", "A198", "A199","A300", "A301","A302","A303","A304","A305","A306","A307","A308","A309")) ~ "Infection due to other mycobacteria",
  dthcause %in%c("A320", "A321","A327","A328","A329") ~ "Listeriosis",
  dthcause %in%c("A390","A391", "A392","A393","A394", "A395","A398","A399") ~ "Meningococcal infection",
  dthcause %in%c("A400", "A401","A402", "A403", "A404","A405", "A406", "A407","A408", "A409") & !(dthcause%in%c("O753","O030", "O03", "O04","O040", "O05","O050", "O06","O060", "O070","O071", "O072","O073","O074","O075","O076","O077","O078","O079","O080", "T880", "T802", "P360","P361","T841", "O85","O850")) ~ "Streptococcal septicaemia",
  dthcause %in%c("A480", "A481","A482", "A483", "A484","A488") & !(dthcause %in%c("B471")) ~ "Other bacterial diseases, not elsewhere classified",
  dthcause %in% c("A410", "A411","A412", "A413", "A414","A415","A418", "A419")& !(dthcause%in%c("O753","A499", "O03", "O04", "O05", "O06", "O030", "O040","O050","O060", "O070","O071", "O072","O073","O074","O075","O076","O077","O078", "O079","O080","T880", "T802","A427", "A227","B377", "A267", "A282", "A548","B007", "A327", "A392","A393","A394", "P360", "P361","P362", "P363","P364","P365","P366", "P367","P368","P369", "T814", "O85","O850","A400", "A401","A402", "A403", "A404","A405", "A406", "A407","A408", "A409", "A217","A241","A207","A483")) ~ "Other septicaemia",
 dthcause %in%c("A480","A481", "A482","A483", "A484","A488") & !(dthcause%in%c("B471","R578", "A419")) ~ "Other bacterial diseases, not elsewhere classified",
 str_detect(dthcause, "^A49") & !(dthcause %in%c("A749","A399","A799","A699", "B950","B951","B952","B953","B954","B955","B956","B957","B958","B960","B961","B962","B963","B964","B965","B966","B967","B968")) ~ "Bacterial infection of unspecified site",
 str_detect(dthcause, "^A77") ~ "Spotted fever [tick-borbe rickettsioses]",
 str_detect(dthcause, "^A81") ~ "Slow virus infections of central nervous system",
 str_detect(dthcause, "^A86") ~ "Unspecified viral encephalitis",
 str_detect(dthcause, "^B00") & !(str_detect(dthcause, "^A60"))  ~ "Herpesviral[herpes simplex] infections",
 str_detect(dthcause, "^B01") ~ "Varicella[chickenpox]",
 str_detect(dthcause, "^B02") ~ "Zoster [herpes zoster]",
 str_detect(dthcause, "^B15") ~ "Acute hepatitis A",
 str_detect(dthcause,"^B16") ~ "Acute hepatitis B",
 str_detect(dthcause,"^B17") ~ "Other acute viral hepatitis",
 str_detect(dthcause,"^B18") ~ "Chronic vjiral hepatitis",
 str_detect(dthcause,"^B19") ~ "Unspecified viral hepatitis",
 str_detect(dthcause,"^B20") & !(dthcause %in% "B230") ~ "Human immunodeficiency virus [HIV]",
 str_detect(dthcause,"^B21") ~ "HIV disease resulting in malignant neoplasms",
 str_detect(dthcause,"^B22") ~ "HIV disease resulting in other specified diseases",
 str_detect(dthcause,"^B23") ~ "HIV disease resulting in other conditions",
 str_detect(dthcause, "^B24") ~ "Unspecified HIV disease",
 str_detect(dthcause, "^B25") ~ "Cytomegaloviral disease",
 str_detect(dthcause, "^B27") ~ "Infectious mononucleosis",
 str_detect(dthcause, "^B33") ~ "Other viral diseases not elsewhere classified",
 str_detect(dthcause, "^B34") & !(dthcause %in%c("B259","B009", "B333","B970", "B971","B970", "B972","B973", "B974","B975", "B976" ,"B977", "B978", "B979")) ~ "Viral infection of unspecified site"
  ),
 str_detect(dthcause, "^B")
 Disease_SubCategory = case_when(
   str_detect(dthcause, "^A0") ~ "Intestinal infectious diseases",
   str_detect(dthcause, "^A1") ~ "Tuberculosis",
   str_detect(dthcause, "^A2") ~ "Certain zoonotic bacterial diseases",
   str_detect(dthcause, "^A3|^A4") ~ "Other bacterial diseases",
   str_detect(dthcause, "^B20|^B24") ~ "HIV disease")
 )

  
