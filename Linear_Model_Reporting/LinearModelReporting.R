# Summary of Regression / Linear Models as HTML Table

# Source: https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html#a-simple-html-table-from-regression-results

# tab_model() is the pendant to plot_model(), however, instead of creating 
# plots, tab_model() creates HTML-tables that will be displayed either in your 
# IDE's viewer-pane, in a web browser or in a knitr-markdown-document 

# HTML is the only output-format, you can't (directly) create a LaTex or PDF 
# output from tab_model() and related table-functions. However, it is possible
# to easily export the tables into Microsoft Word or Libre Office Writer.

# This vignette shows how to create table from regression models with tab_model(). 

# Note. Due to the custom CSS, the layout of the table inside a
# knitr-document differs from the output in the viewer-pane and web browser.

# Install packages in this order:
# sjlabelled -> sjmisc -> sjstats -> ggeffects -> sjPlot


# load packages
library(sjPlot)
library(sjmisc)
library(sjlabelled)

## sample data
data(efc)
efc <- as_factor(efc, c161sex, c172code)

# A simple HTML table from regression results
# First, we fit two linear models to demonstrate the tab_model()-function.

m1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
m2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + e17age, data = efc)

# The simplest way of producing the table output is by passing the fitted model
# as parameter. By default, estimates, confidence intervals (CI) and p-values 
# (p) are reported. As summary, the numbers of observations as well as the 
# R-squared values are shown.

summary(m1)
# compare summary to tab_model:
tab_model(m1)

# Automatic labelling
colnames(efc)
# columns look like quite unremarkable features, but look closely:
str(efc$c160age)
str(efc$c12hour)
# As the sjPlot-packages features labelled data, the coefficients in the table
# are already labelled in this example. The name of the dependent variable(s) 
# is used as main column header for each model. For non-labelled data, the 
# coefficient names are shown.

# Turn off automatic labelling
# To turn off automatic labelling, use auto.label = FALSE, or provide an empty
# character vector for pred.labels and dv.labels.

tab_model(m1, auto.label = FALSE)

# some categorical data are already sufficient
data(mtcars)
m.mtcars <- lm(mpg ~ cyl + hp + wt, data = mtcars)
tab_model(m.mtcars)
# but maybe you want to add details, you can do so manually. Note you need to 
# specify the intercept predictor as well in a linear model:
tab_model(m.mtcars,  
          pred.labels=c("(Intercept)", "Cylinders", "Horse Power", "Weight"))

# What to do about model intercept?
# You can forcibly remove the intercept, at which point, the intercept effect
# simply becomes encapsulated into one of the main categorical variables.
m1.0 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code - 1, data = efc)
tab_model(m1)
tab_model(m1.0)


# More than one model
# tab_model() can print multiple models at once, which are then printed 
# side-by-side. Identical predictor coefficients are matched in a row.
tab_model(m1, m2)


# Generalized linear models
# For generalized linear models, the ouput is slightly adapted. 
# Instead of Estimates, the column is named Odds Ratios, Incidence Rate Ratios
# etc., depending on the model. 
# The coefficients are, by default, automatically
# converted (exponentiated). Furthermore, pseudo R-squared statistics are 
# shown in the summary.

m3 <- glm(
  tot_sc_e ~ c160age + c12hour + c161sex + c172code, 
  data = efc, family = poisson(link = "log")
)

efc$neg_c_7d <- ifelse(efc$neg_c_7 < median(efc$neg_c_7, na.rm = TRUE), 0, 1)

m4 <- glm(
  neg_c_7d ~ c161sex + barthtot + c172code,
  data = efc, family = binomial(link = "logit")
)

tab_model(m3, m4)


# Untransformed estimates on the linear scale
# To plot the estimates on the linear scale, use transform = NULL.
tab_model(m3, m4, transform = NULL, auto.label = T)

# More complex models
# Other models, like hurdle- or zero-inflated models, also work with tab_model().
# In this case, the zero inflation model is indicated in the table.
# Use show.zeroinf = FALSE to hide this part from the table.

library(pscl)
data(bioChemists)

m5 <- zeroinfl(art ~ . | ., data = bioChemists)
tab_model(m5)
tab_model(m5, show.zeroinf = F)



# You can combine any model in one table.

tab_model(m1, m3, auto.label = FALSE)

# Show or hide further columns
# tab_model() has some argument that allow to show or hide specific columns 
# from the output:

# show.est to show/hide the column with model estimates.
tab_model(m1, m3, auto.label = FALSE,  show.est=FALSE)
# show.ci to show/hide the column with confidence intervals.
tab_model(m1, m3, auto.label = FALSE, show.ci=FALSE)
# show.se to show/hide the column with standard errors.
tab_model(m1, m3,  auto.label = FALSE, show.se=FALSE)
# show.std to show/hide the column with standardized estimates 
# (and their standard errors).
tab_model(m1, m3, auto.label = FALSE, show.std=T, show.ci=F)
# show.p to show/hide the column with p-values.
tab_model(m1, m3, auto.label = FALSE, show.p=FALSE, show.ci=F)
# show.stat to show/hide the column with the coefficients' test statistics.
tab_model(m1, m3, auto.label = FALSE, show.stat=T, show.ci=F)
tab_model(m1, m3, auto.label = FALSE, show.stat=F, show.ci=F)

# show.df for linear mixed models, when p-values are based on degrees of
# freedom with Kenward-Rogers approximation, these degrees of freedom are shown.
# p.val needs to be set to "kr"

library(lme4)

data(sleepstudy)
str(sleepstudy)
me1<-lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
tab_model(me1, auto.label = FALSE, show.stat=T, show.se=T, show.df=T, 
          p.val="kr")


# Adding columns
# In the following example, standard errors, standardized coefficients 
# and test statistics are also shown.

tab_model(m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE)

# Removing columns
# In the following example, default columns are removed.

tab_model(m3, m4, show.ci = FALSE, show.p = FALSE, auto.label = FALSE)

# Removing and sorting columns
# Another way to remove columns, which also allows to reorder the columns, 
# is the col.order-argument. This is a character vector, where each element
# indicates a column in the output. The value est, for instance, 
# indicates the estimates, while std.est is the column for standardized
# estimates and so on.

# By default, col.order contains all possible columns. All columns that
# should shown (see previous tables, for example using show.se = TRUE to 
# show standard errors, or show.st = TRUE to show standardized estimates) are 
# then printed by default. Colums that are excluded from col.order are not 
# shown, no matter if the show-arguments are TRUE or FALSE. 
# So if show.se = TRUE, but col.order does not contain the element "se", 
# standard errors are not shown. On the other hand, if show.est = FALSE,
# but col.order does include the element "est", the columns with estimates 
# are not shown.
# In summary, col.order can be used to exclude columns from the table and 
# to change the order of colums.


tab_model(
  m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE,
  col.order = c("p", "stat", "est", "std.se", "se", "std.est")
)

# Collapsing columns
# With collapse.ci and collapse.se, the columns for confidence intervals 
# and standard errors can be collapsed into one column together with the
# estimates. Sometimes this table layout is required.

tab_model(m1, collapse.ci = TRUE)

# Defining own labels
# There are different options to change the labels of the column headers
# or coefficients, e.g. with:

# pred.labels to change the names of the coefficients in the Predictors column. 
# Note that the length of pred.labels must exactly match the amount of predictors 
# in the Predictor column.
# dv.labels to change the names of the model columns, which are labelled with 
# the variable labels / names from the dependent variables.
# Furthermore, there are various string-arguments, to change the name of 
# column headings.

tab_model(
  m1, m2, 
  pred.labels = c("Intercept", "Age (Carer)", "Hours per Week", "Gender (Carer)",
                  "Education: middle (Carer)", "Education: high (Carer)", 
                  "Age (Older Person)"),
  dv.labels = c("First Model", "M2"),
  string.pred = "Coefficient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)

# I don't think there is a way to change the title of the "Estimates" column?

# First Model	M2
# Show asterisks instead of numeric p-values
# You can change the style of how p-values are displayed with the argument
# p.style. With p.style = "asterisk", the p-values are indicated as * in 
# the table.

tab_model(m1, m2, p.style = "a")

# Note: I personally find this annoying as it does not show p values at all but
# gives an impression of importance that may not be warranted.  I.e. when 
# do you normally care about the significance of the intercept term?  Or does
# your field really care about p values, so why use *** to inflate or guide
# the reader toward emphasising something that they should discern themselves.

# Automatic matching for named vectors
# Another way to easily assign labels are named vectors. In this case,
# it doesn't matter if pred.labels has more labels than coefficients in the 
# model(s), or in which order the labels are passed to tab_model(). The only
# requirement is that the labels' names equal the coefficients names as they
# appear in the summary()-output.

# example, coefficients are "c161sex2" or "c172code3"
summary(m1)
# create a named vector, pl:
pl <- c(
  `(Intercept)` = "Intercept",
  e17age = "Age (Older Person)",
  c160age = "Age (Carer)",
  c12hour = "Hours per Week",
  barthtot = "Barthel-Index",
  c161sex2 = "Gender (Carer)",
  c172code2 = "Education: middle (Carer)",
  c172code3 = "Education: high (Carer)",
  a_non_used_label = "We don't care"
)

cbind(pl)
# see how pl is actually named, so you can still use the column names in the
# model call but the pl variable holds more informative information that
# includes words, spaces, capital letters etc..

tab_model(
  m1, m2, m3, m4,
  pred.labels = pl,
  dv.labels = c("Model1", "Model2", "Model3", "Model4"),
  show.ci = FALSE,
  show.p = FALSE,
  transform = NULL
)


# Keep or remove coefficients from the table
# Using the terms- or rm.terms-argument allows us to explicitly show or 
# remove specific coefficients from the table output.

tab_model(m1, terms = c("c160age", "c12hour"))

# Note that the names of terms to keep or remove should match the coefficients
# names. 

# For categorical predictors, one example would be, which will remove the
# terms c172code2 and c161sex2 from the summary, even though those two
# terms were still used to fit the final model:

tab_model(m1, rm.terms = c("c172code2", "c161sex2"))


# For How to format an Anova table output see:

# http://www.understandingdata.net/2017/05/11/anova-tables-in-r/




