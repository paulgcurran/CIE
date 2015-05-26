\name{CIE-package}
\alias{CIE-package}
\alias{CIE}
\docType{package}
\title{
Careless/Insufficient Effort Responding Statistics

}
\description{
Computes a number of statistics used for determination of careless/insufficient effort resonding in polytomous self-report survey data. Values are computed for any or all of these statistics and then appended to the original values and stored as 'Data2' as well as in 'values' - a file with only the statistics and not the original data.  

}
\details{
\tabular{ll}{
Package: \tab CIE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-07\cr
}
NOTE: This version of the package is in development. Currently, these functions will only produce accurate data when presented with a dataset of unidimensional items without reverse coding (or in situations where items have already been reverse coded). Work is underway to allow for these situations. 


There are a number of functions in this package, each designed to compute a different statistic from input data. 

Functions:

longStringFull(...)
This function produces a number of statistics equal to the number of responses present in your input. These will start at 'longStringA' and continue for each different response. This is not necessarily consistent with the range of availiable options unless all options have been used by respondents in the sample. This should be checked before interpretating these values, as unused response options may cause misintrepreation of results. Typos and the use of numeric missing codes (e.g. '99') will result in odd results.  

Each statistic produced gives a numeric value relating to the length of the longest continuous string of that particular response in the original data. This function will output a numbe of vectors equal to the number of response options used by participants in the data. 

longStringMax(...)
This function produces two values. The first of these values, 'longLength' indicates the length of the longest continous string of any singular response option in the original data. The second of these values, 'longResponse' indicates which response corresponds to this longest string.

personTotalCor(...)
This function produces a person-total correlation, similar in concept to an item-total correlation. This value, similar to a item-total correlation, indicates how similar in response a person was to all other respondents in the sample.

polyGutt(...)
This function produces a count of polytoumous Guttman errors in each response string. This is similar in concept to the person-total correlation, as the Guttman ordering is established using the mean values of the sample on each item.  

psychometricAnt(...)
This function finds the 5 item pairs with the largest negative correlations, then computes the within-person correlation on these sets of items. Warnings are generated if adequate magnitude correlations do not exist in the input data.  

psychometricSyn(...)
This function finds the 5 item pairs with the largest positive correlations, then computes the within-person correlation on these sets of items. Warnings are generated if adequate magnitude correlations do not exist in the input data.

withinOddEven(...)
This function produces a within-person correlation of the odd items and even items for each respondent. The first odd item is paired with the first even item, the second odd with the second even, and so forth. The correlation between these resulting arrays is reported for each respondent. In the case of an odd number of items, the final item is discarded for the purposes of this function.


}
\author{
Paul G. Curran

Maintainer: Paul G. Curran <paul.g.curran@gmail.com>

}
\references{

Curran, P.G. (Revise and resubmit). Methods and best practices for the detection of carelessly invalid responses in survey data.

Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data. Psychological Methods, 17, 437–455.

Huang, J. L., Curran, P.G., Keeney, J., Poposki, E. M., & DeShon, R. P. (2012). Detecting and Deterring Insufficient Effort Responding to Surveys. Journal of Business and Psychology, 27, 99-114.

Johnson, J. A. (2005). Ascertaining the validity of individual protocols from Web-based personality inventories. Journal of Research in Personality,
39, 103–129.


}


\keyword{ package }
\seealso{

}
\examples{

}
