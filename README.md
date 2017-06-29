# PLoS_API
A R-based analysis of the Plublic Library of Science using the PLoS API. All data by PLoS. Responsible: Richard Kunert

# Data acquisition
I accessed the PLoS API in R in order to acquire all data. Data acquisition script is called data_acquisition.R and the actual data are called PLOSdata_[date]_[article count].Rdata

# Interactive web-app
I used R-shiny to create an interactive web-app which allows any user to effortlessly play with the PLoS data. The shiny app is implemented in the R-script called app.R. The web-app can be found at [here](https://rikunert.shinyapps.io/PLOS_ALM/).

# Blog post publications
Based on the data and the interactive web-app I found interesting patterns in the data. These findings were published in three blog posts:
- [Do twitter or facebook activity influence scientific impact?](https://brainsidea.wordpress.com/2017/02/22/do-twitter-or-facebook-activity-influence-scientific-impact/)
- [The slowing down of the biggest scientific journal](https://brainsidea.wordpress.com/2017/03/01/the-slowing-down-of-the-biggest-scientific-journal/)
- [The growing divide between higher and low impact scientific journals](https://brainsidea.wordpress.com/2017/03/06/the-growing-divide-between-higher-and-low-impact-scientific-journals/)

Additional figures, which are not part of the interactive we-app, were produced with the script additional_blog_analyses.R

As of March 2017 I consider this project done. If you use this code or find interesting patterns in the PLoS data based on my web-app, please let me know.

@rikunert
