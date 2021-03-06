UW DATASCI 350 project: Washington State business license dataset and exploratory analyses
==============
==============

Summary
--------------
I assembled a (partial) data set of WA state business licenses dating back to the early 20th century. Each observation includes the date the business was registered, the date it closed (if it is not still operating on that license), and an industrial classification code.

In addition to a simple exploration of the data – e.g. registration rates per month, the share of industrial sectors over time – my original objective was to fit a model to this time series, merged with population and capital markets data, to predict monthly business registration rates by industrial sector.

The results are interesting but incomplete. Most problematically, I currently have a convenience sampling, rather than a simple random sample or population.

Data
--------------
The Washington Secretary of State makes available a historical data set of all corporations registered in-state. Unfortunately this does not include an industrial classification code; it also omits businesses which are not corporations (e.g. sole proprietorships). 

The WA Department of Revenue (“DoR”) allows external users to query specific businesses by a name or identifier (“UBI”), returning a computer-generated image of their business license.
In an attempt to acquire a complete historical data set, I wrote a Python script to scrape license images from the DoR’s system based on UBIs in the Secretary of State’s corporations data set. I then processed these images (with imagemagick) and performed OCR (with tesseract) to convert their data to text.

Thus far the downloaded data set is incomplete. And even if I scraped every UBI in the corporations data set, I would be omitting non-corporate business entities.

The next iteration of this analysis should instead start with a random sample. One could be generated by picking random observations from the corporations data set, incrementing their UBI by 1 to generate a identifier which has a chance of referring to a non-corporate business entity, and then scraping the corresponding business license from the WA DoR’s site.

*Project files*
-	WA_business_licenses.R – R script combining available data sets, and performing exploratory analyses and modelling.
-	license_image_process.sh – bash script for processing and reading license images.
-	process_license_data.py – Python script for processing license text.
-	dow_jones_data.csv – Returns data for the Dow Jones Industrial Average. (Source: DATASCI 350 class.)
-	wa_combined_intercensal_pop_data.txt – Annual population figures for Washington State. (Source: Census Bureau intercensal estimates from http://www.census.gov/popest/data/historical/index.html.)
-	wa_blscrape_by_ubi.py – Python script for scraping business license images.
-	batch_n_results.txt – Set of tab-delimited files with data from scraped business licenses.
-	/shiny-app - Standalone files for a Shiny web app.

Next steps
--------------

-	A complete data set would be fascinating; failing that, a random sample would provide robustness to any inferences made.
-	Making real claims requires further domain investigation: are all businesses types represented in the DoR business license database? Who is required to get a business license? How accurate are NAICS industrial codes for companies formed before the NAICS system was devised in the 1990s?
-	There is obvious annual cyclicality in the business registration rates. What is happening in August?
-	The model needs better explanatory variables. What data are actually available at time *t* – e.g. consumer sentiment ratings – which could help predict business formation in time *t* + 1?
-	Finally, is there anything special about these results for Washington State? Are they transferable to the United States, or other countries, as a whole?
