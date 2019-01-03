BT4222: Mining Web Data for Business Insights
Project: Crypto-gurus: Analysing Credibility and Profitability
Group 3

### Environment Setting
1. Rstudio
2. Jupyter notebook

### Instructions of application
## Data Crawling, preprocessing and merging. 
Directory: Codes for simulation-profitability
1. 1.Scraping with Selenium.R  --tweets crawling
2. Get Daily Prices from API.R   --price crawling by cryptocurrency type with 5 minutes time interval
3. Clean Tweets.R  --data cleaning
4. Generate signalsBySymbols.R  --generating signals(coin type) from tweets to merge tweets dataset and price dataset for simulation in next step
5. Simulation (per user by day per coin).R

## GUI
Directory: Dashboard
1. Dashboard2.R  --showing the simulation results with Rshiny

## Modeling
Directory: Machine Learning codes + output
1. 5 Fold CV on simple LR_SVC_MNB (App 1).ipynb
2. 5 Fold CV on simple LR_SVC_MNB words stemmed (App 2).ipynb
3. 5 Fold CV on simple LR_SVC_MNB Words Stem + UpSampling (App 3).ipynb
4. GridSearch LR_SVC_NB_DT 5CV + word_stem + UpSample (App 4).ipynb
5. Ensemble 5CV + word_stem + UpSample (App 5).ipynb
6. 3 Class 5CV LR_SVC_MNB_DT Words Stem + UpSampling (App 6).ipynb
7. GridSearch LR_SVC_NB 5CV + word_stem + UpSample (App 7).ipynb
8. GridSearch Best Params 5CV + stem words + upsampling (App 8).ipynb
Noted that the results we shown in the report is from App 6,7,8 for basic model results, best parameters with grid search, final model results, respectively.

