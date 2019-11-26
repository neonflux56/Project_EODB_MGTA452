# Project_EODB_MGTA452

Ease of doing business in a country.



###############################################

MODEL

-- 1 layer and 11 outputs 
-- cross classification
-- adam




###############################################

VISUALISATION


Shiny app

Home page
-World stats
-Average stats
-World Map Dark light shade


Country Specific Page
-Country Dropdown 
-Probability of easiness (Model)
-Rank Index (Model)
-Trend line from 2006 to 2018 (Xian)
-Urban population percent semi pie chart for each country  (Xian)



###############################################

ISSUES FACED

-- removing NA varibles before running model
-- number of outputs in last layer should match one hot of Y
-- model %>% fit (as.matrix(X), Y) **** convert X data to matrix before running on model, else input to model is NULL
-- Number of Y output ???? bad accuracy


TO DO

-- Improve Model
-- Add more features if possible
-- Probability using proba or lime
-- Shiny




###############################################








