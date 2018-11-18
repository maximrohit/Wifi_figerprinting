# Wifi_figerprinting
Predicting the location of a person based on his wifi fingerprint


1.The validation and train set were skidded w.r.t. different building and validation set had a larger set of longitude and latitudes.
Hence we combined the two sets and  sampled again for a better distribution.

2. We validated  predicting for the whole set vs predicting for individual building , found it to be equivalent so we are predicting for the all three building , in other words we are even predicting the building based on wap values.

3. We initially evaluated SVM and Random forest owing to their availability of better predict in general. Followed it up with dropping SVM for performance reason and predicted everything based on Random forest.

Note we used only 25% of high variance wap columns i.e.  130 of 520. Below are the details of the current Random Forest model:-
1. Number of Wap Columns:-Only high 25%(520*.25= 130) based variance values were used
2. Number of trees:-20 or higher can be used based on performance capability and the need of stability
3. Number of fields in each tree:- 20(15%) or 25(20%) can be used keeping the tree’s diverse enough.
4. Sample Size for each tree:-5400 (30% of total data), its recommend to be between 30-70%.

For the Model we used the minimum of these, we can tune the model based on it performance in production or any other test data to further evolve the model.
The basic model give out below performance number:-
Accuracy 
Floor:- 96.1
Building 99.7

R-squared
Longitude:-99.1
Lattitude:- 98.33
