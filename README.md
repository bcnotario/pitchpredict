# Pitch Prediction Model
Multinomial Logistic Regression for Pitch Prediction

### Pitchf/x Data Preparation
Prepare data for logistic model
* Create additional pitch type responses with fewer classes
* Convert categorical predictors to factors
* Create additional predictors for the count, base runners, and run difference
```
data.prep()
```
### Pitch Prediction Model
Multinomial logistic regression for pitch prediction
* Trained on first 80% of pitches
* Tested on last 20% of pitches
```
pitch.mod(data,i)
```
### Pitch Prediction Testing
Test the model accuracy for a sample of the total pitchers using the pitch prediction model
```
pitch.test(data,sample)
```
