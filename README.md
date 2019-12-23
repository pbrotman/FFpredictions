I have built an elastic net model to predict Fantasy Football D/ST scores.

D/ST scores are calculated according to ESPN default rules.

Play-by-play is scraped using R library nflscrapR. Betting lines data from Kaggle dataset "NFL scores and betting data" by Spreadspoke: https://www.kaggle.com/tobycrabtree/nfl-scores-and-betting-data

Predictions on new data can be made using:
> predict(model,s=model.lambda,newx=NEWDATA)