#import
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import pairwise_distances
#open file & discover dataset
Data = pd.read_csv('/Users/rynadalswyd/Documents/LEARN/Level 8/summer training/MSIT/path1_step3/Amazon - Movies and TV Ratings.csv')
Data.head()
Data.shape
Data.describe().T
#highest views movie
sorted_Data_view= Data.describe().T['count'].sort_values(ascending=False)
Max_views = sorted_Data_view[:1]
Max_views
#highest rating movie
sorted_Data_rating = Data.drop('user_id',axis=1).sum().sort_values(ascending=False)
Max_rating = sorted_Data_rating[:1]
Max_rating
#average rating
avg_rating = Data.drop('user_id',axis=1).mean().sort_values(ascending=False)
#Top 5
top5= avg_rating[:5]
top5
#least audience
sorted_Data_view_least=Data.drop('user_id',axis=1).sum().sort_values()
top5_least=sorted_Data_view_least[:5]
top5_least
#splitting data
train,test = train_test_split(Data,test_size=0.3,random_state=3)
train = train.drop('user_id',axis=1)
test = test.drop('user_id',axis=1)
train = train.fillna(0)
test = test.fillna(0)
print("train: ",len(train)," , test: ",len(test))
#train model
movie_similarity= pairwise_distances(train.T ,metric= 'cosine')
#test the model
predection= test.dot(movie_similarity)/np.array([np.abs(movie_similarity).sum(axis=1)])
predection
