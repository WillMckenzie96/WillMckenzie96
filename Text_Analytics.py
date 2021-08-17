# -*- coding: utf-8 -*-
"""
Created on Thu Jun 17 10:07:40 2021

@author: elemc
"""

import pandas as pd # dataframe library in python
import numpy as np # numerical library in python

# Three classifiers chosen
from sklearn.tree import DecisionTreeClassifier
# from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier

# CountVectorizer creates a term document matrix
from sklearn.feature_extraction.text import CountVectorizer

#Used to split the data into 25% test and 75% train
from sklearn.model_selection import train_test_split

import nltk # Natural language toolkit
# sklearn is used for predictive data analysis
from sklearn.datasets import load_files
from sklearn.feature_extraction.text import TfidfTransformer
import re 
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
nltk.download("stopwords")

from nltk.corpus import stopwords

#%%

"""# **DATA PREPARATION**"""

#Readig csv file into dataframe
new_df = pd.read_csv("Huff_news.csv")

#dropping extra column(axis=1)
new_df.drop('Unnamed: 0',axis=1,inplace=True)

# drop NAs

new_df.dropna(axis=0,inplace=True)

new_df.reset_index(inplace=True)

new_df.drop("index",axis=1,inplace=True)

#%%
"""# **DATA PREPROCESSING**"""

new_df["text_all"] = new_df.headline + " " + new_df.short_description

# Data Preprocessing - removing stopwords and making words lower case

stopwords = nltk.corpus.stopwords.words('english')

def preprocess(words):

    # makes the text lower case
    words = words.lower()

    # remove stop words
    words = " ".join([word for word in words.split() if word not in stopwords])

    # all_words = words.split() 
    # "hello world i am jeff" ==> [hello,world,i,am,jeff]
    # words = []
    # for word in all_words:
    #     if(word not in stopwords):
    #         words.append(word)
    # words = [hello,world,jeff]

    # Removes digits and punctuation. In other words. Removes all NON alphabetic ([^a-zA-Z]) characters
    words = re.sub('[^a-zA-Z]'," ",string=words)

    # Replace one or more spaces with 1 space
    # "hello        world" => "hello world"
    words = re.sub('\s+',' ',words)

    # "hello2000 hurracaine k2atrina"
    # "hello    hurraicaine k atrina"
    # string.split(" ")
    #     ["hello","hurraicaine", ]

    return words

#%%

new_df["text_all"] = new_df.text_all.apply(preprocess) #for each index in text_all we apply preprocess()

"""# **SPILITING DATASET FOR TRAINING AND TESTING** """

# CountVectorizer() creates a term document matrix to use it as training data for the classifers.
# max_features decides the vocabulary based on the number of words that feature most prominently (2500)
# min_df represents the mnimum frequency of words that appears within the document, so any less than
# 5 appearances of a singular word can be ignored
# max_df is to remove any words that appear too frequently, in our case more than 70% of the time
vectorizer = CountVectorizer(max_features=2500, min_df=5, max_df=0.7, stop_words=stopwords)
documents = new_df.text_all.tolist()

X = vectorizer.fit_transform(documents).toarray()

print(X.shape)
y = new_df.category.tolist()
X_train, X_test, y_train, y_test = train_test_split(X,y, test_size=0.25,random_state=0) # Test size = 25% , Training size = 75%

#%%
"""# **tf-idf FEATURE WEIGHTING SCHEME**"""

#Prepare the features using tf-idf feature weighting scheme.
# TF-IDF is a statistical measure, evaluating the relevance of a word in a document
# within a group of documents

tfidfconverter = TfidfTransformer()
X_train = tfidfconverter.fit_transform(X_train).toarray() # necessary to transform into array
X_test = tfidfconverter.fit_transform(X_test).toarray()

#%%
# Train the model and evaluated based on accuracy_score

#DECISION TREE CLASSIFIER

# Decision tree algorithm is a supervised learning technique, it can be used to solve regression
# and classification problems, multiple algorithms are used to split a node into sub-nodes,
# nodes are split on all variables, with the most homogeneous sub-nodes selected

print("Decision Tree Classifier - Training model ...")

tree_model = DecisionTreeClassifier().fit(X_train,y_train) # Decision Tree Classifier

tree_predictions = tree_model.predict(X_test)

print("Decision Tree Score")
print(accuracy_score(tree_predictions,y_test))

#%%


# RANDOM FOREST CLASSIFIER

# The random forest technique constructs a mass of decision trees when training, 
# the utput being the class selected by the most trees (with regards to classifiers)

print("Random Forest Classifier - Training model ...")

random_model = RandomForestClassifier().fit(X_train,y_train)

random_prediction = random_model.predict(X_test)

print("Random Forest Score")
print(accuracy_score(random_prediction,y_test))

#%%


# NAIVE BAYES CLASSIFIER

# Naive Bayes is based off Bayes' theorem, it is particularly appropriate when the dimension p
# of the feature space is large, it assumes independence amng predictors and is  to find the probability
# of an event occuring given that another event has occured


print("Naive Bayes Classifier - Training model ...")

NBayes_model = GaussianNB().fit(X_train,y_train) #Naive Bayes Classifier

NBayes_prediction = NBayes_model.predict(X_test)

print("Naive Bayes Score")
print(accuracy_score(NBayes_prediction,y_test))

#%%

"""# **TOP 3 PREDICTORS**"""

classifier = RandomForestClassifier() # Using random forest as classifier (greatest accuracy from previous steps)
predict_classifier=classifier.fit(X_train,y_train).predict(X_test)

predict_classifier

prob_classifier=pd.DataFrame(classifier.predict_proba(X_test),columns=classifier.classes_)

prob_classifier.head(14)

top_predictions = prob_classifier.sort_values(["ARTS"],0,False)

#%%

top_predictions['ARTS'].head(3)

#%%

"""**Following clf() function is used to show the top three predictions based on their probabilities accross every class under their respective classifier.**"""

column_names = prob_classifier.columns.to_list()

def clf(x):
  classifier = x
  predict_classifier=classifier.fit(X_train,y_train).predict(X_test)
  prob_classifier=pd.DataFrame(classifier.predict_proba(X_test),columns=classifier.classes_)
  for i in column_names:
      tp = prob_classifier.sort_values([i],0,False)
      print(tp[i].head(3))
  return

# TOP 3 PREDICTORS UNDER DECISION TREE CLASSIFIER ACROSS EACH CLASS
clf(DecisionTreeClassifier())

# TOP 3 PREDICTORS UNDER RANDOM FOREST CLASSIFIER ACROSS EACH CLASS
clf(RandomForestClassifier())

# TOP 3 PREDICTORS UNDER NAIVE BAYES CLASSIFIER ACROSS EACH CLASS
clf(GaussianNB())

#%%

"""# **EVALUATION METRIC**"""

#DECISION TREE CLASSIFICATION REPORT
print(classification_report(tree_predictions,y_test))

#RANDOM FOREST CLASSIFICATION REPORT
print(classification_report(random_prediction,y_test))

#NAIVE BAYES CLASSIFICATION REPORT
print(classification_report(NBayes_prediction,y_test))