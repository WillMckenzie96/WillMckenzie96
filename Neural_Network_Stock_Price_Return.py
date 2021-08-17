# -*- coding: utf-8 -*-
"""
Created on Tue Jun  8 19:16:40 2021

@author: elemc
"""

import os   
 
os.getcwd()                         # os lib that we need to create files, dirs or find our way..
import numpy   as np                 # numerical library in python
import pandas  as pd                 # dataframe library in python
import seaborn as sns                # package that is used to create nice graphs
from matplotlib import pyplot as plt # the ploting package in python
sns.set()                            # set the graphics to seaborn standards            
get_ipython().run_line_magic('config', "InlineBackend.figure_format = 'retina' # this will procude higher dpi images. NOTE: if you run this on spyder or another ide than jupyter, you will get an error. Comment it to avoid this.")

from   tensorflow.keras.preprocessing.sequence import TimeseriesGenerator # Required below, you will see why
import tensorflow as tf                                                   # Tensorflow

# Time series generator used for neural networks later

# ## Here we will define the training, validation and testing subsamples

# We need the dates below for the following:
# 1. The data in the training period, will be used to train our models
# 2. The data in the validation to optimise hyperparameters, and finally
# 3. We will use the data in the test period to evaluate our final model.

# In[2]:

# We break our sample into training, validation and testing
# In the training bt of the sample we are estimating the optimal wieghts , w
# Given the weights we will utitlise the vlidation part, for the optimal hyperparameters
# Then we use the training and validation sets for the testing set

train_start_date= "2015-04-28"
train_end_date= "2017-12-29" # Need to change as this is as 31st is a holiday day
val_start_date= "2018-01-03"
val_end_date= "2018-12-31"
test_start_date= "2019-01-02"
test_end_date= "2020-01-31"

# Dates have to be present in the csv files

# In[4]:

    # We use pandas to read the csv
    
import decimal

amzn           = pd.read_csv('AMZN.csv',  header = 0 , parse_dates = True, index_col = 0, decimal='.01')

#%%
intc = pd.read_csv('INTC.csv',  header = 0 , parse_dates = True, index_col = 0)
fb = pd.read_csv('FB.csv',  header = 0 , parse_dates = True, index_col = 0)

#%%

# Find the percentage returns for each stock

intc['return'] = intc['Close'] / intc['Close'].shift() - 1
fb['return'] = fb['Close'] / fb['Close'].shift() - 1
amzn['return'] = amzn['Close'] / amzn['Close'].shift() - 1


# Here we are assigning a value of 1 to the dates where the stock return of AMAZON is positive and 0 otherwise. 

# In[6]:

    # This is our predictive variable, also known as our binary variable 
    # value of 1 for all stock returns that are positive

amzn['label'] = np.where(amzn['return'] > 0, 1, 0)
intc['label'] = np.where(intc['return'] > 0, 1, 0)
fb['label'] = np.where(fb['return'] > 0, 1, 0)


#%%
# As we discussed during our Lectures, it is **good practice** to normalise our data, (even returns). 
# 
# This is exactly what we are doing here. Specifically, we standardise the returns, and in terms of the volume, we use a rolling windows of 50 days. 

# std_return stands for standardised return
# [:val_start_date].std() is the standard deviation

intc["std_return_intc"] = (intc["return"] - intc["return"][:val_start_date].mean()) / intc["return"][:val_start_date].std()
intc["std_volume_intc"] = (intc["Volume"] - intc["Volume"].rolling(50).mean()) / intc["Volume"].rolling(50).std()

fb["std_return_fb"] = (fb["return"] - fb["return"][:val_start_date].mean()) / fb["return"][:val_start_date].std()
fb["std_volume_fb"] = (fb["Volume"] - fb["Volume"].rolling(50).mean()) / fb["Volume"].rolling(50).std()

amzn["std_return_amzn"] = (amzn["return"] - amzn["return"][:val_start_date].mean()) / amzn["return"][:val_start_date].std()
amzn["std_volume_amzn"] = (amzn["Volume"] - amzn["Volume"].rolling(50).mean()) / amzn["Volume"].rolling(50).std()

#%%


#%%

df = pd.merge(intc, fb, how="left", on="Date")
df_total = pd.merge(df, amzn, how="left", on="Date")
cols = [7, 8, 9, 18, 19, 28, 29]
df2 = df_total[df_total.columns[cols]]
df2.index = pd.to_datetime(df2, format = '%Y/%m/%d').strftime('%Y-%m-%d')
df2.index = pd.to_datetime()


# df2 represents the complete data frame with all values from the stocks

#%%
# Drop the NA's we created by taking differences

amzn.dropna(inplace = True)
amzn.head()

fb.dropna(inplace = True)
fb.head()

intc.dropna(inplace = True)
intc.head()

df2.dropna(inplace = True)
df2.head()


#%%

# Create a generator
# First we need to get locations 
# we use get_loc to find the loction for the val_start_date and...



val_start_iloc_amzn  = amzn.index.get_loc(val_start_date,  method = 'bfill') 
test_start_iloc_amzn = amzn.index.get_loc(test_start_date, method = 'bfill' )

val_start_iloc_fb  = fb.index.get_loc(val_start_date,  method = 'bfill') 
test_start_iloc_fb = fb.index.get_loc(test_start_date, method = 'bfill' )

val_start_iloc_intc  = intc.index.get_loc(val_start_date,  method = 'bfill') 
test_start_iloc_intc = intc.index.get_loc(test_start_date, method = 'bfill')

val_start_iloc_df2  = df2.index.get_loc(val_start_date,  method = 'bfill') 
test_start_iloc_df2 = df2.index.get_loc(test_start_date, method = 'bfill')

val_start_iloc_df2 = val_start_iloc_df2.astype(int)
test_start_iloc_df2 = test_start_iloc_df2.astype(int)
# In[12]:

# amzn[["std_return", "std_volume"]].values represent our x values
# amzn[["label"]].values respresent our predictive values (y values)
# length = 7 is our forecasting length
# batch_size = 64, we can use a hyperparameter but we are using 64
# end_index = val_start_iloc-1, telling where the generator to end

# we do the same for the validation set, however we need to give a start and an end point
# We do the same for the test generator bt only specify a start point
 

train_generator_df2 = TimeseriesGenerator(df2[["std_return_intc", "std_volume_intc", "std_return_fb", "std_volume_fb", "std_return_amzn", "std_volume_amzn"]].values, df2[["label_x"]].values,
                                      length = 7, batch_size = 64, end_index = val_start_iloc_df2-1)
val_generator_df2   = TimeseriesGenerator(df2[["std_return_intc", "std_volume_intc", "std_return_fb", "std_volume_fb", "std_return_amzn", "std_volume_amzn"]].values, df2[["label_x"]].values,
                                    length = 7, batch_size = 64, start_index = val_start_iloc_df2,
                                    end_index = test_start_iloc_df2-1)
test_generator_df2 = TimeseriesGenerator(df2[["std_return_intc", "std_volume_intc", "std_return_fb", "std_volume_fb", "std_return_amzn", "std_volume_amzn"]].values, df2[["label_x"]].values,
                                     length = 7, batch_size = 64, start_index = test_start_iloc_df2)


# #### Here we assign the NN with its Architecture
# Notice that in the last Tutorial we simply covered an illustration on how we can use Keras to 
# estimate a Network. The steps that we need are the following:
# 
# 1. Import Keras/Tf and all the necessary libraries
# 2. Create a model -- Typically we start with an empty model that we will then fill on our own.
# 3. Decide on the Architecture and proceed to fill it with the different layers.
# 4. Before we compile our model, we need further to choose on an optimiser, loss function and a metric to track i.e. Accuracy of owr model.

# In[13]:

# Create a model function and the inputs are some parameters, whch will be predictionary
# LSTM is long short term memory netowrks, it is more structures, one flow

def model_fn(params):
    
    model = tf.keras.Sequential()
#   The commented code below, is a fast implementation of LSTM using GPU intead of CPU 
#   model.add(tf.keras.layers.CuDNNLSTM(params["lstm_size"], input_shape=(7, 6)))
# params["lstm_size"] are our neurons
# input_shape = (7, 2) we use 7 because it represents the amount of days, and the 6 represents the amount of series used
# line below is our inut layer
    model.add(tf.keras.layers.LSTM(params["lstm_size"], input_shape = (7, 6))) # Again do not forget the Input in the first layer! Here we use 2 series to forecast based on 30 observations
    # dropout is another types of layer (probability of dropout)
    model.add(tf.keras.layers.Dropout(params["dropout"])) # What is this? It is a form of regularisation
    # Below is our output layer, it is a non-linear layer because we are using SIGMOID!
    model.add(tf.keras.layers.Dense(1, activation = "sigmoid"))

    
    # Here we are using Adam as our optimizer
    # Our loss function is specific for classification tasks
    # metrics represents our MSE or whatever accuracy measurement we need to use
    model.compile(optimizer = tf.keras.optimizers.Adam(params["learning_rate"]),
                  loss = "binary_crossentropy", metrics = ["accuracy"])
# dropout, sigmoid, lstm_size are our hyperpararmeters            

# The concept with early stopping is when we stop the traisning process when the lines strt to converge
    callbacks = [tf.keras.callbacks.EarlyStopping(monitor = "val_acc", patience = 5,
                                                  restore_best_weights = True)]
# This is how we fit the model, with the training data and validation data, specifying the amount of epochs used    
    history   = model.fit_generator(train_generator_df2, validation_data = val_generator_df2,
                                  callbacks = callbacks, epochs = 100, verbose = 0).history
#    model.fit(train_generator, validation_data = val_generator,
         #                         callbacks = callbacks, epochs = 100, verbose = 0).history
    # Why not model.fit and model.fit_generator ??
    # our model and how we take data in is not static anymore..
    
    return (history, model)


# Below is a generic function that does some of the things that we talked about Cross Validation in Neural Nets.
# Notice that you can easilly use this function for other Architectures when you would like to do Cross-Validation
# below the input is the model_fn 
# The search space is going to be all the hyperparameters we are willing to consider
# n_iter is the number of iterations we want to consider
# search_dir is used to create a folder in the directory and save all the parameters in there
def random_search(model_fn, search_space, n_iter, search_dir):
    
    results = [] # initialise an empty set
    
    os.mkdir(search_dir) # use os and create a directory!
    # be careful with above line as it has relevance to the following line > 
    
    best_model_path = os.path.join(search_dir, "best_model.h5")  
    results_path    = os.path.join(search_dir, "results.csv")
    
    # The point is to find the best model for the total amount of iterations used (in this case, 5)
    
    for i in range(n_iter):
        
        params           = {k: v[np.random.randint(len(v))] for k, v in search_space.items()}
        # above is a very "pythonic" way to create a dictionary saying that params are lstm_size:, dropout:, learning rate: 
        history, model   = model_fn(params)
        epochs           = np.argmax(history["val_acc"]) + 1 # looking for the epoch that gives the maximum accuracy
        result           = {k: v[epochs - 1] for k, v in history.items()}
        params["epochs"] = epochs
        
        if i == 0:
            
            best_val_acc = result["val_acc"]
            model.save(best_model_path)
            
        if result["val_acc"] > best_val_acc:
            best_val_acc = result["val_acc"]
            model.save(best_model_path)
            
        result = {**params, **result}
        results.append(result)
        tf.keras.backend.clear_session()
        print(f"iteration {i + 1} – {', '.join(f'{k}:{v:.4g}' for k, v in result.items())}")
        
    best_model = tf.keras.models.load_model(best_model_path)
    results    = pd.DataFrame(results)
    
    results.to_csv(results_path)
    
    return (results, best_model)

# In[14]:

# These are our hyperparameters
# Random search given grids on general hyperparameters
# Notice that as the number of grid points increase a lot, the computational complexity also goes up
# np.linspace(a, b, c) creates a grid from a to b with c points.

search_space = {"lstm_size":     np.linspace(50, 200, 3, dtype = int),
                "dropout":       np.linspace(0, 0.4, 2),
                "learning_rate": np.linspace(0.004, 0.01, 5)}


# In[15]:


# Here I used 10 iterations for computational easiness. You can pick more iterations on finer grids above, for better results!
iterations          = 10
results, best_model = random_search(model_fn, search_space, iterations, "CW_new") # Note that when you run this code a directory named search_new will appear. To re run the code please delete this directory first.
# Now when we check our directory, we will have a folder saved as "CW...", we need to change this each time we change our results

# In[16]:


results.sort_values("val_acc", ascending = False).head()
