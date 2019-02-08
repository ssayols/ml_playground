## Create a predictive model with h2o and serve it with plumber

Create the model:
```R
out = h2o.automl(
   x = 2:50, 
   y = 1,
   training_frame = TrainData, 
   validation_frame = TestData, 
   max_runtime_secs = 1800
)
```

Get the best model:
```R
championModel = out@leaderchampionModel
```

Test with new data:
```R
predict(championModel, newdata)
```

Save the model:
```R
h2o.saveModel(championModel, path = "myChampionmodel")
```

Create a new file with our API exposed to Plumber. Important part here are the Plumber decoractors `#*`:
```R
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
 
library(plumber)
library(h2o)
h2o.init()
mymodel = h2o.loadModel("mySavedModel")
 
#* @apiTitle My model API engine
 
#### expose my model #################################
#* Return the prediction given three input features
#* @param input1 description for inp1
#* @param input2 description for inp2
#* @param input3 description for inp3
#* @post /mypredictivemodel
function( input1, input2, input3){
   scoredata = as.h2o(
      data.frame(input1, input2, input3 )
   )
   as.data.frame(predict(mymodel, scoredata))
}
```

To start the API, open an R session and read the API R file created in the previous paragraph:
```R
rapi <- plumber::plumb("plumber.R")  # Where 'plumber.R' is the location of the code file shown above 
rapi$run(port=8000)
```

This idea and code is taken from this fantastic [post](https://longhowlam.wordpress.com/2019/02/07/are-you-leaking-h2o-call-plumber/).
