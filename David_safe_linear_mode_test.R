
# ---- 4) Build the spec grid and run SCA ------------------------
specs <- setup(
  data     = dat,
  y        = y_present,
  x        = x_var,
  model    = "lm_safe", # the only change we make here is to SWAP OUT lm for lm_safe
                        # lm_safe is going to be a normal/vanilla lm, but wrapped in error handling
  controls = c_present,
  subsets  = subsets
  # (can add robust/cluster/fixed args here later if needed)
)

lm_safe<-function(formula,data,na.action){ # these three parameters are apparently what specr uses internally
                                           # each loop it tries to build a new lm using these
                                           # our problem is that if ANY lm errors, the whole thing crashes

  # lm(formula,data,na.action) # this is what specr NORMALLY does
  # BUT if there are zero usable rows of data this errors
  # and breaks the whole curve. so we want to first find out if an error is GOING TO OCCUR
  # ideally we'd do this apriori and only run models which wouldn't error
  # but i'm not sure if specr's subset function lets us do this :-)
  # so we're doing it this way instead!
    
    tryCatch( # the beginning of the trycatch wrapper says 'we're going to try to do this, but
              # if it throws an error, instead of erroring out totally, do what is in the 
              # error function below instead'
    {
      lm(formula, data,na.action = na.omit) # then, just like specr does, we try a lm
    },
    error = function(e){ # BUT NOW, if there's an error, it doesn't
                         # error out straight away - first it does this
                         # we can use this to put in something non-errory
                         # and keep out outer loop going
      
      print(paste("INSUFFICIENT DATA TO BUILD MODEL - PUTTING PLACEHOLDER IN")) # this is just for us
                                                                                # it's unnecessary but i think it's nice
                                                                                # you could probabl make it more useful as a log but also is it really worth the time
      return(lm(1~1)) # and THIS is my really experimental bit which i'm shocked worked!
                      # it builds the SHELL of a linear model output by predicting the number 1 from the number 1
                      # the trick here is that syntactically it's CORRECT so doesn't throw an error
                      # these 'shell' results get swept out by specr's aggregation function at
                      # the end of curve building. which is very cool and fortunate. otherwise
                      # we COULD do this ourselves by specifying a custom 'fun1' within specr setup
                      # but it's nice we don't have to do it.
    })
}
