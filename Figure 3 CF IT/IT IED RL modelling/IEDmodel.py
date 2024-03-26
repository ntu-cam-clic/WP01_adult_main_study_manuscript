#!/usr/bin/env python
# coding: utf-8

# In[196]:


    
def simulate(params = [0.5,3,0.01], dimension1 = 'lines',subject = '0', rng = None, seed = 42):
    """Simulate IED data for one subject given parameter values. 

    Parameters:
    params (list): list of parameters in order [alpha: learning rate, beta: choice determinism, theta0: dimension primacy]. 
    dimension1 (str): the first relevant stimulus dimension, should be 'lines' or 'shapes'. 
    subject (str): a subject name.
    rng (func): pass an existing random number generator. Default = None.
    seed (int): if rng is None, this seed will be used to create a random number generator. Default = 42.

    Returns:
    class: attributes are 
    .choice - list of participants choices on each trial
    .R - list of
    .r - list of feedback on each trial
    .S  - list of stimuli seen on each trial
    .inputs - list of vectors denoting which features were input into the model on each trial
    .WH1 - list of learnt line feature values over trials
    .WH2 - list of learnt shape feature values over trials
    .THE - list of dimension attention weights over trials
    .alpha - learning rate parameter
    .beta - choice determinism parameter
    .theta0 - dimension primacy parameter
    .wh0 - initial feature weights
    .dimension1 - first relevant stimulus dimension
    .subject - subject name
    .stage - list of stages 
    .Trials - list of trials to criterion for each stage
    .Errors - list of errors per stages
    .Stages - list of stage names
    .Passed - list of booleans, based on whether participant passed or failed each stage.
   """
    import numpy as np  
    import random
    import pandas as pd
    import itertools
    
    rng = rng if rng else np.random.default_rng(seed)

    # params = [alpha,epsilon,beta,theta0] & args = [dimension1, wh0]
    alpha,beta,theta0 = params

    # set weights to their initial values
    wh0 = [np.array([0.0]*6).reshape(1,6),np.array([0.0]*6).reshape(1,6)]
    wh1,wh2,theta = wh0[0].copy(), wh0[1].copy(),np.array((theta0)).reshape(1,1)

    # specify task specs as in CANTAB IED
    maxTrials,PassCriteria,dimension1 = 50,6,dimension1
    stages = ['Simple Discrimination','Simple Reversal','Compound Discrimination','Compound Discrimination 2',                  'Compound Reversal','Intra-Dimensional Shift','Intra-Dimensional Reversal',                  'Extra-Dimensional Shift','Extra-Dimensional Reversal']
    if dimension1 == 'lines': # stimuli and rewarding features in lines first version
        stimuli = ['L0_6','L0_7','L0_6,S0_0','L0_6,S0_1','L0_7,S0_0','L0_7,S0_1','L0_8,S0_2','L0_8,S0_3','L0_9,S0_2','L0_9,S0_3',              'L0_10,S0_4','L0_10,S0_5','L0_11,S0_4','L0_11,S0_5']
        RFeature = ['L0_6','L0_7','L0_7','L0_7','L0_6','L0_8','L0_9','S0_5','S0_4']
    else:  # stimuli and rewarding features in shapes first version
        stimuli = ['S0_0','S0_1','S0_0,L0_6','S0_1,L0_6','S0_0,L0_7','S0_1,L0_7','S0_2,L0_8','S0_3,L0_8','S0_2,L0_9','S0_3,L0_9',              'S0_4,L0_10','S0_5,L0_10','S0_4,L0_11','S0_5,L0_11']
        RFeature = ['S0_0','S0_1','S0_1','S0_1','S0_0','S0_2','S0_3','L0_11','L0_10']

    fs = ['L0_6','L0_7','L0_8','L0_9','L0_10','L0_11','S0_0','S0_1','S0_2','S0_3','S0_4','S0_5'] # list of all features    
    lines,shapes = dict(zip(fs[:6], np.identity(6))),dict(zip(fs[6:], np.identity(6))) # one hot encoded vector for each feature

    # create empty lists of task variables to be filled
    inputs,AH,V,choice,R,WH1,WH2,THE,PE,S,r = [],[],[],[],[],[wh1.copy()],[wh2.copy()],[theta.copy()],[np.array([0.])],[],[]
    Trials, Errors, Stages, Passed = [],[],[],[]

    def sigmoid(x):
        s = 1/(1+np.exp(-x))
        return(s)

    t = 0 # initialise trial number to 0
    for stage in range(len(stages)): # loop over number of stages
        for st in range(maxTrials): # loop over number of trials in each stage
            # stimuli presented
            if (stage == 0) or (stage == 1):
                s = list(rng.permutation(stimuli[0:2])) # first 2 stages, select two one dimensional stimuli 
            else: # select compound stimuli to be presented for all other stages
                if stage in [2,3,4]:
                    n = 7
                    stim1 = 2                   
                elif stage in [5,6]:
                    n = 15
                    stim1 = 6                
                elif stage in [7,8]:
                    n = 23
                    stim1 = 10  

                l = [item for sublist in S[-3:] for item in sublist] # flat list of stimuli from last 3 trials
                m = [item for sublist in S[-6:] for item in sublist] # flat list of stimuli from last 6 trials

                for x in stimuli[stim1:stim1+2]: # check whether stimuli break rules
                # stimuli combination should not repeat more than 3 times in a row or more than 4 in 6 consecutive trials
                    if l.count(x) >= 3 or m.count(x) >= 4: # if so, select other stimuli combination for this trial
                        code = 1
                        stimremain = [stim for stim in stimuli[stim1:stim1 +4] if stim not in [x, stimuli[n - stimuli.index(x)]]]
                        s[0] = rng.choice(stimremain)
                        s[1] = stimuli[n - stimuli.index(s[0])]
                        break
                    else:
                        code = 0

                if code == 0: # if not, select stimuli combination at random for this trial
                    s[0] = rng.choice(stimuli[stim1:stim1 +4])
                    s[1] = stimuli[n - stimuli.index(s[0])]
            S.append(s.copy())

            # input *features* of stimuli presented 
            if (stage == 0) or (stage == 1):
                l1,l2 = (lines[str(S[t][0])].reshape(6,1), lines[str(S[t][1])].reshape(6,1)) if dimension1 =='lines'                            else (np.zeros((6,1)),np.zeros((6,1)))
                s1,s2 = (np.zeros((6,1)),np.zeros((6,1))) if dimension1 =='lines' else                             (shapes[str(S[t][0])].reshape(6,1), shapes[str(S[t][1])].reshape(6,1))
            elif stage in [2,3,4,5,6]: # access line and shapes from stimuli presented to determine feature inputs
                l1,l2 = (lines[str(S[t][0][0:4])].reshape(6,1), lines[str(S[t][1][0:4])].reshape(6,1)) if dimension1 =='lines'                            else (lines[str(S[t][0][5:9])].reshape(6,1),lines[str(S[t][1][5:9])].reshape(6,1))
                s1,s2 = (shapes[str(S[t][0][5:9])].reshape(6,1),shapes[str(S[t][1][5:9])].reshape(6,1)) if dimension1 =='lines' else                             (shapes[str(S[t][0][0:4])].reshape(6,1), shapes[str(S[t][1][0:4])].reshape(6,1))
            elif stage in [7,8]:
                # different indexes due to slightly longer stimulus name
                if dimension1 =='lines':
                    l1,s1 = lines[str(S[t][0][0:5])].reshape(6,1),shapes[str(S[t][0][6:10])].reshape(6,1)
                    l2,s2 = lines[str(S[t][1][0:5])].reshape(6,1),shapes[str(S[t][1][6:10])].reshape(6,1)
                elif dimension1 == 'shapes':
                    s1,l1 = shapes[str(S[t][0][0:4])].reshape(6,1),lines[str(S[t][0][5:10])].reshape(6,1)
                    s2,l2 = shapes[str(S[t][1][0:4])].reshape(6,1),lines[str(S[t][1][5:10])].reshape(6,1)      
            inputs.append([[l1,s1],[l2,s2]]) # list of lists on each trial (sublist for each stimulus)

            # FEEDFORWARD to estimate stimulus values
            if stage == 0 or stage ==1: #simple feature reinforcement learning for stages 1,2
                V1,V2 = (np.dot(wh1, l1),np.dot(wh1, l2)) if dimension1 == 'lines' else                            (np.dot(wh2, s1),np.dot(wh2, s2)) # input multiplied by weight = stimulus value
                V.append(np.concatenate((V1,V2)))
                AH.append([[0,0],[0,0]])
            else: # dimension weights added for other stages
                # inputs multiplied by their weights, weighted by dimension weight = stimulus values
                ah1,ah2,ah3,ah4 = np.dot(wh1, l1),np.dot(wh2, s1),np.dot(wh1, l2),np.dot(wh2, s2)
                AH.append([[ah1,ah2],[ah3,ah4]])
                V1,V2 = sigmoid(theta)*ah1+(1-sigmoid(theta))*ah2, sigmoid(theta)*ah3+(1-sigmoid(theta))*ah4
                V.append(np.concatenate((V1,V2)))

            # make choice using softmax with predicted values    
            ev = np.exp(beta*V[t])
            sev = sum(ev)
            p = ev/sev
            choice.append(S[t][0]) if rng.uniform() < p[0] else choice.append(S[t][1])

            # get feedback on choice 
            R1 = 1 if RFeature[stage] in S[t][0] else -1
            R2 = 0 - R1 # reward for other stimulus is inferred to be the opposite
            R.append([R1,R2])

            r.append(R[t][S[t].index(choice[t])])
            PE.append(R[t][S[t].index(choice[t])] - V[t][S[t].index(choice[t])]) # calculate prediction error

            # Update Weights ================
            if stage in [0,1]: #simple RL on feature weights
                wh1 += alpha * (R[t][0] - V[t][0]) * inputs[t][0][0].T
                wh2 += alpha * (R[t][0] - V[t][0]) * inputs[t][0][1].T
                wh1 += alpha * (R[t][1] - V[t][1]) * inputs[t][1][0].T
                wh2 += alpha * (R[t][1] - V[t][1]) * inputs[t][1][1].T

                WH1.append(wh1.copy())
                WH2.append(wh2.copy())
                THE.append(theta.copy())
            else: # backpropagation for dimension and feature weights
                # Phase1 
                dcost_dV = V[t][S[t].index(choice[t])] - R[t][S[t].index(choice[t])]

                dV_dtheta = sigmoid(theta)*(1-sigmoid(theta))*AH[t][S[t].index(choice[t])][0] -                                 sigmoid(theta)*(1-sigmoid(theta))*AH[t][S[t].index(choice[t])][1]

                dcost_theta = np.dot(dcost_dV, dV_dtheta.T) # for dimension weights ^

                dcost_dV1 = V[t][0] - R[t][0] # for feature weights
                dcost_dV2 = V[t][1] - R[t][1]

                dV_dah1 = sigmoid(theta)
                dV_dah2 = 1-sigmoid(theta)

                dcost_dah1 = np.dot(dV_dah1.T, dcost_dV1)
                dcost_dah2 = np.dot(dV_dah2.T, dcost_dV1)
                dcost_dah3 = np.dot(dV_dah1.T, dcost_dV2)
                dcost_dah4 = np.dot(dV_dah2.T, dcost_dV2)

                # Phase 2
                dah1_dwh1 = inputs[t][0][0]
                dah2_dwh2 = inputs[t][0][1]
                dah3_dwh1 = inputs[t][1][0]
                dah4_dwh2 = inputs[t][1][1]
                dcost1_wh1 = np.dot(dcost_dah1, dah1_dwh1.T)
                dcost1_wh2 = np.dot(dcost_dah2, dah2_dwh2.T)
                dcost2_wh1 = np.dot(dcost_dah3, dah3_dwh1.T)
                dcost2_wh2 = np.dot(dcost_dah4, dah4_dwh2.T)

                # Update Weights ================

                wh1 -= alpha * dcost1_wh1
                wh2 -= alpha * dcost1_wh2
                wh1 -= alpha * dcost2_wh1
                wh2 -= alpha * dcost2_wh2
                theta -= alpha * dcost_theta

                WH1.append(wh1.copy())
                WH2.append(wh2.copy())
                THE.append(theta.copy())

            # terminate stage and move on if select correct stimulus 6 times in a row on new stage
            if (st >= 5)  & (all(RFeature[stage] in x for x in choice[-6:])):
                Trials.append(st + 1) # n trials for current stage
                Errors.append(r[-(st+1):].count(-1))
                Stages.append(stages[stage]) # add stage name
                Passed.append(True) # add that participant passed this stage
                t+=1
                break
            elif st == maxTrials - 1: # or terminate stage and task if reached maxed trials
                Trials.append(st + 1)
                Errors.append(r[-(st+1):].count(-1))
                Stages.append(stages[stage])
                Passed.append(False)
                t+=1
                
                class output:
                    pass

                results = output()
                results.PE = PE
                results.choice = choice
                results.R = R
                results.r = r
                results.S = S
                results.inputs = inputs
                results.WH1 = WH1
                results.WH2 = WH2
                results.THE = THE
                results.alpha = alpha
                results.beta = beta
                results.theta0 = theta0
                results.wh0 = wh0
                results.dimension1 = dimension1
                results.subject = subject
                results.stage = list(itertools.chain(*[[stages[x]]*Trials[x] for x in range(len(Trials))]))
                results.Trials = Trials
                results.Errors = Errors
                results.Stages = Stages
                results.Passed = Passed

                return results
            
            t+=1
    class output:
        pass

    results = output()
    results.PE = PE
    results.choice = choice
    results.R = R
    results.r = r
    results.S = S
    results.inputs = inputs
    results.WH1 = WH1
    results.WH2 = WH2
    results.THE = THE
    results.alpha = alpha
    results.beta = beta
    results.theta0 = theta0
    results.wh0 = wh0
    results.dimension1 = dimension1
    results.subject = subject
    results.stage = list(itertools.chain(*[[stages[x]]*Trials[x] for x in range(len(Trials))]))
    results.Trials = Trials
    results.Errors = Errors
    results.Stages = Stages
    results.Passed = Passed

    return results

def llIED(params,args):
    """Calculate (log) likelihood for subject data given parameter values. 

    Parameters:
    params (list): list of parameters in order [alpha: learning rate, beta: choice determinism, theta0: dimension primacy]. 
    args (list): list of arguments for function, [R: list of rewards, choice: list of stimulus choices, S: list of stimuli seen , dimension1: string of first relevant dimension, likelihood: boolean - False if calculating log likelihood, u: prior mean, v2: prior covariance].

    Returns:
    float: if calculating log likehood log likelihood for ML or log posterior for EM
    or
    class: if calculating likelihood for iBIC or alpt, attributes are 
    .likelihood - total data likelihood for this subject
    .avg_likelihood - average likelihood per trial for this subject
   
   """
    import numpy as np
    nP = len(params)

    alpha, beta, theta0 = params
    aeby = np.asarray(params.copy()).reshape(nP,1)

    R,choice,S,dimension1,likelihood = args[:5]
    
    if not likelihood:
        # transform parameters
        alpha = 1/(1 + np.exp(-aeby[0]))
        beta = np.exp(aeby[1])
        theta0 = np.array((aeby[2])).reshape(1,1)

    if len(args) > 5:
        # we are doing EM
        u = args[5]
        v2 = args[6]
        u = u.reshape(nP,1)

        PP = aeby - u
        L = 0.5*(PP.T @ np.linalg.pinv(v2) @ PP)
        LP = -np.log(2*np.pi) - 0.5*np.log(np.linalg.det(v2)) - L
        NLP = -LP[0] # calculate negative log liklihood of drawing these parameters from a given prior

    fs = ['L0_6','L0_7','L0_8','L0_9','L0_10','L0_11','S0_0','S0_1','S0_2','S0_3','S0_4','S0_5']
    lines,shapes = dict(zip(fs[:6], np.identity(6))),dict(zip(fs[6:], np.identity(6)))

    AH,V,inputs = [],[],[]
    l,ll = [0]*2,0
    like = []
    
    wh0 = [np.array([0.0]*6).reshape(1,6),np.array([0.0]*6).reshape(1,6)]
    wh1 = wh0[0].copy()
    wh2 = wh0[1].copy()
    theta = theta0.copy()

    def sigmoid(x):
        s = 1/(1+np.exp(-x))
        return(s)

    for t in range(len(choice)):
        if len(S[t][0]) == 4:
            if dimension1 == 'lines':
                l1 = lines[str(S[t][0])].reshape(6,1)
                l2 = lines[str(S[t][1])].reshape(6,1)
                s1,s2 = np.zeros((6,1)),np.zeros((6,1))
            else: 
                s1 = shapes[str(S[t][0])].reshape(6,1)
                s2 = shapes[str(S[t][1])].reshape(6,1)
                l1,l2 = np.zeros((6,1)),np.zeros((6,1))
            inputs.append([[l1,s1],[l2,s2]])

            # FEEDFORWARD to estimate stimulus values
            V1,V2 = (np.dot(wh1, l1),np.dot(wh1, l2)) if dimension1 == 'lines' else                            (np.dot(wh2, s1),np.dot(wh2, s2))
            V.append(np.concatenate((V1,V2)))
            AH.append([[0,0],[0,0]])
            vmax = beta*np.amax(V[t])

            l = beta*(V[t][S[t].index(choice[t])] - vmax) - np.log(sum((np.exp(beta*(V[t][x]-vmax)) for x in range(len(S[t])))))
            ll += l.copy()

            if likelihood == True:
                ev = np.exp(beta*V[t])
                sev = sum(ev)
                p = ev/sev

                like.append(p[S[t].index(choice[t])])

            # Update Weights ================

            wh1 += alpha * (R[t][0] - V[t][0]) * inputs[t][0][0].T
            wh2 += alpha * (R[t][0] - V[t][0]) * inputs[t][0][1].T
            wh1 += alpha * (R[t][1] - V[t][1]) * inputs[t][1][0].T
            wh2 += alpha * (R[t][1] - V[t][1]) * inputs[t][1][1].T

        else:
            st1,st2 = S[t][0].replace(',',' '),S[t][1].replace(',',' ')
            if dimension1 == 'lines':
                line1, shape1 = st1.split()
                line2, shape2 = st2.split()
            else:
                shape1, line1 = st1.split()
                shape2, line2 = st2.split()
            l1,s1 = lines[line1].reshape(6,1),shapes[shape1].reshape(6,1)
            l2,s2 = lines[line2].reshape(6,1),shapes[shape2].reshape(6,1)
            inputs.append([[l1,s1],[l2,s2]])

            # FEEDFORWARD to estimate stimulus values
            ah1,ah2,ah3,ah4 = np.dot(wh1, l1),np.dot(wh2, s1),np.dot(wh1, l2),np.dot(wh2, s2)
            AH.append([[ah1,ah2],[ah3,ah4]])

            V1,V2 = sigmoid(theta)*ah1+(1-sigmoid(theta))*ah2, sigmoid(theta)*ah3+(1-sigmoid(theta))*ah4
            V.append(np.concatenate((V1,V2)))

            vmax = beta*np.amax(V[t])

            l = beta*(V[t][S[t].index(choice[t])] - vmax) - np.log(sum((np.exp(beta*(V[t][x]-vmax)) for x in range(len(S[t])))))
            ll += l.copy()

            if likelihood == True:
                ev = np.exp(beta*V[t])
                sev = sum(ev)
                p = ev/sev

                like.append(p[S[t].index(choice[t])])

            dcost_dV = V[t][S[t].index(choice[t])] - R[t][S[t].index(choice[t])]

            dV_dtheta = sigmoid(theta)*(1-sigmoid(theta))*AH[t][S[t].index(choice[t])][0] -                                 sigmoid(theta)*(1-sigmoid(theta))*AH[t][S[t].index(choice[t])][1]

            dcost_theta = np.dot(dcost_dV, dV_dtheta.T)

            dcost_dV1 = V[t][0] - R[t][0]
            dcost_dV2 = V[t][1] - R[t][1]

            dV_dah1 = sigmoid(theta)
            dV_dah2 = 1-sigmoid(theta)

            dcost_dah1 = np.dot(dV_dah1.T, dcost_dV1)
            dcost_dah2 = np.dot(dV_dah2.T, dcost_dV1)
            dcost_dah3 = np.dot(dV_dah1.T, dcost_dV2)
            dcost_dah4 = np.dot(dV_dah2.T, dcost_dV2)

            # Phase 2
            dah1_dwh1 = inputs[t][0][0]
            dah2_dwh2 = inputs[t][0][1]
            dah3_dwh1 = inputs[t][1][0]
            dah4_dwh2 = inputs[t][1][1]
            dcost1_wh1 = np.dot(dcost_dah1, dah1_dwh1.T)
            dcost1_wh2 = np.dot(dcost_dah2, dah2_dwh2.T)
            dcost2_wh1 = np.dot(dcost_dah3, dah3_dwh1.T)
            dcost2_wh2 = np.dot(dcost_dah4, dah4_dwh2.T)

            # Update Weights ================

            wh1 -= alpha * dcost1_wh1
            wh2 -= alpha * dcost1_wh2
            wh1 -= alpha * dcost2_wh1
            wh2 -= alpha * dcost2_wh2
            theta -= alpha * dcost_theta

    if likelihood == True:
        return (np.prod(like), np.mean(like))


    if len(args) > 6:
        llEM = -ll + NLP
        return llEM
    else:
        return -ll

def fit(params):
    """Optimise (log) likelihood for subject data given parameter values. 

    Parameters:
    params (list): list of parameters to pass to minimisation function, depends on whether using EM or ML. 

    Returns:
    tuple: of (minimisation result, number of minimisation attempts) if using EM
    or
    array: of best-fitting parameters if using ML
   
   """
    #read in appropriate data and set up
    import scipy.optimize
    import numpy as np

    if len(params) > 6:
        # we are using EM
        args = params[:-3]
        rng = params[-1]
        var = 1
        while var >= 1:
            for x in range(50):
                if x == 0:
                    m = params[-3]
                else:
                    m = params[-3]+ x*0.1*np.matmul(rng.standard_normal((1,(len(m)))), scipy.linalg.sqrtm(np.linalg.inv(0.1*np.identity((len(m))))))
                xopt = scipy.optimize.minimize(fun=llIED, x0=list(m),args =  (args))
                if xopt.success:
                    return([xopt,var])
                else:
                    continue             
                var+=1

    else:
        # we are using ML
        args = params[:-1]
        m = params[-1]   
        # minimise log likelihood for subject data and given parameters
        xopt = scipy.optimize.minimize(fun=llIED2, x0=list(m),args =  (args))
        params =  list(xopt['x'])
        return (params)


# In[ ]:




