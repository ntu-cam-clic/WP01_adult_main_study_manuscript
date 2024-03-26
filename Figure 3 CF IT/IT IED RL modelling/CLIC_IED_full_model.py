#!/usr/bin/env python
# coding: utf-8

import pandas
import numpy as np
import pandas as pd
import tqdm
import IED
import IEDmodel
import modelling

# adult main dataset
data_stage_dir = "path to stage data"
data_trial_dir = "path to trial data"
parameter_out_path = "path to output file"

# list of IDs, separated by comma

# prepare ID list
IED_stage_data = pd.read_csv(data_stage_dir)
subjectsID = IED_stage_data['subject'].unique().tolist()

# Running the master function
# Loads data, fits the model,
# simulates data and compares it with real data,
# qualitative and quantitative model fits.
IED_fitting_results = IED.assess_fit(trials_csv=data_trial_dir,
                                     stages_csv=data_stage_dir,
                                     subjects=subjectsID,
                                     dimension1='lines',
                                     nP=3,
                                     fit_func=IEDmodel.fit,
                                     sim_func=IEDmodel.simulate,
                                     transforms=['sigmoid', 'exp', None],
                                     n_jobs=8,
                                     names=['alpha', 'beta', 'theta'],
                                     like_func=IEDmodel.llIED,
                                     seed=42,
                                     fit='EM',
                                     rng=None,
                                     Nsamples=5000,
                                     EM_iter=100
                                     )
# Returns:
# class: attributes are .m, .s2, .u, .v2
# If fit is 'EM',
#     .m is array of individuals best fitting parameter estimates,
#     .s2 is individual parameter covariances,
#     .u is group level prior mean estimate,
#     .v2 is group level prior covariance estimate.
# If fit is 'ML', only .m attribute exists.

# save model fitting individual parameters
IED_RL_out = pd.DataFrame(IED_fitting_results.est.m)
IED_RL_out = IED_RL_out.rename(columns={0: 'IED_Alpha', 1: 'IED_Beta', 2: 'IED_Theta0'})
IED_RL_out['ID'] = subjectsID
column_order = ['ID', 'IED_Alpha', 'IED_Beta', 'IED_Theta0']
IED_RL_out = IED_RL_out[column_order]
IED_RL_out.to_csv(parameter_out_path, index=False)



