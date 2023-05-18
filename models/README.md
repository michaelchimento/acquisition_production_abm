# Agent based models

In this folder are all of the necessary files to reproduce the models presented in the manuscript. The model used for the main sensitivity analysis ```generative_model.py``` requires a parameter file in the import preamble, which are the files starting with ```paramlist_*.py```.

Models that generated data upon which the inferential EWA analysis was done needed custom programming, and thus are standalone files starting with ```EWA_recoverable_*```. The same goes for "production only" and "transmission only" baseline models, starting with ```baseline_*.py```.

Running any of these scripts will generate one CSV per simulation, all of which are concatenated into one CSV after all simulations have completed.

## Contents
File  | Description
------------- | -------------
```EWA_recoverable_heterogeneous_asocial.py``` | the novel behaviour spreads through asocial learning
```EWA_recoverable_heterogeneous_social.py``` | the novel behaviour spreads through social transmission
```EWA_recoverable_homoegeneous.py``` | agents begin with knowledge of both behaviours generating idealized data for EWA
```generative_model.py``` | the main generative model used for most results presented in the manuscript
```paramlist_GENERATIVE_equiv_payoffs.py``` | defines parameter constellations for main results
```paramlist_GENERATIVE_fullweights.py``` | defines parameter constellations for the extended exploration of phi in which agents already had full information about the established behaviour
```paramlist_NBDA_ideal_asocial.py``` | The novel behaviour spreads through asocial learning
```paramlist_NBDA_realistic_asocial.py``` | The novel behaviour spreads through asocial learning
```paramlist_NBDA_ideal_social.py``` | The novel behaviour spreads through social transmission, but behavioural frequencies do not influence transmission probabilities, generating idealized data for NBDA
```paramlist_NBDA_realistic_social.py``` | The novel behaviour spreads through social transmission, and behavioural frequencies do influence transmission probabilities
