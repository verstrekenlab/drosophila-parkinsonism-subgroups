"""
Implementation of the gene interaction bayesian model for Kaempf et al 2026

The model quantifies the significance of the difference between the product of the ERG signal for each mutant separately
and the signal observed in the double mutants
If the genes are independent, the signal in the double mutant should be similar to the product (=independence assumption)
of the signal in the single mutants, but if there is a dependence between the two genes, the effect is less
The bayesian model captures the noise in the observations observed for both single mutants and the double mutant,
as opposed to a model where the mean of the single mutant is compared to the distribution observed in the double mutant
"""

import pandas as pd
import numpy as np
import pymc as pm
import arviz as az
from typing import Dict, Union
from pymc_extras.model_builder import ModelBuilder


RANDOM_SEED = 8927
rng = np.random.default_rng(RANDOM_SEED)
az.style.use("arviz-darkgrid")


class GeneInteractionModel(ModelBuilder):
    # https://www.pymc.io/projects/examples/en/latest/howto/model_builder.html
    # Give the model a name
    _model_type = "GeneInteractionModel"

    # And a version
    version = "0.1"
        

    def build_model(self, X: pd.DataFrame, y: pd.Series, **kwargs):
        """
        build_model creates the PyMC model

        Parameters:
        X : pd.DataFrame
            The input data that is going to be used in the model. This should be a DataFrame
            containing the features (predictors) for the model. For efficiency reasons, it should
            only contain the necessary data columns, not the entire available dataset, as this
            will be encoded into the data used to recreate the model.
            In the gene interaction model, X contains genotypea

        y : pd.Series
            The target data for the model. This should be a Series representing the output
            or dependent variable for the model.
            In the gene interaction model, y contains peak depolizations of the ERG in mV, or a metric derived from it
            (e.g. peak normalized with the control peak)

        kwargs : dict
            Additional keyword arguments that may be used for model configuration.
        """
        # Check the type of X and y and adjust access accordingly
        
        X_values = X.values if isinstance(X, pd.DataFrame) else X
        y_values = y.values if isinstance(y, pd.Series) else y
        self._generate_and_preprocess_model_data(X_values, y_values)

        with pm.Model() as self.model:
            # Priors for the individual gene effects
            alpha = pm.Beta('alpha', alpha=2, beta=5) * 1.05
            beta = pm.Beta('beta', alpha=2, beta=5) * 1.05
            
            # alpha = pm.Normal('alpha', mu=1, sigma=1)
            # beta = pm.Normal('beta', mu=1, sigma=1)
        
            # Interaction term, only applies to the double mutant
            interaction = pm.Normal('interaction', mu=0, sigma=1)
        
            # Expected metric for the double mutant if genes interact
            expected_dm = alpha * beta + interaction
            
            # Expected metric for single mutants, assuming no interaction
            # so the metric is simply the effect of the single gene
            expected_a = alpha
            expected_b = beta
        
            # Link the expected metric to the observed data
            # i.e. select one of the 3 expected metrics depending on the genotype
            expected_metric = pm.math.switch(
                X["sm1"],
                expected_a,
                pm.math.switch(
                    X["sm2"],
                    expected_b,
                    expected_dm
                )
            )
        
            # Data likelihood
            # The observed metric comes from a normal distribution centered around the expected metric
            obs = pm.Normal('y', mu=expected_metric, sigma=.1, observed=y)
            # obs = pm.Deterministic('y', expected_metric, observed=y)
            

    def _data_setter(
        self, X: Union[pd.DataFrame, np.ndarray], y: Union[pd.Series, np.ndarray] = None
    ):
        if isinstance(X, pd.DataFrame):
            x_values = X.values
        else:
            # Assuming "input" is the first column
            x_values = X[:, 0]

        with self.model:
            pm.set_data({"x_data": x_values})
            if y is not None:
                pm.set_data({"y_data": y.values if isinstance(y, pd.Series) else y})

    @staticmethod
    def get_default_model_config() -> Dict:
        """
        Returns a class default config dict for model builder if no model_config is provided on class initialization.
        The model config dict is generally used to specify the prior values we want to build the model with.
        It supports more complex data structures like lists, dictionaries, etc.
        It will be passed to the class instance on initialization, in case the user doesn't provide any model_config of their own.
        """
        model_config: Dict = {}
        return model_config

    @staticmethod
    def get_default_sampler_config() -> Dict:
        """
        Returns a class default sampler dict for model builder if no sampler_config is provided on class initialization.
        The sampler config dict is used to send parameters to the sampler .
        It will be used during fitting in case the user doesn't provide any sampler_config of their own.
        """
        sampler_config: Dict = {
            "draws": 1_000,
            "tune": 1_000,
            "chains": 3,
            "target_accept": 0.95,
            "idata_kwargs": {'log_likelihood': True},
        }
    
        return sampler_config

    @property
    def output_var(self):
        return "y"

    @property
    def _serializable_model_config(self) -> Dict[str, Union[int, float, Dict]]:
        """
        _serializable_model_config is a property that returns a dictionary with all the model parameters that we want to save.
        as some of the data structures are not json serializable, we need to convert them to json serializable objects.
        Some models will need them, others can just define them to return the model_config.
        """
        return self.model_config

    def _save_input_params(self, idata) -> None:
        """
        Saves any additional model parameters (other than the dataset) to the idata object.

        These parameters are stored within `idata.attrs` using keys that correspond to the parameter names.
        If you don't need to store any extra parameters, you can leave this method unimplemented.

        Example:
            For saving customer IDs provided as an 'customer_ids' input to the model:
            self.customer_ids = customer_ids.values #this line is done outside of the function, preferably at the initialization of the model object.
            idata.attrs["customer_ids"] = json.dumps(self.customer_ids.tolist())  # Convert numpy array to a JSON-serializable list.
        """
        pass

        pass

    def _generate_and_preprocess_model_data(
        self, X: Union[pd.DataFrame, pd.Series], y: Union[pd.Series, np.ndarray]
    ) -> None:
        """
        Depending on the model, we might need to preprocess the data before fitting the model.
        all required preprocessing and conditional assignments should be defined here.
        """
        self.model_coords = None  # in our case we're not using coords, but if we were, we would define them here, or later on in the function, if extracting them from the data.
        # as we don't do any data preprocessing, we just assign the data given by the user. Note that it's a very basic model,
        # and usually we would need to do some preprocessing, or generate the coords from the data.
        self.X = X
        self.y = y