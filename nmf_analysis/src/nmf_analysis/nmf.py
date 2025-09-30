import logging
import os

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.ndimage import gaussian_filter
from scipy.sparse import issparse
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.decomposition import non_negative_factorization
from sklearn.decomposition._nmf import _beta_divergence
from sklearn.metrics import explained_variance_score
from sklearn.utils.validation import check_is_fitted

from . import settings as s
from .utils import get_directory
from .visualisation import plot_vec


def train_model(behavior: str, smoothing_sigma: int, nb_components: int):
    # 1 - Load data
    df_vec = []
    df_vec_meta = []
    df_meta = pd.read_csv(os.path.join(s.DATA_TRANSFORMED, "meta.csv"))
    for _, exp in df_meta.iterrows():
        if exp.status == "OK":
            directory = get_directory(
                s.DATA_TRANSFORMED, exp.experiment_id, exp.machine_name, exp.region_id
            )
            try:
                df_vec_exp = pd.read_csv(directory / f"{behavior}.csv", index_col=0)
                df_vec.append(df_vec_exp)
                for _ in range(len(df_vec_exp.columns)):
                    df_vec_meta.append(exp)
            except FileNotFoundError:
                logging.error("Not found: {}".format(directory))
    df_vec = pd.concat(df_vec, axis=1)
    df_vec.index = pd.TimedeltaIndex(df_vec.index)
    df_vec_meta = pd.concat(df_vec_meta, axis=1).T

    # 2 - Smooth
    heatmap = _smooth(df_vec, "gaussian", sigma=[smoothing_sigma, 0])
    fig, ax = plt.subplots(figsize=(17, 15))
    df = pd.DataFrame(data=heatmap, index=df_vec.index)
    plot_vec(df, robust=True, ax=ax, xlabel="Fly")
    fig.savefig(s.MODEL_DIR / f"{behavior}_{nb_components}c_heatmap.png")

    # 3 - NMF
    model = NMFVectorizer(nb_components)
    # mx = np.ma.masked_invalid(heatmap)
    mx = np.nan_to_num(heatmap, nan=0.0)
    model.fit(mx)
    joblib.dump(model, s.MODEL_DIR / f"{behavior}_{nb_components}c_model.gz")

    # 4 - Plot components
    df = pd.DataFrame(data=model.W, index=df_vec.index)
    # fig, ax = plt.subplots(ncols=model.n_components, figsize=(model.n_components, 10))
    # if model.n_components > 1:
    #     for i in range(model.n_components):
    #         sns.heatmap(df[[i]], ax=ax[i], cbar=False)
    #         if i > 0:
    #             ax[i].axes.get_yaxis().set_visible(False)
    #     plt.tight_layout()
    # else:
    #     sns.heatmap(df[[0]], ax=ax, cbar=False)
    fig, ax = plt.subplots(figsize=(model.n_components, 10))
    plot_vec(df, robust=False, ax=ax, xlabel="Component")
    fig.savefig(s.MODEL_DIR / f"{behavior}_{nb_components}c_components.png")


def apply_model(behavior, nb_components, smoothing_sigma, df_meta):
    # 1 - Load data
    df_vec = []
    df_vec_meta = []
    for _, exp in df_meta.iterrows():
        if exp.status == "OK":
            directory = get_directory(
                s.DATA_TRANSFORMED, exp.experiment_id, exp.machine_name, exp.region_id
            )
            try:
                df_vec_exp = pd.read_csv(
                    os.path.join(directory, f"{behavior}.csv"), index_col=0
                )
                df_vec.append(df_vec_exp)
                for _ in range(len(df_vec_exp.columns)):
                    df_vec_meta.append(exp)
            except FileNotFoundError:
                logging.error("Not found: {}".format(directory))
    df_vec = pd.concat(df_vec, axis=1)
    df_vec_meta = pd.concat(df_vec_meta, axis=1).T

    # 2 - Smooth
    heatmap = _smooth(df_vec, "gaussian", sigma=[smoothing_sigma, 0])

    # 3 - NMF
    model = joblib.load(
        os.path.join(s.MODEL_DIR, f"{behavior}_{nb_components}c_model.gz")
    )
    # mx = np.ma.masked_invalid(heatmap)
    mx = np.nan_to_num(heatmap, nan=0.0)
    return df_vec_meta, model.transform(mx)


def _smooth(heatmap, method, **kwargs):
    """Applies a Gaussian blur to a heatmap.
    We would expect some spatial coherence, or smoothness, in the locations
    where the actions were performed. However, this coherence can be disrupted
    by laying a high granularity grid (i.e., high values for parameters `m`
    and `n`) over the pitch as the boundaries between grid cells are abrupt
    and somewhat arbitrary. Hence, the counts for nearby cells may exhibit
    more variance than they should. To promote smoothness in the counts of
    nearby cells, a Gaussian blur is applied to the heatmap.
    Args:
        heatmap (np.array(float)): The normalized heatmap of player `p`.
        smooth (scalar, optional): Standard deviation for Gaussian kernel.
    Returns:
        np.array(float): `heatmap` normalized for minutes on the pitch.
    """
    if method == "gaussian":
        return gaussian_filter(heatmap, kwargs["sigma"])
    elif method == "none":
        return heatmap
    else:
        raise ValueError("The smoothing method {} is not supported.".format(method))


class NMFVectorizer(BaseEstimator, TransformerMixin):
    def __init__(self, components):
        self.n_components = components

    def fit(self, X, y=None):
        # X = check_array(X, accept_sparse=True)
        # TODO: sklearn NMF crashes when sparse matrices are used in
        # combination with KL beta loss.
        if issparse(X):
            X = X.todense()

        self.n_features_ = X.shape[0]

        # Return the transformer
        self.W, self.H, n_iter = non_negative_factorization(
            X,
            n_components=self.n_components,
            init="random",
            beta_loss="kullback-leibler",
            random_state=0,
            max_iter=100000,
            tol=1e-8,
            solver="mu",
        )
        return self

    def transform(self, X):
        # Check is fit had been called
        check_is_fitted(self, "n_features_")

        # Input validation
        # X = check_array(X, accept_sparse=True)
        if issparse(X):
            X = X.todense()

        # Check that the input is of the same shape as the one passed
        # during fit.
        if X.shape[0] != self.n_features_:
            raise ValueError("Shape of input is different from what was seenin `fit`")

        # Sklearn can only solve X=WH while keeping the second matrix,
        # H fixed. To deal with this, solve X^T=H^T W^T, making sklearn keep
        # the second matrix, now W^T fixed, solving for H^T.
        W, H, n_iter = non_negative_factorization(
            X.T,
            n_components=self.n_components,
            init="custom",
            beta_loss="kullback-leibler",
            random_state=0,
            update_H=False,
            H=self.W.T,
            max_iter=10000,
            tol=1e-8,
            solver="mu",
        )

        return W.T

    def inverse_transform(self, pv):
        check_is_fitted(self, "n_features_")

        return self.W.dot(pv)

    def score(self, X, scorer="reconstruction error"):
        # X = check_array(X, accept_sparse=True)
        # TODO: sklearn NMF crashes when sparse matrices are used in
        # combination with KL beta loss.
        if issparse(X):
            X = X.todense()

        # Compute the beta-divergence of X and dot(W, H).
        pv = self.transform(X)
        X_rec = self.inverse_transform(pv)

        if scorer == "reconstruction error":
            return _beta_divergence(X, self.W, pv, "frobenius", square_root=True)
        elif scorer == "explained variance":
            return explained_variance_score(X, X_rec)
        else:
            raise ValueError("Scorer is not supported.")
