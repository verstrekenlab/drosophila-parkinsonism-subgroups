from abc import ABC, abstractmethod

import numpy as np
import pandas as pd
from sklearn.base import BaseEstimator, TransformerMixin

time_index = pd.MultiIndex.from_product(
    iterables=[range(0, 24), range(0, 60)], names=["hour", "minute"]
)


class ToVector(ABC, BaseEstimator, TransformerMixin):
    """ """

    def __init__(self, aggregate_days=True):
        self.aggregate_days = aggregate_days

    def fit(self, X, y=None):
        return self

    def transform(self, df, y=None):
        df = df.copy()
        df["t"] = df["t"] - pd.Timedelta(hours=df.iloc[0]["t"].hour)
        # origin = df.iloc[0].t.replace(hour=self.reference_hour, minute=0, second=0, microsecond=0)
        groups = (
            [(0, df)]
            if self.aggregate_days
            else df.groupby(pd.Grouper(key="t", freq="24H", origin="epoch"))
        )
        df_vec = []
        for label, group in groups:
            transformed_group = self.transform_group(group)
            df_vec.append(transformed_group)
        df_vec = pd.concat(df_vec, axis=1)
        df_vec.columns = list(range(len(df_vec.columns)))
        return df_vec

    @abstractmethod
    def transform_group(self, df):
        pass

    def __repr__(self):
        string = self.__class__.__name__
        if self.aggregate_days:
            return f"{string}_aggregated"
        return string


class ToActivityVector(ToVector):
    """Compute the total activity of a fly during
    a period of one week, grouped by minute of the day.
    """

    def transform_group(self, group):
        activty_by_min = (
            group.groupby([group.t.dt.hour, group.t.dt.minute], dropna=False)
            # .xy_dist_log10x1000.apply(lambda x: np.power(np.sum(x) / 1000, 10))
            .xy_dist_log10x1000.apply(lambda x: np.nansum(10.0 ** (x / 1000.0)))
            .reindex(time_index, fill_value=0)
        )
        activty_by_min.index = pd.TimedeltaIndex(range(24 * 60), unit="m")
        return activty_by_min  # .loc[
        # pd.date_range("1900-01-01 08:00:00", "1900-01-02 07:59:00", freq="1min").time
        # ]


class ToFoodVector(ToVector):
    """Compute the total activity of a fly during
    a period of one week, grouped by minute of the day.
    """

    def transform_group(self, group):
        near_food = (group["x"] > 400) & (True)
        group["eating"] = (group.behavior == "micromovement") & near_food
        activty_by_min = (
            group.groupby([group.t.dt.hour, group.t.dt.minute], dropna=False)
            .eating.apply(lambda x: 100 * x.sum() / float(len(x)))
            .reindex(time_index, fill_value=float("nan"))
        )
        activty_by_min.index.names = ["hour", "minute"]
        activty_by_min.index = pd.to_datetime(
            activty_by_min.index.get_level_values("hour").astype(str)
            + ":"
            + activty_by_min.index.get_level_values("minute").astype(str),
            format="%H:%M",
        ).time
        return activty_by_min.loc[
            pd.date_range(
                "1900-01-01 08:00:00", "1900-01-02 07:59:00", freq="1min"
            ).time
        ]


class ToSleepVector(ToVector):
    """Compute the total activity of a fly during
    a period of one week, grouped by minute of the day.
    """

    def transform_group(self, group):
        group = group.set_index("t")
        group["is_sleep"] = (group.behavior == "immobile").astype(int)
        group["nb_is_sleep"] = group.rolling("5T").is_sleep.sum()
        group["nb"] = 1
        group["nb"] = group.rolling("5T").nb.sum()
        group["is_sleep"] = (group["nb_is_sleep"] / group["nb"] > 0.99).astype(int)
        group.reset_index(inplace=True)
        activty_by_min = (
            group.groupby([group.t.dt.hour, group.t.dt.minute], dropna=False)
            .is_sleep.apply(lambda x: 100 * x.sum() / float(len(x)))
            .reindex(time_index, fill_value=float("nan"))
        )
        activty_by_min.index = pd.TimedeltaIndex(range(24 * 60), unit="m")
        return activty_by_min


class ToImmobileVector(ToVector):
    """Compute the total activity of a fly during
    a period of one week, grouped by minute of the day.
    """

    def transform_group(self, group):
        group = group.copy()
        group["asleep"] = group["behavior"] == "immobile"
        activty_by_min = (
            group.groupby([group.t.dt.hour, group.t.dt.minute], dropna=False)
            .asleep.apply(lambda x: 100 * x.sum() / float(len(x)))
            .reindex(time_index, fill_value=float("nan"))
        )
        activty_by_min.index.names = ["hour", "minute"]
        activty_by_min.index = pd.TimedeltaIndex(range(24 * 60), unit="m")
        return activty_by_min


class ToWalkVector(ToVector):
    """Compute the total activity of a fly during
    a period of one week, grouped by minute of the day.
    """

    def transform_group(self, group):
        group = group.copy()
        group["walking"] = group["behavior"] == "walking"
        activty_by_min = (
            group.groupby([group.t.dt.hour, group.t.dt.minute], dropna=False)
            .walking.apply(lambda x: 100 * x.sum() / float(len(x)))
            .reindex(time_index, fill_value=float("nan"))
        )
        activty_by_min.index.names = ["hour", "minute"]
        activty_by_min.index = pd.TimedeltaIndex(range(24 * 60), unit="m")
        return activty_by_min


class ToLocationVector(ToVector):
    """Compute the region of the tube in which the fly is, expressed as
    a percentage of the tube's length.
    """

    def __init__(self, n_bins=50, xmin=0, tube_length=500, aggregate_days=True):
        self.n = n_bins
        self.xmin = xmin
        self.tube_length = tube_length
        super().__init__(aggregate_days)

    def transform_group(self, df):
        x = df.x.values
        x = x[~np.isnan(x)]

        xi = (x - self.xmin) / self.tube_length * self.n
        xi = xi.astype(int).clip(0, self.n - 1)

        return pd.Series(np.bincount(xi, minlength=(self.n)) / len(x))
