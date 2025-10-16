import datetime

import matplotlib.pyplot as plt
import matplotlib.ticker
import numpy as np
import pandas as pd
import seaborn as sns
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


def plot_fly_data(
    df_fly, col="xy_dist_log10x1000", col_time="t", robust=True, ax=None, **kwargs
):
    vmin = np.nanpercentile(df_fly[col], 2) if robust else df_fly[col].min()
    vmax = np.nanpercentile(df_fly[col], 98) if robust else df_fly[col].max()

    origin = df_fly.iloc[0][col_time].replace(microsecond=0)
    groups = df_fly.groupby(pd.Grouper(key=col_time, freq="24H", origin=origin))
    fig, ax = plt.subplots(1, len(groups), figsize=(len(groups), 6), sharey=True)
    for i, (label, df_vec) in enumerate(groups):
        df_vec = df_vec[[col_time, col]].copy()
        df_vec = df_vec.sort_values(by=col_time)
        df_vec[col_time] = df_vec[col_time].dt.strftime("%d-%m %H:%M:%S")
        df_vec.set_index(col_time, inplace=True)
        sns.heatmap(
            (df_vec),
            vmin=vmin,
            vmax=vmax,
            ax=ax[i],
            cbar=(i == len(groups) - 1),
            xticklabels=False,
            mask=df_vec.isnull(),
        )
        ax[i].set_ylabel("")
        ax[i].set_xlabel(label, rotation=270)
    return ax


def plot_vec(df_vec, robust=True, title="", cmap=None, xlabel="Day", ax=None, **kwargs):
    ax = ax or plt.gca()

    def timeTicks(x, pos):
        d = datetime.timedelta(minutes=x)
        hours, remainder = divmod(d.seconds, 3600)
        minutes, seconds = divmod(remainder, 60)
        if hours == 0:
            return ""
        return "+{}h".format(int(hours))

    cbar = kwargs.get("cbar", True)
    if cbar:
        cax = inset_axes(
            ax,
            width="100%",  # width: 40% of parent_bbox width
            height="10%",  # height: 10% of parent_bbox height
            loc="lower left",
            bbox_to_anchor=(0, 1.1, 1, 1),
            bbox_transform=ax.transAxes,
            borderpad=0,
        )
        cax.set_title(title, loc="left")
    else:
        cax = None
        ax.set_title(title, loc="left")
    ax = sns.heatmap(
        (df_vec),
        robust=robust,
        ax=ax,
        cbar_ax=cax,
        cbar_kws={"orientation": "horizontal"},
        cmap=cmap,
        **kwargs,
    )
    if df_vec.index.dtype.type == np.timedelta64:
        formatter = matplotlib.ticker.FuncFormatter(timeTicks)
        ax.yaxis.set_major_formatter(formatter)
        ax.yaxis.set_major_locator(plt.MultipleLocator(2 * 60))
    if len(df_vec.columns) > 1:
        ax.set_xlabel(xlabel)
    else:
        ax.set_xticks([])
    return ax
