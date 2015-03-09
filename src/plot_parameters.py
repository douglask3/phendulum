#!/usr/bin/env python

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import gridspec

def main():
    pars_df = pd.read_csv(pars_path)
    site_df = pd.read_csv(site_path)

    big_tab = pd.merge( pars_df, site_df, left_on="Site", right_on="Label" )
    plab = ["slope","bias","resist","drag"]

    fig = plt.figure(figsize=(8,11))
    gs = gridspec.GridSpec(4, 3)
    pplots = np.arange(0,12,3)
    for a,b in zip(range(4),pplots):

        sub_tab = big_tab.query('Sampling=="in" and k=={0}'.format(a))
        m_labels = np.array(sub_tab["Site"])
        r_colors = cm.rainbow(np.linspace(0, 1, len(m_labels)))

        # plot the three site covariates
        ax1 = fig.add_subplot(gs[b])
        ax2 = fig.add_subplot(gs[b+1])
        ax3 = fig.add_subplot(gs[b+2])

        # only use ticks on bordering plots
        ax2.tick_params( axis='y', which='both', labelleft='off')
        ax3.tick_params( axis='y', which='both', labelleft='off')
        if b<8:
            ax1.tick_params( axis='x', which='both', labelbottom='off')
            ax2.tick_params( axis='x', which='both', labelbottom='off')
            ax3.tick_params( axis='x', which='both', labelbottom='off')

        ax1.scatter(sub_tab["MAP"], sub_tab["Value"], s=50, c=r_colors, zorder=100 )
        # add points individually for legend
        for i,j,k in zip(sub_tab.index, r_colors, m_labels):
            out = ax2.scatter(sub_tab["MAT"].loc[i], sub_tab["Value"].loc[i], s=50, c=j, label=k, zorder=100)
            if b<1:
                ax2.legend( handles=out, labels=m_labels, loc='center', fontsize=8, scatterpoints=1, \
                        ncol=len(m_labels), bbox_to_anchor=(0.5, 1.15))
        ax3.scatter(sub_tab["Latitude"], sub_tab["Value"], marker='D', s=50, c=r_colors, zorder=100)
        ax1.set_ylabel('k$_{'+plab[a]+'}$', fontsize=16)
        ax1.xaxis.set_ticks(np.arange(0,2500,500))
        ax2.xaxis.set_ticks(np.arange(20,30,2))
        ax3.xaxis.set_ticks(np.arange(-24,-9,3))
        # add error bars
        ax1.errorbar(sub_tab["MAP"], sub_tab["Value"], xerr=sub_tab["sig_MAP"], yerr=sub_tab["Error"], \
                     fmt='None', marker=None, ecolor='black', alpha=0.5, zorder=0)
        ax2.errorbar(sub_tab["MAT"], sub_tab["Value"], xerr=sub_tab["sig_MAT"], yerr=sub_tab["Error"], \
                     fmt='None', ecolor='black', alpha=0.5, zorder=0)
        ax3.errorbar(sub_tab["Latitude"], sub_tab["Value"], yerr=sub_tab["Error"], \
                     fmt='None', ecolor='black', alpha=0.5, zorder=0)


    # setup labelling here
    ax1.set_xlabel('MAP (mm)')
    ax2.set_xlabel('MAT ($\degree$C)')
    ax3.set_xlabel('Latitude')
    #fig.tight_layout(w_pad=0.8, h_pad=0.1)
    plt.savefig("../figs/parameters.pdf")

    return None


if __name__=="__main__":
    pars_path = "../outputs/spring_parameters.csv"
    site_path = "../data/site_char.csv"

    main()