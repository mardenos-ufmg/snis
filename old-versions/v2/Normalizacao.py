import pandas as pd
import scipy

def normalizacao (scores):
    col=scores.columns
    for i in col:
        normal = scipy.stats.norm(scores[i])
        scores[i]= scipy.stats.norm.cdf(scores[i], loc=scores[i].mean(),scale=scores[i].std())
    return scores