import pandas as pd
import scipy

def normalizacao (scores):
    # df = pd.DataFrame(dfs)
    # df = df.transpose()

    # scores_eee=pd.DataFrame()
    col=scores.columns
    for i in col:
        normal = scipy.stats.norm(scores[i])
        scores[i]= scipy.stats.norm.cdf(scores[i], loc=scores[i].mean(),scale=scores[i].std())

    #score["Score_Medio"]=round(scores_eee.mean(axis=1),3)
    return scores