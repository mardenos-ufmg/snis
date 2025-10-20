import numpy as np
def quant_fatores(df_x,delta=0.05):
    threshold=1-delta
    CorrMx = df_x.corr()
    autovalores, v = np.linalg.eig(CorrMx)
    return (autovalores>threshold).sum()