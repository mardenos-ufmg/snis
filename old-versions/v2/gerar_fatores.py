from utils.Bartlett_test import Bartlett
from utils.Quant_fatores import quant_fatores
from utils.Sentido import sentido
from factor_analyzer import FactorAnalyzer
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler

def fatores (df,df_features,delta=0.05,rotacao='oblimin',verbose=False):
    groups = df_features.groupby('Grupo')    
    keys = groups.groups.keys()
    dfs =[]
    #inter=0

    for i in keys:
        gr=groups.get_group(i)
        gr_var=gr.Variavel
        dados=df.loc[:,gr_var]
        labs = dados.columns
        
        barlet=Bartlett(dados)
        if (barlet==False):
            print ('Não passou no teste de Bartlett') 
            break          

        P = len(labs)
        FA_eficiencia = FactorAnalyzer(quant_fatores(dados,delta), rotation=rotacao) # Não-ortogonal oblimin
        #inter=inter+1
        #print("Quant de fatores:")
        #print(quant_fatores(dados,delta))
        FA_eficiencia.fit(dados)
        FA_loadings_eficiencia = pd.DataFrame.from_records(FA_eficiencia.loadings_)
        FA_loadings_eficiencia.index = labs 
                
        if verbose == True:
            plt.figure(figsize=(6,8))
            sns.set(font_scale=.9)
            FA_loadings_eficiencia.rename(columns=lambda x: "Fator "+ str(x+1), inplace=True)
            sns.heatmap(FA_loadings_eficiencia, linewidths=1, linecolor='#ffffff', cmap="binary",xticklabels=1, yticklabels=1, annot=True)
            plt.title(i)
            plt.show()

        scores = pd.DataFrame(FA_eficiencia.transform(dados))
        scores['Score_Total'] = scores.loc[:,list(range(len(scores.columns)))].sum(axis=1)
        scores_efetividade=scores

        STR = sentido(labs,FA_eficiencia,df_features,rotacao,verbose)
        W = pd.DataFrame(np.linalg.solve(FA_eficiencia.corr_, STR))

        scaler = StandardScaler()
        X_scale = pd.DataFrame(scaler.fit_transform(dados), columns=dados.columns)
        novo_efetividade = pd.DataFrame(np.dot(X_scale, W))
        novo_efetividade[i] = novo_efetividade.loc[:, list(range(len(novo_efetividade.columns)))].sum(axis=1)
        dfs.append(novo_efetividade[i])
        

    scores = pd.DataFrame(dfs)
    scores = scores.transpose()   
    return scores