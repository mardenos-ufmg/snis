from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity
from factor_analyzer import FactorAnalyzer

import pandas as pd
import scipy
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.preprocessing import StandardScaler

def Bartlett(dados):
    chi_square_value, p_value = calculate_bartlett_sphericity(dados)
    if (p_value > 0.05):
        return False
    else:
        return True


def quant_fatores(df_x,delta=0.05):
    threshold=1-delta
    CorrMx = df_x.corr()
    autovalores, v = np.linalg.eig(CorrMx)
    return (autovalores>threshold).sum()


def normalizacao (scores):
    col=scores.columns
    for i in col:
        normal = scipy.stats.norm(scores[i])
        scores[i]= scipy.stats.norm.cdf(scores[i], loc=scores[i].mean(),scale=scores[i].std())
    return scores


def fatores (df, df_features, delta=0.05, rotacao='oblimin', verbose=False):
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


def Score(df, df_features, delta=0.05, normalizar_score=True, save_excel=True, rotacao='oblimin', verbose=False):
    scores=fatores(df,df_features,delta,rotacao,verbose)

    if (normalizar_score==True):
        scores = normalizacao(scores)
     
    scores["Score_Medio"]=round(scores.mean(axis=1),3)      
             
    if (save_excel == True):
        scores.to_excel("Scores.xlsx",index=False)

    print(50*"-"+" Scores "+ 50*"-")
    print(scores)
    return scores


def sentido(labs, FA_eficiencia, df_features, rotacao, verbose):
    
    if (rotacao in ['promax','oblimin','quartimin']):
        STR = pd.DataFrame(FA_eficiencia.structure_).copy()
    else:
        STR = pd.DataFrame(FA_eficiencia.loadings_).copy()  
          
    STR.index = labs
    sentido=list(df_features.Variavel[df_features.Sentido==1])
    #print(sentido)
    STR_origin=STR.copy()

    for x in (x for x in list(labs)):
        cut_df=pd.DataFrame(STR.loc[x])
        max_abs=abs(cut_df).max()
        indice=(abs(cut_df) == max_abs).idxmax(axis=0)[0]
        if ( (x in sentido and STR.loc[x][indice] > 0) or (x not in sentido and STR.loc[x][indice] < 0) ) :
                  STR.loc[x]=STR.loc[x]*-1 
    
    if verbose == True:
        print("Fatores sem inverter:")
        print(STR_origin)
        print()
        print("Fatores invertidos:")
        print(STR)
    
    return(STR)

