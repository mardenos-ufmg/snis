from utils.Normalizacao import normalizacao
from utils.gerar_fatores import fatores

def Score(df,df_features,delta=0.05,normalizar_score=True,save_excel=True,rotacao='oblimin',verbose=False):
    scores=fatores(df,df_features,delta,rotacao,verbose)

    if (normalizar_score==True):
        scores = normalizacao(scores)
     
    scores["Score_Medio"]=round(scores.mean(axis=1),3)      
             
    if (save_excel == True):
        scores.to_excel("Scores.xlsx",index=False)

    print(50*"-"+" Scores "+ 50*"-")
    print(scores)
    return scores