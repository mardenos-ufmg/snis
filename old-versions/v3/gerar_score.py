from utils.Normalizacao import normalizacao
from utils.gerar_fatores import fatores

def Score(df,df_features,delta=0.05,normalizar_score=True,save_excel=True):
    scores=fatores(df,df_features,delta)

    if (normalizar_score==True):
        scores_norm = normalizacao(scores)
        scores_norm["Score_Medio"]=round(scores_norm.mean(axis=1),3)
        return scores_norm
    else:
        scores["Score_Medio"]=round(scores.mean(axis=1),3)
        return scores
    
    
    # if (save_excel == True):
    #    return scores_eee.to_excel("Scores")

