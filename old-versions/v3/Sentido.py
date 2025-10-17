import pandas as pd
def sentido(labs,FA_eficiencia,df_features):
    STR = pd.DataFrame(FA_eficiencia.structure_).copy()
    #print(STR)
    #print(labs)
    STR.index = labs
    sentido=list(df_features.Variavel[df_features.Sentido==1])
    #print("Fatores sem inverter:")
    #print(STR)
    for x in (x for x in list(labs) if x in sentido):
        STR.loc[x]=STR.loc[x]*-1
    return(STR)