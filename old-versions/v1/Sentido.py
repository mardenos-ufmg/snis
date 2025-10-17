import pandas as pd
def sentido(labs,FA_grupos,df_features,rotacao,verbose):
    
    if (rotacao in ['promax','oblimin','quartimin']):
        STR = pd.DataFrame(FA_grupos.structure_).copy()
    else:
        STR = pd.DataFrame(FA_grupos.loadings_).copy()  
          
    STR.index = labs
    sentido=list(df_features.Variavel[df_features.Sentido==1])
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