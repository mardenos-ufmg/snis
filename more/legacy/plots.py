import matplotlib.pyplot as plt
import seaborn as sns
import scipy
from matplotlib import pyplot
from numpy import median
import numpy as np
import pandas as pd

from joypy import joyplot

import shapefile as shp
from unidecode import unidecode
import geopandas as gpd
import folium
from folium import plugins
import json
import branca.colormap as cmp
import shapely.geometry
import plotly.express as px


def plot_tipo_servico(df):
    df_aux=df.loc[:,["Tipo de serviço",'Score']]
    fig, b2 = plt.subplots(1,1, figsize=(7,7))
    b2 = sns.boxplot(x="Tipo de serviço", y="Score", palette='Set2', data=df_aux)

    b2.set_xlabel("Tipo de serviço", fontsize=12, labelpad=12)
    b2.set_ylabel("Score Médio", fontsize=12, labelpad=12)
    b2.set_yticks(np.arange(0.0, 1.1, 0.1))

    b2.set_title("Com Esgoto")
    plt.show()


def plot_grau_urb(df):
    df_aux=df.loc[:, ['Score','grau_urbanizacao','Nome_Município']]
    df_aux['Niveis_GU'] = np.where(df_aux.grau_urbanizacao<=0.50, "Até 50%", 
                                    np.where((df_aux.grau_urbanizacao>0.50)&(df_aux.grau_urbanizacao<=0.75), "de 50% a 75%",
                                             "Acima de 75%") )
    fig, ax = plt.subplots(1,1, figsize=(10, 7))

    order=["Até 50%","de 50% a 75%","Acima de 75%"]

    sns.set_style("ticks")
    ax = sns.boxplot(x='Niveis_GU', y='Score', order=order, showfliers=True, linewidth=0.8, showmeans=False, data=df_aux)
    ax = sns.pointplot(x='Niveis_GU', y='Score', order=order, data=df_aux, ci=None, color='black', linestyles='--',markers = '.')

    ax.set_xlabel("Grau Urbanizacao", fontsize=12, labelpad=12)
    ax.set_ylabel("Score Médio", fontsize=12, labelpad=12)


def plot_abrangencia(df):
    df_aux=df.loc[:, ['Score','Abrangência']]
    violins = sns.violinplot(x="Abrangência", y="Score", data=df_aux,width=0.45, palette='Blues')


def plot_natureza_juridica(df, type):
    #####  plot 1  #####
    
    df_aux=df.loc[:, ['Score','Natureza jurídica']]
    order=df_aux.groupby(["Natureza jurídica"])["Score"].median().sort_values().index
    p=sns.barplot(x='Score', y='Natureza jurídica', data=df_aux, estimator=np.median, order=order)
    
    #####  plot 2  #####
    
    df_aux=df.loc[:, ['Score','Prestador2']]

    fig, b2 = plt.subplots(1,1, figsize=(10, 7))
    b2 = sns.boxplot(x="Prestador2", y="Score", palette='Set2', data=df_aux)

    b2.set_xlabel("Prestador", fontsize=12, labelpad=12)
    b2.set_ylabel("Score Médio", fontsize=12, labelpad=12)

    plt.show()
    
    #####  plot 3  #####
    if (type == 'DH'):
        df_aux=df.loc[:, ['Score_Universalidade','Score_Sustentabilidade','Prestador2']]   
        # df_aux=df_aux.melt(id_vars=["prestador2"], 
        #         var_name="Tipo_Score", 
        #         value_name="Scores")
        # df_aux["Merge"]=df_aux["prestador2"]+" "+df_aux["Tipo_Score"]

        plt.figure()

        ax, fig = joyplot(
            data=df_aux, 
            by='Prestador2',
            column=['Score_Universalidade','Score_Sustentabilidade'],
            color=[ '#eb4d4b','#686de0',"#f37b2d"],
            legend=True,
            alpha=0.80,
            figsize=(12, 8),
            ylim='own',
            overlap=0
        )
        plt.title('Scores DH', fontsize=20)
        plt.show()
    elif (type == 'EEE'):
        df_aux=df.loc[:, ['Score_Efetividade','Score_Eficiencia','Score_Eficacia','Prestador2']]   
        # df_aux=df_aux.melt(id_vars=["prestador2"], 
        #         var_name="Tipo_Score", 
        #         value_name="Scores")
        # df_aux["Merge"]=df_aux["prestador2"]+" "+df_aux["Tipo_Score"]

        plt.figure()

        ax, fig = joyplot(
            data=df_aux, 
            by='Prestador2',
            column=['Score_Eficiencia','Score_Eficacia','Score_Efetividade'],
            color=['#686de0'#azul
                   ,"#f37b2d"#laranja           
                   , '#eb4d4b' #vermelho
                  ],
            legend=True,
            alpha=0.9,
            figsize=(12, 8),
            ylim='own',
            overlap=0
        )
        plt.title('Scores EEE', fontsize=20)
        plt.show()


def plot_mesoregiao(df):
    df_aux=df.loc[:, ['Score','Nome_Mesorregião']]
    order=df_aux.groupby(["Nome_Mesorregião"])["Score"].median().sort_values().index
    p=sns.barplot(x='Score', y='Nome_Mesorregião', data=df_aux, estimator=np.median, order=order)


def plot_mapa(df, type):
    df_media = pd.DataFrame( {'name': df['Nome_Município'], 'Score':round(df['Score'],4),
                          'Score_Universalidade':round(df['Score_Universalidade'],4),
                          'Score_Sustentabilidade':round(df['Score_Sustentabilidade'],4),
                          'Nome_Mesorregião':df['Nome_Mesorregião']})
    df_MG = df_media[~df_media.duplicated(subset=['name'], keep='first')]
    #df_MG.loc[:, 'Cidade'] = df_MG.loc[:, 'Cidade'].str.upper()
    
    if (type == "DH"):
        df_MG['Quantil'] = np.where(df_MG.Score<=0.25, "(0-25)%", 
                                    np.where((df_MG.Score>0.25)&(df_MG.Score<=0.5), "(25-50)%",
                                             np.where((df_MG.Score>0.5)&(df_MG.Score<=0.75), "(50-75)%", "(75-100)%")
                                            )
                                   )
    elif (type == "Universalidade"):
        df_MG['Quantil'] = np.where(df_MG.Score_Universalidade<=0.25, "(0-25)%", 
                            np.where((df_MG.Score_Universalidade>0.25)&(df_MG.Score_Universalidade<=0.5), "(25-50)%",
                                     np.where((df_MG.Score_Universalidade>0.5)&(df_MG.Score_Universalidade<=0.75), "(50-75)%", "(75-100)%")
                                    )
                           )
    elif (type == "Sustentabilidade"):
        df_MG['Quantil'] = np.where(df_MG.Score_Sustentabilidade<=0.25, "(0-25)%", 
                            np.where((df_MG.Score_Sustentabilidade>0.25)&(df_MG.Score_Sustentabilidade<=0.5), "(25-50)%",
                                     np.where((df_MG.Score_Sustentabilidade>0.5)&(df_MG.Score_Sustentabilidade<=0.75), "(50-75)%", "(75-100)%")
                                    )
                           )
    
    with open("geojs-31-mun.json", encoding="utf8") as file:
        geo_json_data = json.load(file)
    
    geo_df = gpd.GeoDataFrame.from_features(geo_json_data["features"]).merge(df_MG, on="name").set_index("name")
    fig = px.choropleth_mapbox(geo_df,
                               geojson=geo_df.geometry,
                               locations=geo_df.index,
                               color="Quantil",
                               category_orders= {'Quantil':["(0-25)%","(25-50)%","(50-75)%", "(75-100)%"]},
                               color_discrete_sequence=["#922B21", "#E67E22", "#F4D03F"," #52BE80"],
                               center={"lat": -19.84164, "lon": -43.98651},
                               mapbox_style="open-street-map",
                               zoom=6,
                               #hover_name='name',
                               hover_data=['Quantil','Score','Score_Universalidade','Score_Sustentabilidade'],
                               title=type
                              )
    fig.show()

