#DF
from dbfread import DBF
import geopandas as gpd
import pandas as pd

from datetime import datetime
import statistics
import numpy as np
import math

from shapely.wkb import loads as wkb_loads

#Graph
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.ticker import MaxNLocator
import seaborn as sns

import os
#### Linéaire de réseaux Unitaire 
def LRU(filtered_data, group_column, typereseau, ZG, niveau =''):
    dff = pd.DataFrame() 
    if typereseau == 'EU_U' :
        df_bis = filtered_data[2][(filtered_data[2]['typeadministratifreseau'] == 'UN') | ((filtered_data[2]['typeadministratifreseau'] == 'NO') & (filtered_data[2]['typeeffluent'] == 'UN'))]
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000).astype(int)
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'longueur_sum']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            resultats = int(df_bis.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000)
            dff = pd.DataFrame([resultats], columns=["longueur_sum"])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Linéaire de réseaux Eaux Usées
def LREU(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' : 
        df_bis = filtered_data[2][(filtered_data[2]['typeadministratifreseau'] == 'EU') | ((filtered_data[2]['typeadministratifreseau'] == 'NO') & (filtered_data[2]['typeeffluent'] == 'EU'))]
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000).astype(int)
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'longueur_sum']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            resultats = int(df_bis.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000)
            dff = pd.DataFrame([resultats], columns=["longueur_sum"])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Linéaire de réseaux 
def LEP(filtered_data, group_column, ZG, niveau =''):
    if group_column:
        groupes = filtered_data[2].groupby(group_column)
        indicateur_sum = groupes.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000)).astype(int)
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    else : 
        resultats_list = [int(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000)]
        dff = pd.DataFrame(resultats_list, columns=["longueur_sum"])
    return(dff)

#### Linéaire de réseaux par famille de materiau (au global)
def LRM(filtered_data, palette_CCD_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''): 
    if group_column :
        groupes = filtered_data[2].groupby([group_column, 'materiaupresume'])
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'materiaupresume', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
    else : 
        groupes = filtered_data[2].groupby('materiaupresume')
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)
        resultats = indicateur_sum.reset_index()
        resultats.columns = ['materiaupresume', 'longueur_sum']
        dff = pd.DataFrame(resultats)
    mapping = {
        'AMCI': 'Amiante cimenté',
        'AUTR': 'Autre',
        'BTAM': 'Béton armé',
        'BTAU': 'Béton autre',
        'FONT': 'Fonte',
        'PLAS': 'Plastique',
        'ROCH': 'Maçonnerie'
    }
    dff['materiaupresume'] = dff['materiaupresume'].map(mapping).fillna('Pas de données')
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        piecolorway = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_CCD_RGBA.values()]
        legend_handles = [mpatches.Patch(color=color) for color in piecolorway]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff.sort_values(by='longueur_sum', ascending=False)
        plt.pie(
            df_sorted_desc['longueur_sum'], 
            labels = df_sorted_desc['longueur_sum'].astype(int).astype(str) + ' km',
            labeldistance = 1.05,
            colors= piecolorway,
            textprops={'fontsize': 12},
            wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
            startangle = 90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            df_sorted_desc['materiaupresume'],
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 12})
        
        out_figure = os.path.join(out_folder, f'Linéaire par Famille de Matériau_{typereseau}_{NivExt}_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#### Linéaire de réseaux avec matériaux connu
def LRMC(filtered_data, group_column, ZG, niveau =''):
    df_bis = filtered_data[2][filtered_data[2]['materiau'].notna()].copy()
    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)
    if group_column:
        df_grouped = df_bis.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column) 
        
        indicateur_sum = df_grouped.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3))
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)).reset_index()
        
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'longueur_sum']
        indicateur_tot.columns = [group_column, 'longueur_tot']
        dff =  pd.merge(resultats, indicateur_tot, on = group_column, how = 'outer').reset_index()
        dff = dff.fillna(0)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index()
        if dff.empty:
            dff['pourcentage'] = 0
        else :
            dff['Pourcentage'] = int((dff['longueur_sum'] * 100) / dff['longueur_tot'])
    else : 
        resultats = 0
        df = filtered_data[2].drop_duplicates(subset='identifiantcanap')
        for index, row in df.iterrows():
            if not pd.isnull(row["materiau"]):
                resultats += row["longueur"] / 1000
        pourcentage = int(resultats * 100 / tot)
        dff = pd.DataFrame({'longueur_sum': [int(resultats)], 'Pourcentage': pourcentage})        
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Linéaire de réseaux avec matériaux estimé
def LRME(filtered_data, group_column, ZG, niveau =''):

    df_bis1 = filtered_data[2][~filtered_data[2]['materiau'].isnull()]
    resultat1 = round(df_bis1.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)

    df_bis2 = filtered_data[2][~filtered_data[2]['materiaupresume'].isnull()]
    resultat2 = round(df_bis2.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)

    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)

    if group_column: 

        groupes1 = df_bis1.groupby(group_column)
        groupes2 = df_bis2.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column) 

        indicateur_sum = (groupes2.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))-(groupes1.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)).reset_index()

        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'longueur_sum']
        indicateur_tot.columns = [group_column, 'longueur_tot']

        dff =  pd.merge(resultats, indicateur_tot, on = group_column, how = 'outer').reset_index()
        dff = dff.fillna(0)

        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty:
            dff['pourcentage'] = 0
        else :
            dff['Pourcentage'] = round((dff['longueur_sum'] * 100) / dff['longueur_tot'], 2)
    else : 
        resultats = round(resultat2 - resultat1, 3)
        pourcentage = round(resultats * 100 /tot, 2)
        dff = pd.DataFrame({'longueur_sum': [resultats], 'Pourcentage': pourcentage})
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Linéaire de réseaux avec matériaux connus et estimes
def LRM_E_C(filtered_data, group_column, ZG, niveau =''):
    
    df_connu = filtered_data[2][filtered_data[2]['materiau'].notna()]
    df_mat_est = filtered_data[2][filtered_data[2]['materiau'].isnull() & filtered_data[2]['materiaupresume'].notna()]
    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)
     
    if group_column:
        
        groupes_estimes = df_mat_est.groupby(group_column)
        groupes_connus = df_connu.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column) 
        
        resultats_connus = (round(groupes_connus.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)).reset_index()
        resultats_estimes = (round(groupes_estimes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)).reset_index()
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)).reset_index()

        resultats_estimes.columns = [group_column, 'materiaux_estimes']
        resultats_connus.columns = [group_column, 'materiaux_connus']
        indicateur_tot.columns = [group_column, 'longueur_tot']

        dff = pd.merge(pd.merge(resultats_estimes, resultats_connus, on= group_column), indicateur_tot, on= group_column)
        dff = dff.fillna(0)

        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index()
        if dff.empty:
            dff['Pourcentage_estimes'] = 0
            dff['Pourcentage_connus'] = 0
            dff['Pourcentage_total'] = 0
        else :
            dff['Pourcentage_estimes'] = round((dff['materiaux_estimes'] * 100)/dff['longueur_tot'], 2)
            dff['Pourcentage_connus'] = round((dff['materiaux_connus'] * 100)/dff['longueur_tot'], 2)
            dff['Pourcentage_total'] = round((dff['longueur_tot'] * 100)/dff['longueur_tot'], 2)
    else : 
        resultats1 = round(df_connu.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)
        resultats2 = round(df_mat_est.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 3)
        pourcentage1 = round(resultats1 * 100 / tot,2)
        pourcentage2 = round(resultats2 * 100 / tot,2)
        pourcentage3 = round(tot * 100 / tot,2)
        dff = pd.DataFrame({'materiaux_estimes': [resultats2], 'Pourcentage_estimes': pourcentage2, 'materiaux_connus': resultats1, 'Pourcentage_connus': pourcentage1, 'longueur_tot': tot, 'Pourcentage_total': pourcentage3})
    
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Linéaire de réseaux visitable (> ou = à 1,5 m) et non visitable (< 150)
def LRV(filtered_data, group_column, ZG, niveau=''):
    df_bis1 = filtered_data[2][filtered_data[2]['hauteur'] >= 150].copy()
    df_bis2 = filtered_data[2][filtered_data[2]['hauteur'] < 150].copy()
    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000, 3)
    
    if group_column:
        groupes1 = df_bis1.groupby(group_column)
        groupes2 = df_bis2.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column)

        indicateur_sum1 = groupes1.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000, 3))
        indicateur_sum2 = groupes2.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000, 3))
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000, 3)).reset_index()

        # Créer un DataFrame pour les résultats
        resultats1 = indicateur_sum1.reset_index()
        resultats2 = indicateur_sum2.reset_index()
        resultats1.columns = [group_column, 'longueur_visitable']
        resultats2.columns = [group_column, 'longueur_non_visitable']
        indicateur_tot.columns = [group_column, 'longueur_tot']

        dff = pd.merge(pd.merge(resultats1, resultats2, on=group_column, how= 'outer'), indicateur_tot, on=group_column, how= 'outer')
        dff = dff.fillna(0)
        
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else:
            dff = dff[dff[group_column] == ZG].reset_index()

        if dff.empty:
            dff['Pourcentage_visitable'] = 0
            dff['Pourcentage_non_visitable'] = 0
        else:
            dff['Pourcentage_visitable'] = round((dff['longueur_visitable'] * 100) / dff['longueur_tot'])
            dff['Pourcentage_non_visitable'] = round((dff['longueur_non_visitable'] * 100) / dff['longueur_tot'])
    else:
        resultats1 = 0
        resultats2 = 0
        df = filtered_data[2].drop_duplicates(subset='identifiantcanap')
        
        for index, row in df.iterrows():
            if row["hauteur"] >= 150:
                resultats1 += round(row["longueur"] / 1000, 3)
            else:
                resultats2 += round(row["longueur"] / 1000, 3)
        
        pourcentage_vis = round(resultats1 * 100 / tot)
        pourcentage_non_vis = round(resultats2 * 100 / tot)
        dff = pd.DataFrame({'longueur_visitable': [resultats1], 'longueur_non_visitable': [resultats2], 'Pourcentage_visitable': [pourcentage_vis], 'Pourcentage_non_visitable': [pourcentage_non_vis]})

    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    
    return dff

#### Nombre de DO
def NDO(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' :
        if group_column:
            groupes = filtered_data[4].groupby(group_column)
            indicateur_nbr = groupes['identifiantdevorage'].nunique()
            resultats = indicateur_nbr.reset_index()
            resultats.columns = [group_column, 'nombre']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else :
            resultats = filtered_data[4]['identifiantdevorage'].nunique()
            resultats_list = [resultats]
            dff = pd.DataFrame(resultats_list, columns=["nombre"])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de dessableurs
def NDes(filtered_data, group_column, ZG, niveau =''):
    df_bis = filtered_data[12][filtered_data[12]['typetraitement'] == 'SAB'].copy()
    if group_column:
        groupes = df_bis.groupby(group_column)
        dff = groupes['identifianttraitement'].nunique().reset_index(name='nombre')
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
    else:
        indicateur_nbr = df_bis['identifianttraitement'].nunique()
        dff = pd.DataFrame({'nombre': [indicateur_nbr]})
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Nombre de STR
def NSTR(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' :
        df_NSTR = filtered_data[8][filtered_data[8]['numero'].notna() & (filtered_data[8]['numero'] != 0)]
        if group_column:
            groupes = df_NSTR.groupby(group_column)
            indicateur_nbr = groupes['identifiantstare'].nunique()
            resultats = indicateur_nbr.reset_index()
            resultats.columns = [group_column, 'nombre_STR']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else :
            resultats = df_NSTR['identifiantstare'].nunique()
            resultats_list = [resultats]
            dff = pd.DataFrame(resultats_list, columns=["nombre_STR"])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de STEU
def NSTEU(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' :
        steu = {
                13 : 'Genay',
                6 : 'Fontaines',
                8 : 'Jonage',
                1 : 'La Feyssine',
                11 : 'Lissieu Le Bourg',
                12 : 'Lissieu Sémanet',
                10 : 'Meyzieu',
                5 : 'Neuville',
                7 : 'Pierre-Bénite',
                17 : 'Quincieux',
                2 : 'Saint-Fons',
                4 : 'Saint-Germain',
                3 : 'Givors'
            }
        dfsteu_bis = filtered_data[9].copy()
        dfsteu_bis['nomsteu'] = dfsteu_bis['identifiantsteu'].map(steu).fillna(dfsteu_bis['nomsteu'])
        if group_column:
            groupes = dfsteu_bis.groupby(group_column)
            indicateur_nbr = groupes['identifiantsteu'].nunique()
            resultats = indicateur_nbr.reset_index()
            resultats.columns = [group_column, 'nombre_STEU']
            dff = pd.DataFrame(resultats)
            print(dff)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else :
            resultats = filtered_data[9]['identifiantsteu'].nunique()
            resultats_list = [resultats]
            dff = pd.DataFrame(resultats_list, columns=["nombre_STEU"])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de bassins enterrés et ciel ouvert
def NBEO(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_OUV = filtered_data[0][filtered_data[0]['nature'] == 'OUV'] 
        df_ENT = filtered_data[0][filtered_data[0]['nature'] == 'ENT']

        if group_column:
            groupes1 = df_ENT.groupby([group_column])
            groupes2 = df_OUV.groupby([group_column])

            resultats1 = groupes1['identifiantbassin'].nunique().reset_index()
            resultats2 = groupes2['identifiantbassin'].nunique().reset_index()
            
            resultats1.columns = [group_column, 'NBR_BE']
            resultats1['nature'] = 'ENT'
            resultats2.columns = [group_column, 'NBR_BE']
            resultats2['nature'] = 'OUV'

            dff = pd.concat([resultats1, resultats2])
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau]
                if dff[dff['nature'] == 'OUV'].empty:
                    new_row = pd.DataFrame({group_column: niveau,'nature': ['OUV'], 'NBR_BE': [0]})
                    dff = pd.concat([dff, new_row], ignore_index=True)
                if dff[dff['nature'] == 'ENT'].empty:
                    new_row = pd.DataFrame({group_column: niveau, 'nature': ['ENT'], 'NBR_BE': [0]})
                    dff = pd.concat([dff, new_row], ignore_index=True)
            else : 
                dff = dff[dff[group_column] == ZG]
                if dff[dff['nature'] == 'OUV'].empty:
                    new_row = pd.DataFrame({group_column: ZG,'nature': ['OUV'], 'NBR_BE': [0]})
                    dff = pd.concat([dff, new_row], ignore_index=True)
                if dff[dff['nature'] == 'ENT'].empty:
                    new_row = pd.DataFrame({group_column: ZG, 'nature': ['ENT'], 'NBR_BE': [0]})
                    dff = pd.concat([dff, new_row], ignore_index=True)
            dff = dff.sort_values(by='nature', ascending=True).reset_index()
        else : 
            resultats1 = df_ENT['identifiantbassin'].nunique()
            resultats2 = df_OUV['identifiantbassin'].nunique()
            dff = pd.DataFrame({'nature': ['ENT', 'OUV'], 'NBR_BE': [resultats1, resultats2]})
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de branchements
def NBr(filtered_data, group_column, ZG, niveau =''):
    if group_column:
        groupes = filtered_data[3].groupby(group_column)
        indicateur_sum = groupes['identifiantcanas'].nunique()
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'nbr_branch']
        dff = pd.DataFrame(resultats)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
    else : 
        resultats_list = [filtered_data[3]['identifiantcanas'].nunique()]
        dff = pd.DataFrame(resultats_list, columns=["nbr_branch"])
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Nombre de trémie
def NT(filtered_data, group_column, typereseau, ZG, niveau= ''):
    if typereseau == 'EP' : 
        if group_column:
            groupes = filtered_data[11].groupby(group_column)
            indicateur_sum = groupes['numero'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'nbr_trmie']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [filtered_data[11]['numero'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['nbr_trmie'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre d'arbres de pluie
def NAP(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_bis = filtered_data[5][filtered_data[5]['natureogep'] == 7].copy()
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes['identifiantogep'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'NBR_AP']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [filtered_data[5][filtered_data[5]['natureogep']== 7]['identifiantogep'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['NBR_AP'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de puits d'infiltration
def NPI(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        if group_column:
            groupes = filtered_data[6].groupby(group_column)
            indicateur_sum = groupes['identifiantpuitsf'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'NBR_PI']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [filtered_data[6]['identifiantpuitsf'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['NBR_PI'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de FPR
def NFPR(filtered_data, group_column, typereseau, ZG, niveau =''):

    if typereseau == 'EU_U' :
        df_bis = filtered_data[9][filtered_data[9]['nomsteu'].str.contains("Filtre planté de roseaux", na=False)]
        if group_column:
            grouped_data = df_bis.groupby(group_column)
            resultat = grouped_data['identifiantsteu'].nunique().reset_index()
            resultat.columns = [group_column, 'NBR_FPR']
            dff = pd.DataFrame(resultat)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = filtered_data[9][filtered_data[9]['nomsteu'].str.contains("Filtre planté de roseaux", na=False)]['identifiantsteu'].nunique()
            dff = pd.DataFrame([resultats_list], columns=['NBR_FPR'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf Réseaux (Milliards d'euros)
def VPR(filtered_data, group_column, VP_RV, VP_RNV, ZG, niveau =''):
    df_bis1 = filtered_data[2][filtered_data[2]["hauteur"] >= 150].copy()
    resultat1 = df_bis1.drop_duplicates(subset='identifiantcanap')['longueur'].sum()
    df_bis2 = filtered_data[2][filtered_data[2]["hauteur"] < 150].copy()
    resultat2 = df_bis2.drop_duplicates(subset='identifiantcanap')['longueur'].sum()
    
    if group_column:
        groupes1 = df_bis1.groupby(group_column)
        groupes2 = df_bis2.groupby(group_column)
        indicateur_sum = round(((groupes1.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum())*VP_RV)+ (groupes2.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()) * VP_RNV))/1000000,1)
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'Valeur']
        dff = pd.DataFrame(resultats)
        if ZG == 'Tous': 
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index()
    else : 
        resultats_list = [round(((resultat1 * VP_RV) + (resultat2 * VP_RNV) )/1000000,1)]
        dff = pd.DataFrame(resultats_list, columns=['Valeur'])
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Valeur patrimoniale à neuf Branchements (Milliards d'euros)
def VPBr(filtered_data, group_column, VP_branchement, ZG, niveau =''):
    if group_column:
        groupes = filtered_data[3].groupby(group_column)
        indicateur_sum = round(groupes['identifiantcanas'].nunique()* VP_branchement/1000000, 1)
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'VP_branch']
        dff = pd.DataFrame(resultats)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
    else : 
        resultats = 0
        for index, row in filtered_data[3].iterrows():
            resultats += (VP_branchement/1000000)
        resultats_list = [round(resultats, 1)]
        dff = pd.DataFrame(resultats_list, columns=['VP_branch'])
    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Valeur patrimoniale à neuf arbres de pluie (k€)
def VPAP(filtered_data, group_column, VP_AP, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_bis = filtered_data[5][filtered_data[5]['natureogep'] == 7].copy()
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = round(groupes['identifiantogep'].nunique() * VP_AP, 1)
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'VAL_AP']
            dff = pd.DataFrame(resultats) 
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [round(filtered_data[5][filtered_data[5]['natureogep']== 7]['identifiantogep'].nunique()* VP_AP, 1)]
            dff = pd.DataFrame(resultats_list, columns=['VAL_AP'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf puits d'infiltration (M€)
def VPPI(filtered_data, group_column, VP_PI, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        if group_column:
            groupes = filtered_data[6].groupby(group_column)
            indicateur_sum = round(groupes['identifiantpuitsf'].nunique() * VP_PI / 1000, 1)
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'VAL_PI']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [round((filtered_data[6]['identifiantpuitsf'].nunique()) * VP_PI /1000, 1)]
            dff = pd.DataFrame(resultats_list, columns=['VAL_PI'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

####Linéaire de reseaux avec années de pose connues & estimées 
def LRAPC(filtered_data, group_column, palette_basique, out_folder, typereseau, ZG, NODATA_PATH, niveau =''):
    df_connues = filtered_data[2][filtered_data[2]['anneepose'].notna()].copy()
    df_estimees =  filtered_data[2][(filtered_data[2]['anneepose'].isnull()) & ((filtered_data[2]['anneeposesuperieur'].notna()) | (filtered_data[2]['anneeposeinferieur'].notna())) ].copy()
    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)
    
    if group_column:
        
        groupes_connues = df_connues.groupby(group_column)
        groupes_estimees = df_estimees.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column) 

        resultats_connues = (round(groupes_connues.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 2)).reset_index()
        resultat_estimees = (round(groupes_estimees.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 2)).reset_index()
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)).reset_index()

        resultats_connues.columns = [group_column, 'Longueur_connues']
        resultat_estimees.columns = [group_column, 'Longueur_estimees']
        indicateur_tot.columns = [group_column, 'longueur_tot']

        dff = pd.merge(pd.merge(resultats_connues, resultat_estimees, on= group_column), indicateur_tot, on = group_column, how = 'outer')
        dff = dff.fillna(0)
        
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty:
            dff['Pourcentage_connues'] = 0
            dff['Pourcentage_estimees'] = 0
        else :
            dff['Pourcentage_connues'] = round((dff['Longueur_connues'] * 100)/dff['longueur_tot'], 2)
            dff['Pourcentage_estimees'] = round((dff['Longueur_estimees'] * 100)/dff['longueur_tot'], 2)
    else : 
        resultat_connues = round(df_connues.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)
        resultat_estimees = round(df_estimees.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)
        
        pourcentage_connues = round((resultat_connues *100)/tot, 2)
        pourcentage_estimees = round((resultat_estimees *100)/tot, 2)

        dff = pd.DataFrame({'Longueur_connues': [resultat_connues],'Pourcentage_connues': [pourcentage_connues],'Longueur_estimees': [resultat_estimees], 'Pourcentage_estimees': [pourcentage_estimees]})

    # Mise en forme de l'indicateur
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        fig = go.Figure()
        fig.add_trace(go.Bar(
            y=[0.1],
            x=dff['Longueur_connues'],
            orientation='h',
            name='Année de pose connue',
            marker=dict(color= palette_basique['Vert_Cyan']),
            text= dff['Longueur_connues'].round(1).astype(str) + 'km<Br>' +(dff['Pourcentage_connues'].round()).astype(str) +'%',
            textposition='auto',  
            width = 0.05
        ))

        fig.add_trace(go.Bar(
            y=[0.17],
            x=dff['Longueur_estimees'],
            orientation='h',
            name='Année de pose estimée',
            marker=dict(color= palette_basique['Rouge']),
            text= dff['Longueur_estimees'].round(1).astype(str) + 'km<Br>' +(dff['Pourcentage_estimees'].round()).astype(str) +'%',
            textposition='auto',  
            width = 0.05
        ))

        # Mise en forme du titre et de la légende
        fig.update_layout(
            barmode='stack',
            xaxis=dict(title='Linéaire de réseaux (Km)'),
            yaxis=dict(showticklabels=False,
                automargin=True,
            tickfont_size=10),
            legend=dict(yanchor="bottom", y=1, xanchor="left", x=0.8),  
            plot_bgcolor='rgba(0,0,0,0)',
            margin=dict(l=10, r=10, t=10, b=10)
        )
        out_figure = os.path.join(out_folder, f'LRACE_{typereseau}_{NivExt}_{niveau}.png')
        pio.write_image(fig, out_figure)
        return(dff,out_figure)

#### Age moyen de réseaux 
def AMR(filtered_data, group_column, ZG, niveau =''):
    ages = []
    df = []
    year = datetime.now().year
    for index, row in filtered_data[2].iterrows():
        if (not pd.isnull(row["anneepose"])) and (int(row["anneepose"]) != 0):
            ages.append((row["identifiantcanap"], year - int(row["anneepose"])))
            df.append(row)
    df_bis = pd.DataFrame(df)
    df_ages = pd.DataFrame(ages)
    df_ages.columns = ['identifiantcanap', 'ages']
    df_bis = pd.merge(df_bis, df_ages, on= 'identifiantcanap', how= 'left')
    if group_column:
        groupes = df_bis.groupby([group_column])
        indicateur_moy = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['ages'].mean()),0)
        resultats = indicateur_moy.reset_index()
        resultats.columns = [group_column, 'moy_age']
        dff = pd.DataFrame(resultats)
        #dff= dff.fillna(0)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
    else:
        resultats_list = [round(df_bis.drop_duplicates(subset='identifiantcanap')['ages'].mean(),0)]
        dff = pd.DataFrame(resultats_list, columns=['moy_age'])
    
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Age médian des réseaux
def AMeR(filtered_data, group_column, ZG, niveau =''):
    ages = []
    df = []
    year = datetime.now().year
    for index, row in filtered_data[2].iterrows():
        if (not pd.isnull(row["anneepose"])) and (int(row["anneepose"]) != 0):
            ages.append((row["identifiantcanap"], year - int(row["anneepose"])))
            df.append(row)
    df_bis = pd.DataFrame(df)
    df_ages = pd.DataFrame(ages)
    df_ages.columns = ['identifiantcanap', 'ages']
    df_bis = pd.merge(df_bis, df_ages, on= 'identifiantcanap', how= 'left')
    if group_column:
        groupes = df_bis.groupby([group_column])
        indicateur_med = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['ages'].median()), 0)
        resultats = indicateur_med.reset_index()
        resultats.columns = [group_column, 'med_age']
        dff = pd.DataFrame(resultats)
        dff= dff.fillna(0)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
    else:
        resultats_list = [round(df_bis.drop_duplicates(subset='identifiantcanap')['ages'].median(), 0)]
        dff = pd.DataFrame(resultats_list, columns=['med_age'])
    
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Année de mise en place des STR
def APSTR(filtered_data, group_column, colors_pie, out_folder, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U' :
        if group_column:
            groupes = filtered_data[8].groupby([group_column, 'anneeconstruction'])
            indicateur_sum = groupes['identifiantstare'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'anneeconstruction', 'nb_stare']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            groupes = filtered_data[8].groupby('anneeconstruction')
            indicateur_sum = groupes['identifiantstare'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['anneeconstruction', 'nb_stare']
            dff = pd.DataFrame(resultats)
        dff['anneeconstruction'] = dff['anneeconstruction'].apply(lambda x : int(x))
        dff['anneeconstruction'][dff['anneeconstruction']==0] = None

        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            barplot= sns.barplot(x='anneeconstruction', y = 'nb_stare', data=dff, color=colors_pie[0], width=0.35)
            plt.grid(True, which='major', axis='y', linestyle='--', linewidth=0.5)
            barplot.set_axisbelow(True)
            barplot.xaxis.set_major_locator(MaxNLocator(integer=True))
            plt.xlabel('Année de construction', fontsize=10)
            plt.ylabel('Nombre de STR', fontsize=10)
            plt.xticks(rotation=45, ha='right', fontsize=10)
            plt.yticks(fontsize=10)
            sns.despine(left=True, bottom=True)
            out_figure = os.path.join(out_folder,  f'Annee_de_mise_en_place_des_STR_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff, out_figure)

#### Année de mise en place des STEU
def APSTEU(filtered_data, group_column, colors_pie, out_folder, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U' :
        steu = {
                13 : 'Genay',
                6 : 'Fontaines',
                8 : 'Jonage',
                1 : 'La Feyssine',
                11 : 'Lissieu Le Bourg',
                12 : 'Lissieu Sémanet',
                10 : 'Meyzieu',
                5 : 'Neuville',
                7 : 'Pierre-Bénite',
                17 : 'Quincieux',
                2 : 'Saint-Fons',
                4 : 'Saint-Germain',
                3 : 'Givors'
            }
        dfsteu_bis = filtered_data[9].copy()
        dfsteu_bis['nomsteu'] = dfsteu_bis['identifiantsteu'].map(steu).fillna(dfsteu_bis['nomsteu'])
        if group_column:
            groupes = dfsteu_bis.groupby([group_column, 'anneeconstruction'])
            indicateur_sum = groupes['identifiantsteu'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'anneeconstruction', 'nb_steu']
            if ZG != 'Tous':
                resultats = resultats[resultats[group_column] == ZG].reset_index()
            else:
                resultats = resultats[resultats[group_column] == niveau].reset_index()
        else : 
            groupes = dfsteu_bis.groupby('anneeconstruction')
            indicateur_sum = groupes['identifiantsteu'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['anneeconstruction', 'nb_steu']
        dff = pd.DataFrame(resultats)
        dff['anneeconstruction'] = dff['anneeconstruction'].apply(lambda x : int(x))
        dff['anneeconstruction'][dff['anneeconstruction']==0] = None
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else :
            barplot= sns.barplot(x='anneeconstruction', y = 'nb_steu', data=dff, color=colors_pie[0], width= 0.35)
            plt.grid(True, which='major', axis='y', linestyle='--', linewidth=0.5)
            barplot.set_axisbelow(True)
            plt.xlabel('Année de construction', fontsize=10)
            plt.ylabel('Nombre de STEU', fontsize=10)
            plt.xticks(rotation=45, ha='right', fontsize=10)
            plt.yticks(range(0, int(dff['nb_steu'].max())+1, 1), fontsize=10)
            sns.despine(left=True, bottom=True)
            out_figure = os.path.join(out_folder,  f'Année_de_mise_en_place_des_STEU_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff, out_figure)

#### Linéaire de réseaux en classe A, B et C
def LinClass(filtered_data, group_column, palette_basique_RGBA, out_folder, ZG, NODATA_PATH, niveau =''):
    df_A =  filtered_data[2][filtered_data[2]['precisionplanimetrique'].isin([0, 1, 2, 3])].copy()
    df_B =  filtered_data[2][filtered_data[2]['precisionplanimetrique'] == 4].copy()
    df_C =  filtered_data[2][filtered_data[2]['precisionplanimetrique'] == 5].copy()
    tot = round(filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)
    
    if group_column:
        
        groupesA = df_A.groupby(group_column)
        groupesB = df_B.groupby(group_column)
        groupesC = df_C.groupby(group_column)
        df_tot = filtered_data[2].groupby(group_column) 

        resultatsA = (round(groupesA.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 2)).reset_index()
        resultatsB = (round(groupesB.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 2)).reset_index()
        resultatsC = (round(groupesC.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 2)).reset_index()
        indicateur_tot = df_tot.apply(lambda x: round(x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2)).reset_index()

        resultatsA.columns = [group_column, 'longueur_sum_A']
        resultatsB.columns = [group_column, 'longueur_sum_B']
        resultatsC.columns = [group_column, 'longueur_sum_C']
        indicateur_tot.columns = [group_column, 'longueur_tot']

        dff = pd.merge(pd.merge(pd.merge(resultatsA, resultatsB, on= group_column), resultatsC, on= group_column), indicateur_tot, on = group_column)
        dff = dff.fillna(0)
        if ZG == 'Tous': 
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty:
            dff['Pourcentage_A'] = 0
            dff['Pourcentage_B'] = 0
            dff['Pourcentage_C'] = 0
        else:
            dff['Pourcentage_A'] = round((dff['longueur_sum_A'] * 100)/dff['longueur_tot'], 2)
            dff['Pourcentage_B'] = round((dff['longueur_sum_B'] * 100)/dff['longueur_tot'], 2)
            dff['Pourcentage_C'] = round((dff['longueur_sum_C'] * 100)/dff['longueur_tot'], 2)
    else : 
        resultatsA = [round(df_A.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2),round(df_A.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/(tot*10))]
        resultatsB = [round(df_B.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2),round(df_B.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/(tot*10))]
        resultatsC = [round(df_C.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000, 2),round(df_C.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/(tot*10))]

        dff = pd.DataFrame({'longueur_sum_A': [resultatsA[0]], 'Pourcentage_A': [resultatsA[1]], 'longueur_sum_B': [resultatsB[0]], 'Pourcentage_B':  [resultatsB[1]],
            'longueur_sum_C': [resultatsC[0]], 'Pourcentage_C':  [resultatsC[1]]})
    
    # Mise en forme de l'indicateur
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        palette_basique_RGBA_list = [palette_basique_RGBA["État bon"],palette_basique_RGBA["État dégradé"],palette_basique_RGBA["État mauvais"]]
        colors_pie = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_basique_RGBA_list]
        legend_labels = ['A (< 50 cm)', 'B (< 150 cm)', 'C (>= 150 cm)']
        legend_handles = [mpatches.Patch(color=color) for color in colors_pie]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        # Création de la liste des étiquettes
        labels = [
            f"{dff['longueur_sum_A'].sum().astype(int).astype(str)} km\n{dff['Pourcentage_A'].sum().astype(int).astype(str)} %",
            f"{dff['longueur_sum_B'].sum().astype(int).astype(str)} km\n{dff['Pourcentage_B'].sum().astype(int).astype(str)} %",
            f"{dff['longueur_sum_C'].sum().astype(int).astype(str)} km\n{dff['Pourcentage_C'].sum().astype(int).astype(str)} %"
                ] 
        plt.pie(
            [dff['longueur_sum_A'].sum(), dff['longueur_sum_B'].sum(), dff['longueur_sum_C'].sum()], 
            labels= labels, 
            labeldistance = 1.05,
            colors= colors_pie,
            textprops={'fontsize': 8},
            wedgeprops = { 'linewidth' : 7, 'edgecolor' : 'white' },
            startangle = 90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            legend_labels,
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 10})
        
        out_figure = os.path.join(out_folder, f'classe_reseaux_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#### Linéaire de réseaux visitables- inspection (DIAG et PREDIAG)
def LDiagPrediag(df_vis, group_column,group_list, et_2_bis_value):
    #LDIAG(df_vis, group_column, ZG, et_2_bis_value, niveau=None)
    df_total = df_vis.filter(items=['identifiantcanap', 'ET_2', 'ET_2_BIS', 'longueur', group_column])
    df_et_2_bis = df_total[df_total['ET_2_BIS'] == et_2_bis_value]
    df_year = df_total[(df_total['ET_2'] == datetime.now().year - 1) & (df_total['ET_2_BIS'] == et_2_bis_value)]

    if group_column:
        groupes = df_total.groupby(group_column)
        groupes_total = df_et_2_bis.groupby(group_column)
        groupes_year = df_year.groupby(group_column)
        #print(df_total)
        #print(df_et_2_bis)
        #print(df_year)
        # Collecteurs concernables : Total
        if groupes.ngroups != 0:
            indicateur_sum = pd.DataFrame((round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000)))).reset_index(names=group_column)
            indicateur_sum.columns = [group_column, 'Total']
        else:
            indicateur_sum = pd.DataFrame(columns=[group_column, 'Total'])

        # Collecteurs concernés depuis 1990 : Total_cumule
        if groupes_total.ngroups != 0:
            resultats_total = pd.DataFrame((round(groupes_total.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000)))).reset_index(names=group_column)
            resultats_total.columns = [group_column, 'Total_cumule']
        else:
            resultats_total = pd.DataFrame(columns=[group_column, 'Total_cumule'])
    
        # Collecteurs réalisés dans l'année
        if groupes_year.ngroups != 0:
            resultats_year = pd.DataFrame((round(groupes_year.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum() / 1000)))).reset_index(names=group_column)
            resultats_year.columns = [group_column, 'Realise_dans_lannee']
        else:
            resultats_year = pd.DataFrame(columns=[group_column, 'Realise_dans_lannee'])

        dff = (pd.merge(indicateur_sum, 
                pd.merge(resultats_year, resultats_total, on=group_column, how='outer'), 
                on=group_column, 
                how='outer')
         .fillna(0))
        """
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index(drop=True)
        else:
            dff = dff[dff[group_column] == niveau].reset_index(drop=True)
        """
        if dff.empty:
            dff['Pourcentage'] = 0
        else:
            dff = dff.fillna(0)
            percentage = dff['Total_cumule'] * 100 / dff['Total']
            percentage = percentage.fillna(0)
            dff['Pourcentage'] = np.where(dff['Total'] != 0, percentage.astype(np.int64), 0)
            
        missing_items = set(group_list) - set(dff[group_column])
        dff = pd.concat([dff,pd.DataFrame(missing_items,columns=[group_column])], ignore_index=True)
        dff.fillna("Non concerné") 
    else:
        df_total = df_vis.filter(items=['identifiantcanap', 'ET_2', 'ET_2_BIS', 'longueur', group_column])
        df_et_2_bis = df_total[df_total['ET_2_BIS'] == et_2_bis_value]
        df_year = df_total[(df_total['ET_2'] == datetime.now().year - 1) & (df_total['ET_2_BIS'] == et_2_bis_value)]

        longueur_elements_concernes = df_total.drop_duplicates(subset='identifiantcanap')["longueur"].div(1000).sum().astype(np.int64)
        longueur_elements_realises_depuis_1990 = df_et_2_bis.drop_duplicates(subset='identifiantcanap')["longueur"].div(1000).sum().astype(np.int64)

        resultats = [[
            longueur_elements_concernes,
            df_year.drop_duplicates(subset='identifiantcanap')["longueur"].div(1000).sum().astype(np.int64),
            longueur_elements_realises_depuis_1990,
            ((longueur_elements_realises_depuis_1990 * 100) / longueur_elements_concernes).astype(np.int64) if longueur_elements_concernes != 0 else 0
        ]]

        dff = pd.DataFrame(resultats, columns=['Total', 'Realise_dans_lannee', 'Total_cumule', 'Pourcentage'])
        dff.index = [et_2_bis_value]

    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        
    return dff

#### Linéaires de réseaux secs courant faible / Fort
def LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau):
    # Creation de data Frame
    if typereseau == 'EU_U':
        gdf_Fa =  gdf_glob_EU_U[gdf_glob_EU_U["typereseausec"] == 'FAIBLE'].copy()
        gdf_Fo =  gdf_glob_EU_U[gdf_glob_EU_U["typereseausec"] == 'FORT'].copy()
    else :
        gdf_Fa =  gdf_glob_EP[gdf_glob_EP["typereseausec"] == 'FAIBLE'].copy()
        gdf_Fo =  gdf_glob_EP[gdf_glob_EP["typereseausec"] == 'FORT'].copy()

    if not(group_column): 
        resultatsFa = [int(gdf_Fa['longueur_troncon'].sum())]
        resultatsFo = [int(gdf_Fo['longueur_troncon'].sum())]
        dff = pd.DataFrame({'longueur_Rsec_Faible': resultatsFa, 'longueur_Rsec_Fort': resultatsFo})
        dff['total'] = int(dff['longueur_Rsec_Faible'] + dff['longueur_Rsec_Fort'])
        dff['Pourcentage_Fa'] = int((dff['longueur_Rsec_Faible'] * 100)/dff['total'])
        dff['Pourcentage_Fo'] = int((dff['longueur_Rsec_Fort'] * 100)/dff['total'])
        dff['Pourcentage_tot'] = int((dff['total'] * 100)/dff['total'])
    
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Linéaire de réseaux secs en classe A, B et C    
def LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U':
        gdf_A =  gdf_glob_EU_U[gdf_glob_EU_U["classeprecision"] == 'A'].copy()
        gdf_B =  gdf_glob_EU_U[gdf_glob_EU_U["classeprecision"] == 'B'].copy()
        gdf_C =  gdf_glob_EU_U[gdf_glob_EU_U["classeprecision"] == 'C'].copy()
        tot = gdf_glob_EU_U[gdf_glob_EU_U["classeprecision"].notna()]['longueur_troncon'].sum()
    else :
        gdf_A =  gdf_glob_EP[gdf_glob_EP["classeprecision"] == 'A'].copy()
        gdf_B =  gdf_glob_EP[gdf_glob_EP["classeprecision"] == 'B'].copy()
        gdf_C =  gdf_glob_EP[gdf_glob_EP["classeprecision"] == 'C'].copy()
        tot = gdf_glob_EP[gdf_glob_EP["classeprecision"].notna()]['longueur_troncon'].sum()

    if group_column:
        
        groupesA = gdf_A.groupby(group_column)
        groupesB = gdf_B.groupby(group_column)
        groupesC = gdf_C.groupby(group_column)
        
        A = (int(groupesA['longueur_troncon'].sum())).reset_index()
        B = (int(groupesB['longueur_troncon'].sum())).reset_index()
        C = (int(groupesC['longueur_troncon'].sum())).reset_index()
        
        A.columns = [group_column, 'longueur_sum_A']
        B.columns = [group_column, 'longueur_sum_B']
        C.columns = [group_column, 'longueur_sum_C']

        # Liste des DataFrames à traiter
        dff = pd.DataFrame(A)
        dataframes = [B, C]
        # Fusion de B et C en utilisant la colonne 'group_column'
        merged = pd.concat(dataframes, ignore_index=True)
        # Fusion avec dff sur la colonne 'group_column'
        dff = dff.merge(merged, on=group_column, how='outer')
        dff = dff.fillna(0)
        dff['tot'] = dff['longueur_sum_A'] + dff['longueur_sum_B'] + dff['longueur_sum_C']
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau]
        else: 
            dff = dff[dff[group_column] == ZG] 
        if dff.empty:
            dff['Pourcentage_A'] = 0
            dff['Pourcentage_B'] = 0
            dff['Pourcentage_C'] = 0
        else:
            dff['Pourcentage_A'] = int((dff['longueur_sum_A'] * 100)/dff['tot'])
            dff['Pourcentage_B'] = int((dff['longueur_sum_B'] * 100)/dff['tot'])
            dff['Pourcentage_C'] = int((dff['longueur_sum_C'] * 100)/dff['tot'])
    else : 
        resultatsA = [int(gdf_A["longueur_troncon"].sum()), int(gdf_A["longueur_troncon"].sum()*100/tot)]
        resultatsB = [int(gdf_B["longueur_troncon"].sum()), int(gdf_B["longueur_troncon"].sum()*100/tot)]
        resultatsC = [int(gdf_C["longueur_troncon"].sum()), int(gdf_C["longueur_troncon"].sum()*100/tot)]

        dff = pd.DataFrame({'longueur_sum_A': [resultatsA[0]], 'longueur_sum_B': [resultatsB[0]], 'longueur_sum_C': [resultatsC[0]],
            'Pourcentage_A': [resultatsA[1]], 'Pourcentage_B': [resultatsB[1]], 'Pourcentage_C': [resultatsC[1]]})
    dff = dff.fillna(0)
    # Mise en forme de l'indicateur
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
    return(dff)

#### Linéaire d'ITV
def LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG, niveau =''):
    df_total = filtered_data[2][filtered_data[2]['identifiantcanap'].isin(valeurs_uniques)].copy()
    df_year = filtered_data[2][filtered_data[2]['identifiantcanap'].isin(valeurs_uniques_annuelles)].copy()

    if group_column:
        groupes = filtered_data[2].groupby(group_column)
        groupes_total = df_total.groupby(group_column)
        groupes_year = df_year.groupby(group_column)
        
        if groupes.ngroups != 0:
            indicateur_sum = (round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))).reset_index()
            indicateur_sum.columns = [group_column, 'Total']
        else : 
            indicateur_sum = pd.DataFrame(columns=[group_column,'Total'])
        if groupes_total.ngroups != 0 :
            resultats_total = (round(groupes_total.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))).reset_index()
            resultats_total.columns = [group_column, 'Total_cumule']
        else : 
            resultats_total = pd.DataFrame(columns=[group_column,'Total_cumule'])
        if groupes_year.ngroups != 0:  # Vérifie si le GroupBy a des groupes (pas vide)
            resultats_year = (round(groupes_year.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))).reset_index()
            resultats_year.columns = [group_column, 'Realise_dans_lannee']
        else: 
            resultats_year = pd.DataFrame(columns=[group_column,'Realise_dans_lannee'])
        
        dff = pd.merge(indicateur_sum, pd.merge( resultats_year, resultats_total, on= group_column), on= group_column)
        
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else: 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty:
            dff['Pourcentage_itv'] = 0
        else : 
            dff['Pourcentage_itv'] = (dff['Total_cumule']*100) /dff['Total']
            dff = dff.fillna(0)
    else : 
        resultat_itv = [
            int(filtered_data[2].drop_duplicates(subset='identifiantcanap')["longueur"].sum()/1000),
            int((df_year.drop_duplicates(subset='identifiantcanap')["longueur"].sum())/1000),
            int((df_total.drop_duplicates(subset='identifiantcanap')["longueur"].sum())/1000),
            int((df_total.drop_duplicates(subset='identifiantcanap')["longueur"].sum()*100)/filtered_data[2].drop_duplicates(subset='identifiantcanap')["longueur"].sum())]
        dff = pd.DataFrame({'Total': [resultat_itv[0]], 'Realise_dans_lannee': [resultat_itv[1]], 'Total_cumule': [resultat_itv[2]], 'Pourcentage_itv': [resultat_itv[3]] })

    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)   

    return(dff)

#### Nombre d'inspection sur les STEU 
def NISTEU(filtered_data, ins_steu, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U':
        
        current_year = datetime.now().year
        date_debut = datetime(current_year-1, 1, 1) 
        date_fin = datetime(current_year-1, 12, 31)
        
        ins_steu_grp = ins_steu.groupby(['N°', 'Création'])
        df_steu = ins_steu_grp.first().reset_index()
        df_steu = df_steu.rename(columns={'N°': 'N°', 'Création': 'Date_ins'})
        df_steu= df_steu[['N°', 'Date_ins']]
        #Extraire les codes de steu dans une nouvelle colonne 'N°'
        filtered_data['N°'] = filtered_data['nomsteu'].apply(lambda x: x.split()[1].split('_')[0] if len(x.split()) > 1 else None) 
        filtered_data['N°'] = filtered_data['N°'].apply(lambda x: x if x.isnumeric() else None)
        #Joindre les deux dataframes
        df_merged = pd.merge( df_steu, filtered_data, on='N°', how='inner')
        df_merged['Date_ins'] = pd.to_datetime(df_merged['Date_ins'], dayfirst= True, errors='coerce')
        
        if group_column:
            
            df_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)].copy()
            
            groupes = filtered_data.groupby(group_column)
            groupes_total = df_merged.groupby(group_column)
            groupes_year = df_year.groupby(group_column)

            total = (groupes['N°'].nunique()).reset_index()
            resultats_total = (groupes_total['N°'].nunique()).reset_index()
            resultats_year = (groupes_year['N°'].nunique()).reset_index()
            
            total.columns = [group_column, 'Total']
            resultats_total.columns = [group_column, 'Total_cumule']
            resultats_year.columns = [group_column, 'Realise_dans_lannee']
            
            dff = pd.merge(total, pd.merge( resultats_year, resultats_total, on= group_column, how='right'), on= group_column)
            dff = dff.fillna(0)
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else: 
                dff = dff[dff[group_column] == ZG].reset_index() 
            if dff.empty:
                dff['Pourcentage_ins'] = 0
            else:
                dff['Pourcentage_ins'] = round((dff['Realise_dans_l''annee']*100) /dff['Total'])
        else : 
            tot_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)]['N°'].nunique()
            tot = df_merged['N°'].nunique()
            total = filtered_data['N°'].nunique()
            resultat_ins = [total, tot_year, tot, int((tot_year*100)/total)]
            dff = pd.DataFrame({'Total': [resultat_ins[0]], 'Realise_dans_lannee': [resultat_ins[1]], 'Total_cumule': [resultat_ins[2]], 'Pourcentage_ins': [resultat_ins[3]] })

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
        return(dff)

#### Nombre d'inspection sur les STR
def NISTR(filtered_data, ins_str, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U':
        
        current_year = datetime.now().year
        date_debut = datetime(current_year-1, 1, 1) 
        date_fin = datetime(current_year-1, 12, 31)
        
        ins_str_grp = ins_str.groupby(['N°', 'Création'])
        df_str = ins_str_grp.first().reset_index()
        df_str = df_str.rename(columns={'N°': 'N°', 'Création': 'Date_ins'})
        df_str= df_str[['N°', 'Date_ins']]
        #Extraire les codes de steu dans une nouvelle colonne 'N°'
        filtered_data = filtered_data[filtered_data['numero'].notna() & (filtered_data['numero'] != 0)]
        filtered_data['N°'] = filtered_data['numero'].astype(str).apply(lambda x: x if x.isnumeric() else None)
        #Joindre les deux dataframes
        df_merged = pd.merge( df_str, filtered_data, on='N°', how='inner')
        df_merged['Date_ins'] = pd.to_datetime(df_merged['Date_ins'], dayfirst= True, errors='coerce')
        
        if group_column:
            
            df_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)].copy()
            
            groupes = filtered_data.groupby(group_column)
            groupes_total = df_merged.groupby(group_column)
            groupes_year = df_year.groupby(group_column)

            total = (groupes['N°'].nunique()).reset_index()
            resultats_total = (groupes_total['N°'].nunique()).reset_index()
            resultats_year = (groupes_year['N°'].nunique()).reset_index()
            
            total.columns = [group_column, 'Total']
            resultats_total.columns = [group_column, 'Total_cumule']
            resultats_year.columns = [group_column, 'Realise_dans_lannee']
            
            dff = pd.merge(total, pd.merge(resultats_year, resultats_total, on= group_column, how='right'), on= group_column)
            dff = dff.fillna(0)
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else: 
                dff = dff[dff[group_column] == ZG].reset_index() 
            if dff.empty:
                dff['Pourcentage_ins'] = 0
            else:
                dff['Pourcentage_ins'] = int((dff['Realise_dans_l''annee']*100) /dff['Total'])
        else: 
            tot_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)]['N°'].nunique()
            tot = df_merged['N°'].nunique()
            total = filtered_data['N°'].nunique()
            resultat_ins = [total, tot_year, tot, int((tot_year*100)/total)]
            dff = pd.DataFrame({'Total': [resultat_ins[0]], 'Realise_dans_lannee': [resultat_ins[1]], 'Total_cumule': [resultat_ins[2]], 'Pourcentage_ins': [resultat_ins[3]] })
        
        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
        return(dff)

#### Nombre d'inspection sur les trémies
def NITREMIES(filtered_data, ins_tremies, group_column, typereseau, ZG, date_debut, date_fin, niveau =''):
    if typereseau == 'EP':
        
        ins_tremies_grp = ins_tremies.groupby(['N°', 'Création'])
        df_tremies = ins_tremies_grp.first().reset_index()
        df_tremies = df_tremies.rename(columns={'N°': 'numero', 'Création': 'Date_ins'})
        df_tremies= df_tremies[['numero', 'Date_ins']]
        df_tremies['numero'] = df_tremies['numero'].astype(int)
        #Joindre les deux dataframes
        df_merged = pd.merge( df_tremies, filtered_data, on='numero', how='inner')
        df_merged['Date_ins'] = pd.to_datetime(df_merged['Date_ins'], dayfirst= True, errors='coerce')
        
        if group_column:
            
            df_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)].copy()
            groupes = filtered_data.groupby(group_column)
            groupes_total = df_merged.groupby(group_column)
            groupes_year = df_year.groupby(group_column)

            total = (groupes['numero'].nunique()).reset_index()
            resultats_total = (groupes_total['numero'].nunique()).reset_index()
            resultats_year = (groupes_year['numero'].nunique()).reset_index()
            
            total.columns = [group_column, 'Total']
            resultats_total.columns = [group_column, 'Total_cumule']
            resultats_year.columns = [group_column, 'Realise_dans_lannee']
            
            dff = pd.merge(total, pd.merge(resultats_year, resultats_total, on= group_column, how='right'),  on= group_column)
            dff = dff.fillna(0)
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
            if dff.empty:
                dff['Pourcentage_ins'] = 0
            else:
                dff['Pourcentage_ins'] = int((dff['Realise_dans_l''annee']*100) /dff['Total'])
        else : 
            tot_year = df_merged[(df_merged['Date_ins'] >= date_debut) & (df_merged['Date_ins'] <= date_fin)]['numero'].nunique()
            tot = df_merged['numero'].nunique()
            Total = filtered_data['numero'].nunique()
            resultat_ins = [Total, tot_year, tot, int((tot_year*100)/Total)]
            dff = pd.DataFrame({'Total': [resultat_ins[0]], 'Realise_dans_lannee': [resultat_ins[1]],'Total_cumule': [resultat_ins[2]], 'Pourcentage_ins': [resultat_ins[3]] })

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
        return(dff)

##### Linéaire de réseaux sur parcelles ou voie privée
def LRSPVP(filtered_data, canexp_cadastre_merged, canexp_voie_merged, group_column, ZG, niveau= ''):
    if group_column:
        
        canexp_cadastre_merged['lin_f'] = canexp_cadastre_merged['longueur'] * canexp_cadastre_merged['T_P_hors_Metropole']/100000
        canexp_voie_merged['lin_f'] = canexp_voie_merged['longueur'] * canexp_voie_merged['T_P_hors_Metropole']/100000
        
        df_cad_grouped = canexp_cadastre_merged.groupby(group_column)
        df_voie_grouped = canexp_voie_merged.groupby(group_column)
        df_niv_grouped = filtered_data[1].groupby(group_column)

        resultat_cad = round(df_cad_grouped.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['lin_f'].sum()),3).reset_index()
        resultat_voie = round(df_voie_grouped.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['lin_f'].sum()),3).reset_index()
        resultat_niv = round(df_niv_grouped.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['longueur'].sum()/1000),3).reset_index()
        
        resultat_cad.columns = [group_column, 'lin_cad']
        resultat_voie.columns = [group_column, 'lin_voie']
        resultat_niv.columns = [group_column, 'lin_niv']
        dff =  pd.merge(resultat_cad, resultat_voie, on = group_column, how = 'outer').reset_index()
        dff['lineaire'] = dff['lin_cad'] + dff['lin_voie']
        dff = dff.fillna(0)

        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty:
            dff['pourcentage'] = 0
        else : 
            if ZG == 'Tous':
                dff['pourcentage'] = round((dff['lineaire']*100) /resultat_niv[resultat_niv[group_column] == niveau]['lin_niv'], 2)
            else : 
                dff['pourcentage'] = round((dff['lineaire']*100) /resultat_niv[resultat_niv[group_column] == ZG]['lin_niv'], 2)
        dff = dff.fillna(0)    
    else : 
        canexp_cadastre_merged = canexp_cadastre_merged.drop_duplicates(subset='idcanaexp')
        canexp_voie_merged = canexp_voie_merged.drop_duplicates(subset='idcanaexp')
        canexp_cadastre_merged['lin_f'] = canexp_cadastre_merged['longueur'] * canexp_cadastre_merged['T_P_hors_Metropole']/100000
        canexp_voie_merged['lin_f'] = canexp_voie_merged['longueur'] * canexp_voie_merged['T_P_hors_Metropole']/100000
        resultat = round(canexp_cadastre_merged['lin_f'].sum() + canexp_voie_merged['lin_f'].sum(), 3)
        tot = filtered_data[1].drop_duplicates(subset='idcanaexp')['longueur'].sum()/1000
        dff = pd.DataFrame({'lineaire': [resultat], 'pourcentage': [round(resultat*100/tot, 2)]})

    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)   
    return(dff)

#### Nombre de parcelles privées traversées par les réseaux
def NPPTR(filtered_data, gdf_cad, group_column, ZG, niveau =''):
    filtered_data[13]['geom'] = filtered_data[13]['geom'].apply(lambda x: wkb_loads(bytes.fromhex(x)) if isinstance(x, str) else x)
    filtered_data[13] = gpd.GeoDataFrame(filtered_data[13], geometry='geom', crs='EPSG:3946')

    intersection = gpd.overlay(filtered_data[13], gdf_cad[gdf_cad['T_P_hors_Metropole'] >= 50], how='intersection')
    liste_colonnes=['idparcelle', 'codenatureproprietaire', 'natureproprietaire', 'identifiantbvgrand', 'identifiantsteu', 'nomsteu', 'id_com', 'texte', 'identifiantcoursdeau', 'identifiantbassinversantruisseau', 'identifiantcaptage', 'nomcaptage', 'Bassin']
    intersection = intersection[liste_colonnes]

    if group_column:
        df_grouped = intersection.groupby(group_column)
        df_niv_grouped = filtered_data[13].groupby(group_column)

        resultat = df_grouped['idparcelle'].nunique().reset_index()
        resultat_niv = df_niv_grouped['idparcelle'].nunique().reset_index()

        resultat.columns = [group_column, 'nbre_parcelle']
        resultat_niv.columns = [group_column, 'nbre_parcelle']
        dff = pd.DataFrame(resultat)

        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index() 
        if dff.empty :
           dff['pourcentage'] = 0
        else: 
            dff['pourcentage'] = round((dff['nbre_parcelle']*100) /resultat_niv[resultat_niv[group_column] == ZG]['nbre_parcelle'], 0)
    else : 
        resultat = intersection['idparcelle'].nunique()
        tot = filtered_data[13]['idparcelle'].nunique()
        dff = pd.DataFrame({'nbre_parcelle': [resultat], 'pourcentage': [resultat*100/tot]})

    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)   
    return(dff)

#### Valeur patrimoniale à neuf OGEP (Millions €)
def VPOGEP(filtered_data, VP_OGEP, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' : 
        df_filtererd = filtered_data[5][filtered_data[5]['natureogep'].isin([1, 2, 3, 5])] #{1: Tranchée, 2: Noue, 3: Noue composée, 5: Tranchée Stockholm}
        df_filtererd['valeur'] = df_filtererd['longueur'] * VP_OGEP / 1000
        if group_column:
            df_grouped = df_filtererd.groupby(group_column)
            resultat = round(df_grouped.apply(lambda x: x.drop_duplicates(subset='identifiantogep')['valeur'].sum()),1).reset_index()
            resultat.columns = [group_column, 'VP_OGEP']
            dff = pd.DataFrame(resultat) 
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultat = round(df_filtererd.drop_duplicates(subset=['identifiantogep'])['valeur'].sum(), 1)
            dff = pd.DataFrame({'VP_OGEP': resultat}, index=[0])
        
        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf Bassins (ENT ET OUV) (Millions €)
def VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' : 
        df_OUV = filtered_data[0][filtered_data[0]['nature'] == 'OUV'] 
        df_ENT = filtered_data[0][filtered_data[0]['nature'] == 'ENT']

        df_OUV['valeur'] = df_OUV['volumeexploitation'] * VP_OUV / 1000000 #(Millions €)
        df_ENT['valeur'] = df_ENT['volumeexploitation'] * VP_ENT / 1000000 #(Millions €)
        if group_column:
            if not(df_OUV[df_OUV[group_column].notna()].empty):
                df_OUV_grouped = df_OUV.groupby(group_column)
                resultat_OUV = (round(df_OUV_grouped.apply(lambda x: x.drop_duplicates(subset='identifiantbassin')['valeur'].sum()),1)).reset_index()
                resultat_OUV.columns = [group_column, 'VP_OUV']
            else :
                resultat_OUV = pd.DataFrame(columns=[group_column, 'VP_OUV'])
            if not(df_ENT[df_ENT[group_column].notna()].empty):
                df_ENT_grouped = df_ENT.groupby(group_column)
                resultat_ENT = (round(df_ENT_grouped.apply(lambda x: x.drop_duplicates(subset='identifiantbassin')['valeur'].sum()),1)).reset_index()
                resultat_ENT.columns = [group_column, 'VP_ENT']
            else :
                resultat_ENT = pd.DataFrame(columns=[group_column, 'VP_ENT'])
            
            if (not resultat_OUV.empty) and (not resultat_ENT.empty):
                dff = pd.merge(resultat_OUV, resultat_ENT, on=group_column)
                dff['VP_tot'] = dff['VP_OUV'] + dff['VP_ENT']
            elif not resultat_OUV.empty:
                dff = resultat_OUV
                dff['VP_ENT'] = 0
                dff['VP_tot'] = dff['VP_OUV']
            elif not resultat_ENT.empty:
                dff = resultat_ENT
                dff['VP_OUV'] = 0
                dff['VP_tot'] = dff['VP_ENT']
            else:
                dff = pd.DataFrame(columns=[group_column, 'VP_OUV', 'VP_ENT', 'VP_tot'])
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultat = [
                round(df_OUV.drop_duplicates(subset=['identifiantbassin'])['valeur'].sum(), 1),
                round(df_ENT.drop_duplicates(subset=['identifiantbassin'])['valeur'].sum(), 1),
                round(((df_OUV.drop_duplicates(subset=['identifiantbassin'])['valeur'].sum()) + (df_ENT.drop_duplicates(subset=['identifiantbassin'])['valeur'].sum())), 1)
            ]
            dff = pd.DataFrame({'VP_OUV': resultat[0], 'VP_ENT': resultat[1], 'VP_tot': resultat[2]}, index=[0])
        
        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf STR (Millions €)
def VPSTR(filtered_data, VP_STR, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' : 
        if group_column:
            df_grouped = filtered_data[8].groupby(group_column)
            resultat = (round(df_grouped['identifiantstare'].nunique() * VP_STR, 1)).reset_index()
            resultat.columns = [group_column, 'VP_STR']
            dff = pd.DataFrame(resultat) 
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultat = round(filtered_data[8]['identifiantstare'].nunique() * VP_STR, 1)
            dff = pd.DataFrame({'VP_STR': resultat}, index=[0])

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf STEU (MD€)
def VPSTEU(filtered_data, VP_STEU, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' : 
        filtered_data[9]['valeur'] = filtered_data[9]['capacite'] * VP_STEU / 1000000
        if group_column:
            df_grouped = filtered_data[9].groupby(group_column)
            resultat = round(df_grouped.apply(lambda x: x.drop_duplicates(subset='identifiantsteu')['valeur'].sum()),1).reset_index()
            resultat.columns = [group_column, 'VP_STEU']
            dff = pd.DataFrame(resultat) 
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            df_met = filtered_data[9]
            resultat = round(df_met.drop_duplicates(subset=['identifiantsteu'])['valeur'].sum(), 1)
            dff = pd.DataFrame({'VP_STEU': resultat}, index=[0])

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf FPR (K€)
def VPFPR(filtered_data, VP_FPR, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U' : 
        df_filtered = filtered_data[9][filtered_data[9]['nomsteu'].str.contains("Filtre planté de roseaux", na=False)]

        if group_column:
            df_grouped = df_filtered.groupby(group_column)
            resultat = (df_grouped['identifiantsteu'].nunique() * VP_FPR).reset_index()
            resultat.columns = [group_column, 'VP_FPR']
            dff = pd.DataFrame(resultat) 
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultat = int(df_filtered['identifiantsteu'].nunique() * VP_FPR)
            dff = pd.DataFrame({'VP_FPR': resultat}, index=[0])

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Valeur patrimoniale à neuf Trémies (Millions €)
def VPTREM(filtered_data, VP_TREM, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' : 
        
        if group_column:
            df_grouped = filtered_data[11].groupby(group_column)
            resultat = (round(df_grouped['identifianttremie'].nunique() * VP_TREM, 1)).reset_index()
            resultat.columns = [group_column, 'VP_TREM']
            dff = pd.DataFrame(resultat)
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultat = round(filtered_data[11]['identifianttremie'].nunique() * VP_TREM, 1)
            dff = pd.DataFrame({'VP_TREM': resultat}, index=[0])

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de bassins (ENT et OUV) récolés
def NBR(dfRecBas, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP':   

        dfRecBas_bis =  dfRecBas[dfRecBas['recolement'] == 'Oui']

        if group_column:
            
            df_rec_grouped = dfRecBas_bis.groupby(group_column)
            df_tot_grouped = dfRecBas.groupby(group_column)

            resultat_rec = df_rec_grouped['identifiantbassin'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifiantbassin'].nunique().reset_index()
            
            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
            if not(dff.empty):
                dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'])
            else:
                dff['pourcentage'] = 0
            dff = dff.fillna(0)
        else : 
            resultat = dfRecBas_bis['identifiantbassin'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [dfRecBas['identifiantbassin'].nunique()], 'pourcentage': [int(round(resultat*100/dfRecBas['identifiantbassin'].nunique()))]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
                return(dff)
        else: 
            return(dff)

#### Linéaire de linéaire d'infiltration 
def LLI(gdf, current_year, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP':   
            if group_column:
                df_LLI = gdf[gdf['annee'] == current_year][group_column].dropna()
                if df_LLI.empty:
                    dff = pd.DataFrame(columns=['longueur', 'longueur_cum', 'Pourcentage'])
                    dff.loc[0] = [0] * len(dff.columns)
                    return(dff)
                else:
                    gdf_grouped = gdf[gdf['annee'] == current_year].groupby(group_column)
                    gdf_tot_grouped = gdf.groupby(group_column)
                    resultat_grp = round(gdf_grouped.apply(lambda x: x.drop_duplicates(subset='iddrain')['longueur'].sum()/1000), 3).reset_index()
                    resultat_tot = round(gdf_tot_grouped.apply(lambda x: x.drop_duplicates(subset='iddrain')['longueur'].sum()/1000), 3).reset_index()
                    resultat_grp.columns = [group_column, 'longueur']
                    resultat_tot.columns = [group_column, 'longueur_cum']
                    # Fusionner les résultats
                    dff = pd.merge(resultat_grp, resultat_tot, on = group_column)
                    dff = dff.fillna(0)   
                    
                    if ZG == 'Tous':
                        dff = dff[dff[group_column] == niveau].reset_index()
                    else : 
                        dff = dff[dff[group_column] == ZG].reset_index()
                    if not(dff.empty):
                        dff['Pourcentage'] = round((dff['longueur']*100) /dff['longueur_cum'])
                    else:
                        dff['Pourcentage'] = 0
            else : 
                gdf_sans_doublons = gdf.drop_duplicates(subset=['iddrain'])
                current_data = gdf_sans_doublons[gdf_sans_doublons['annee'] == current_year].copy()
                resultat = current_data['longueur'].sum() / 1000
                dff = pd.DataFrame({'longueur': [round(resultat,1)], 'longueur_cum': [int(gdf_sans_doublons['longueur'].sum() / 1000)], 'Pourcentage': [int((resultat * 100000) / gdf_sans_doublons['longueur'].sum())]})
            if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)
            return(dff)    
####  Nombre de STEU récolés
def NSteuR(filtered_data, dossier_FPR, group_column, typereseau, ZG, niveau =''):   
    return(None)

#### Nombre de STR récolés
def NSR(dfRecSTR, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U':   

        dfRecSTR_bis =  dfRecSTR[dfRecSTR['recolement'] == 'O']

        if group_column:
            
            df_rec_grouped = dfRecSTR_bis.groupby(group_column)
            df_tot_grouped = dfRecSTR.groupby(group_column)

            resultat_rec = df_rec_grouped['identifiantstare'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifiantstare'].nunique().reset_index()
            
            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            dff = dff.fillna(0)
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
            if not(dff.empty):
                dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'], 2)
            else:
                dff['pourcentage'] = 0
        else : 
            resultat = dfRecSTR_bis['identifiantstare'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [dfRecSTR['identifiantstare'].nunique()], 'pourcentage': [round(resultat*100/dfRecSTR['identifiantstare'].nunique(), 2)]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
                return(dff)
        else: 
            return(dff)

def NDOR(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U':   
        filtered_data[4] = filtered_data[4].fillna(False)
        filtered_data_bis =  filtered_data[4][filtered_data[4]['recolement']]

        if group_column:
            
            df_rec_grouped = filtered_data_bis.groupby(group_column)
            df_tot_grouped = filtered_data[4].groupby(group_column)

            resultat_rec = df_rec_grouped['identifiantdevorage'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifiantdevorage'].nunique().reset_index()
            
            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            dff = dff.fillna(0)
            
            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
            if not(dff.empty):
                dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'], 2)
            else:
                dff['pourcentage'] = 0
        else : 
            resultat = filtered_data_bis['identifiantdevorage'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [filtered_data[4]['identifiantdevorage'].nunique()], 'pourcentage': [round(resultat*100/filtered_data[4]['identifiantdevorage'].nunique(), 2)]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
                return(dff)
        else: 
            return(dff)

def NDesR(filtered_data, group_column, typereseau, ZG, niveau =''):   
    filtered_data[12] = filtered_data[12].fillna(False)
    filtered_data_bis =  filtered_data[12][filtered_data[12]['recolement']]

    if group_column:
        
        df_rec_grouped = filtered_data_bis.groupby(group_column)
        df_tot_grouped = filtered_data[12].groupby(group_column)

        resultat_rec = df_rec_grouped['identifianttraitement'].nunique().reset_index()
        resultat_tot = df_tot_grouped['identifianttraitement'].nunique().reset_index()
        
        resultat_rec.columns = [group_column, 'nbre_rec']
        resultat_tot.columns = [group_column, 'nbre_tot']
        dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
        dff = dff.fillna(0)
        
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            dff = dff[dff[group_column] == ZG].reset_index()
        if not(dff.empty):
            dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'], 2)
        else:
            dff['pourcentage'] = 0
    else : 
        resultat = filtered_data_bis['identifianttraitement'].nunique()
        dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [filtered_data[12]['identifianttraitement'].nunique()], 'pourcentage': [round(resultat*100/filtered_data[12]['identifianttraitement'].nunique(), 2)]})

    if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)   
            return(dff)
    else: 
        return(dff)

#### Nombre de Tremies récolés
def NTR(dfRecTRM, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP':   

        dfRecTRM_bis =  dfRecTRM[dfRecTRM['recolement'] == 'O']

        if group_column:
            
            df_rec_grouped = dfRecTRM_bis.groupby(group_column)
            df_tot_grouped = dfRecTRM.groupby(group_column)

            resultat_rec = df_rec_grouped['identifianttremie'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifianttremie'].nunique().reset_index()

            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            dff = dff.fillna(0)

            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
            if not(dff.empty):
                dff['pourcentage'] = int(round((dff['nbre_rec']*100) /dff['nbre_tot']))
            else:
                dff['pourcentage'] = 0
        else : 
            resultat = dfRecTRM_bis['identifianttremie'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [dfRecTRM['identifianttremie'].nunique()], 'pourcentage': [int(round(resultat*100/dfRecTRM['identifianttremie'].nunique()))]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
                return(dff)
        else: 
            return(dff)

#### Nombre de FPR récolés
def NFR(filtered_data, dossier_FPR, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EU_U': 
        
        dossier_trouve=[]
        for dossier_parent, sous_dossiers, fichiers in os.walk(dossier_FPR):
            for sous_dossier in sous_dossiers:
                if sous_dossier == "plan":
                    chemin_complet = os.path.join(dossier_parent, sous_dossier)
                    if os.path.isdir(chemin_complet) and len(os.listdir(chemin_complet)) > 0:
                        dossier_trouve.append(dossier_parent.split('\\')[-1])
        
        dfRec_FPR = filtered_data[9][filtered_data[9]['nomsteu'].str.contains("Filtre planté de roseaux", na=False)]
        dfRec_FPR['recolement'] = dfRec_FPR['nomsteu'].apply(lambda x: 'O' if any(x in dossier for dossier in dossier_trouve)else 'N')


        dfRec_FPR_bis =  dfRec_FPR[dfRec_FPR['recolement'] == 'O']

        if group_column:
            
            df_rec_grouped = dfRec_FPR_bis.groupby(group_column)
            df_tot_grouped = dfRec_FPR.groupby(group_column)

            resultat_rec = df_rec_grouped['identifiantsteu'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifiantsteu'].nunique().reset_index()

            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            dff = dff.fillna(0)

            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
                dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'], 2)
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
                dff['pourcentage'] = round((dff['nbre_rec']*100) /dff['nbre_tot'], 2)
            dff = dff.fillna(0)
        else : 
            resultat = dfRec_FPR_bis['identifiantsteu'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [dfRec_FPR['identifiantsteu'].nunique()], 'pourcentage': [round(resultat*100/dfRec_FPR['identifiantsteu'].nunique(), 2)]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
                return(dff)
        else: 
            return(dff)

#### Nombre de  Arbres de pluie récolés
def NAPR(Rec_AP, group_column, typereseau, ZG, niveau = ''):
    if typereseau == 'EP':   

        Rec_AP_bis =  Rec_AP[Rec_AP['recolement'] == 'oui']

        if group_column:
            
            df_rec_grouped = Rec_AP_bis.groupby(group_column)
            df_tot_grouped = Rec_AP.groupby(group_column)

            resultat_rec = df_rec_grouped['identifiantogep'].nunique().reset_index()
            resultat_tot = df_tot_grouped['identifiantogep'].nunique().reset_index()

            resultat_rec.columns = [group_column, 'nbre_rec']
            resultat_tot.columns = [group_column, 'nbre_tot']
            dff =  pd.merge(resultat_rec, resultat_tot, on = group_column, how = 'outer').reset_index()
            dff = dff.fillna(0)

            if ZG == 'Tous':
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index()
            
            if dff.empty:
                dff['pourcentage'] = 0
            else :
                dff['pourcentage'] = int(round((dff['nbre_rec']*100) /dff['nbre_tot']))
        else : 
            resultat = Rec_AP_bis['identifiantogep'].nunique()
            dff = pd.DataFrame({'nbre_rec': [resultat], 'nbre_tot': [Rec_AP['identifiantogep'].nunique()], 'pourcentage': [int(round(resultat*100/Rec_AP['identifiantogep'].nunique()))]})

        if dff.empty:
                dff.loc[0] = [0] * len(dff.columns)   
        return(dff)

#### Linéaire total d'idées de faire travaux Réseaux 
def LRIT(df_idees_res, group_column, ZG, niveau = ''):
    if group_column:
        df_grouped = df_idees_res.groupby(group_column)
        resultat = (round(df_grouped.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['longueur'].sum() / 1000))).reset_index(name= 'long_idee')
        dff = pd.DataFrame(resultat)
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else: 
            dff = dff[dff[group_column] == ZG].reset_index()   
    else:
        resultat = [int(round(df_idees_res.drop_duplicates(subset=['idcanaexp'])['longueur'].sum() / 1000))]
        dff = pd.DataFrame({'long_idee': resultat})
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)   
    return(dff)

#### Age de réseaux 
def AR(filtered_data, palette_age_Res_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    year = datetime.now().year
    df_ages = filtered_data[2].copy()
    
    condition1 = df_ages['anneepose'].isnull() & df_ages['anneeposesuperieur'].notna() & df_ages['anneeposeinferieur'].notna()
    condition2 = df_ages['anneepose'].isnull() & df_ages['anneeposesuperieur'].isnull() & df_ages['anneeposeinferieur'].notna()
    condition3 = df_ages['anneepose'].isnull() & df_ages['anneeposesuperieur'].notna() & df_ages['anneeposeinferieur'].isnull()

    df_ages.loc[condition1, 'anneepose'] = df_ages.loc[condition1].apply(lambda row: np.mean([row['anneeposesuperieur'], row['anneeposeinferieur']]), axis=1)
    df_ages.loc[condition2, 'anneepose'] = df_ages.loc[condition2, 'anneeposeinferieur']
    df_ages.loc[condition3, 'anneepose'] = df_ages.loc[condition3, 'anneeposesuperieur']
    
    df_ages = df_ages.dropna(subset=['anneepose'])
    df_ages = df_ages[df_ages['anneepose'].astype(int) != 0]
    df_ages['age'] = year - df_ages["anneepose"].astype(int)

    bornes = [0, 25, 50, 75, float('inf')]
    labels = ['0-25 ans', '26-50 ans', '51-75 ans', '> 75 ans']
    df_ages['tranche_age'] = pd.cut(df_ages['age'], bins=bornes, labels=labels)
    
    if group_column :
        groupes = df_ages.groupby([group_column, 'tranche_age'])
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'tranche_age', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
    else : 
        groupes = df_ages.groupby('tranche_age')
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)
        resultats = indicateur_sum.reset_index()
        resultats.columns = ['tranche_age', 'longueur_sum']
        dff = pd.DataFrame(resultats)

    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        df_sorted_desc = dff.sort_values(by='tranche_age')
            
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_age_Res_RGBA.items()}
        # Extraire les couleurs dans l'ordre des catégories triées
        piecolorway = [category_colors[age] for age in df_sorted_desc['tranche_age']]
        # Créer des handles pour la légende
        legend_handles = [mpatches.Patch(color=category_colors[age]) for age in df_sorted_desc['tranche_age']]

        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')

        plt.pie(
            df_sorted_desc['longueur_sum'], 
            labels = df_sorted_desc['longueur_sum'].astype(int).astype(str) + ' km',
            labeldistance = 1.05,
            colors= piecolorway,
            textprops={'fontsize': 12},
            wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
            counterclock=False,
            startangle=90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            df_sorted_desc['tranche_age'],
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 12},
            title='Tranches d\'âge')
        
        out_figure = os.path.join(out_folder, f'Linéaire_par_age_{typereseau}_{NivExt}_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#### Connaissance Année de pose
def ConAge(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    df_ConAnnee = filtered_data[2]

    # Remplir la colonne 'Connaissance' avec 'Connue' par défaut
    df_ConAnnee['Connaissance'] = 'Pas de données'

    # Appliquer les conditions pour changer à 'Estimée'
    condition1 = (df_ConAnnee['anneepose'].isnull()) & ((df_ConAnnee['anneeposesuperieur'].notna()) | (df_ConAnnee['anneeposeinferieur'].notna()))
    condition2 = df_ConAnnee['anneepose'].notna()
    df_ConAnnee.loc[condition1, 'Connaissance'] = 'Estimée'
    df_ConAnnee.loc[condition2, 'Connaissance'] = 'Connue'

    if group_column :
        groupes = df_ConAnnee.groupby([group_column, 'Connaissance'])
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'Connaissance', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
        if dff.empty:
            dff['Pourcentage'] = 0
        else :
            dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
    else : 
        groupes = df_ConAnnee.groupby('Connaissance')
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))
        resultats = indicateur_sum.reset_index()
        resultats.columns = ['Connaissance', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
    dff = dff.fillna(0)   

    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        piecolorway = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_basique_RGBA.values()]
        legend_handles = [mpatches.Patch(color=color) for color in piecolorway]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff.sort_values(by='Connaissance')
        if (df_sorted_desc['longueur_sum'] == 0).all() and (df_sorted_desc['Pourcentage'] == 0).all() :
            return(dff, NODATA_PATH)
        else:
            plt.pie(
                df_sorted_desc['longueur_sum'], 
                labels = df_sorted_desc['longueur_sum'].astype(int).astype(str) + ' km\n' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %',
                labeldistance = 1.07,
                colors= piecolorway,
                textprops={'fontsize': 12},
                wedgeprops = { 'linewidth' : 6, 'edgecolor' : 'white' },
                counterclock=False,
                startangle=90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['Connaissance'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 12},
                title='Année de pose')
            
            out_figure = os.path.join(out_folder, f'Linéaire_connaissance_age_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#### Connaissance Matériaux
def ConMat(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    df_ConMat = filtered_data[2]

    # Remplir la colonne 'Connaissance' avec 'Connue' par défaut
    df_ConMat['Connaissance'] = 'Pas de données'
    # Appliquer les conditions pour changer à 'Estimée'
    condition1 = df_ConMat['materiau'].isnull() & df_ConMat['materiaupresume'].notna()
    condition2 = df_ConMat['materiau'].notna()
    df_ConMat.loc[condition1, 'Connaissance'] = 'Estimés'
    df_ConMat.loc[condition2, 'Connaissance'] = 'Connus'

    if group_column :
        groupes = df_ConMat.groupby([group_column, 'Connaissance'])
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))
        resultats = indicateur_sum.reset_index()
        resultats.columns = [group_column, 'Connaissance', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        dff['longueur_sum'] = dff['longueur_sum'].fillna(0).astype(int)

        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
        if dff.empty:
            dff['Pourcentage'] = 0
        else :
            dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
        dff = dff.fillna(0)   
        
    else : 
        groupes = df_ConMat.groupby('Connaissance')
        indicateur_sum = round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))
        resultats = indicateur_sum.reset_index()
        resultats.columns = ['Connaissance', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
        dff = dff.fillna(0)   

    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        piecolorway = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_basique_RGBA.values()]
        legend_handles = [mpatches.Patch(color=color) for color in piecolorway]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff.sort_values(by='Connaissance')
        df_sorted_desc['longueur_sum'] = df_sorted_desc['longueur_sum'].fillna(0)
        if (df_sorted_desc['longueur_sum'] == 0).all() and (df_sorted_desc['Pourcentage'] == 0).all() :
            return(dff, NODATA_PATH)
        else:
            plt.pie(
                df_sorted_desc['longueur_sum'], 
                labels = df_sorted_desc['longueur_sum'].astype(int).astype(str) + ' km\n' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %',
                labeldistance = 1.09,
                colors= piecolorway,
                textprops={'fontsize': 12},
                wedgeprops = { 'linewidth' : 6, 'edgecolor' : 'white' },
                counterclock=False,
                startangle=90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['Connaissance'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 12},
                title='Matériaux :')
            
            out_figure = os.path.join(out_folder, f'Linéaire_connaissance_Materiaux_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#### Age STEU
def AgeSTEU(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U' :
        year = datetime.now().year
        
        df_ages = filtered_data[9].dropna(subset=['anneeconstruction'])
        df_ages = df_ages[df_ages['anneeconstruction'].astype(int) != 0]
        df_ages['age'] = year - df_ages["anneeconstruction"].astype(int)

        bornes = [0, 10, 20, 30, float('inf')]
        labels = ['0-10 ans', '11-20 ans', '21-30 ans', '> 30 ans']
        df_ages['tranche_age'] = pd.cut(df_ages['age'], bins=bornes, labels=labels)
        df_ages['tranche_age'] = df_ages['tranche_age'].cat.add_categories(['Pas de données'])
        df_ages['tranche_age'].fillna('Pas de données', inplace=True)
        
        if group_column :
            groupes = df_ages.groupby([group_column, 'tranche_age'])
            indicateur_sum = groupes['identifiantsteu'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'tranche_age', 'nb_steu']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            groupes = df_ages.groupby('tranche_age')
            indicateur_sum = groupes['identifiantsteu'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['tranche_age', 'nb_steu']
            dff = pd.DataFrame(resultats)

        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            df_sorted_desc = dff.sort_values(by='tranche_age')
            
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_age_ouvrage_RGBA.items()}
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[age] for age in df_sorted_desc['tranche_age']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[age]) for age in df_sorted_desc['tranche_age']]
            
            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')
            
            plt.pie(
                df_sorted_desc['nb_steu'], 
                labels = df_sorted_desc['nb_steu'].astype(int).astype(str),
                labeldistance = 1.05,
                colors= piecolorway,
                textprops={'fontsize': 12},
                wedgeprops = { 'linewidth' : 2.5, 'edgecolor' : 'white' }, 
                counterclock=False,
                startangle=90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['tranche_age'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 12},
                title='Tranches d\'âge')
            
            out_figure = os.path.join(out_folder, f'STEU_par_age_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#### Age STR
def AgeSTR(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U' :
        year = datetime.now().year
        df_NSTR = filtered_data[8][filtered_data[8]['numero'].notna() & (filtered_data[8]['numero'] != 0)]
        df_ages = df_NSTR.dropna(subset=['anneeconstruction'])
        df_ages = df_ages[df_ages['anneeconstruction'].astype(int) != 0]
        df_ages['age'] = year - df_ages["anneeconstruction"].astype(int)

        bornes = [0, 10, 20, 30, float('inf')]
        labels = ['0-10 ans', '11-20 ans', '21-30 ans', '> 30 ans']
        df_ages['tranche_age'] = pd.cut(df_ages['age'], bins=bornes, labels=labels)
        df_ages['tranche_age'] = df_ages['tranche_age'].cat.add_categories(['Pas de données'])
        df_ages['tranche_age'].fillna('Pas de données', inplace=True)
        
        if group_column :
            groupes = df_ages.groupby([group_column, 'tranche_age'])
            indicateur_sum = groupes['numero'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            groupes = df_ages.groupby('tranche_age')
            indicateur_sum = groupes['numero'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)

        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            df_sorted_desc = dff.sort_values(by='tranche_age')
            
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_age_ouvrage_RGBA.items()}
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[age] for age in df_sorted_desc['tranche_age']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[age]) for age in df_sorted_desc['tranche_age']]

            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')
            plt.pie(
                df_sorted_desc['nb_str'], 
                labels = df_sorted_desc['nb_str'].astype(int).astype(str),
                labeldistance = 1.05,
                colors= piecolorway,
                textprops={'fontsize': 12},
                wedgeprops = { 'linewidth' : 2.5, 'edgecolor' : 'white' },
                counterclock=False,
                startangle=90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['tranche_age'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 12},
                title='Tranches d\'âge')
            
            out_figure = os.path.join(out_folder, f'STR_par_age_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#### Nombre de branchements connu et estimé
def NBCE(filtered_data, group_column, Ratio, palette_basique_RGBA, out_folder, NivExt, typereseau, ZG, NODATA_PATH, niveau=''):
    if group_column:
        groupes = filtered_data[3].groupby(group_column)
        grp_canap = filtered_data[2].groupby(group_column)
        
        resultats = groupes['identifiantcanas'].nunique().reset_index()
        resultat_tot = grp_canap.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()).reset_index(name='somme_longueur')

        resultat_tot.columns = [group_column, 'longueur_res']
        resultats.columns = [group_column, 'branch_connus']
        # Fusionner les résultats
        dff = pd.merge(resultat_tot, resultats, on = group_column, how= 'outer')
        dff['branch_estimes'] = round(dff['longueur_res'] * Ratio,-3)
        dff['branch_tot'] = dff['branch_estimes'] + dff['branch_connus']
        if ZG == 'Tous':
            dff = dff[dff[group_column] == niveau].reset_index()
        else:
            dff = dff[dff[group_column] == ZG].reset_index() 
        dff = dff.fillna(0)

    else: 
        brch_connu = filtered_data[3]['identifiantcanas'].nunique()
        lin_tot = filtered_data[2].drop_duplicates(subset='identifiantcanap')['longueur'].sum()
        brch_estime = round(lin_tot * Ratio,-3)
        brch_tot = brch_estime + brch_connu
        dff = pd.DataFrame({'longueur_res': [lin_tot], 'branch_connus': [brch_connu], 'branch_estimes': [brch_estime], 'branch_tot': [brch_tot]})
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        dff_bis = pd.DataFrame({'Connaissance': ['Connu', 'Estimé'], 'nb_brch': [dff['branch_connus'][0], dff['branch_estimes'][0]]})
        dff_bis['Pourcentage'] = round((dff_bis['nb_brch'] * 100)/ (sum(dff_bis['nb_brch'])))
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        piecolorway = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_basique_RGBA.values()]
        legend_handles = [mpatches.Patch(color=color) for color in piecolorway]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff_bis.sort_values(by='nb_brch', ascending=False)
        plt.pie(
            df_sorted_desc['nb_brch'], 
            labels = df_sorted_desc['nb_brch'].astype(int).astype(str) + '\n' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %',
            labeldistance = 1.09,
            colors= piecolorway,
            textprops={'fontsize': 12},
            wedgeprops = { 'linewidth' : 6, 'edgecolor' : 'white' },
            startangle = 90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            df_sorted_desc['Connaissance'],
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 12},
            title='Nombre de branchements :')
        
        out_figure = os.path.join(out_folder, f'Linéaire_connaissance_Branchements_{typereseau}_{NivExt}_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#ETAT DE SANTE NON VISITABLE (DONUTS)
def ES_NV(filtered_data, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    df_Sante_NV = filtered_data[1][filtered_data[1]['hauteur'] < 150]
    df_Sante_NV.loc[(df_Sante_NV['noteindig'] == 'G1'), 'noteindig'] = 'État bon'
    df_Sante_NV.loc[(df_Sante_NV['noteindig'] == 'G2'), 'noteindig'] = 'État correct'
    df_Sante_NV.loc[(df_Sante_NV['noteindig'] == 'G3'), 'noteindig'] = 'État dégradé'
    df_Sante_NV.loc[(df_Sante_NV['noteindig'] == 'G4'), 'noteindig'] = 'État mauvais'
    df_Sante_NV.loc[(df_Sante_NV['noteindig'] == 'G0'), 'noteindig'] = 'ITV incomplètes'
    df_Sante_NV['noteindig'] = df_Sante_NV['noteindig'].fillna('Pas de données')

    if group_column:
        groupes = df_Sante_NV.groupby([group_column, 'noteindig'])
        resultats = (round(groupes.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['longueur'].sum()/1000))).reset_index()
        resultats.columns = [group_column, 'noteindig', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
        if dff.empty:
            dff['Pourcentage'] = 0
        else :
            dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
        dff['Pourcentage'] = dff['Pourcentage'].fillna(0)
    else : 
        groupes = df_Sante_NV.groupby('noteindig')
        resultats = (round(groupes.apply(lambda x: x.drop_duplicates(subset='idcanaexp')['longueur'].sum()/1000))).reset_index()
        resultats.columns = ['noteindig', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
    if dff.empty or (dff[['longueur_sum', 'Pourcentage']] == 0).all().all():
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_couleur_etat_sante_RGBA.items()}
        #Trier le DataFrame
        df_sorted_desc = dff.sort_values(by='noteindig', ascending= False)
        # Extraire les couleurs dans l'ordre des catégories triées
        piecolorway = [category_colors[etat] for etat in df_sorted_desc['noteindig']]
        # Créer des handles pour la légende
        legend_handles = [mpatches.Patch(color=category_colors[etat]) for etat in df_sorted_desc['noteindig']]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff.sort_values(by='noteindig', ascending= False)
        plt.pie(
            df_sorted_desc['longueur_sum'], 
            labels = df_sorted_desc['longueur_sum'].astype(int).astype(str) + ' km (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)',
            labeldistance = 1.07,
            colors= piecolorway,
            textprops={'fontsize': 10},
            wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
            startangle = 90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            df_sorted_desc['noteindig'],
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 10})
        
        out_figure = os.path.join(out_folder, f'Etat_sante_Non_Visitables_{typereseau}_{NivExt}_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#ETAT DE SANTE STEU (DONUTS)
def ES_STEU(filtered_data, df_steu, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U':
        current_year = datetime.now().year
        steu = {
                13 : 'Genay',
                6 : 'Fontaines',
                8 : 'Jonage',
                1 : 'La Feyssine',
                11 : 'Lissieu Le Bourg',
                12 : 'Lissieu Sémanet',
                10 : 'Meyzieu',
                5 : 'Neuville',
                7 : 'Pierre-Bénite',
                17 : 'Quincieux',
                2 : 'Saint-Fons',
                4 : 'Saint-Germain',
                3 : 'Givors'
            }
        filtered_data[9]['N°'] = filtered_data[9]['nomsteu'].apply(lambda x: x.split()[1].split('_')[0] if len(x.split()) > 1 else None) 
        filtered_data[9]['N°'] = filtered_data[9]['N°'].apply(lambda x: x if x.isnumeric() else None)
        dfsteu_bis = filtered_data[9].copy()
        dfsteu_bis['nomsteu'] = dfsteu_bis['identifiantsteu'].map(steu).fillna(dfsteu_bis['nomsteu'])
        #Joindre les deux dataframes
        df_merged = pd.merge( df_steu, dfsteu_bis, on='N°', how='inner')

        df_merged['Etat'] = 'Pas de données'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 1), 'Etat'] = 'État bon'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 2), 'Etat'] = 'État correct'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 3), 'Etat'] = 'État dégradé'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 4), 'Etat'] = 'État mauvais'

        if group_column:
            
            groupes = df_merged.groupby([group_column, 'Etat'])
            resultat = (groupes['N°'].nunique()).reset_index()
            resultat.columns = [group_column, 'Etat', 'nbr']
            dff = pd.DataFrame(resultat)

            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
            if dff.empty:
                dff['Pourcentage'] = 0
            else :
                dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        else : 
            groupes = df_merged.groupby('Etat')
            resultats = (groupes['N°'].nunique()).reset_index()
            resultats.columns = ['Etat', 'nbr']
            dff = pd.DataFrame(resultats)
            dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_couleur_etat_sante_RGBA.items()}
            #Trier le DataFrame
            df_sorted_desc = dff.sort_values(by='Etat', ascending= False)
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[etat] for etat in df_sorted_desc['Etat']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[etat]) for etat in df_sorted_desc['Etat']]

            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')

            plt.pie(
                df_sorted_desc['nbr'], 
                labels = df_sorted_desc['nbr'].astype(int).astype(str) + ' (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)',
                labeldistance = 1.07,
                colors= piecolorway,
                textprops={'fontsize': 10},
                wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
                startangle = 90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['Etat'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 10})
            
            out_figure = os.path.join(out_folder, f'Etat_sante_STEU_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#ETAT DE SANTE STR (DONUTS)
def ES_STR(filtered_data, df_str, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EU_U':
        current_year = datetime.now().year
        #Extraire les codes de steu dans une nouvelle colonne 'N°'
        filtered_data[8] = filtered_data[8][filtered_data[8]['numero'].notna() & (filtered_data[8]['numero'] != 0)]
        filtered_data[8]['N°'] = filtered_data[8]['numero'].astype(str).apply(lambda x: x if x.isnumeric() else None)
        #Joindre les deux dataframes
        df_merged = pd.merge( filtered_data[8], df_str, on='N°', how='left')
        df_merged['Etat'] = 'Pas de données'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 1), 'Etat'] = 'État bon'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 2), 'Etat'] = 'État correct'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 3), 'Etat'] = 'État dégradé'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 4), 'Etat'] = 'État mauvais'
        df_merged= df_merged.drop_duplicates(subset='N°', keep='first')
        if group_column:
            
            groupes = df_merged.groupby([group_column, 'Etat'])
            resultat = (groupes['N°'].nunique()).reset_index()
            resultat.columns = [group_column, 'Etat', 'nbr']
            dff = pd.DataFrame(resultat)

            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
            if dff.empty:
                dff['Pourcentage'] = 0
            else :
                dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        else : 
            groupes = df_merged.groupby('Etat')
            resultats = (groupes['N°'].nunique()).reset_index()
            resultats.columns = ['Etat', 'nbr']
            dff = pd.DataFrame(resultats)
            dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        if dff.empty or (len(dff) == 1 and dff['nbr'][0] == 0):
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            dff= dff.fillna(0)
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_couleur_etat_sante_RGBA.items()}
            #Trier le DataFrame
            df_sorted_desc = dff.sort_values(by='Etat', ascending= False)
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[etat] for etat in df_sorted_desc['Etat']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[etat]) for etat in df_sorted_desc['Etat']]

            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')

            plt.pie(
                df_sorted_desc['nbr'], 
                labels = df_sorted_desc['nbr'].astype(int).astype(str) + ' (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)',
                labeldistance = 1.07,
                colors= piecolorway,
                textprops={'fontsize': 10},
                wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
                startangle = 90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['Etat'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 10})
            
            out_figure = os.path.join(out_folder, f'Etat_sante_STR_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#ETAT DE SANTE Visitables (DONUTS)
def ES_V(df_vis, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):

    if group_column:
        groupes = df_vis.groupby([group_column, 'ET_7'])
        resultats = (round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000), 3)).reset_index()
        resultats.columns = [group_column, 'ET_7', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        if ZG != 'Tous':
            dff = dff[dff[group_column] == ZG].reset_index()
        else:
            dff = dff[dff[group_column] == niveau].reset_index()
        if dff.empty:
            dff['Pourcentage'] = 0
        else :
            dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
    else : 
        groupes = df_vis.groupby('ET_7')
        resultats = (round(groupes.apply(lambda x: x.drop_duplicates(subset='identifiantcanap')['longueur'].sum()/1000))).reset_index()
        resultats.columns = ['ET_7', 'longueur_sum']
        dff = pd.DataFrame(resultats)
        dff['Pourcentage'] = round((dff['longueur_sum'] * 100)/ (sum(dff['longueur_sum'])))
    dff = dff.fillna(0)
    if dff.empty:
        dff.loc[0] = [0] * len(dff.columns)
        return(dff, NODATA_PATH)
    else:
        # Convertir les valeurs RVB en valeurs RGBA divisées par 255
        category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_couleur_etat_sante_RGBA.items()}
        #Trier le DataFrame
        df_sorted_desc = dff.sort_values(by='ET_7', ascending= False)
        # Extraire les couleurs dans l'ordre des catégories triées
        piecolorway = [category_colors[etat] for etat in df_sorted_desc['ET_7']]
        # Créer des handles pour la légende
        legend_handles = [mpatches.Patch(color=category_colors[etat]) for etat in df_sorted_desc['ET_7']]
        # Create a circle at the center of the plot
        my_circle = plt.Circle( (0,0), 0.7, color='white')
        df_sorted_desc = dff.sort_values(by='ET_7', ascending= False)
        if group_column:
            label = df_sorted_desc['longueur_sum'].round(1).astype(str) + ' km (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)'
        else:
            label = df_sorted_desc['longueur_sum'].round(1).astype(str) + ' km (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)'
        plt.pie(
            df_sorted_desc['longueur_sum'], 
            labels = label,
            labeldistance = 1.07,
            colors= piecolorway,
            textprops={'fontsize': 10},
            wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
            startangle = 90)
        p = plt.gcf()
        p.gca().add_artist(my_circle)

        plt.legend(
            legend_handles,
            df_sorted_desc['ET_7'],
            bbox_to_anchor=(1.2, 0.75),
            loc='best',
            frameon = False,
            prop={'size': 10})
        
        out_figure = os.path.join(out_folder, f'Etat_Sante_Visitables_{typereseau}_{NivExt}_{niveau}.png')
        plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
        plt.close()
        return(dff,out_figure)

#ETAT DE SANTE STR_EP (DONUTS)
def ES_STR_EP(filtered_data, df_tremies, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''): 
    if typereseau == 'EP':
        current_year = datetime.now().year
        
        df_tremies['numero'] = df_tremies['numero'].astype(int)
        df_merged = pd.merge(filtered_data[11], df_tremies, on='numero', how='left')
        
        df_merged['Etat'] = 'Pas de données'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 1), 'Etat'] = 'État bon'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 2), 'Etat'] = 'État correct'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 3), 'Etat'] = 'État dégradé'
        df_merged.loc[(df_merged[f'Campagne {current_year-1}'] == 4), 'Etat'] = 'État mauvais'

        if group_column:
            
            groupes = df_merged.groupby([group_column, 'Etat'])
            resultat = (groupes['numero'].nunique()).reset_index()
            resultat.columns = [group_column, 'Etat', 'nbr']
            dff = pd.DataFrame(resultat)

            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
            if dff.empty:
                dff['Pourcentage'] = 0
            else :
                dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        else : 
            groupes = df_merged.groupby('Etat')
            resultats = (groupes['numero'].nunique()).reset_index()
            resultats.columns = ['Etat', 'nbr']
            dff = pd.DataFrame(resultats)
            dff['Pourcentage'] = round((dff['nbr'] * 100)/ (sum(dff['nbr'])))
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_couleur_etat_sante_RGBA.items()}
            #Trier le DataFrame
            df_sorted_desc = dff.sort_values(by='Etat', ascending= False)
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[etat] for etat in df_sorted_desc['Etat']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[etat]) for etat in df_sorted_desc['Etat']]

            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')

            plt.pie(
                df_sorted_desc['nbr'], 
                labels = df_sorted_desc['nbr'].astype(int).astype(str) + ' (' + df_sorted_desc['Pourcentage'].astype(int).astype(str) + ' %)',
                labeldistance = 1.07,
                colors= piecolorway,
                textprops={'fontsize': 10},
                wedgeprops = { 'linewidth' : 1.5, 'edgecolor' : 'white' },
                startangle = 90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['Etat'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 10})
            
            out_figure = os.path.join(out_folder, f'Etat_sante_STR_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff,out_figure)

#### Age Bassins
def AgeBassins(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EP' :
        df_ages = filtered_data[0].copy()
        df_ages['anneeconstruction'].fillna(-1, inplace=True)
        year = datetime.now().year
        df_ages['age'] = year - df_ages['anneeconstruction'].astype(int)
        bornes = [0, 10, 20, 30, float('inf')]
        labels = ['0-10 ans', '11-20 ans', '21-30 ans', '> 30 ans']
        df_ages['tranche_age'] = pd.cut(df_ages['age'], bins=bornes, labels=labels)
        df_ages['tranche_age'] = df_ages['tranche_age'].cat.add_categories(['Pas de données'])
        df_ages.loc[df_ages['anneeconstruction'] == -1, 'tranche_age'] = 'Pas de données'
        
        if group_column :
            groupes = df_ages.groupby([group_column, 'tranche_age'])
            indicateur_sum = groupes['identifiantbassin'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            groupes = df_ages.groupby('tranche_age')
            indicateur_sum = groupes['identifiantbassin'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)
        dff ['nb_str']= dff['nb_str'].fillna(0) 

        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            df_sorted_desc = dff.sort_values(by='tranche_age')
            if (df_sorted_desc['nb_str'] == 0).all()  :
                return(dff, NODATA_PATH)
            else:
                # Convertir les valeurs RVB en valeurs RGBA divisées par 255
                category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_age_ouvrage_RGBA.items()}
                # Extraire les couleurs dans l'ordre des catégories triées
                piecolorway = [category_colors[age] for age in df_sorted_desc['tranche_age']]
                # Créer des handles pour la légende
                legend_handles = [mpatches.Patch(color=category_colors[age]) for age in df_sorted_desc['tranche_age']]

                # Create a circle at the center of the plot
                my_circle = plt.Circle( (0,0), 0.7, color='white')
                plt.pie(
                    df_sorted_desc['nb_str'], 
                    labels = df_sorted_desc['nb_str'].astype(int).astype(str),
                    labeldistance = 1.05,
                    colors= piecolorway,
                    textprops={'fontsize': 12},
                    wedgeprops = { 'linewidth' : 2.5, 'edgecolor' : 'white' },
                    counterclock=False,
                    startangle=90)
                p = plt.gcf()
                p.gca().add_artist(my_circle)

                plt.legend(
                    legend_handles,
                    df_sorted_desc['tranche_age'],
                    bbox_to_anchor=(1.2, 0.75),
                    loc='best',
                    frameon = False,
                    prop={'size': 12},
                    title='Tranches d\'âge')
                
                out_figure = os.path.join(out_folder, f'Bassins_par_age_{typereseau}_{NivExt}_{niveau}.png')
                plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
                plt.close()
                return(dff, out_figure)

#### Age STR_EP
def AgeSTR_EP(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =''):
    if typereseau == 'EP' :
        year = datetime.now().year
        
        df_ages = filtered_data[11].dropna(subset=['anneeconstruction'])
        df_ages = df_ages[df_ages['anneeconstruction'].astype(int) != 0]
        df_ages['age'] = year - df_ages["anneeconstruction"].astype(int)

        bornes = [0, 10, 20, 30, float('inf')]
        labels = ['0-10 ans', '11-20 ans', '21-30 ans', '> 30 ans']
        df_ages['tranche_age'] = pd.cut(df_ages['age'], bins=bornes, labels=labels)
        df_ages['tranche_age'] = df_ages['tranche_age'].cat.add_categories(['Pas de données'])
        df_ages['tranche_age'].fillna('Pas de données', inplace=True)

        if group_column :
            groupes = df_ages.groupby([group_column, 'tranche_age'])
            indicateur_sum = groupes['identifianttremie'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)
            if ZG != 'Tous':
                dff = dff[dff[group_column] == ZG].reset_index()
            else:
                dff = dff[dff[group_column] == niveau].reset_index()
        else : 
            groupes = df_ages.groupby('tranche_age')
            indicateur_sum = groupes['identifianttremie'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = ['tranche_age', 'nb_str']
            dff = pd.DataFrame(resultats)

        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
            return(dff, NODATA_PATH)
        else:
            df_sorted_desc = dff.sort_values(by='tranche_age')
            
            # Convertir les valeurs RVB en valeurs RGBA divisées par 255
            category_colors = {k: (v[0]/255, v[1]/255, v[2]/255) for k, v in palette_age_ouvrage_RGBA.items()}
            # Extraire les couleurs dans l'ordre des catégories triées
            piecolorway = [category_colors[age] for age in df_sorted_desc['tranche_age']]
            # Créer des handles pour la légende
            legend_handles = [mpatches.Patch(color=category_colors[age]) for age in df_sorted_desc['tranche_age']]

            # Create a circle at the center of the plot
            my_circle = plt.Circle( (0,0), 0.7, color='white')
            plt.pie(
                df_sorted_desc['nb_str'], 
                labels = df_sorted_desc['nb_str'].astype(int).astype(str),
                labeldistance = 1.05,
                colors= piecolorway,
                textprops={'fontsize': 12},
                wedgeprops = { 'linewidth' : 2.5, 'edgecolor' : 'white' },
                counterclock=False,
                startangle=90)
            p = plt.gcf()
            p.gca().add_artist(my_circle)

            plt.legend(
                legend_handles,
                df_sorted_desc['tranche_age'],
                bbox_to_anchor=(1.2, 0.75),
                loc='best',
                frameon = False,
                prop={'size': 12},
                title='Tranches d\'âge')
            
            out_figure = os.path.join(out_folder, f'STR_par_age_{typereseau}_{NivExt}_{niveau}.png')
            plt.savefig(out_figure, bbox_inches = 'tight', pad_inches=0.1)
            plt.close()
            return(dff, out_figure)

#### Nombre de Noues (simples et composées)
def NNo(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_bis = filtered_data[5][filtered_data[5]['natureogep'].isin([2, 3])].copy()
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes['identifiantogep'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'NBR_No']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [df_bis['identifiantogep'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['NBR_No'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de Tranchée (+ tranchée de stockholm)
def NTr(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_bis = filtered_data[5][filtered_data[5]['natureogep'].isin([1, 5])].copy()
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes['identifiantogep'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'NBR_Tr']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [df_bis['identifiantogep'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['NBR_Tr'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)

#### Nombre de Linéaires d'infiltaration
def NLi(filtered_data, group_column, typereseau, ZG, niveau =''):
    if typereseau == 'EP' :
        df_bis = filtered_data[5][filtered_data[5]['natureogep'].isin([1, 2, 3, 5])].copy()
        if group_column:
            groupes = df_bis.groupby(group_column)
            indicateur_sum = groupes['identifiantogep'].nunique()
            resultats = indicateur_sum.reset_index()
            resultats.columns = [group_column, 'NBR_Li']
            dff = pd.DataFrame(resultats)
            if ZG == 'Tous': 
                dff = dff[dff[group_column] == niveau].reset_index()
            else : 
                dff = dff[dff[group_column] == ZG].reset_index() 
        else : 
            resultats_list = [df_bis['identifiantogep'].nunique()]
            dff = pd.DataFrame(resultats_list, columns=['NBR_Li'])
        if dff.empty:
            dff.loc[0] = [0] * len(dff.columns)
        return(dff)