# DF
from dbfread import DBF
import geopandas as gpd
import pandas as pd

from datetime import datetime
import numpy as np

# Window
import tkinter as tk
from tkinter import filedialog, ttk, messagebox
import os
from pathlib import Path


# SQL
from sqlalchemy import create_engine
import concurrent.futures


from shapely.wkb import loads as wkb_loads

# Carto
import subprocess as sub

# Fonctions Indicateurs :
from FonctionsIndicateurs import *

################## NIVEAU D'EXTRACTION & TYPE DE RESEAUX ##################

class App:
    def __init__(self, root):
        self.root = root
        self.selected_item1 = None
        self.selected_item2 = None
        self.selected_item3 = None

        self.root.title("Sélectionner le niveau d'extraction")

        self.combo_box1 = ttk.Combobox(root, values=["Metropole", "Commune", "Systeme Assainissement", "Bassin versant", "Captage"], width = 30)
        self.combo_box2 = ttk.Combobox(root, values=["EP", "EU_U"], width = 30)
        self.combo_box3 = ttk.Combobox(root, values=[], state="readonly", width = 30)  # Lecture seule au début

        label1 = tk.Label(root, text="Niveau d'extraction :")
        label1.grid(row=0, column=0, padx=10, pady=10)
        label2 = tk.Label(root, text="Type de réseaux :")
        label2.grid(row=2, column=0, padx=10, pady=10)
        label3 = tk.Label(root, text="Sous-catégorie :")
        label3.grid(row=1, column=0, padx=10, pady=10)

        self.combo_box1.grid(row=0, column=1, padx=10, pady=10)
        self.combo_box2.grid(row=2, column=1, padx=10, pady=10)
        self.combo_box3.grid(row=1, column=1, padx=10, pady=10)

        # Ajouter un bouton d'information à côté de combo_box2
        self.info_button = tk.Button(root, text="i", command=self.show_info)
        self.info_button.grid(row=2, column=2, padx=10, pady=10)

        self.combo_box1.bind("<<ComboboxSelected>>", self.populate_combo_box3)

        confirm_button = tk.Button(root, text="Sélectionner et Fermer", command=self.select_and_close)
        confirm_button.grid(row=3, column=0, columnspan=2, padx=10, pady=10)

        self.error_label = tk.Label(root, text="", fg="red")
        self.error_label.grid(row=4, column=0, columnspan=2)
    def show_info(self):
        typedic={
            'EU_U' : 'Patrimoine Eaux Usées',
            'EP' : 'Patrimoine Eaux Pluviales'
        }
        message = "\n".join([f'{code} : {description}' for code, description in typedic.items()])
        messagebox.showinfo("Information", message)

    def populate_combo_box3(self, event=None):
        self.selected_item1 = self.combo_box1.get()

        if self.selected_item1 == "Commune":
            values3 = [
                'Tous', 'ALBIGNY-SUR-SAONE', 'BRON', 'CAILLOUX-SUR-FONTAINES', 'CALUIRE ET CUIRE', 'CHAMPAGNE-AU-MONT-D OR', 'CHARBONNIERES-LES-BAINS', 'CHARLY', 'CHASSIEU', 'COLLONGES-AU-MONT- D OR', 'CORBAS', 'COUZON-AU-MONT-D OR', 'CRAPONNE', 'CURIS-AU-MONT-D OR', 'DARDILLY', 'DECINES CHARPIEU', 'ECULLY', 'FEYZIN', 'FLEURIEU-SUR-SAONE', 'FONTAINES SUR SAONE', 'FONTAINES-SAINT-MARTIN', 'FRANCHEVILLE', 'GENAY', 'GIVORS', 'GRIGNY', 'IRIGNY', 'JONAGE', 'LA MULATIERE', 'LA TOUR DE SALVAGNY', 'LIMONEST', 'LISSIEU', 'LYON 1ER', 'LYON 2EME', 'LYON 3EME', 'LYON 4EME', 'LYON 5EME', 'LYON 6EME', 'LYON 7EME', 'LYON 8EME', 'LYON 9EME', 'MARCY-L ETOILE', 'MEYZIEU', 'MIONS', 'MONTANAY', 'NEUVILLE SUR SAONE', 'OULLINS', 'PIERRE BENITE', 'POLEYMIEUX-AU-MONT-D OR', 'QUINCIEUX', 'RILLIEUX LA PAPE', 'ROCHETAILLEE-SUR-SAONE', 'SAINT CYR AU MONT D OR', 'SAINT-DIDIER-AU-MONT-D OR', 'SAINT-GENIS-LAVAL', 'SAINT-GENIS-LES-OLLIERES', 'SAINT-GERMAIN-AU-MONT-D OR', 'SAINT-ROMAIN-AU-MONT-D OR', 'SAINTE FOY LES LYON', 'SATHONAY-CAMP', 'SATHONAY-VILLAGE', 'SOLAIZE', 'ST FONS', 'ST PRIEST', 'TASSIN-LA-DEMI-LUNE', 'VAULX EN VELIN', 'VENISSIEUX', 'VERNAISON', 'VILLEURBANNE'
            ]
        elif self.selected_item1 == "Systeme Assainissement":
            values3 = ['Tous', 'Genay', 'Fontaines', 'Jonage', 'La Feyssine', 'Lissieu Le Bourg', 'Lissieu Sémanet', 'Meyzieu', 'Neuville', 'Pierre-Bénite', 'Quincieux', 'Saint-Fons', 'Saint-Germain']
        elif self.selected_item1 == "Bassin versant":
            values3 = ['Tous', 'GENAY, GRAND RIEUX', 'MAINTENU, COMBE, GRANDES GORGES', 'THOU', 'TORRIERES', 'ECHETS', 'VOSGES', 'RAVIN', 'PINAY DE ST ROMAIN, MONGELAS, CHARBOTTES', 'ROCHECARDON', 'SEMANET, GORGES, MALIGNEUX, AZERGUES', 'PLANCHES', 'MOUCHE', 'VERNIERES, FEE DES EAUX, SERVE, RAZAT', 'YZERON', 'GUA, EPI, RIZAN, BLETTA', 'GARON', 'CHANAUX', 'OZON', 'GIER', 'MERDARY, COMBE DE BANS', 'SAONE', 'RHONE', 'RIZE', 'RATAPON']
        elif self.selected_item1 == "Captage":
            values3 = ['Tous', 'Captage de Chassieu', 'Captage de Corbas Romanettes', 'Captage de Crépieux-Charmy', 'Captage de Curis', 'Captage de Dècines', 'Captage de Fleurieu', 'Captage de Jonage', 'Captage de Meyzieu', 'Captage de Mions', 'Captage de St Priest', 'Lac de Miribel-Jonage']
        else:
            values3 = None

        self.combo_box3.set('')
        self.combo_box3['values'] = values3 if values3 else []

        # Si le niveau d'extraction est 'Metropole', désactiver la sélection de l'élément 3
        if self.selected_item1 == 'Metropole':
            self.combo_box3.set('')
            self.combo_box3.config(state="disabled")
        else:
            self.combo_box3.config(state="readonly")

    def select_and_close(self):
        self.selected_item1 = self.combo_box1.get()
        self.selected_item2 = self.combo_box2.get()
        self.selected_item3 = self.combo_box3.get()
        
        if self.selected_item1 and self.selected_item2 and (self.selected_item1 == 'Metropole' or self.selected_item3):
            self.root.destroy()
        else:
            self.error_label.config(text="Veuillez remplir tous les champs.", fg="red")

root = tk.Tk()
app = App(root)
root.mainloop()

print("Niveau d'extraction sélectionné : ", app.selected_item1)
print("Sous-catégorie : ", app.selected_item3)
print("Type de réseaux sélectionné : ", app.selected_item2)

################## file path ##################
pdf_file = filedialog.askdirectory(initialdir=os.getcwd(), title="Sélectionner le dossier de sortie")
################################# Partie : Importation et traitement des données ########################################
# Définition des paramètres de connexion à la base de données PostgreSQL
db_user = 'u_qualigeo'
db_password = 'JhPquU5mOPv7kulg'
db_host = 'bdd-pgssocle-pro.grandlyon.fr'
db_port = '5432'
db_name = 'socle'

# Période d'étude
current_year = datetime.now().year
date_debut = datetime(current_year-1, 1, 1) 
date_fin = datetime(current_year-1, 12, 31)

# Chemins des fichiers à importer
# Vérifier si __file__ est défini (uniquement pour les scripts Python)
if "__file__" in globals():
    SCRIPT_DIR = Path(__file__).resolve().parent
else:
    SCRIPT_DIR = Path(os.getcwd())  # Si exécuté dans un Notebook, prend le dossier actuel
# Définition du chemin de base du projet

ROOT_DIR = SCRIPT_DIR.parent  # "Indicateurs"
SORTIE_DIR = Path("L:/EGDD/DAEA/I_DAE/ServicesThemes/Trans/GDP/SIG/Dossier_JB/Dossier_partage/20_MAJ_23_Amen Allah NOUIRA/Indicateurs/sortiejbb")
# Définition des répertoires de données
DOSSIER_PAG = Path("L:/EGDD/DAEA/PAG")
#DOSSIER_SIG = Path("L:/EGDD/DAEA/I_DAE/ServicesThemes/Trans/GDP/SIG")
DOSSIER_ASSAINISSEMENT = Path("L:/EGDD/DAEA/I_DAE/Dsm/ASS")
DOSSIER_OGEP = Path("L:/EGDD/DAEA/I_DAE/Dsm/ASS/OGEP")
#DOSSIER_PARTAGE = Path("L:/EGDD/DAEA/I_DAE/ServicesThemes/Trans/GDP/SIG/Dossier_JB/Dossier_partage")
DOSSIER_CARTO = Path("L:/EGDD/DAEA/PAG/E_GDP/Projets transversaux/_Cartographie/__Environnement Cartographique")
DOSSIER_ARCHIVE = Path(f"H:/DTEE/EAU/ARCHIVES SIG/archives {current_year}/Documents_connexes")

chemin_regrpbv = DOSSIER_PAG / "K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/BV_REGROUPEMENT.xlsx"
AGEC_path = DOSSIER_CARTO / "_DATA_PARTAGE/Reseaux_AGEC/fevrier2025/portions_avec_etudes_fev2025.shp"
ASSCANEXP_ITV_path = DOSSIER_CARTO / f"14_Observations_ITV_12-2023/DATA/dec_{current_year-1}/ASSCANEXP_ITV.shp"
chemin_fichier_excel_idcapt = DOSSIER_ARCHIVE / "Patrimoines_Usines_Etat_Sante.xls"
chemin_excel_idcapt_ns = DOSSIER_ARCHIVE / "idcapture-Patrimoines Usines-Campagne.xlsx"
canexp_overlaps_cadstre_path = DOSSIER_CARTO / "4_Etude_domanialite_reseau_emprise_ouvrage/DATA/mars_2025/canexp_overlaps_cadastre.gpkg"
canexp_overlaps_voie_path = DOSSIER_CARTO / "4_Etude_domanialite_reseau_emprise_ouvrage/DATA/mars_2025/canexp_overlaps_voie.gpkg"
bassin_path = DOSSIER_ASSAINISSEMENT / "Bassins/BDD_Bassin.xlsx"
Rec_path = DOSSIER_ARCHIVE / "Ouvrages_ESU.xlsx"
dossier_FPR = Path("H:/DTEE/EAU/FPR") 
Rec_AP_path = DOSSIER_OGEP / "01_Inventaire_OGEP/Inventaire_OGEP.xlsx"
Idee_res_path = DOSSIER_CARTO / "_DATA_PARTAGE/Reseaux_Idée_tx/couche_idf.dbf"
chemin_ratio = DOSSIER_PAG / "E_GDP/Projets transversaux/INDICATEURS/Indicateurs et données patrimoniaux/00_Doc de travail/Ratio cout patrimoine.xlsx"


############ SELECTION DE DOSSIER DE SORTIE FIG ##########
if app.selected_item2 == 'EP':
    if app.selected_item1 == "Commune":
        out_folder = SORTIE_DIR / 'EP/Commune/'
    elif app.selected_item1 == "Bassin versant":
        out_folder = SORTIE_DIR / 'EP/BV/'
    elif app.selected_item1 == "Metropole":
        out_folder = SORTIE_DIR / 'EP/Metropole/'
    elif app.selected_item1 == "Captage":
        out_folder = SORTIE_DIR / 'EP/Captage/'
    else : 
        out_folder = SORTIE_DIR / 'EP/Sys_Ass/'
elif app.selected_item2 == 'EU_U':
    if app.selected_item1 == "Commune":
        out_folder = SORTIE_DIR / 'EU_U/Commune/'
    elif app.selected_item1 == "Bassin versant":
        out_folder = SORTIE_DIR / 'EU_U/BV/'
    elif app.selected_item1 == "Metropole":
        out_folder = SORTIE_DIR / 'EU_U/Metropole/'
    elif app.selected_item1 == "Captage":
        out_folder = SORTIE_DIR / 'EU_U/Captage/'
    else : 
        out_folder = SORTIE_DIR / 'EU_U/Sys_Ass/'
  
################################ Partie : Calcul des indicateurs ####################################################

#Ration branchement estimé
''' 300000 : nbr de branchement estimé (total),
    54246 : nbr de bronchement connu,
    3298416 : linéaire de reseau (m)
'''
Ratio = (300000 - 54246) / 3298416 
#Palettes de Couleurs
if app.selected_item2 == 'EP':
    palette_basique = {
        'Rouge': 'rgb(38, 101, 94)',
        'Vert_Cyan': 'rgb(0, 197, 175)',
        'Gris': 'rgb(38, 51, 50)'
    }
    palette_basique_RGBA = {
        'Niveau1': (0, 197, 175),
        'Niveau2': (38, 101, 94),
        'Niveau3': (38, 51, 50)
    }
    palette_CCD = {
        'Niveau1': 'rgb(0, 197, 175)',
        'Niveau2': 'rgb(38, 101, 94)',
        'Niveau3': 'rgb(196, 118, 0)',
        'Niveau4': 'rgb(0, 196, 66)',
        'Niveau5': 'rgb(196, 62, 0)',
        'Niveau6': 'rgb(69, 55, 34)',
        'Niveau7': 'rgb(69, 45, 34)',
        'Niveau8': 'rgb(38, 51, 50)'
    }
    palette_CCD_RGBA = {
        'Niveau1': (0, 197, 175),
        'Niveau2': (38, 101, 94),
        'Niveau3': (196, 118, 0),
        'Niveau4': (0, 196, 66),
        'Niveau5': (196, 62, 0),
        'Niveau6': (69, 55, 34),
        'Niveau7': (69, 45, 34),
        'Niveau8': (38, 51, 50)
    }
    palette_age_ouvrage_RGBA = {
        '0-10 ans': (0, 197, 175),
        '11-20 ans': (38, 101, 94),
        '21-30 ans': (196, 118, 0),
        '> 30 ans': (237, 33, 43),
        'Pas de données': (0, 0, 0)}
    palette_age_Res_RGBA = {
        '0-25 ans': (0, 197, 175),
        '26-50 ans': (38, 101, 94),
        '51-75 ans': (196, 118, 0),
        '> 75 ans': (237, 33, 43)}

else :
    #Bleu
    palette_basique = {
        'Rouge': 'rgb(0, 72, 128)',
        'Vert_Cyan': 'rgb(71, 151, 212)',
        'Gris': 'rgb(85, 85, 85)'
    }
    palette_basique_RGBA = {
        'Niveau1': (71, 151, 212),
        'Niveau2': (0, 72, 128),
        'Niveau3': (85, 85, 85)
    }
    palette_CCD = {
        'Niveau1': 'rgb(164, 255, 128)',
        'Niveau2': 'rgb(128, 0, 116)',
        'Niveau3': 'rgb(0, 72, 128)',
        'Niveau4': 'rgb(128, 200, 255)',
        'Niveau5': 'rgb(255, 220, 128)',
        'Niveau6': 'rgb(36, 128, 0)',
        'Niveau7': 'rgb(128, 55, 0)',
        'Niveau8': 'rgb(128, 92, 0)'
    }
    palette_CCD_RGBA = {
        'Niveau1': (0, 72, 128),
        'Niveau2': (71, 151, 212),
        'Niveau3': (255, 220, 128),
        'Niveau4': (0,128, 115),
        'Niveau5': (255, 0, 0),
        'Niveau6': (128, 55, 0),
        'Niveau7': (128, 0, 116),
        'Niveau8': (36, 128, 0)
    }
    palette_age_ouvrage_RGBA = {
        '0-10 ans': (0, 72, 128),
        '11-20 ans': (71, 151, 212),
        '21-30 ans': (255, 220, 128),
        '> 30 ans': (237, 33, 43),
        'Pas de données': (0, 0, 0)}
    palette_age_Res_RGBA = {
        '0-25 ans': (0, 72, 128),
        '26-50 ans': (71, 151, 212),
        '51-75 ans': (255, 220, 128),
        '> 75 ans': (237, 33, 43)}


#Palettes de Couleurs (Etat de santé) 
palette_couleur_etat_sante = {
    'Niveau1': 'rgb(69, 183,87)',
    'Niveau2': 'rgb(190, 215, 71)',
    'Niveau3': 'rgb(247, 148, 62)',
    'Niveau4': 'rgb(237, 33, 43)',
    'Niveau5': 'rgb(85, 85, 85)',
    'Niveau6': 'rgb(0, 0, 0)'
}
palette_couleur_etat_sante_RGBA = {
    'État mauvais': (237, 33, 43),
    'État dégradé': (247, 148, 62),
    'État correct': (190, 215, 71),
    'État bon': (69, 183,87),
    'Pas de données': (0, 0, 0),
    'ITV incomplètes': (85, 85, 85)
}

colors_pie = [(color[0]/255, color[1]/255, color[2]/255) for color in palette_basique_RGBA.values()]

######################################## Partie : Carto ##############################################################
osgeo4w_bat = r'C:/Program Files/QGIS3/OSGeo4W.bat'
python_command = r'python'
script =  r'L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/script/Atlas.py'
type_reseau = app.selected_item2

####################################### Partie : Mise en forme #######################################################
def format_number_with_spaces(number):
    return "{:,}".format(number).replace(',', ' ')

taille= {'T1': 20, 'T2': 16, 'T3': 14, 'T4': 11, 'T5': 10}
styles= {'T1': "Helvetica-Bold", 'T2': "Helvetica-Bold", 'T3': "Helvetica-Bold", 'T4': "Helvetica-Bold", 'T5': "Helvetica"}
n = 15 #interligne
path_logo_metropole = "L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/Mise_En_Forme/logo.jpg"
path_dossier_atlas = 'L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/carto/export'
dic_path_atlas = {
    "Commune": f'{path_dossier_atlas}/Atlas_{app.selected_item2}_Commune_',
    "Systeme Assainissement": f'{path_dossier_atlas}/Atlas_{app.selected_item2}_SA_',
    "Bassin versant": f'{path_dossier_atlas}/Atlas_{app.selected_item2}_BV_',
    "Captage": f'{path_dossier_atlas}/Atlas_{app.selected_item2}_Captage_'
}

############# IMPORTATION DES DONNEES ##############

# Création d'une chaîne de connexion à la base de données PostgreSQL
db_url = f'postgresql://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}'
# Connexion à la base de données
engine = create_engine(db_url)
# Importation des données
data_frames = []
liste_requetes = [
    '''
    SELECT 
        ref_assainissement.bassin.identifiantbassin,
        ref_assainissement.bassin.nombassin,
        ref_assainissement.bassin.nature,
        ref_assainissement.bassin.typebassin,
        ref_assainissement.bassin.anneeconstruction,
        ref_assainissement.bassin.domanialite,
        ref_assainissement.bassin.exploitant,
        ref_assainissement.bassin.etat,
        ref_assainissement.bassin.volumeexploitation,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        ref_assainissement.bassin.geom,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.bassin
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.bassin.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.bassin.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.bassin.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages

        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.bassin.geom), captageperim.geom) 	
    Where 
        ref_assainissement.bassin.domanialite = 2;''',
    '''
    SELECT 
        ref_assainissement.canaexp.idcanaexp,
        ref_assainissement.canaexp.longueur,
        ref_assainissement.canaexp.typreseau,
        ref_assainissement.canaexp.typeffluen,
        ref_assainissement.canaexp.noteindig,
        ref_assainissement.canaexp.domanialit,
        ref_assainissement.canaexp.hauteur,
        ref_assainissement.canaexp.annereha,
        ref_assainissement.canaexp.typeetude,
        ref_assainissement.canaexp.geom,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.canaexp
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canaexp.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canaexp.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canaexp.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
        ON          
            ST_Intersects(ST_Centroid(ref_assainissement.canaexp.geom), captageperim.geom)
    WHERE 
        canaexp.domanialit = 'Métropole';''',
    '''
    SELECT 
        ref_assainissement.canap.typeadministratifreseau,
        ref_assainissement.canap.typeeffluent,
        ref_assainissement.canap.identifiantcanap,
        ref_assainissement.canap.longueur,
        ref_assainissement.canap.domanialite,
        ref_assainissement.canap.materiaupresume,
        ref_assainissement.canap.materiau,
        ref_assainissement.canap.hauteur,
        ref_assainissement.canap.anneepose,
        ref_assainissement.canap.anneeposeinferieur,
        ref_assainissement.canap.anneeposesuperieur,
        ref_assainissement.canap.precisionplanimetrique,
        ref_assainissement.canap.geom,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.canap
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canap.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canap.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canap.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
            ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
            ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.canap.geom), captageperim.geom)
    WHERE 
        canap.domanialite = 2; ''',
    '''
    SELECT 
        ref_assainissement.canas.identifiantcanas, 
        ref_assainissement.canas.typereseau,
        ref_assainissement.canas.precisionplanimetrique,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_assainissement.canas.geom,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.canas
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canas.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canas.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.canas.geom), ref_milieuxaquatiques.ruisseaubv.geom)

    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            			
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.canas.geom), captageperim.geom);	
    ''',
    '''
WITH ranked_results AS (
    SELECT
        ref_assainissement.devorage.identifiantdevorage,
        (DATE_PART('year', ref_assainissement.devorage.datecreation) <= planrecol.anne_recol) AS recolement,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage,
        captageperim.nomcaptage,
        planrecol.identifiantplanrecol,
        ROW_NUMBER() OVER (
            PARTITION BY ref_assainissement.devorage.identifiantdevorage 
            ORDER BY 
                (DATE_PART('year', ref_assainissement.devorage.datecreation) <= planrecol.anne_recol) DESC
        ) AS rn,
		ref_assainissement.devorage.geom
    FROM
        ref_assainissement.devorage
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.devorage.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.devorage.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.devorage.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN
        (
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
        ) AS captageperim
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.devorage.geom), captageperim.geom)
    LEFT JOIN
        (
            SELECT
                ref_assainissement.planrecol.identifiantplanrecol,
                ref_assainissement.planrecol.etatplanrecolement,
                EXTRACT(YEAR FROM ref_assainissement.planrecol.dateplanrecolement) as anne_recol,
                ref_assainissement.planrecol.geom
            FROM ref_assainissement.planrecol
            WHERE ref_assainissement.planrecol.etatplanrecolement = 'I'
        ) AS planrecol
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.devorage.geom), planrecol.geom)
)
SELECT
    identifiantdevorage,
    recolement,
    identifiantbvgrand,
    identifiantsteu,
    nomsteu,
    id_com,
    texte,
    identifiantcoursdeau,
    identifiantbassinversantruisseau,
    identifiantcaptage,
    nomcaptage,
    geom
FROM
    ranked_results
WHERE
    rn = 1;''',
    '''
    SELECT 
        ref_assainissement.ogep.identifiantogep, 
        ref_assainissement.ogep.natureogep, 
        ref_assainissement.ogep.longueur,
        ref_assainissement.ogep.domanialite,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        ref_assainissement.ogep.geom,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.ogep
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.ogep.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.ogep.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.ogep.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.ogep.geom), captageperim.geom)
        where ref_assainissement.ogep.domanialite = 2; ''',
    '''
    SELECT 
        ref_assainissement.puitsf.identifiantpuitsf,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        ref_assainissement.puitsf.geom,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.puitsf
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.puitsf.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.puitsf.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.puitsf.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.puitsf.geom), captageperim.geom);''',
    '''
    SELECT 
        ref_eaugeneral.reseausec.identifiantreseausec,
        ref_eaugeneral.reseausec.typereseausec, 
        ref_eaugeneral.reseausec.classeprecision,
        ref_eaugeneral.reseausec.installation,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage,
        ref_eaugeneral.reseausec.geom
    FROM
        ref_eaugeneral.reseausec
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_eaugeneral.reseausec.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_eaugeneral.reseausec.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_eaugeneral.reseausec.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_eaugeneral.reseausec.geom), captageperim.geom); ''',
    '''
	SELECT DISTINCT
			ref_assainissement.stare.identifiantstare,
			ref_assainissement.stare.nomstare,
			ref_assainissement.stare.numero,
			ref_assainissement.stare.anneeconstruction,
			pub_assainissement.bvgrand.identifiantbvgrand,
			pub_assainissement.bvgrand.identifiantsteu,
			pub_assainissement.bvgrand.nomsteu,
			ref_cadastre.commune.id_com,
			ref_cadastre.commune.texte,
			ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
			ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
			captageperim.identifiantcaptage, 
			captageperim.nomcaptage,
			ref_assainissement.stare.geom

	FROM
		ref_assainissement.stare
	LEFT JOIN
		pub_assainissement.bvgrand
	ON
		ST_Intersects(ST_Centroid(ref_assainissement.stare.geom), pub_assainissement.bvgrand.geom)
	LEFT JOIN
		ref_cadastre.commune
	ON
		ST_Intersects(ST_Centroid(ref_assainissement.stare.geom), ref_cadastre.commune.geom)
	LEFT JOIN
		ref_milieuxaquatiques.ruisseaubv
	ON
		ST_Intersects(ST_Centroid(ref_assainissement.stare.geom), ref_milieuxaquatiques.ruisseaubv.geom)
	LEFT JOIN          
		( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages

		) AS captageperim  
	ON          
		ST_Intersects(ST_Centroid(ref_assainissement.stare.geom), captageperim.geom);''',
    '''
    SELECT 
        ref_assainissement.steu.nomsteu,
        ref_assainissement.steu.anneeconstruction,
        ref_assainissement.steu.propriete,
        ref_assainissement.steu.capacite,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage,
        ref_assainissement.steu.geom
    FROM
        ref_assainissement.steu
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.steu.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.steu.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.steu.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.steu.geom), captageperim.geom)
    where
        ref_assainissement.steu.propriete = '2';''',
    '''
    SELECT 
        ref_assainissement.traitement.identifianttraitement, 
        ref_assainissement.traitement.typetraitement,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte, 
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage,
        ref_assainissement.traitement.geom
    FROM
        ref_assainissement.traitement
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ref_assainissement.traitement.geom, ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), captageperim.geom);''',
    '''
    SELECT 
        ref_assainissement.tremie.identifianttremie, 
        ref_assainissement.tremie.numero, 
        ref_assainissement.tremie.anneeconstruction,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage
    FROM
        ref_assainissement.tremie
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.tremie.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.tremie.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.tremie.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.tremie.geom), captageperim.geom);''',
    '''
	WITH ranked_results AS (
		SELECT DISTINCT
			ref_assainissement.traitement.identifianttraitement,
			ref_assainissement.traitement.typetraitement,
			(DATE_PART('year', ref_assainissement.traitement.datecreation) <= planrecol.anne_recol) AS recolement,
			ref_cadastre.commune.id_com,
			ref_cadastre.commune.texte,
			ref_assainissement.canap.typeadministratifreseau,
			ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
			ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
			pub_assainissement.bvgrand.identifiantbvgrand,
			pub_assainissement.bvgrand.identifiantsteu,
			pub_assainissement.bvgrand.nomsteu,
			captageperim.identifiantcaptage, 
			captageperim.nomcaptage,
			ROW_NUMBER() OVER (
					PARTITION BY ref_assainissement.traitement.identifianttraitement 
					ORDER BY 
						(DATE_PART('year', ref_assainissement.traitement.datecreation) <= planrecol.anne_recol) DESC
				) AS rn,
			ref_assainissement.traitement.geom
		FROM ref_assainissement.traitement
		LEFT JOIN ref_assainissement.canap
		ON ST_Intersects(ref_assainissement.traitement.geom, ref_assainissement.canap.geom)
		LEFT JOIN
			pub_assainissement.bvgrand
		ON
			ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), pub_assainissement.bvgrand.geom)
		LEFT JOIN
			ref_cadastre.commune
		ON
			ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), ref_cadastre.commune.geom)
		LEFT JOIN
			ref_milieuxaquatiques.ruisseaubv
		ON
			ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), ref_milieuxaquatiques.ruisseaubv.geom)
		LEFT JOIN          
			( 	
			WITH captage_e565 AS (
				SELECT
					ref_eaupotable.captageperim.identifiantcaptage,
					ref_eaupotable.captage.nomcaptage,
					ref_eaupotable.captageperim.geom
				FROM
					ref_eaupotable.captageperim
				INNER JOIN
					ref_eaupotable.captage
				ON
					ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
				WHERE
					ref_eaupotable.captageperim.niveau = 'E'
					AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
			),
			autres_captages AS (
				SELECT
					ref_eaupotable.captageperim.identifiantcaptage,
					ref_eaupotable.captage.nomcaptage,
					ref_eaupotable.captageperim.geom,
					ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
				FROM
					ref_eaupotable.captageperim
				INNER JOIN
					ref_eaupotable.captage
				ON
					ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
				WHERE
					ref_eaupotable.captageperim.niveau = 'E'
					AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
			)
			-- Récupération du E565 découpé
			SELECT
				e565.identifiantcaptage,
				e565.nomcaptage,
				ST_Difference(e565.geom, autres.geom_union) AS geom
			FROM
				captage_e565 e565,
				(SELECT DISTINCT geom_union FROM autres_captages) autres

			UNION ALL

			-- Récupération de tous les autres polygones sans modification
			SELECT
				identifiantcaptage,
				nomcaptage,
				geom
			FROM
				autres_captages

			) AS captageperim  
		ON          
			ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), captageperim.geom)
		LEFT JOIN
			(
				SELECT
					ref_assainissement.planrecol.identifiantplanrecol,
					ref_assainissement.planrecol.etatplanrecolement,
					EXTRACT(YEAR FROM ref_assainissement.planrecol.dateplanrecolement) as anne_recol,
					ref_assainissement.planrecol.geom
				FROM ref_assainissement.planrecol
				WHERE ref_assainissement.planrecol.etatplanrecolement = 'I'
			) AS planrecol
		ON
			ST_Intersects(ST_Centroid(ref_assainissement.traitement.geom), planrecol.geom)
		WHERE
		NOT EXISTS (
			SELECT 1
			FROM ref_assainissement.steu
			WHERE ST_Intersects(ref_assainissement.traitement.geom, ref_assainissement.steu.geom)
		)
		AND
		NOT EXISTS (
			SELECT 1
			FROM ref_assainissement.stare
			WHERE ST_Intersects(ref_assainissement.traitement.geom, ref_assainissement.stare.geom)
		)
	)
	SELECT
		identifianttraitement,
		typetraitement,
		recolement,
		id_com,
		texte,
		typeadministratifreseau,
		identifiantcoursdeau,
		identifiantbassinversantruisseau,
		identifiantbvgrand,
		identifiantsteu,
		nomsteu,
		identifiantcaptage, 
		nomcaptage,
		geom
	FROM
		ranked_results
	WHERE
		rn = 1
	ORDER BY identifianttraitement;
    ''',
    '''
    SELECT 
        pub_fed_cadastre.parcelleajour.idparcelle,
        pub_fed_cadastre.parcelleajour.codenatureproprietaire,
        pub_fed_cadastre.parcelleajour.natureproprietaire,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage,
        pub_fed_cadastre.parcelleajour.geom
    FROM
        pub_fed_cadastre.parcelleajour
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(pub_fed_cadastre.parcelleajour.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(pub_fed_cadastre.parcelleajour.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(pub_fed_cadastre.parcelleajour.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(pub_fed_cadastre.parcelleajour.geom), captageperim.geom)
    Where pub_fed_cadastre.parcelleajour.codenatureproprietaire IN ('0', '5', '6', '7', '8', '10')''',
    '''
    SELECT 
        ref_assainissement.drain.iddrain,
        ref_assainissement.drain.datecreation,
        pub_assainissement.bvgrand.identifiantbvgrand,
        pub_assainissement.bvgrand.identifiantsteu,
        pub_assainissement.bvgrand.nomsteu,
        ref_cadastre.commune.id_com,
        ref_cadastre.commune.texte,
        ref_milieuxaquatiques.ruisseaubv.identifiantcoursdeau,
        ref_milieuxaquatiques.ruisseaubv.identifiantbassinversantruisseau,
        captageperim.identifiantcaptage, 
        captageperim.nomcaptage,
        ref_assainissement.drain.geom
    FROM
        ref_assainissement.drain
    LEFT JOIN
        pub_assainissement.bvgrand
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.drain.geom), pub_assainissement.bvgrand.geom)
    LEFT JOIN
        ref_cadastre.commune
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.drain.geom), ref_cadastre.commune.geom)
    LEFT JOIN
        ref_milieuxaquatiques.ruisseaubv
    ON
        ST_Intersects(ST_Centroid(ref_assainissement.drain.geom), ref_milieuxaquatiques.ruisseaubv.geom)
    LEFT JOIN          
        ( 	
		WITH captage_e565 AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage = 'E565'
		),
		autres_captages AS (
			SELECT
				ref_eaupotable.captageperim.identifiantcaptage,
				ref_eaupotable.captage.nomcaptage,
				ref_eaupotable.captageperim.geom,
				ST_Union(ref_eaupotable.captageperim.geom) OVER () AS geom_union
			FROM
				ref_eaupotable.captageperim
			INNER JOIN
				ref_eaupotable.captage
			ON
				ref_eaupotable.captageperim.identifiantcaptage = ref_eaupotable.captage.identifiantcaptage
			WHERE
				ref_eaupotable.captageperim.niveau = 'E'
				AND ref_eaupotable.captageperim.identifiantcaptage != 'E565'
		)
		-- Récupération du E565 découpé
		SELECT
			e565.identifiantcaptage,
			e565.nomcaptage,
			ST_Difference(e565.geom, autres.geom_union) AS geom
		FROM
			captage_e565 e565,
			(SELECT DISTINCT geom_union FROM autres_captages) autres

		UNION ALL

		-- Récupération de tous les autres polygones sans modification
		SELECT
			identifiantcaptage,
			nomcaptage,
			geom
		FROM
			autres_captages
            
        ) AS captageperim  
    ON          
        ST_Intersects(ST_Centroid(ref_assainissement.drain.geom), captageperim.geom);'''
]
# Optimisation de la connexion à la base de données
if not hasattr(engine, 'pool_size'):
    engine = engine.execution_options(
        pool_size=len(liste_requetes),  # Augmente le nombre de connexions dans le pool
        max_overflow=20,
        pool_timeout=30,
        pool_pre_ping=True,  # Vérifie que les connexions sont valides avant utilisation
        pool_recycle=3600  # Recycle les connexions après une heure
    )

def execute_query(args):
    k, query = args
    try:
        # Utiliser chunksize pour les requêtes qui retournent beaucoup de données
        df = pd.read_sql_query(query, engine, chunksize=None)
        print(f'Requête {k} exécutée')
        return df
    except Exception as e:
        print(f'Erreur lors de l\'exécution de la requête {k}: {e}')
        return pd.DataFrame()  # Retourne un DataFrame vide en cas d'erreur

# Utilisation de ThreadPoolExecutor pour paralléliser les requêtes
with concurrent.futures.ThreadPoolExecutor(max_workers=min(8, len(liste_requetes))) as executor:
    # Création d'une liste d'arguments (index, requête)
    query_args = [(k, query) for k, query in enumerate(liste_requetes)]
    
    # Exécution parallèle des requêtes
    data_frames = list(executor.map(execute_query, query_args))

print(f'Nbre des couches importées : {len(data_frames)}')

############ EXTRACTION DES DONNEES ##################
print("EXTRACTION DES DONNEES")

filtered_data = []
if app.selected_item2 == 'EU_U' :
    for df in data_frames :
        if df.equals(data_frames[1]):
            filtered_df = df[((df["typreseau"] == 'Eaux usées') | (df["typreseau"] == 'Unitaire') | ((df["typreseau"] == 'N''accepte pas de raccordement') & (df['typeffluen'] != 'Eaux pluviales'))) & (df["domanialit"] == 'Métropole')]
            filtered_data.append(filtered_df)
        elif df.equals(data_frames[2]):
            filtered_df = df[((df["typeadministratifreseau"] == 'EU') | (df["typeadministratifreseau"] == 'UN')| ((df["typeadministratifreseau"] == 'NO') & ((df['typeeffluent'] == 'UN') | (df['typeeffluent'] == 'EU')))) & (df["domanialite"] == 2)]
            filtered_data.append(filtered_df)
        elif df.equals(data_frames[3]):
            filtered_df = df[(df["typereseau"] == 'EU') | (df["typereseau"] == 'UN')]
            filtered_data.append(filtered_df)
        elif df.equals(data_frames[12]):
            filtered_df = df[(df["typeadministratifreseau"] == 'EU') | (df["typeadministratifreseau"] == 'UN')]
            filtered_data.append(filtered_df)
        else : filtered_data.append(df)
else:
	for df in data_frames:
		if df.equals(data_frames[1]):
			filtered_df = df[((df["typreseau"] == 'Eaux pluviales') | ((df["typreseau"] == 'N''accepte pas de raccordement') & (df['typeffluen'] == 'Eaux pluviales'))) & (df["domanialit"] == 'Métropole')]
			filtered_data.append(filtered_df)
		elif df.equals(data_frames[2]):
			filtered_df = df[((df["typeadministratifreseau"] == app.selected_item2) | ((df["typeadministratifreseau"] == 'NO') & (df['typeeffluent'] == 'EP'))) & (df["domanialite"] == 2)]
			filtered_data.append(filtered_df)
		elif df.equals(data_frames[3]):
			filtered_df = df[(df["typereseau"] == app.selected_item2)]
			filtered_data.append(filtered_df)
		elif df.equals(data_frames[12]):
			filtered_df = df[df["typeadministratifreseau"] == app.selected_item2]
			filtered_data.append(filtered_df)
		else:
			filtered_data.append(df)

#Remplisssage des données pour les deux steu hors territoire métropolitain
print("Remplisssage des données pour les deux steu hors territoire métropolitain")

conditions = (filtered_data[9]['nomsteu'].isin(['Code 207_Lissieu Le Bourg Le Roty', 'Code 208_Lissieu Dommartin Sémanet']))
condition1 = filtered_data[9]['nomsteu'] == 'Code 207_Lissieu Le Bourg Le Roty'
condition2 = filtered_data[9]['nomsteu'] == 'Code 208_Lissieu Dommartin Sémanet'

filtered_data[9].loc[conditions, 'texte'] = 'LISSIEU'
filtered_data[9].loc[conditions, 'id_com'] = 69117
filtered_data[9].loc[condition1, 'identifiantbvgrand'] = 'LB_LIB'
filtered_data[9].loc[condition1, 'identifiantsteu'] = 11
filtered_data[9].loc[condition2, 'identifiantbvgrand'] = 'LS_LIS'
filtered_data[9].loc[condition2, 'identifiantsteu'] = 12

# Traitement pour le niveau d'extraction bassin versant
print("Traitement pour le niveau d'extraction bassin versant")
def concatenate_bassins(group):
    return ', '.join(group)

bvregrp = pd.read_excel(chemin_regrpbv, sheet_name="Regroupement BV")
bvregrp = bvregrp.rename(columns={'identifiant_bassin': 'identifiantbassinversantruisseau', '2e regroupement' : 'identifiant_bassin'})
bvregrp = bvregrp[['identifiantbassinversantruisseau', 'identifiant_bassin', 'Bassin']]
bvregrp['Bassin'] = bvregrp.groupby('identifiant_bassin')['Bassin'].transform(concatenate_bassins)
bvregrp.loc[bvregrp['Bassin'] == 'RHONE, RHONE', 'Bassin'] = 'RHONE'

filtered_data = [df.merge(bvregrp, on='identifiantbassinversantruisseau') for df in filtered_data]

# # importation data de la database et les convertir en GeoDataFrames
# for index in list_req:
#     df = gpd.GeoDataFrame.from_postgis(index, engine, geom_col='geom')
#     BD_ext.append(df)

#### importation du shapefile 'portions_avec_travaux_mmmYYYY' et le convertir en GeoDataFrame
print("importation du shapefile 'portions_avec_travaux_mmmYYYY' et le convertir en GeoDataFrame")

gdf = gpd.read_file(AGEC_path)

if app.selected_item2 == 'EU_U' :
    filtered_gdf = gdf[(gdf["FH_4"] == 'EU') | (gdf["FH_4"] == 'UN')]
else:
    filtered_gdf = gdf[(gdf["FH_4"] == 'EP')]
filtered_gdf = filtered_gdf.rename(columns= {'IID_IDENTC':'identifiantcanap'})
df_vis = pd.merge(filtered_data[2][filtered_data[2]['hauteur'] >= 150], filtered_gdf, on='identifiantcanap', how= 'left')
type = {'Conservatoire': 'État mauvais', 'Curative': 'État dégradé', 'Préventive': 'État correct', 'Surveillance': 'État bon'}
for key, value in type.items():
    df_vis.loc[df_vis['ET_7'] == key, 'ET_7'] = value
df_vis['ET_7'] = df_vis['ET_7'].fillna('Pas de données')

################### Importation et traitment de la couche ASSCANEXP_ITV 
print("Importation et traitment de la couche ASSCANEXP_ITV ")

# Ouvrir le fichier shapefile
gdf = gpd.read_file(ASSCANEXP_ITV_path)
# Creation de la data frame annuelle
gdf['ABF'] = pd.to_datetime(gdf['ABF'], format='%Y-%m-%d', errors='coerce')  # Assurez-vous que la colonne 'ABF' est bien en format datetime
gdf_year = gdf[(gdf['ABF'] >= date_debut) & (gdf['ABF'] <= date_fin)].copy()

# Valeurs uniques de l'identifiant de troncons
valeurs_uniques = gdf['IID_IDENTC'].unique()
valeurs_uniques_annuelles = gdf_year['IID_IDENTC'].unique()

#################### Prétraitement sur les données pour les indicateurs liés aux réseaux secs
print("Prétraitement sur les données pour les indicateurs liés aux réseaux secs")

# Création du GeoDataFrame pour les réseaux secs
df_RS = filtered_data[7][filtered_data[7]['installation'].isin(['VANN','METR'])].copy()
df_RS['geometry'] = df_RS['geom'].apply(lambda x: wkb_loads(bytes.fromhex(x)))
gdf_RS = gpd.GeoDataFrame(df_RS, geometry = 'geometry', crs='EPSG:3946')
# Création du GeoDataFrame à  partir de canap
df_RS1 = filtered_data[2].copy()
df_RS1['geometry'] = df_RS1['geom'].apply(lambda x: wkb_loads(bytes.fromhex(x)))
gdf_RS1 = gpd.GeoDataFrame(df_RS1, geometry='geometry', crs='EPSG:3946')
# Jointure des deux GDF et suppression de la colonne 'geom_right'
spatial_join = gpd.sjoin_nearest(gdf_RS, gdf_RS1,  how="left", max_distance = 10)
#spatial_join = spatial_join.drop(columns='geom_right')
# Création de GDF pour le résultat
spatial_join['geometry'] = spatial_join['geom_left'].apply(lambda x: wkb_loads(bytes.fromhex(x)))
gdf_spatial_join= gpd.GeoDataFrame(spatial_join, geometry= 'geometry', crs='EPSG:3946')
gdf_spatial_join.rename(columns={'geom_left': 'geom'}, inplace=True)
#Création de GDF pour l'ensemble des données
gdf_glob_EP = pd.concat([filtered_data[7][filtered_data[7]['installation'] == 'TREM'], gdf_spatial_join[~gdf_spatial_join['typeadministratifreseau'].isin(['EP'])]], ignore_index=True)
gdf_glob_EU_U = pd.concat([filtered_data[7][filtered_data[7]['installation'].isin(['DO','STAR'])], gdf_spatial_join[gdf_spatial_join['typeadministratifreseau'].isin(['UN','EU'])]], ignore_index=True)
# Ajout d'une colonne longueur
gdf_glob_EP = gdf_glob_EP.dropna(subset=['geometry'])
gdf_glob_EU_U = gdf_glob_EU_U.dropna(subset=['geometry'])
gdf_glob_EP['geometry'] = gdf_glob_EP['geometry'].astype('object')  # Forcer la conversion en objet générique
gdf_glob_EP['longueur_troncon'] = gdf_glob_EP['geometry'].apply(lambda x: x.length if hasattr(x, "geom_type") and x.geom_type in ['LineString', 'MultiLineString'] else None)
gdf_glob_EU_U['geometry'] = gdf_glob_EU_U['geometry'].astype('object')  # Forcer la conversion en objet générique
gdf_glob_EU_U['longueur_troncon'] = gdf_glob_EU_U['geometry'].apply(lambda x: x.length if hasattr(x, "geom_type") and x.geom_type in ['LineString', 'MultiLineString'] else None)
#Supression des duplications
gdf_glob_EP['identifiantreseausec'] = gdf_glob_EP['identifiantreseausec'].drop_duplicates(keep='first')
gdf_glob_EU_U['identifiantreseausec'] = gdf_glob_EU_U['identifiantreseausec'].drop_duplicates(keep='first')
gdf_glob_EU_U = gdf_glob_EU_U.drop(['id_com','texte','identifiantbvgrand','identifiantsteu','geom_right','id_com_right','identifiantsteu_right','identifiantbvgrand_right','texte_right'],axis=1)
gdf_glob_EP = gdf_glob_EP.drop(['id_com','texte','identifiantbvgrand','identifiantsteu','geom_right','id_com_right','identifiantsteu_right','identifiantbvgrand_right','texte_right'],axis=1)

nouveaux_noms = {'id_com_left': 'id_com',
    'texte_left': 'texte',
    'identifiantbvgrand_left': 'identifiantbvgrand',
    'identifiantsteu_left': 'identifiantsteu'}
gdf_glob_EU_U = gdf_glob_EU_U.rename(columns=nouveaux_noms)
gdf_glob_EP = gdf_glob_EP.rename(columns=nouveaux_noms)
gdf_glob_EP = gpd.GeoDataFrame(gdf_glob_EP)
gdf_glob_EU_U = gpd.GeoDataFrame(gdf_glob_EU_U)

########################### CREATION DATAFRAMES POUR L'ETAT DE SANTE (SOURCE : IDCapture)#################################
print("CREATION DATAFRAMES POUR L'ETAT DE SANTE (SOURCE : IDCapture)")

# Charger le fichier Excel dans un DataFrame
df_etat = pd.read_excel(chemin_fichier_excel_idcapt)
#Créer des dataframes (steu, str et tremies)
df_str = df_etat[df_etat['Type de Station'] == 'STR'].copy()
df_steu = df_etat[(df_etat['Type de Station'] == 'STEU') | (df_etat['Type de Station'] == 'STEP')].copy()
df_tremies = df_etat[df_etat['Type de Station'] == 'TREMIES'].copy()
# Créer une nouvelle colonne pour numéro de l'ouvrage
df_str['N°'] = df_str['N° - Nom Station'].str.split().str[0].str.split('_').str[0] #Récupérer tous les chiffres avant le premier espace
df_str = df_str[['N°', f'Campagne {current_year - 1}']]

df_steu['N°'] = df_steu['N° - Nom Station'].str.split().str[0]
df_steu = df_steu[['N°', f'Campagne {current_year - 1}']]

df_tremies['numero'] = df_tremies['N° - Nom Station'].str.split().str[0]
df_tremies = df_tremies[['numero', f'Campagne {current_year - 1}']]

########################### CREATION DATAFRAMES POUR LE NOMBRE D'INSPECTION (SOURCE : IDCapture)#################################
print("CREATION DATAFRAMES POUR LE NOMBRE D'INSPECTION (SOURCE : IDCapture)")

# Charger le fichier Excel dans un DataFrame
ins = pd.read_excel(chemin_excel_idcapt_ns, skiprows=1)
#Créer des dataframes (steu, str et tremies)
ins_str = ins[ins['Type de Station'] == 'STR'].copy()
ins_steu = ins[ins['Type de Station'] == 'STEP'].copy()
ins_tremies = ins[ins['Type de Station'] == 'TREMIES'].copy()
# Créer une nouvelle colonne pour numéro de l'ouvrage
ins_str['N°'] = ins_str['N° - Nom Station'].str.split().str[0].str.split('_').str[0] #Récupérer tous les chiffres avant le premier espace
ins_steu['N°'] = ins_steu['N° - Nom Station'].str.split().str[0]
ins_tremies['N°'] = ins_tremies['N° - Nom Station'].str.split().str[0]

########################### CREATION DATAFRAMES POUR LINEAIRE DE RESEAUX SUR PARCELLES OU VOIES PRIVEES #################################
print("CREATION DATAFRAMES POUR LINEAIRE DE RESEAUX SUR PARCELLES OU VOIES PRIVEES")

#importation des couches
gdf_cad = gpd.read_file(canexp_overlaps_cadstre_path)
gdf_voie = gpd.read_file(canexp_overlaps_voie_path)

gdf_cad.rename(columns={'IID_IDENTC':'idcanaexp'})
gdf_voie.rename(columns={'IID_IDENTC':'idcanaexp'})

canexp_cadastre_merged = pd.concat([filtered_data[1], gdf_cad], axis=1, join='inner', keys='idcanaexp')
canexp_voie_merged = pd.concat([filtered_data[1], gdf_voie], axis=1, join='inner', keys='idcanaexp')
canexp_cadastre_merged = canexp_cadastre_merged.droplevel(level=0, axis=1)
canexp_voie_merged = canexp_voie_merged.droplevel(level=0, axis=1)

################################# Importation et traitement des données liées au récolement des bassins ##############################################
print("Importation et traitement des données liées au récolement des bassins")

dfRecBas = pd.read_excel(bassin_path, sheet_name="BDD")
Liste_columns = ['Identifiant','Nom ','Recolement']
dfRecBas = dfRecBas[Liste_columns]
dfRecBas.columns = ['identifiantbassin', 'nombassin', 'recolement']
dfRecBas = pd.merge(dfRecBas, filtered_data[0], on = 'identifiantbassin')
dfRecBas = dfRecBas[(dfRecBas['typebassin'].isin(['BI', 'BR', 'BRI'])) & (dfRecBas['etat'] == 'CON') & (dfRecBas['exploitant'] == 'GLE')]

################################# Traitement des données liées aux drains (linéaire d'infiltration) ####################################
print("Traitement des données liées aux drains (linéaire d'infiltration)")

filtered_data[14]['geom'] = filtered_data[14]['geom'].apply(lambda x: wkb_loads(bytes.fromhex(x)) if isinstance(x, str) else x)
gdf_drain = gpd.GeoDataFrame(filtered_data[14], geometry='geom')
gdf_drain['longueur'] = gdf_drain['geom'].length
gdf_drain['datecreation'] = pd.to_datetime(gdf_drain['datecreation'])
gdf_drain['annee'] = gdf_drain['datecreation'].dt.year

################################ Importation et traitement des données liées au récolement des STR ##########################################
print("Importation et traitement des données liées au récolement des STR")

dfRecSTR = pd.read_excel(Rec_path, sheet_name="Str", header= 1)
Liste_columns = ['Identifiant', 'récolé (O/N)']
dfRecSTR = dfRecSTR[Liste_columns]
dfRecSTR.columns = ['identifiantstare', 'recolement']
dfRecSTR = pd.merge(dfRecSTR, filtered_data[8][filtered_data[8]['numero'].notna() & (filtered_data[8]['numero'] != 0)], on='identifiantstare')

################################ Importation et traitement des données liées au récolement des Tremies ##########################################
print("Importation et traitement des données liées au récolement des Tremies")

dfRecTRM = pd.read_excel(Rec_path, sheet_name="Tremie", header= 1)
Liste_columns = ['Identifiant', 'Récolé (O/N)']
dfRecTRM = dfRecTRM[Liste_columns]
dfRecTRM.columns = ['identifianttremie', 'recolement']
dfRecTRM = pd.merge(dfRecTRM, filtered_data[11], on = 'identifianttremie')

################################ Importation et traitement des données liées au récolement des Arbres de Pluie ##########################################
print("Importation et traitement des données liées au récolement des Arbres de Pluie")

Rec_AP = pd.read_excel(Rec_AP_path, sheet_name="INV_OUV_DATAGL")
Rec_AP = Rec_AP[Rec_AP['Ouvrage'] == 'Arbre de pluie']
Liste_columns = ['Identifiant', 'Ouvrage récolé']
Rec_AP = Rec_AP[Liste_columns]
Rec_AP.columns = ['identifiantogep', 'recolement']
Rec_AP = pd.merge(Rec_AP, filtered_data[5][filtered_data[5]['natureogep'] == 7], on = 'identifiantogep')

################################ Importation et traitement des données relatives aux idées de faire travaux Réseaux ##########################################
print("Importation et traitement des données relatives aux idées de faire travaux Réseaux")

table_dbf = DBF(Idee_res_path)
df_idees = pd.DataFrame(iter(table_dbf))
df_idees_res = pd.merge(df_idees, filtered_data[1], on = 'idcanaexp')

################################ Importation des ratio cout patrimoine ##########################################
print("Importation des ratio cout patrimoine")

df_ratio = pd.read_excel(chemin_ratio, header= 4)
#Valeur patrimoniale à neuf OGEP 
VP_OGEP = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == "Linéaire d’infiltration (noue, tranchées) "].iloc[0] / 1000  #(k€ HT / m3)
#Valeur patrimoniale à neuf STR
VP_STR = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'STR/trémie '].iloc[0]  #(Millions € HT / ouvrage)
#Valeur patrimoniale à neuf tremies
VP_TREM = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'STR/trémie '].iloc[0]  #(Millions € HT / ouvrage)
#Valeur patrimoniale à neuf STEU
VP_STEU = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'STEU '].iloc[0] / 1000 #(k€ HT/EH )
#Valeur patrimoniale à neuf FPR
VP_FPR = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'FPR '].iloc[0] / 1000 #(k€/ouvrage)
#Valeur patrimoniale à neuf branchement
VP_branchement = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Branchement'].iloc[0] / 1000  #(k€ HT/ bchts)
# Valeur patrimoniale à neuf réseau
VP_RV = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Réseau visitable'].iloc[0] / 1000  #(k€ HT /m)
VP_RNV = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Réseau non visitable'].iloc[0] / 1000 #(k€ HT /m)
# Valeur patrimoniale à neuf arbres de pluie
VP_AP = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Arbres de pluie'].iloc[0] / 1000 #(k€ HT / arbre)
#Valeur patrimoniale à neuf puits d'infiltration
VP_PI = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == "puits d’infiltration"].iloc[0] / 1000 #(k€ HT /puits)
#Valeur patrimoniale à neuf Bassins
VP_ENT = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Bassins enterrés'].iloc[0] #(€ HT /m²)
VP_OUV = df_ratio['Ratio choisi'][df_ratio["Type d'ouvrage"] == 'Bassins ciel ouvert '].iloc[0] #(€ HT /m3)

group_column = ''
if app.selected_item1 == 'Commune' : 
    group_column = 'texte'
elif app.selected_item1 == "Systeme Assainissement" :
    group_column = 'nomsteu'
elif app.selected_item1 == 'Bassin versant':
    group_column = 'Bassin'
elif app.selected_item1 == 'Captage':
    group_column = 'nomcaptage'
    
NODATA_PATH = 'L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/Mise_En_Forme/PAS_DE_DONNEES.png'
NivExt = app.selected_item1
typereseau = app.selected_item2
ZG = app.selected_item3

if app.selected_item1 != 'Metropole':
    # Construction de la commande
    my_call = [osgeo4w_bat, python_command, script, type_reseau,
               app.selected_item1, app.selected_item3, group_column]

    # Exécution de la commande
    result = sub.run(my_call, capture_output=True, text=True, shell=True)

    # Vérifiez le code de retour et affichez les sorties
    if result.returncode != 0:
        print("Erreur lors de l'exécution de la commande:")
        print(result.stderr)
    else:
        print("Commande exécutée avec succès:")
        print(result.stdout)

if app.selected_item3 != 'Tous': 
    ####################################### Créer le PDF #############################################################
    file_name = f'Fiche_patrimoine_{app.selected_item2}_{app.selected_item1}_{app.selected_item3}.pdf'
    doc = canvas.Canvas(f'{pdf_file}/{file_name}', pagesize= landscape(A3))

    ###################################### Première page #############################################################
    ##### En-tête
    #Logo
    doc.drawImage(path_logo_metropole , x=10, y= 765, width=180, height=75)
    #date
    doc.setFont(styles['T4'], taille['T4'])
    doc.setFillColor(colors.black)
    doc.drawString(990, 800,f'Date d\'export : {datetime.today().strftime("%d/%m/%Y")}')
    doc.drawString(990, 780,'Source : BD PostGIS "SOCLE"')
    #Titre
    doc.setFont(styles['T1'], taille['T1'])
    
    if app.selected_item2 == 'EP':
        doc.drawString(230, 800,'Patrimoine eaux pluviales')
    else :
        doc.drawString(230, 800,'Patrimoine eaux usées')
    
    doc.setFillColor(palette_basique['Rouge'])
    if group_column:
        doc.drawString(230, 770,  f'{app.selected_item1} : {app.selected_item3}')
    else:
        doc.drawString(230, 770,'Métropole de Lyon')

    ###### Body
    #Niv1 : rect 
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[0])
    else:
        doc.setFillColor(colors_pie[1])
    doc.rect(0, 720, width= 1200, height= 35, fill=1, stroke=0)
    doc.line(600, 700, 600, 30)
    doc.setFont(styles['T2'], taille['T2'])
    doc.setFillColor(colors.white)
    doc.drawString(15, 730,'CONNAITRE LES PATRIMOINES')
    #Niv2 
    ##Collecteurs
    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])
    doc.drawString(15, 700,'COLLECTEURS')
    doc.setFont(styles['T4'], taille['T4'])
    doc.setFillColor(colors.black)
    doc.drawString(15, 680,'Linéaire')
    doc.line(15, 679, 58, 679)  # underline

    doc.setFont(styles['T5'], taille['T5'])
    if app.selected_item2 == 'EU_U':
        text_lines1 = [
            'Réseaux unitaires  (U) :',
            'Réseaux eaux usées (EU) :',
            'Total EU|U :',
            '',
            'Réseaux visitables (>= 1,5m) :',
            'Réseaux non visitables :'
        ]
        text_lines2 = [
            f'{format_number_with_spaces(LRU(filtered_data, group_column, typereseau, ZG)["longueur_sum"][0])} km',
            f'{format_number_with_spaces(LREU(filtered_data, group_column, typereseau, ZG)["longueur_sum"][0])} km',
            f'{format_number_with_spaces(LEP(filtered_data, group_column, ZG)["longueur_sum"][0])} km',
            '',
            f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG)["longueur_visitable"][0]))} km',
            f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG)["longueur_non_visitable"][0]))} km'
        ]
    else:
        text_lines1 = [
            'Réseaux visitables (>= 1,5m) :',
            'Réseaux non visitables :',
            'Total : '
        ]
        text_lines2 = [
            f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG)["longueur_visitable"][0]))} km',
            f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG)["longueur_non_visitable"][0]))} km',
            f'{format_number_with_spaces(LEP(filtered_data, group_column, ZG)["longueur_sum"][0])} km'
        ]
    x, y = 15, 665
    for line in text_lines1:
        doc.drawString(x, y, line)
        y -= n 
    x1, y1 = 200, 665
    for line in text_lines2:
        doc.drawString(x1, y1, line)
        y1-= n 

    y -= n
    y1 -= n
    x2, y2 = 310, 680
    doc.setFont(styles['T4'], taille['T4'])
    doc.drawString(x2, y2,'Précision planimétrique')
    doc.line(x2, y2-1, x2+125, y2-1)
    y2 -= n
    doc.drawImage(LinClass(filtered_data, group_column, palette_couleur_etat_sante_RGBA, out_folder, ZG, NODATA_PATH)[1], x=x2, y= y2-150, width= 250, height= 150)
    y = y2 = y2 - 150 - n
    doc.drawString(x, y,'Matériaux')
    doc.line(x, y-1, x+52, y-1)
    doc.drawString(x2, y2,'Famille de matériaux (Total)')
    doc.line(x2, y2-1, x2+109, y2-1)
    y = y2 = y - n
    doc.drawImage(ConMat(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x, y= y-150, width= 250, height= 150)
    doc.drawImage(LRM(filtered_data, palette_CCD_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x2, y= y2-150, width= 264, height= 150)
    
    y = y2 = y2 - 150 - n
    doc.drawString(x, y,'Année de pose')
    doc.line(x, y-1, 93, y-1)
    doc.drawString(x2, y2,'Tranches d\'âge (Total)')
    doc.line(x2, y2-1, x2+80, y2-1)
    y2 = y = y - n
    doc.drawImage(ConAge(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x, y= y-150, width= 250, height= 150)
    doc.drawImage(AR(filtered_data, palette_age_Res_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x2, y= y2-150, width= 250, height= 150)
    y2 = y = y - 150 - n
    doc.drawString(x, y,'Servitudes')
    doc.line(x, y-1, 72, y-1)
    doc.drawString(x2, y,'Valeur à neuf')
    doc.line(x2, y2-1, x2+69, y2-1)
    
    y1 = y2 = y = y - n
    doc.setFont(styles['T5'], taille['T5'])
    
    text_lines = [
        'Parcelles privées traversées :',
        'Réseaux sur parcelles ou voies', 
        'privées :',
        '',
        'Total à régulariser :',
        'Régularisées :'
    ]
    for line in text_lines:
        doc.drawString(x, y, line)
        y -= n 
    text_lines = [
        f'{format_number_with_spaces(NPPTR(filtered_data, gdf_cad, group_column, ZG)["nbre_parcelle"][0])}',
        '',
        f'{format_number_with_spaces(int(LRSPVP(filtered_data, canexp_cadastre_merged, canexp_voie_merged, group_column, ZG)["lineaire"][0]))} km ({int(LRSPVP(filtered_data, canexp_cadastre_merged, canexp_voie_merged, group_column, ZG)["pourcentage"][0])} %)',
        '',
        '-',
        '-'
    ]
    for line in text_lines:
        doc.drawString(x1, y1, line)
        y1 -= n 
    x3 = 385
    doc.drawString(x2, y2,'Réseaux :')
    doc.drawString(x3, y2,f'{format_number_with_spaces(VPR(filtered_data, group_column, VP_RV, VP_RNV, ZG)["Valeur"][0])} Md{chr(8364)}')

    x3, x3_bis, x4, x4_bis = 615, 790, 900, 1075
    y3, y3_bis, y4 = 700, (700- 2*n), (700- 5*n)
    
    ##Branchement
    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])

    doc.drawString(x3, y3,'BRANCHEMENTS')
    doc.setFont(styles['T4'], taille['T4'])
    doc.setFillColor(colors.black)
    y3 -= 2*n
    doc.drawString(x4, y4,'Nombre')
    doc.line(x4, y4-1, x4+42, y4-1)  # underline
    doc.setFont(styles['T5'], taille['T5'])
    y4 -= n
    doc.drawString(x4, y4,'Total')
    doc.drawString(x4_bis, y4,f'{format_number_with_spaces(int(NBCE(filtered_data, group_column, Ratio, palette_basique_RGBA, out_folder, NivExt, typereseau, ZG, NODATA_PATH)[0]["branch_tot"][0]))}')
    y4 -= 2*n
    doc.setFont(styles['T4'], taille['T4'])
    doc.drawImage(NBCE(filtered_data, group_column, Ratio, palette_basique_RGBA, out_folder, NivExt, typereseau, ZG, NODATA_PATH, niveau='')[1], x=x3, y= y3-150, width= 250, height= 150)
    doc.drawString(x4, y4, 'Valeur à neuf')
    doc.line(x4, y4-1, x4+68, y4-1)
    y4 -= n
    doc.setFont(styles['T5'], taille['T5'])
    doc.drawString(x4, y4, 'Branchements :')
    doc.drawString(x4_bis, y4, f'{format_number_with_spaces(VPBr(filtered_data, group_column, VP_branchement, ZG)["VP_branch"][0])} Md{chr(8364)}')
    y3 = y4 = y3-150- 2*n 
    ##Ouvrages
    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])
    doc.drawString(x3, y3,'OUVRAGES')
    y3 -= 2*n
    doc.setFillColor(colors.black)
    doc.setFont(styles['T4'], taille['T4'])
    doc.drawString(x3_bis, y3,'Nombre')
    doc.line(x3_bis, y3-1, x3_bis+42, y3-1) 
    doc.drawString(x3_bis+72, y3, 'Valeur à neuf')
    doc.line(x3_bis+72, y3-1, x3_bis+140, y3-1)
    doc.drawString(x3_bis+170, y3, 'Récolements')
    doc.line(x3_bis+170, y3-1, x3_bis+237, y3-1)
    doc.drawString(x3_bis+267, y3, 'Foncier à acquérir')
    doc.line(x3_bis+267, y3-1, x3_bis+360, y3-1)
    doc.setFont(styles['T5'], taille['T5'])
    y4 = y3 =  y3 - n

    if app.selected_item2 =='EP':
        text_lines = [
            'Bassins :',
            '    À ciel ouvert :',
            '    Enterrés :',
            'Linéaires d’infiltration :',
            '    Noues (simples/composées) :',
            '    Tranchées (simples/Stockholm) :',
            'Arbres de pluie :',
            'Puits d’infiltration :' ,
            'Dessableurs :',
            'Stations de refoulement (STR) :',
            'Bassins versants connus / ouvrage :'
        ]   
        for line in text_lines:
            doc.drawString(x3, y3, line)
            y3 -= n  
        
        y3 = y4  
        text_lines = [
            f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG)["NBR_BE"].sum())}',
            f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG)[NBEO(filtered_data, group_column, typereseau, ZG)["nature"]=="OUV"]["NBR_BE"][1])}',
            f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG)[NBEO(filtered_data, group_column, typereseau, ZG)["nature"]=="ENT"]["NBR_BE"][0])}',
            f'{format_number_with_spaces(NLi(filtered_data, group_column, typereseau, ZG)["NBR_Li"][0])}',
            f'{format_number_with_spaces(NNo(filtered_data, group_column, typereseau, ZG)["NBR_No"][0])}',
            f'{format_number_with_spaces(NTr(filtered_data, group_column, typereseau, ZG)["NBR_Tr"][0])}',
            f'{format_number_with_spaces(NAP(filtered_data, group_column, typereseau, ZG)["NBR_AP"][0])}' ,
            f'{format_number_with_spaces(NPI(filtered_data, group_column, typereseau, ZG)["NBR_PI"][0])}',
            f'{format_number_with_spaces(NDes(filtered_data, group_column, ZG)["nombre"][0])}',
            f'{format_number_with_spaces(NT(filtered_data, group_column, typereseau, ZG)["nbr_trmie"][0])}',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            y3 -= n  
        y3 = y4  
        text_lines = [
            f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG)["VP_tot"][0])} M{chr(8364)}', 
            f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG)["VP_OUV"][0])} M{chr(8364)}',
            f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG)["VP_ENT"][0])} M{chr(8364)}',
            f'{format_number_with_spaces(VPOGEP(filtered_data, VP_OGEP, group_column, typereseau, ZG)["VP_OGEP"][0])} M{chr(8364)}',
            '-',
            '-',
            f'{format_number_with_spaces(VPAP(filtered_data, group_column, VP_AP, typereseau, ZG)["VAL_AP"][0])} k{chr(8364)}' ,
            f'{format_number_with_spaces(VPPI(filtered_data, group_column, VP_PI, typereseau, ZG)["VAL_PI"][0])} M{chr(8364)}',
            '-',
            f'{format_number_with_spaces(VPTREM(filtered_data, VP_TREM, group_column, typereseau, ZG)["VP_TREM"][0])} M{chr(8364)}',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+72, y3, line)
            y3 -= n  
        y3 = y4
        text_lines = [
            f'{format_number_with_spaces(NBR(dfRecBas, group_column, typereseau, ZG)["nbre_rec"][0])} ({NBR(dfRecBas, group_column, typereseau, ZG)["pourcentage"][0]}%)', 
            '-',
            '-',
            '-',
            '-',
            '-',
            f'{format_number_with_spaces(NAPR(Rec_AP, group_column, typereseau, ZG)["nbre_rec"][0])} ({NAPR(Rec_AP, group_column, typereseau, ZG)["pourcentage"][0]}%)' ,
            '-',
            f'{NDesR(filtered_data, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NDesR(filtered_data, group_column, typereseau, ZG)["pourcentage"][0])} %)',
            f'{format_number_with_spaces(NTR(dfRecTRM, group_column, typereseau, ZG)["nbre_rec"][0])} ({NTR(dfRecTRM, group_column, typereseau, ZG)["pourcentage"][0]}%)',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+170, y3, line)
            y3 -= n  
        y3 = y4 
        text_lines = [
            '--',
            '-',
            '-',
            '-',
            '-',
            '-',
            '-',
            '-',
            '-',
            '--',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+267, y3, line)
            y3 -= n  
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x3, y3,'Année de construction')
        doc.line(x3, y3-1, x3+120, y3-1) 
        y3 = y3-n 
        y4 = y3
        doc.setFont(styles['T5'], taille['T5'])
        doc.drawString(x3, y3,'Bassins')
        doc.drawString(x4, y4,'STR')
        y3 = y3-n
        y4 = y4-n
        doc.drawImage(AgeSTR_EP(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x4, y= y4-150, width= 250, height= 150)
        doc.drawImage(AgeBassins(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x3, y= y3-150, width= 250, height= 150)
        y3 = y3- (150+2*n)
        y4 = y4- (150+2*n)
    else:
        text_lines = [
            'Stations de traitement des eaux',
            'usées (STEU) :',
            'Stations de refoulement (STR) :',
            'Filtres plantés de roseaux (FPR) :',
            'Déversoirs d’orage :',
            'Dessableurs :'
        ]   
        for line in text_lines:
            doc.drawString(x3, y3, line)
            y3 -= n  
        
        y3 = y4 
        text_lines = [
            '',
            f'{NSTEU(filtered_data, group_column, typereseau, ZG)["nombre_STEU"][0]}',
            f'{NSTR(filtered_data, group_column, typereseau, ZG)["nombre_STR"][0]}',
            'Attente évolution BDD ', #f'{NFPR(filtered_data, group_column, typereseau, ZG)["NBR_FPR"][0]}'
            f'{NDO(filtered_data, group_column, typereseau, ZG)["nombre"][0]}',
            f'{NDes(filtered_data, group_column, ZG)["nombre"][0]}'
        ]    
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            y3 -= n  
        
        y3 = y4  
        text_lines = [
            '',
            f'{VPSTEU(filtered_data, VP_STEU, group_column, typereseau, ZG)["VP_STEU"][0]} Md{chr(8364)}', 
            f'{VPSTR(filtered_data, VP_STR, group_column, typereseau, ZG)["VP_STR"][0]} M{chr(8364)}',
            '-', #f'{int(VPFPR(filtered_data, VP_FPR, group_column, typereseau, ZG)["VP_FPR"][0])} K{chr(8364)}'
            '-',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+72, y3, line)
            y3 -= n  
        
        y3 = y4
        text_lines = [
            '',
            'Non calculé',#f'{NSteuR(filtered_data, dossier_FPR, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NSteuR(filtered_data, dossier_FPR, group_column, typereseau, ZG)["pourcentage"][0])} %)',
            f'{NSR(dfRecSTR, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NSR(dfRecSTR, group_column, typereseau, ZG)["pourcentage"][0])} %)',
            '-', #f'{NFR(filtered_data, dossier_FPR, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NFR(filtered_data, dossier_FPR, group_column, typereseau, ZG)["pourcentage"][0])} %)'
            f'{NDOR(filtered_data, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NDOR(filtered_data, group_column, typereseau, ZG)["pourcentage"][0])} %)',
            f'{NDesR(filtered_data, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NDesR(filtered_data, group_column, typereseau, ZG)["pourcentage"][0])} %)'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+170, y3, line)
            y3 -= n  
        
        y3 = y4  
        text_lines = [
            '',
            '-',
            '-',
            '-',
            '-',
            '-'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis+267, y3, line)
            y3 -= n  
        y3 -= 1.5*n  
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x3, y3,'Tranches d\'âge (Total)')
        doc.line(x3, y3-1, x3+120, y3-1) 
        y4 = y3 = y3- n 
        doc.setFont(styles['T5'], taille['T5'])
        doc.drawString(x3, y3,'STEU')
        doc.drawString(x4, y4,'STR')
        y3 = y3-n
        y4 = y4-n 
        doc.drawImage(AgeSTEU(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x3, y= y3-150, width= 250, height= 150)
        doc.drawImage(AgeSTR(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x4, y= y4-150, width= 250, height= 150)
        y3 = y3- (150+2*n)
        y4 = y4- (150+2*n)

    ##RESEAU ELECTRIQUE
    if app.selected_item1 == 'Metropole':
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(x3, y3,'RESEAU ELECTRIQUE')
        y3 -= n
        y4 -= n
        doc.setFillColor(colors.black)
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x3, y3,'Courants')
        doc.line(x3, y3-1, x3+48, y3-1)
        doc.drawString(x4, y4,'Précision planimétrique')
        doc.line(x4, y4-1, x4+125, y4-1)
        y3 -= n
        y4 -= n
        y3_bis, y4_bis = y3, y4
        doc.setFont(styles['T5'], taille['T5'])
        text_lines = [
            'Total :',
            'Haute Tension :',
            'Basse Tension :'
        ]   
        for line in text_lines:
            doc.drawString(x3, y3, line)
            y3 -= n  
        y3 -= n  
        text_lines = [
            f'{format_number_with_spaces(int(LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["total"][0]))} m ({LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["Pourcentage_tot"][0]} %)',
            f'{format_number_with_spaces(int(LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["longueur_Rsec_Fort"][0]))} m ({LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["Pourcentage_Fo"][0]} %)',
            f'{format_number_with_spaces(int(LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["longueur_Rsec_Faible"][0]))} m ({LRSFa(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau)["Pourcentage_Fa"][0]} %)'
        ]   
        for line in text_lines:
            doc.drawString(x3_bis, y3_bis, line)
            y3_bis -= n 
        text_lines = [
            'Classe A (< 50cm) :',
            'Classe B (< 150cm) :',
            'Classe C (>= 150cm) :'
        ]   
        for line in text_lines:
            doc.drawString(x4, y4, line)
            y4 -= n 
        text_lines = [
            f'{format_number_with_spaces(int(LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["longueur_sum_A"][0]))}m ({LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["Pourcentage_A"][0]}%)',
            f'{format_number_with_spaces(int(LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["longueur_sum_B"][0]))}m ({LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["Pourcentage_B"][0]}%)',
            f'{format_number_with_spaces(int(LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["longueur_sum_C"][0]))}m ({LinSecClass(filtered_data, gdf_glob_EU_U, gdf_glob_EP, group_column, typereseau, ZG)["Pourcentage_C"][0]}%)'
        ]   
        for line in text_lines:
            doc.drawString(x4_bis, y4_bis, line)
            y4_bis -= n  

    doc.drawString(1170, 10, '1')       

    # Terminer la première page et commencer une nouvelle page
    doc.showPage()
    ################################### Deuxième page ####################################################################################
    #Niv1 : rect 
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[0])
    else:
        doc.setFillColor(colors_pie[1])
    doc.rect(0, 790, width= 1200, height= 30, fill=1, stroke=0)
    doc.line(600, 780, 600, 210)
    doc.line(600, 155, 600, 15)
    doc.setFont(styles['T2'], taille['T2'])
    doc.setFillColor(colors.white)
    doc.drawString(15, 800,'CONNAITRE L\'ETAT DE SANTE')
    x = 15
    x2, x3, x3_bis, x4 = 310, 615, 615, 900
    y= (800- 2*n)
    ##Collecteurs
    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])
    doc.drawString(x, y,'COLLECTEURS : linéaire inspecté')
    doc.drawString(x3, y,"OUVRAGES : nombre d'inspections (IDCAPTUR)")
    y -= 2*n
    y3 = y
    x += 120+n
    x3_bis += 120+n
    doc.setFont(styles['T4'], taille['T5'])
    doc.setFillColor(colors.black)

    text_lines = [
        'Total',
        'Inspecté',
        'Total inspecté',
        'Pourcentage par rapport' 
    ]   
    for line in text_lines:
        doc.drawString(x, y, line)
        x += 100 

    text_lines = [
        'Nombre',
        'Inspecté',
        'Total inspecté',
        'Pourcentage par rapport' 
    ]   
    for line in text_lines:
        doc.drawString(x3_bis, y3, line)
        x3_bis += 100 

    x= 135+n
    x3_bis = 735+n
    y -= n
    y3 -= n

    text_lines = [
        'réseaux',
        "dans l'année",
        'depuis 1990',
        'au total réseaux' 
    ]   
    for line in text_lines:
        doc.drawString(x, y, line)
        x += 100 

    text_lines = [
        'total',
        "dans l'année",
        'depuis 1990',
        'au total ouvrages' 
    ]   
    for line in text_lines:
        doc.drawString(x3_bis, y3, line)
        x3_bis += 100 
    x= 15
    x3_bis = 615
    y -= n
    y3 -= n
    doc.setFont(styles['T4'], taille['T4'])
    doc.drawString(x, y,'Réseaux non visitables')
    doc.line(x, y-1, x+120, y-1)  
    y -= 2*n
    doc.drawString(x, y,'Réseaux visitables')
    doc.line(x, y-1, x+98, y-1)  
    doc.setFont(styles['T5'], taille['T5'])
    if app.selected_item2 == 'EP':
        text_lines=[
            'Bassins :', 
            'Linéaires d’infiltration (km) :',
            'Arbres de pluie :',
            'Puits d’infiltration :',
            'STR :'
        ]
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG)["NBR_BE"].sum()),
            '-',
            format_number_with_spaces(NAP(filtered_data, group_column, typereseau, ZG)["NBR_AP"][0]),
            format_number_with_spaces(NPI(filtered_data, group_column, typereseau, ZG)["NBR_PI"][0]),
            format_number_with_spaces(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin)["Total"][0])
        ]
        for line in text_lines:
            doc.drawString(x3_bis+135, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            '-',
            format_number_with_spaces(LLI(gdf_drain, current_year, group_column, typereseau, ZG)["longueur"][0]),
            '-',
            '-',
            format_number_with_spaces(int(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin)["Realise_dans_lannee"][0]))
        ]
        for line in text_lines:
            doc.drawString(x3_bis+235, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            '-',
            format_number_with_spaces(int(LLI(gdf_drain, current_year, group_column, typereseau, ZG)["longueur_cum"][0])),
            '-',
            '-',
            format_number_with_spaces(int(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin)["Total_cumule"][0]))
        ]
        for line in text_lines:
            doc.drawString(x3_bis+335, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            '-',
            f'{LLI(gdf_drain, current_year, group_column, typereseau, ZG)["Pourcentage"][0]} %',
            '-',
            '-',
            f'{NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin)["Pourcentage_ins"][0]} %'
        ]
        for line in text_lines:
            doc.drawString(x3_bis+435, y3, line)
            y3 -= n
    else:
        text_lines=[
            'STEU',
            'STR',
            'FPR'
        ]
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            format_number_with_spaces(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG)["Total"][0]),
            format_number_with_spaces(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG)["Total"][0]),
            'Attente évolution BDD'#format_number_with_spaces(NFPR(filtered_data, group_column, typereseau, ZG)["NBR_FPR"][0])
        ]
        for line in text_lines:
            doc.drawString(x3_bis+135, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            format_number_with_spaces(int(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG)["Realise_dans_lannee"][0])),
            format_number_with_spaces(int(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG)["Realise_dans_lannee"][0])),
            '-'
        ]
        for line in text_lines:
            doc.drawString(x3_bis+235, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            format_number_with_spaces(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG)["Total_cumule"][0]),
            format_number_with_spaces(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG)["Total_cumule"][0]),
            '-'
        ]
        for line in text_lines:
            doc.drawString(x3_bis+335, y3, line)
            y3 -= n
        y3 += (len(text_lines)*n)
        text_lines= [
            f'{int(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG)["Pourcentage_ins"][0])} %',
            f'{int(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG)["Pourcentage_ins"][0])} %',
            '-'
        ]
        for line in text_lines:
            doc.drawString(x3_bis+435, y3, line)
            y3 -= n
    y += n
    doc.drawString(x, y,'itv (km)')
    x += 120+n
    text_lines = [
        format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG)["Total"][0])),
        format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG)["Realise_dans_lannee"][0])),
        format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG)["Total_cumule"][0])),
        f'{format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG)["Pourcentage_itv"][0]))} %'
    ]   
    for line in text_lines:
        doc.drawString(x, y, line)
        x += 100
    y -= 2*n
    x = 15
    doc.drawString(x, y,'Prédiagnostics (km)')
    x += 120+n
    dff_prediag = LDiagPrediag(df_vis, group_column,None,'PREDIAG')
    text_lines = [
        format_number_with_spaces(dff_prediag['Total'].values[0]),
        format_number_with_spaces(dff_prediag['Realise_dans_lannee'].values[0]),
        format_number_with_spaces(dff_prediag['Total_cumule'].values[0]),
        format_number_with_spaces(dff_prediag['Pourcentage'].values[0]) 
    ]   
    for line in text_lines:
        doc.drawString(x, y, line)
        x += 100
    y -= n
    x = 15
    doc.drawString(x, y,'Diagnostics (km)')
    x += 120+n
    dff_diag = LDiagPrediag(df_vis, group_column,None,'DIAGNOSTIC')
    text_lines = [
        format_number_with_spaces(dff_diag['Total'].values[0]),
        format_number_with_spaces(dff_diag['Realise_dans_lannee'].values[0]),
        format_number_with_spaces(dff_diag['Total_cumule'].values[0]),
        format_number_with_spaces(dff_diag['Pourcentage'].values[0]) 
    ]     
    for line in text_lines:
        doc.drawString(x, y, line)
        x += 100

    x= 15
    y -= 4*n
    ##ETAT DE SANTÉ  STRUCTUREL ET FONCTIONNEL
    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])
    doc.drawString(x, y,'COLLECTEURS : état de santé')
    doc.drawString(x3, y,'OUVRAGES : état de santé (IDCAPTUR)')
    y -= n
    doc.setFillColor(colors.black)
    doc.setFont(styles['T4'], taille['T4'])
    doc.drawString(x, y,'Réseaux non visitables (INDIGAU)')
    doc.line(x, y-1, x+120, y-1)  
    doc.drawString(x2, y,'Réseaux visitables (AGEC)')
    doc.line(x2, y-1, x2+98, y-1)  
    if app.selected_item2 == 'EP':
        doc.drawString(x3, y,'Bassins')
        doc.line(x3, y-1, x3+40, y-1)  
    else :
        doc.drawString(x3, y,'STEU')
        doc.line(x3, y-1, x3+28, y-1)  

    doc.drawString(x4, y,'STR')
    doc.line(x4, y-1, x4+22, y-1)  
    y -= n
    doc.drawImage(ES_NV(filtered_data, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x, y= y-150, width= 250, height= 150)
    doc.drawImage(ES_V(df_vis, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x2, y= y-150, width= 250, height= 150)
    if app.selected_item2 == 'EP':
        doc.setFont(styles['T5'], taille['T5'])
        doc.drawString(x3+50, y-90, 'PAS DE DONNEES CONNUES')
        doc.drawImage(ES_STR_EP(filtered_data, df_tremies, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x4, y= y-150, width= 250, height= 150)
    else :
        doc.drawImage(ES_STEU(filtered_data, df_steu, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x3, y= y-150, width= 250, height= 150)
        doc.drawImage(ES_STR(filtered_data, df_str, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x4, y= y-150, width= 250, height= 150)
    y -= (150+2*n)

    doc.setFont(styles['T3'], taille['T3'])
    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[1])
    else:
        doc.setFillColor(colors_pie[0])
    doc.drawString(x, y,'BRANCHEMENTS : état de santé')
    doc.setFillColor(colors.black)
    doc.setFont(styles['T4'], taille['T4'])
    if app.selected_item2 == 'EP':
        doc.drawString(x3, y,'Linéaires d’infiltration')
        doc.line(x3, y-1, x3+115, y-1)  
        doc.drawString(x4, y,'Puits d’infiltration')
        doc.line(x4, y-1, x4+95, y-1)  
    else :
        doc.drawString(x3, y,'FPR')
        doc.line(x3, y-1, x3+22, y-1)  
    y -= (75+n)
    doc.setFont(styles['T5'], taille['T5'])
    doc.drawString(x+230, y, 'PAS DE DONNEES CONNUES')
    doc.drawString(x3+50, y, 'PAS DE DONNEES CONNUES')
    doc.drawString(x4+50, y, 'PAS DE DONNEES CONNUES')
    y -= (100+n)

    if app.selected_item2 == 'EP':
        doc.setFillColor(colors_pie[0])
    else:
        doc.setFillColor(colors_pie[1])
    
    ##PROGRAMME DE TRAVAUX
    doc.rect(0, y, width= 1200, height= 30, fill=1, stroke=0)
    doc.setFont(styles['T2'], taille['T2'])
    doc.setFillColor(colors.white)
    doc.drawString(15, y+10,'PROGRAMME DE TRAVAUX')
    y -= (n+3)
    doc.setFont(styles['T3'], taille['T3'])
    doc.setFillColor(colors_pie[1])
    doc.drawString(x, y,'COLLECTEURS')
    doc.drawString(x2, y,'BRANCHEMENTS')
    doc.drawString(x3, y,'OUVRAGES (IDCAPTUR)')
    y -= 2*n
    y3 = y
    doc.setFont(styles['T5'], taille['T5'])
    doc.setFillColor(colors.black)
    doc.drawString(x2, y,'Branchements réhabilités :')
    doc.drawString(x2+200, y,'-')

    text_lines = [
        "Total d'idées de faire travaux Réseaux :",
        'Total réhabilité (remplacement, gainage,',
        'réparation) :',
        'Extension annuelle (année en cours) :' ,
        'Taux annuel de renouvellement :'
    ]  
    text_lines1 = [
        f'{format_number_with_spaces(int(LRIT(df_idees_res, group_column, ZG)["long_idee"][0]))}km',
        '',
        '-',
        '-',
        '-' 
    ] 
    for index in range(len(text_lines)):
        doc.drawString(x, y, text_lines[index])
        doc.drawString(x+200, y, text_lines1[index])
        y -= n
    if app.selected_item2 == 'EP':
        text_lines = [
            "Ouvrages EP créés :",
            '',
            'Bassins réhabilités :',
            "linéaires d'infiltration réhabilités :" ,
            'STR réhabilitées :']  
        text_lines1 = [
            '-',
            '',
            '-',
            '-',
            '-' ] 
        for index in range(len(text_lines)):
            doc.drawString(x3, y3, text_lines[index])
            doc.drawString(x3+200, y3, text_lines1[index])
            y3 -= n
    else :
        text_lines = [
            "STEU réhabilitées :",
            'STR  réhabilitées :',
            'FPR réhabilités :']  
        text_lines1 = [
            '-',
            '-',
            '-'] 
        for index in range(len(text_lines)):
            doc.drawString(x3, y3, text_lines[index])
            doc.drawString(x3+200, y3, text_lines1[index])
            y3 -= n

    doc.drawString(1170, 10, '2') 
    
    #Page 3
    if app.selected_item1 != 'Metropole':
        path_atlas = f'{dic_path_atlas[app.selected_item1]}{app.selected_item3}.png'
        if os.path.exists(path_atlas):
            doc.showPage()
            doc.drawImage(path_atlas, x=0* cm, y= 0* cm, width= 42* cm, height= 29.7* cm)
    ################################### Exportation du fichier pdf #######################################################################
    doc.save()
    os.startfile(f'{pdf_file}/{file_name}')
    print(f"Le fichier PDF '{file_name}' a été généré avec succès.")
else:
    dic_niveau_extraction={
        "Commune": ['ALBIGNY-SUR-SAONE', 'BRON', 'CAILLOUX-SUR-FONTAINES', 'CALUIRE ET CUIRE', 'CHAMPAGNE-AU-MONT-D OR', 'CHARBONNIERES-LES-BAINS', 'CHARLY', 'CHASSIEU', 'COLLONGES-AU-MONT- D OR', 'CORBAS', 'COUZON-AU-MONT-D OR', 'CRAPONNE', 'CURIS-AU-MONT-D OR', 'DARDILLY', 'DECINES CHARPIEU', 'ECULLY', 'FEYZIN', 'FLEURIEU-SUR-SAONE', 'FONTAINES SUR SAONE', 'FONTAINES-SAINT-MARTIN', 'FRANCHEVILLE', 'GENAY', 'GIVORS', 'GRIGNY', 'IRIGNY', 'JONAGE', 'LA MULATIERE', 'LA TOUR DE SALVAGNY', 'LIMONEST', 'LISSIEU', 'LYON 1ER', 'LYON 2EME', 'LYON 3EME', 'LYON 4EME', 'LYON 5EME', 'LYON 6EME', 'LYON 7EME', 'LYON 8EME', 'LYON 9EME', 'MARCY-L ETOILE', 'MEYZIEU', 'MIONS', 'MONTANAY', 'NEUVILLE SUR SAONE', 'OULLINS', 'PIERRE BENITE', 'POLEYMIEUX-AU-MONT-D OR', 'QUINCIEUX', 'RILLIEUX LA PAPE', 'ROCHETAILLEE-SUR-SAONE', 'SAINT CYR AU MONT D OR', 'SAINT-DIDIER-AU-MONT-D OR', 'SAINT-GENIS-LAVAL', 'SAINT-GENIS-LES-OLLIERES', 'SAINT-GERMAIN-AU-MONT-D OR', 'SAINT-ROMAIN-AU-MONT-D OR', 'SAINTE FOY LES LYON', 'SATHONAY-CAMP', 'SATHONAY-VILLAGE', 'SOLAIZE', 'ST FONS', 'ST PRIEST', 'TASSIN-LA-DEMI-LUNE', 'VAULX EN VELIN', 'VENISSIEUX', 'VERNAISON', 'VILLEURBANNE'],
        "Systeme Assainissement": ['Genay', 'Fontaines', 'Jonage', 'La Feyssine', 'Lissieu Le Bourg', 'Lissieu Sémanet', 'Meyzieu', 'Neuville', 'Pierre-Bénite', 'Quincieux', 'Saint-Fons', 'Saint-Germain'],
        "Bassin versant": ['GENAY, GRAND RIEUX', 'MAINTENU, COMBE, GRANDES GORGES', 'THOU', 'TORRIERES', 'ECHETS', 'VOSGES', 'RAVIN', 'PINAY DE ST ROMAIN, MONGELAS, CHARBOTTES', 'ROCHECARDON', 'SEMANET, GORGES, MALIGNEUX, AZERGUES', 'PLANCHES', 'MOUCHE', 'VERNIERES, FEE DES EAUX, SERVE, RAZAT', 'YZERON', 'GUA, EPI, RIZAN, BLETTA', 'GARON', 'CHANAUX', 'OZON', 'GIER', 'MERDARY, COMBE DE BANS', 'SAONE', 'RHONE', 'RIZE', 'RATAPON'],
        "Captage": ['Captage de Chassieu', 'Captage de Corbas Romanettes', 'Captage de Crépieux-Charmy', 'Captage de Curis', 'Captage de Dècines', 'Captage de Fleurieu', 'Captage de Jonage', 'Captage de Meyzieu', 'Captage de Mions', 'Captage de St Priest', 'Lac de Miribel-Jonage']
    }
    for element in dic_niveau_extraction[app.selected_item1]:
        ####################################### Créer le PDF #############################################################
        file_name = f'Fiche_patrimoine_{app.selected_item2}_{app.selected_item1}_{element}.pdf'
        doc = canvas.Canvas(f'{pdf_file}/{file_name}', pagesize= landscape(A3))

        ###################################### Première page #############################################################
        ##### En-tête
        #Logo
        doc.drawImage(path_logo_metropole, x=10, y= 765, width=180, height=75)
        #date
        doc.setFont(styles['T4'], taille['T4'])
        doc.setFillColor(colors.black)
        doc.drawString(990, 800,f'Date d\'export : {datetime.today().strftime("%d/%m/%Y")}')
        doc.drawString(990, 780,'Source : BD PostGIS "SOCLE"')
        #Titre
        doc.setFont(styles['T1'], taille['T1'])
        if app.selected_item2 == 'EP':
            doc.drawString(230, 800,'Patrimoine eaux pluviales')
        else : 
            doc.drawString(230, 800,'Patrimoine eaux usées')
        doc.setFillColor(palette_basique['Rouge'])
        doc.drawString(230, 770, f'{app.selected_item1} : {element}')
        ###### Body
        #Niv1 : rect 
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[0])
        else:
            doc.setFillColor(colors_pie[1])
        doc.rect(0, 720, width= 1200, height= 35, fill=1, stroke=0)
        doc.line(600, 700, 600, 30)
        doc.setFont(styles['T2'], taille['T2'])
        doc.setFillColor(colors.white)
        doc.drawString(15, 730,'CONNAITRE LES PATRIMOINES')
        #Niv2 
        ##Collecteurs
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(15, 700,'COLLECTEURS')
        doc.setFont(styles['T4'], taille['T4'])
        doc.setFillColor(colors.black)
        doc.drawString(15, 680,'Linéaire')
        doc.line(15, 679, 58, 679)  # underline

        doc.setFont(styles['T5'], taille['T5'])
        if app.selected_item2 == 'EU_U':
            text_lines1 = [
                'Réseaux unitaires  (U) :',
                'Réseaux eaux usées (EU) :',
                'Total EU|U :',
                '',
                'Réseaux visitables (>= 1,5m) :',
                'Réseaux non visitables :'
            ]
            text_lines2 = [
                f'{format_number_with_spaces(LRU(filtered_data, group_column, typereseau, ZG, niveau= element )["longueur_sum"][0])} km',
                f'{format_number_with_spaces(LREU(filtered_data, group_column, typereseau, ZG, niveau= element)["longueur_sum"][0])} km',
                f'{format_number_with_spaces(LEP(filtered_data, group_column, ZG, niveau= element)["longueur_sum"][0])} km',
                '',
                f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG, niveau= element)["longueur_visitable"][0]))} km',
                f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG, niveau= element)["longueur_non_visitable"][0]))} km'
            ]
        else:
            text_lines1 = [
                'Réseaux visitables (>= 1,5m) :',
                'Réseaux non visitables :',
                'Total : '
            ]
            text_lines2 = [
                f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG, niveau= element)["longueur_visitable"][0]))} km',
                f'{format_number_with_spaces(int(LRV(filtered_data, group_column, ZG, niveau= element)["longueur_non_visitable"][0]))} km',
                f'{format_number_with_spaces(LEP(filtered_data, group_column, ZG, niveau= element)["longueur_sum"][0])} km'
            ]
        x, y = 15, 665
        for line in text_lines1:
            doc.drawString(x, y, line)
            y -= n 
        x1, y1 = 200, 665
        for line in text_lines2:
            doc.drawString(x1, y1, line)
            y1-= n 

        y -= n
        y1 -= n
        x2, y2 = 310, 680
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x2, y2,'Précision planimétrique')
        doc.line(x2, y2-1, x2+125, y2-1)
        y2 -= n
        doc.drawImage(LinClass(filtered_data, group_column, palette_couleur_etat_sante_RGBA, out_folder, ZG, NODATA_PATH, niveau= element)[1], x=x2, y= y2-150, width= 250, height= 150)
        y = y2 = y2 - 150 - n
        doc.drawString(x, y,'Matériaux')
        doc.line(x, y-1, x+52, y-1)
        doc.drawString(x2, y2,'Famille de matériaux (Total)')
        doc.line(x2, y2-1, x2+109, y2-1)
        y = y2 = y - n
        doc.drawImage(ConMat(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau =element)[1], x=x, y= y-150, width= 250, height= 150)
        doc.drawImage(LRM(filtered_data, palette_CCD_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[1], x=x2, y= y2-150, width= 264, height= 150)
        
        y = y2 = y2 - 150 - n
        doc.drawString(x, y,'Année de pose')
        doc.line(x, y-1, 93, y-1)
        doc.drawString(x2, y2,'Tranches d\'âge (Total)')
        doc.line(x2, y2-1, x2+80, y2-1)
        y2 = y = y - n
        doc.drawImage(ConAge(filtered_data, palette_basique_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[1], x=x, y= y-150, width= 250, height= 150)
        doc.drawImage(AR(filtered_data, palette_age_Res_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[1], x=x2, y= y2-150, width= 250, height= 150)
        y2 = y = y - 150 - n
        doc.drawString(x, y,'Servitudes')
        doc.line(x, y-1, 72, y-1)
        doc.drawString(x2, y,'Valeur à neuf')
        doc.line(x2, y2-1, x2+69, y2-1)
        
        y1 = y2 = y = y - n
        doc.setFont(styles['T5'], taille['T5'])
        
        text_lines = [
            'Parcelles privées traversées :',
            'Réseaux sur parcelles ou voies', 
            'privées :',
            '',
            'Total à régulariser :',
            'Régularisées :'
        ]
        for line in text_lines:
            doc.drawString(x, y, line)
            y -= n 
        text_lines = [
            f'{format_number_with_spaces(NPPTR(filtered_data, gdf_cad, group_column, ZG, niveau = element)["nbre_parcelle"][0])}',
            '',
            f'{format_number_with_spaces(int(LRSPVP(filtered_data, canexp_cadastre_merged, canexp_voie_merged, group_column, ZG, niveau = element)["lineaire"][0]))} km ({int(LRSPVP(filtered_data, canexp_cadastre_merged, canexp_voie_merged, group_column, ZG, niveau = element)["pourcentage"][0])} %)',
            '',
            '-',
            '-'
        ]
        for line in text_lines:
            doc.drawString(x1, y1, line)
            y1 -= n 
        x3 = 385
        doc.drawString(x2, y2,'Réseaux :')
        doc.drawString(x3, y2,f'{format_number_with_spaces(VPR(filtered_data, group_column, VP_RV, VP_RNV, ZG, niveau = element)["Valeur"][0])} Md{chr(8364)}')

        x3, x3_bis, x4, x4_bis = 615, 790, 900, 1075
        y3, y3_bis, y4 = 700, (700- 2*n), (700- 5*n)
        
        ##Branchement
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])

        doc.drawString(x3, y3,'BRANCHEMENTS')
        doc.setFont(styles['T4'], taille['T4'])
        doc.setFillColor(colors.black)
        y3 -= 2*n
        doc.drawString(x4, y4,'Nombre')
        doc.line(x4, y4-1, x4+42, y4-1)  # underline
        doc.setFont(styles['T5'], taille['T5'])
        y4 -= n
        doc.drawString(x4, y4,'Total')
        doc.drawString(x4_bis, y4,f'{format_number_with_spaces(int(NBCE(filtered_data, group_column, Ratio, palette_basique_RGBA, out_folder, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[0]["branch_tot"][0]))}')
        y4 -= 2*n
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawImage(NBCE(filtered_data, group_column, Ratio, palette_basique_RGBA, out_folder, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x3, y= y3-150, width= 250, height= 150)
        doc.drawString(x4, y4, 'Valeur à neuf')
        doc.line(x4, y4-1, x4+68, y4-1)
        y4 -= n
        doc.setFont(styles['T5'], taille['T5'])
        doc.drawString(x4, y4, 'Branchements :')
        doc.drawString(x4_bis, y4, f'{format_number_with_spaces(VPBr(filtered_data, group_column, VP_branchement, ZG, niveau= element)["VP_branch"][0])} Md{chr(8364)}')
        y3 = y4 = y3-150- 2*n 
        ##Ouvrages
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(x3, y3,'OUVRAGES')
        y3 -= 2*n
        doc.setFillColor(colors.black)
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x3_bis, y3,'Nombre')
        doc.line(x3_bis, y3-1, x3_bis+42, y3-1) 
        doc.drawString(x3_bis+72, y3, 'Valeur à neuf')
        doc.line(x3_bis+72, y3-1, x3_bis+140, y3-1)
        doc.drawString(x3_bis+170, y3, 'Récolements')
        doc.line(x3_bis+170, y3-1, x3_bis+237, y3-1)
        doc.drawString(x3_bis+267, y3, 'Foncier à acquérir')
        doc.line(x3_bis+267, y3-1, x3_bis+360, y3-1)
        doc.setFont(styles['T5'], taille['T5'])
        y4 = y3 =  y3 - n

        if app.selected_item2 =='EP':
            text_lines = [
                'Bassins :',
                '    À ciel ouvert :',
                '    Enterrés :',
                'Linéaires d’infiltration :',
                '    Noues (simples/composées) :',
                '    Tranchées (simples/Stockholm) :',
                'Arbres de pluie :',
                'Puits d’infiltration :' ,
                'Dessableurs :',
                'Stations de refoulement (STR) :',
                'Bassins versants connus / ouvrage :'
            ]   
            for line in text_lines:
                doc.drawString(x3, y3, line)
                y3 -= n  
            
            y3 = y4  
            text_lines = [
                f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_BE"].sum())}',
                f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)[NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)["nature"]=="OUV"]["NBR_BE"][1])}',
                f'{format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)[NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)["nature"]=="ENT"]["NBR_BE"][0])}',
                f'{format_number_with_spaces(NLi(filtered_data, group_column, typereseau, ZG, niveau = element)["NBR_Li"][0])}',
                f'{format_number_with_spaces(NNo(filtered_data, group_column, typereseau, ZG, niveau = element)["NBR_No"][0])}',
                f'{format_number_with_spaces(NTr(filtered_data, group_column, typereseau, ZG, niveau = element)["NBR_Tr"][0])}',
                f'{format_number_with_spaces(NAP(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_AP"][0])}' ,
                f'{format_number_with_spaces(NPI(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_PI"][0])}',
                f'{format_number_with_spaces(NDes(filtered_data, group_column, ZG, niveau= element)["nombre"][0])}',
                f'{format_number_with_spaces(NT(filtered_data, group_column, typereseau, ZG, niveau= element)["nbr_trmie"][0])}',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis, y3, line)
                y3 -= n  
            y3 = y4  
            text_lines = [
                f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG, niveau= element)["VP_tot"][0])} M{chr(8364)}', 
                f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG, niveau= element)["VP_OUV"][0])} M{chr(8364)}',
                f'{format_number_with_spaces(VPBassins(filtered_data, VP_OUV, VP_ENT, group_column, typereseau, ZG, niveau= element)["VP_ENT"][0])} M{chr(8364)}',
                f'{format_number_with_spaces(VPOGEP(filtered_data, VP_OGEP, group_column, typereseau, ZG, niveau= element)["VP_OGEP"][0])} M{chr(8364)}',
                '-',
                '-',
                f'{format_number_with_spaces(VPAP(filtered_data, group_column, VP_AP, typereseau, ZG, niveau= element)["VAL_AP"][0])} k{chr(8364)}' ,
                f'{format_number_with_spaces(VPPI(filtered_data, group_column, VP_PI, typereseau, ZG, niveau= element)["VAL_PI"][0])} M{chr(8364)}',
                '-',
                f'{format_number_with_spaces(VPTREM(filtered_data, VP_TREM, group_column, typereseau, ZG, niveau= element)["VP_TREM"][0])} M{chr(8364)}',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+72, y3, line)
                y3 -= n  
            y3 = y4
            text_lines = [
                f'{format_number_with_spaces(NBR(dfRecBas, group_column, typereseau, ZG, niveau= element)["nbre_rec"][0])} ({NBR(dfRecBas, group_column, typereseau, ZG, niveau= element)["pourcentage"][0]}%)', 
                '-',
                '-',
                '-',
                '-',
                '-',
                f'{format_number_with_spaces(NAPR(Rec_AP, group_column, typereseau, ZG, niveau= element)["nbre_rec"][0])} ({NAPR(Rec_AP, group_column, typereseau, ZG, niveau= element)["pourcentage"][0]}%)' ,
                '-',
                f'{NDesR(filtered_data, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NDesR(filtered_data, group_column, typereseau, ZG)["pourcentage"][0])} %)',
                f'{format_number_with_spaces(NTR(dfRecTRM, group_column, typereseau, ZG, niveau= element)["nbre_rec"][0])} ({NTR(dfRecTRM, group_column, typereseau, ZG, niveau= element)["pourcentage"][0]}%)',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+170, y3, line)
                y3 -= n  
            y3 = y4  
            text_lines = [
                '--',
                '-',
                '-',
                '-',
                '-',
                '-',
                '-',
                '-',
                '-',
                '--',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+267, y3, line)
                y3 -= n  
            
            y3 -= n   
            doc.setFont(styles['T4'], taille['T4'])
            doc.drawString(x3, y3,'Année de construction')
            doc.line(x3, y3-1, x3+120, y3-1) 
            y3 = y3-n 
            y4 = y3
            doc.setFont(styles['T5'], taille['T5'])
            doc.drawString(x3, y3,'Bassins')
            doc.drawString(x4, y4,'STR')
            y3 = y3-n
            y4 = y4-n
            doc.drawImage(AgeSTR_EP(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[1], x=x4, y= y4-150, width= 250, height= 150)
            doc.drawImage(AgeBassins(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau = element)[1], x=x3, y= y3-150, width= 250, height= 150)
            y3 = y3- (150+2*n)
            y4 = y4- (150+2*n)
        else:
            text_lines = [
                'Stations de traitement des eaux',
                'usées (STEU) :',
                'Stations de refoulement (STR) :',
                'Filtres plantés de roseaux (FPR) :',
                'Déversoirs d’orage :',
                'Dessableurs :'
            ]   
            for line in text_lines:
                doc.drawString(x3, y3, line)
                y3 -= n  
            
            y3 = y4 
            text_lines = [
                '',
                f'{NSTEU(filtered_data, group_column, typereseau, ZG, niveau = element)["nombre_STEU"][0]}',
                f'{NSTR(filtered_data, group_column, typereseau, ZG, niveau = element)["nombre_STR"][0]}',
                'Attente évolution BDD'#f'{NFPR(filtered_data, group_column, typereseau, ZG, niveau = element)["NBR_FPR"][0]}',
                f'{NDO(filtered_data, group_column, typereseau, ZG, niveau = element)["nombre"][0]}',
                f'{NDes(filtered_data, group_column, ZG, niveau = element)["nombre"][0]}'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis, y3, line)
                y3 -= n  
            
            y3 = y4  
            text_lines = [
                '',
                f'{VPSTEU(filtered_data, VP_STEU, group_column, typereseau, ZG, niveau = element)["VP_STEU"][0]} Md{chr(8364)}', 
                f'{VPSTR(filtered_data, VP_STR, group_column, typereseau, ZG, niveau = element)["VP_STR"][0]} M{chr(8364)}',
                '', #f'{int(VPFPR(filtered_data, VP_FPR, group_column, typereseau, ZG, niveau = element)["VP_FPR"][0])} K{chr(8364)}'
                '-',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+72, y3, line)
                y3 -= n  
            
            y3 = y4
            text_lines = [
                '',
                '-',
                f'{NSR(dfRecSTR, group_column, typereseau, ZG, niveau = element)["nbre_rec"][0]} ({int(NSR(dfRecSTR, group_column, typereseau, ZG, niveau = element)["pourcentage"][0])} %)',
                '',#f'{NFR(filtered_data, dossier_FPR, group_column, typereseau, ZG, niveau = element)["nbre_rec"][0]} ({int(NFR(filtered_data, dossier_FPR, group_column, typereseau, ZG, niveau = element)["pourcentage"][0])} %)',
                f'{NDOR(filtered_data, group_column, typereseau, ZG, niveau = element)["nbre_rec"][0]} ({int(NDOR(filtered_data, group_column, typereseau, ZG, niveau = element)["pourcentage"][0])} %)',
                f'{NDesR(filtered_data, group_column, typereseau, ZG)["nbre_rec"][0]} ({int(NDesR(filtered_data, group_column, typereseau, ZG)["pourcentage"][0])} %)'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+170, y3, line)
                y3 -= n  
            
            y3 = y4  
            text_lines = [
                '',
                '-',
                '-',
                '-',
                '-',
                '-'
            ]   
            for line in text_lines:
                doc.drawString(x3_bis+267, y3, line)
                y3 -= n  
            y3 -= 1.5*n  
            doc.setFont(styles['T4'], taille['T4'])
            doc.drawString(x3, y3,'Tranches d\'âge (Total)')
            doc.line(x3, y3-1, x3+120, y3-1) 
            y4 = y3 = y3- n 
            doc.setFont(styles['T5'], taille['T5'])
            doc.drawString(x3, y3,'STEU')
            doc.drawString(x4, y4,'STR')
            y3 = y3-n
            y4 = y4-n 
            doc.drawImage(AgeSTEU(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x3, y= y3-150, width= 250, height= 150)
            doc.drawImage(AgeSTR(filtered_data, palette_age_ouvrage_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x4, y= y4-150, width= 250, height= 150)
            y3 = y3- (150+2*n)
            y4 = y4- (150+2*n)

        doc.drawString(1170, 10, '1')       

        # Terminer la première page et commencer une nouvelle page
        doc.showPage()
        ################################### Deuxième page ####################################################################################
        #Niv1 : rect 
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[0])
        else:
            doc.setFillColor(colors_pie[1])
        doc.rect(0, 790, width= 1200, height= 30, fill=1, stroke=0)
        doc.line(600, 780, 600, 210)
        doc.line(600, 155, 600, 15)
        doc.setFont(styles['T2'], taille['T2'])
        doc.setFillColor(colors.white)
        doc.drawString(15, 800,'CONNAITRE L\'ETAT DE SANTE')
        x = 15
        x2, x3, x3_bis, x4 = 310, 615, 615, 900
        y= (800- 2*n)
        ##Collecteurs
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(x, y,'COLLECTEURS : linéaire inspecté')
        doc.drawString(x3, y,"OUVRAGES : nombre d'inspections (IDCAPTUR)")
        y -= 2*n
        y3 = y
        x += 120+n
        x3_bis += 120+n
        doc.setFont(styles['T4'], taille['T5'])
        doc.setFillColor(colors.black)

        text_lines = [
            'Total',
            'Inspecté',
            'Total inspecté',
            'Pourcentage par rapport' 
        ]   
        for line in text_lines:
            doc.drawString(x, y, line)
            x += 100 

        text_lines = [
            'Nombre',
            'Inspecté',
            'Total inspecté',
            'Pourcentage par rapport' 
        ]   
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            x3_bis += 100 

        x= 135+n
        x3_bis = 735+n
        y -= n
        y3 -= n

        text_lines = [
            'réseaux',
            "dans l'année",
            'depuis 1990',
            'au total réseaux' 
        ]   
        for line in text_lines:
            doc.drawString(x, y, line)
            x += 100 

        text_lines = [
            'total',
            "dans l'année",
            'depuis 1990',
            'au total ouvrages' 
        ]   
        for line in text_lines:
            doc.drawString(x3_bis, y3, line)
            x3_bis += 100 
        x= 15
        x3_bis = 615
        y -= n
        y3 -= n
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x, y,'Réseaux non visitables')
        doc.line(x, y-1, x+120, y-1)  
        y -= 2*n
        doc.drawString(x, y,'Réseaux visitables')
        doc.line(x, y-1, x+98, y-1)  
        doc.setFont(styles['T5'], taille['T5'])
        if app.selected_item2 == 'EP':
            text_lines=[
                'Bassins :', 
                'Linéaires d’infiltration (km) :',
                'Arbres de pluie :',
                'Puits d’infiltration :',
                'STR :'
            ]
            for line in text_lines:
                doc.drawString(x3_bis, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                format_number_with_spaces(NBEO(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_BE"].sum()),
                '-',
                format_number_with_spaces(NAP(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_AP"][0]),
                format_number_with_spaces(NPI(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_PI"][0]),
                format_number_with_spaces(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin, niveau = element)["Total"][0])
            ]
            for line in text_lines:
                doc.drawString(x3_bis+135, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                '-',
                format_number_with_spaces(int(LLI(gdf_drain, current_year, group_column, typereseau, ZG, niveau= element)["longueur"][0])),
                '-',
                '-',
                format_number_with_spaces(int(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin, niveau= element)["Realise_dans_lannee"][0]))
            ]
            for line in text_lines:
                doc.drawString(x3_bis+235, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                '-',
                format_number_with_spaces(LLI(gdf_drain, current_year, group_column, typereseau, ZG, niveau= element)["longueur_cum"][0]),
                '-',
                '-',
                format_number_with_spaces(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin, niveau= element)["Total_cumule"][0])
            ]
            for line in text_lines:
                doc.drawString(x3_bis+335, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                '-',
                f'{int(LLI(gdf_drain, current_year, group_column, typereseau, ZG, niveau= element)["Pourcentage"][0])} %',
                '-',
                '-',
                f'{int(NITREMIES(filtered_data[11], ins_tremies, group_column, typereseau, ZG, date_debut, date_fin, niveau= element)["Pourcentage_ins"][0])} %'
            ]
            for line in text_lines:
                doc.drawString(x3_bis+435, y3, line)
                y3 -= n
        else:
            text_lines=[
                'STEU',
                'STR',
                'FPR'
            ]
            for line in text_lines:
                doc.drawString(x3_bis, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                format_number_with_spaces(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG, niveau= element)["Total"][0]),
                format_number_with_spaces(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG, niveau= element)["Total"][0]),
                'Attente évolution BDD'#format_number_with_spaces(NFPR(filtered_data, group_column, typereseau, ZG, niveau= element)["NBR_FPR"][0])
            ]
            for line in text_lines:
                doc.drawString(x3_bis+135, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                format_number_with_spaces(int(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG, niveau= element)["Realise_dans_lannee"][0])),
                format_number_with_spaces(int(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG, niveau= element)["Realise_dans_lannee"][0])),
                '-'
            ]
            for line in text_lines:
                doc.drawString(x3_bis+235, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                format_number_with_spaces(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG, niveau= element)["Total_cumule"][0]),
                format_number_with_spaces(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG, niveau= element)["Total_cumule"][0]),
                '-'
            ]
            for line in text_lines:
                doc.drawString(x3_bis+335, y3, line)
                y3 -= n
            y3 += (len(text_lines)*n)
            text_lines= [
                f'{int(NISTEU(filtered_data[9], ins_steu, group_column, typereseau, ZG, niveau= element)["Pourcentage_ins"][0])} %',
                f'{int(NISTR(filtered_data[8], ins_str, group_column, typereseau, ZG, niveau= element)["Pourcentage_ins"][0])} %',
                '-'
            ]
            for line in text_lines:
                doc.drawString(x3_bis+435, y3, line)
                y3 -= n
        y += n
        doc.drawString(x, y,'itv (km)')
        x += 120+n
        text_lines = [
            format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG, niveau= element)["Total"][0])),
            format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG, niveau= element)["Realise_dans_lannee"][0])),
            format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG, niveau= element)["Total_cumule"][0])),
            f'{format_number_with_spaces(int(LITV(filtered_data, group_column, valeurs_uniques, valeurs_uniques_annuelles, ZG, niveau= element)["Pourcentage_itv"][0]))} %'
        ]   
        for line in text_lines:
            doc.drawString(x, y, line)
            x += 100
        y -= 2*n
        x = 15
        doc.drawString(x, y,'Prédiagnostics (km)')
        x += 120+n
        dff_prediag = LDiagPrediag(df_vis, group_column,dic_niveau_extraction[app.selected_item1],'DIAGNOSTIC')
        text_lines = [
            format_number_with_spaces(dff_prediag[dff_prediag[group_column] == element]['Total'].values[0]),
            format_number_with_spaces(dff_prediag[dff_prediag[group_column] == element]['Realise_dans_lannee'].values[0]),
            format_number_with_spaces(dff_prediag[dff_prediag[group_column] == element]['Total_cumule'].values[0]),
            format_number_with_spaces(dff_prediag[dff_prediag[group_column] == element]['Pourcentage'].values[0]) 
        ]    
        for line in text_lines:
            doc.drawString(x, y, line)
            x += 100
        y -= n
        x = 15
        doc.drawString(x, y,'Diagnostics (km)')
        x += 120+n
        dff_diag = LDiagPrediag(df_vis, group_column,dic_niveau_extraction[app.selected_item1],'DIAGNOSTIC')
        text_lines = [
            format_number_with_spaces(dff_diag[dff_diag[group_column] == element]['Total'].values[0]),
            format_number_with_spaces(dff_diag[dff_diag[group_column] == element]['Realise_dans_lannee'].values[0]),
            format_number_with_spaces(dff_diag[dff_diag[group_column] == element]['Total_cumule'].values[0]),
            format_number_with_spaces(dff_diag[dff_diag[group_column] == element]['Pourcentage'].values[0]) 
        ]  
        for line in text_lines:
            doc.drawString(x, y, line)
            x += 100

        x= 15
        y -= 4*n
        ##ETAT DE SANTÉ  STRUCTUREL ET FONCTIONNEL
        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(x, y,'COLLECTEURS : état de santé')
        doc.drawString(x3, y,'OUVRAGES : état de santé (IDCAPTUR)')
        y -= n
        doc.setFillColor(colors.black)
        doc.setFont(styles['T4'], taille['T4'])
        doc.drawString(x, y,'Réseaux non visitables (INDIGAU)')
        doc.line(x, y-1, x+120, y-1)  
        doc.drawString(x2, y,'Réseaux visitables (AGEC)')
        doc.line(x2, y-1, x2+98, y-1)  
        if app.selected_item2 == 'EP':
            doc.drawString(x3, y,'Bassins')
            doc.line(x3, y-1, x3+40, y-1)  
        else :
            doc.drawString(x3, y,'STEU')
            doc.line(x3, y-1, x3+28, y-1)  

        doc.drawString(x4, y,'STR')
        doc.line(x4, y-1, x4+22, y-1)  
        y -= n
        doc.drawImage(ES_NV(filtered_data, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x, y= y-150, width= 250, height= 150)
        doc.drawImage(ES_V(df_vis, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x2, y= y-150, width= 250, height= 150)
        if app.selected_item2 == 'EP':
            doc.setFont(styles['T5'], taille['T5'])
            doc.drawString(x3+50, y-90, 'PAS DE DONNEES CONNUES')
            doc.drawImage(ES_STR_EP(filtered_data, df_tremies, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau ='')[1], x=x4, y= y-150, width= 250, height= 150)
        else :
            doc.drawImage(ES_STEU(filtered_data, df_steu, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x3, y= y-150, width= 250, height= 150)
            doc.drawImage(ES_STR(filtered_data, df_str, palette_couleur_etat_sante_RGBA, out_folder, group_column, NivExt, typereseau, ZG, NODATA_PATH, niveau= element)[1], x=x4, y= y-150, width= 250, height= 150)
        y -= (150+2*n)

        doc.setFont(styles['T3'], taille['T3'])
        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[1])
        else:
            doc.setFillColor(colors_pie[0])
        doc.drawString(x, y,'BRANCHEMENTS : état de santé')
        doc.setFillColor(colors.black)
        doc.setFont(styles['T4'], taille['T4'])
        if app.selected_item2 == 'EP':
            doc.drawString(x3, y,'Linéaires d’infiltration')
            doc.line(x3, y-1, x3+115, y-1)  
            doc.drawString(x4, y,'Puits d’infiltration')
            doc.line(x4, y-1, x4+95, y-1)  
        else :
            doc.drawString(x3, y,'FPR')
            doc.line(x3, y-1, x3+22, y-1)  
        y -= (75+n)
        doc.setFont(styles['T5'], taille['T5'])
        doc.drawString(x+230, y, 'PAS DE DONNEES CONNUES')
        doc.drawString(x3+50, y, 'PAS DE DONNEES CONNUES')
        doc.drawString(x4+50, y, 'PAS DE DONNEES CONNUES')
        y -= (100+n)

        if app.selected_item2 == 'EP':
            doc.setFillColor(colors_pie[0])
        else:
            doc.setFillColor(colors_pie[1])
        
        ##PROGRAMME DE TRAVAUX
        doc.rect(0, y, width= 1200, height= 30, fill=1, stroke=0)
        doc.setFont(styles['T2'], taille['T2'])
        doc.setFillColor(colors.white)
        doc.drawString(15, y+10,'PROGRAMME DE TRAVAUX')
        y -= (n+3)
        doc.setFont(styles['T3'], taille['T3'])
        doc.setFillColor(colors_pie[1])
        doc.drawString(x, y,'COLLECTEURS')
        doc.drawString(x2, y,'BRANCHEMENTS')
        doc.drawString(x3, y,'OUVRAGES')
        y -= 2*n
        y3 = y
        doc.setFont(styles['T5'], taille['T5'])
        doc.setFillColor(colors.black)
        doc.drawString(x2, y,'Branchements réhabilités :')
        doc.drawString(x2+200, y,'-')

        text_lines = [
            "Total d'idées de faire travaux Réseaux :",
            'Total réhabilité (remplacement, gainage,',
            'réparation) :',
            'Extension annuelle (année en cours) :' ,
            'Taux annuel de renouvellement :'
        ]  
        text_lines1 = [
            f'{format_number_with_spaces(int(LRIT(df_idees_res, group_column, ZG, niveau= element)["long_idee"][0]))}km',
            '',
            '-',
            '-',
            '-' 
        ] 
        for index in range(len(text_lines)):
            doc.drawString(x, y, text_lines[index])
            doc.drawString(x+200, y, text_lines1[index])
            y -= n
        if app.selected_item2 == 'EP':
            text_lines = [
                "Ouvrages EP créés :",
                '',
                'Bassins réhabilités :',
                "linéaires d'infiltration réhabilités :" ,
                'STR réhabilitées :']  
            text_lines1 = [
                '-',
                '',
                '-',
                '-',
                '-' ] 
            for index in range(len(text_lines)):
                doc.drawString(x3, y3, text_lines[index])
                doc.drawString(x3+200, y3, text_lines1[index])
                y3 -= n
        else :
            text_lines = [
                "STEU réhabilitées :",
                'STR  réhabilitées :',
                'FPR réhabilités :']  
            text_lines1 = [
                '-',
                '-',
                '-'] 
            for index in range(len(text_lines)):
                doc.drawString(x3, y3, text_lines[index])
                doc.drawString(x3+200, y3, text_lines1[index])
                y3 -= n

        doc.drawString(1170, 10, '2') 
        doc.showPage()
        #page3
        path_atlas = f'{dic_path_atlas[app.selected_item1]}{element}.png'
        if os.path.exists(path_atlas) :
            doc.drawImage(path_atlas, x=0* cm, y= 0* cm, width= 42* cm, height= 29.7* cm)
        ################################### Exportation du fichier pdf #######################################################################
        doc.save()
        print(f"Le fichier PDF '{file_name}' a été généré avec succès.")
    os.startfile(f'{pdf_file}')