import sys
import os
import PyQt5
import argparse

# Chemin vers le dossier d'installation QGIS
qgis_path = 'C:/Program Files/QGIS3'

###################configuration de l'envrionnement#######################
sys.path.append(os.path.join(qgis_path, 'apps', 'qgis-ltr', 'python'))
sys.path.append(os.path.join(qgis_path, 'apps', 'qgis-ltr', 'python', 'plugins'))
# Variables d'environnement
os.environ['QT_PLUGIN_PATH'] = os.path.join(qgis_path, 'apps', 'Qt5', 'plugins')
os.environ['PATH'] += os.pathsep + os.path.join(qgis_path, 'apps', 'qgis-ltr', 'bin')
os.environ['PATH'] += os.pathsep + os.path.join(qgis_path, 'apps', 'Qt5', 'bin')

################### Exportation des pages de l'Atlas##############################
from qgis.core import (
    QgsApplication,
    QgsProject,
    QgsLayoutExporter,
    QgsLayout
)
from qgis.PyQt.QtXml import QDomDocument

def ExportAtlas(project_path, output_path, type_reseau, ZG, group, group_column):
    dic_layout = {
        "Commune": 'carto_communes',
        "Systeme Assainissement": 'carto_SA',
        "Bassin versant": 'carto_BV',
        "Captage": 'carto_Captage'
    }
    # Charger le projet QGIS
    project = QgsProject.instance()
    if not project.read(project_path):
        print(f"Erreur lors du chargement du projet : {project_path}")
        qgs.exitQgis()
        sys.exit(1)
    # Accéder au layout contenant l'Atlas
    if type_reseau == "EU_U":
        layout_name = f'{dic_layout[ZG]}_EU_U'
    else:
        layout_name = f'{dic_layout[ZG]}_EP'
    
    layout = project.layoutManager().layoutByName(layout_name)
    if not layout:
        print(f"Layout nommé '{layout_name}' non trouvé dans le projet.")
        qgs.exitQgis()
        sys.exit(1)

    atlas = layout.atlas()
    if not atlas.enabled():
        print("L'Atlas n'est pas activé dans ce layout.")   
        qgs.exitQgis()
        sys.exit(1)

    # Exporter les pages de l'Atlas
    exporter = QgsLayoutExporter(layout)
    settings = QgsLayoutExporter.ImageExportSettings()
    if group != 'Tous':
        atlas.setFilterFeatures(True)
        atlas.setFilterExpression(f"\"{group_column}\" = '{group}'")
    exporter.exportToImage(atlas, output_path, 'png', settings)
    print("Exportation de l'Atlas terminée.")

def main(type_reseau, ZG, group, group_column):
    # Initialiser l'application QGIS
    QgsApplication.setPrefixPath(qgis_path, True)
    qgs = QgsApplication([], False)
    qgs.initQgis()

    # Chemin vers le projet QGIS
    project_path = 'L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/carto/carto.qgz'
    # Chemin vers le dossier de sortie
    output_path = f'L:/EGDD/DAEA/PAG/K_STAGES/2023_Amen Allah NOUIRA/Indicateurs/carto/export/'

    ExportAtlas(project_path, output_path, type_reseau, ZG, group, group_column)

    # Fermer l'application QGIS
    qgs.exitQgis()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('type_reseau', type=str, help='Type de réseau')
    parser.add_argument('ZG', type=str, help='Zone geographique')
    parser.add_argument('group', type=str, help='Nom du groupe')
    parser.add_argument('group_column', type=str, help='Nom du champ')
    args = parser.parse_args()
    main(args.type_reseau, args.ZG, args.group, args.group_column)