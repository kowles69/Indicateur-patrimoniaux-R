from setuptools import setup, find_packages

setup(
    name="GeoIndicateurs",
    version="1.0.0",
    packages=find_packages(),
    install_requires=[
        "dbfread==2.0.7",
        "geopandas==0.14.3",
        "sqlalchemy==2.0.29",
        "psycopg2==2.9.9",
        "numpy==1.26.1",
        "pandas",
        "shapely",
    ],
    python_requires=">=3.8",
    author="Besnier Jean-Baptiste ",
    author_email="jbesnier@grandlyon.com",
    description="Analyse de données géospatiales et calcul d'indicateurs",
    keywords="gis, spatial analysis, indicators",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "Programming Language :: Python :: 3",
        "Topic :: Scientific/Engineering :: GIS",
    ],
)