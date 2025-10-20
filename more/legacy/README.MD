# Códigos antigos

Essa é uma "refatoração" dos códigos usados para calcular os fatores e escores originalmente.
Os códigos em si não foram alterados, apenas os colocamos num único arquivo (`final.py`) para que as funções possam ser executadas como um script.
Ademais, selecionamos um jupyter notebook que mostra como as funções devem ser usadas.

## Instalar venv
`sudo apt install python3-venv`
`python3 -m venv "venv"`
`source venv/bin/activate`
`pip install factor-analyzer pandas scipy numpy matplotlib joypy openpyxl seaborn scikit-learn pyshp Unidecode geopandas folium branca shapely plotly notebook jupyterlab ipykernel`
`python3 -m ipykernel install --user --name=venv --display-name "Python (venv)"`
