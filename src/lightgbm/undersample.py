import pandas as pd
import numpy as np
from sklearn.utils import resample

# Especifica la ruta del archivo CSV
file_path = ".\datasets\competencia_02.csv.gz"

# Definir el tamaño del bloque para procesar en cada iteración
block_size = 10000

# Inicializar un DataFrame vacío para almacenar los resultados undersampleados
undersampled_data = pd.DataFrame()

# Leer el archivo CSV en bloques
for chunk in pd.read_csv(file_path, chunksize=block_size):
   
    # Separar los bloques por clases
    clase_baja_1 = chunk[chunk['clase_ternaria'] == 'BAJA+1']
    clase_baja_2 = chunk[chunk['clase_ternaria'] == 'BAJA+2']
    clase_continua = chunk[chunk['clase_ternaria'] == 'CONTINUA']
    
    undersampled_continua = resample(clase_continua, replace=False, n_samples=int((len(clase_baja_1) + len(clase_baja_2))/2), random_state=42)

    # Concatenar los bloques undersampleados al DataFrame final
    undersampled_data = pd.concat([undersampled_data, clase_baja_1, clase_baja_2, undersampled_continua])


# Ahora `undersampled_data` contiene los datos undersampleados para la clase "CONTINUA"
