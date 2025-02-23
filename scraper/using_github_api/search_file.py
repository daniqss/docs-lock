import requests
import os
from dotenv import load_dotenv

load_dotenv()



def buscar_archivos_en_github(repo, archivo):
    """
    Busca un archivo específico en un repositorio de GitHub, incluyendo todos los directorios.
    Devuelve una lista con el contenido de todas las instancias del archivo encontradas.

    Args:
        repo: El nombre del repositorio en formato "usuario/repositorio".
        archivo: El nombre del archivo a buscar.

    Returns:
        Una lista con el contenido de todas las instancias del archivo encontradas.
        Si no se encuentra ninguna instancia, devuelve una lista vacía.
    """

    token = os.environ.get("GITHUB_TOKEN")

    if token is None:
        raise ValueError("La variable de entorno GITHUB_TOKEN no está definida.")

    headers = {
        "Authorization": f"token {token}"
    }

    url_arboles = f"https://api.github.com/repos/{repo}/git/trees/main?recursive=5"
    respuesta_arboles = requests.get(url_arboles, headers=headers)

    if respuesta_arboles.status_code == 200:
        arboles = respuesta_arboles.json()["tree"]
        contenidos_archivos = []  # Lista para almacenar los contenidos de los archivos encontrados

        for elemento in arboles:
            if elemento["type"] == "blob" and elemento["path"].endswith(archivo):
                # El archivo se ha encontrado, descargar su contenido
                url_descarga = f"https://raw.githubusercontent.com/{repo}/main/{elemento['path']}"
                respuesta_descarga = requests.get(url_descarga, headers=headers)
                if respuesta_descarga.status_code == 200:
                    contenidos_archivos.append(respuesta_descarga.text)  # Agregar contenido a la lista
                else:
                    print(f"Error al descargar el archivo: {respuesta_descarga.status_code}")

        if not contenidos_archivos:  # Si la lista está vacía, no se encontró el archivo
            print(f"No se ha encontrado el archivo {archivo} en el repositorio {repo}")
        
        return contenidos_archivos  # Devolver la lista con todos los contenidos encontrados
    else:
        print(f"Error al obtener la lista de árboles: {respuesta_arboles.status_code}")
        return []  # Devolver una lista vacía en caso de error

# Ejemplo de uso
repo = "daniqss/pomodoro"  # Repositorio de ejemplo
archivo = "package.json"  # Archivo a buscar

contenidos = buscar_archivos_en_github(repo, archivo)

if contenidos:
    for i, contenido in enumerate(contenidos):
        print(f"Contenido de {archivo} (instancia {i+1}):\n{contenido}")
else:
    print(f"No se ha encontrado el archivo {archivo} en el repositorio {repo}")