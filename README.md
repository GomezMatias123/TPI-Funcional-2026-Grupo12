# TPI Funcional 2026 — Grupo 12

Trabajo Práctico Integrador de la cátedra Paradigmas y Lenguajes.
Implementación de un sistema semafórico en Common Lisp con comparativa 
en ERLANG.

## Integrantes

- Gomez Matias Nicolas
- Arce Federico Hernán
- Maidana Adrián Ezequiel

## Estructura del proyecto

- lisp/ — Archivo principal "core-lisp" que contiene las funciones implementadas y listas para su ejecucion demostrando su utilidad en la resolucion de los diferentes problemas planteados por las consignas del trabajo, junto con casos de prueba al final del archivo.
- comparativa/ — Dentro de esta carpeta se encuentra el archivo que contiene la traduccion de las funciones "semaforo-timer" y "transicion" al lenguaje ERLANG, junto con el desarrollo de la comparacion con lisp que elaboramos como grupo.
- docs/ — Dentro de esta carpeta nos encontramos con dos achivos, en HONOR.md podemos leer los codigos de honor realizados por cada integrantes con respecto a su participacion y utilizacion de la IA como herramienta. En la carpeta informe se encuentra detallado todo el proceso que conllevo el trabajo, bugs, desafio que tuvimos como grupo, el proceso de la elaboracion de las funciones y conclusiones finales.

## Cómo ejecutar
Una vez abierto core.lisp con sublime text se podra ver en pantalla todo el contenido (funciones implementadas, clasificacion de cada funcion, caso de prueba). se debe apretar las combinaciones de letras "alt + ctrl + h" se abrira el interprete con SBCL, se debera cargar el archivo con la funcion (load "C:/Lisp/TPI-Funcional-2026-Grupo12/TPI-Funcional-2026-Grupo12/Lisp/core"), "de ser necesario modificar a la ruta donde se guardaron los archivos y tener modificado el apartado de key bindigns para que el interperte pueda abrirse con SBCL:"
[
{
"keys": ["alt+ctrl+h"],
"command": "repl_open",
"args": {
"type": "subprocess",
"encoding": "utf8",
"cmd": ["C:\\Program Files\\Steel Bank Common Lisp\\sbcl.exe"],
"cwd": "C:\\Lisp",
"external_id": "lisp",
"syntax": "Packages/Lisp/Lisp.tmLanguage"
}
}
]

Una vez aparezca T luego de la funcion LOAD se puede ejecutar las diversas funciones sin complicaciones siguiendo la sintaxis normal de LISP.

## Videos

- https://youtu.be/gBKAqd_r_lg