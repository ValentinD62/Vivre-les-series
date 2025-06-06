# Vivre-les-series

Groupe: Valentin Decroix & Paul Maillard

Back: ASP.NET Core / C# Core

Front: Typescript avec framework LitElement

# API

L'API qui se situe dans le dossier backend est un essai de création d'une API faite en Haskell par Paul. Des problèmes ont été rencontrés quand il a fallu trouver un moyen de mettre JWT, un cache et autres fonctionnalités du genre. Il a donc été décidé de ne pas l'utiliser pour le projet. Pour tester les routes on vous conseille d'utiliser [l'application Bruno](https://www.usebruno.com/) 

L'API qui se situe dans le dossier back, faite par Valentin, est une API REST faite en C# avec le framework ASP.NET Core. C'est l'API qui est utilisé sur le site. Elle est utilisée pour gérer les séries, les utilisateurs, les commentaires des utilisateurs et leurs notes sur ces séries.

Elle utilise JWT pour l'authentification et un cache pour améliorer les performances de certaine routes comme la liste des séries les mieux notés et getListeById car ce sont les 2 routes les plus utilisés et les moins susceptibles de changer ce sont donc les 2 meilleures candidates.

Pour l'API externe, nous avons utilisé l'API de The Movie Database (TMDB) pour récupérer les informations sur les séries. Nous avons créé un compte et obtenu une clé API pour pouvoir faire des requêtes.

### Les problèmes de l'API externe
Mais nous avons repéré plusieurs problèmes avec cette API :
- Il n'y a pas de route PUT (donc on ne peux pas remplir l'objectif des 4 verbes sur l'API externe)
- Les Users de l'API externe utilisent des sessions. Lorsque l'on veut créer un utilisateur, on doit d'abord créer un token qui expire au bout de 60 minutes, puis demander à l'utilisateur la "permission" en utilisant un lien vers le site tmdb puis créer un sessionId qui sert d'utilisateur (qui ne dure que 60 minutes).
- Le point précédent a pour conséquence que l'on n'a pas réellement de User dans l'API externe. Les appels vers l'API qui nécessitent un utilisateur sont fait avec un bearer commun correspondant à mon compte TMDB.

Nous avons également une base de données qui permet de stocker les utilisateurs de notre API, les notes et les commentaires des séries.

# Frontend

Le frontend est fait en Typescript avec le framework , fait par Valentin. Il est utilisé pour afficher les séries, les utilisateurs, les commentaires et les notes des utilisateurs sur les séries.

## Architecture

Le dossier `public` contient les images nécessaires à l'application.

Le dossier `src` contient le code source de l'application.

Le dossier `src/API` contient le code pour interagir avec l'API. Il y a un fichier `main.ts` qui contient la configuration de l'API et les routes pour interagir avec l'API.

Le reste des dossiers servent à stocker les composants Lit et ce dont ils ont besoin pour fonctionner.

## Les fonctionnalités du frontend

Sur la page principale, on peut voir la série recommandée (par nos soins héhé).

Il y a un menu en haut qui permet de rechercher une série par son nom. Cela fait apparaître la liste des séries qui sont dans la sélection. Avec un debounce qui permet de ne pas noyer l'API sous les appels si l'on tape trop vite.

Ensuite, on peut voir la liste des séries les mieux notées de l'API externe.

On peut également se créer un compte et/ou se connecter à notre compte.

Lorsque l'on clique sur la carte d'une série, une modale apparaît pour nous montrer plus d'informations sur la série, comme le résumé, le titre, la note de l'API externe.

Lorsque l'on est connecté, on peut noter la série, voir les commentaires des autres utilisateurs et laisser le sien. On peut également supprimer sa note, supprimer ou modifier son propre commentaire.

### En résumé
- lorsque l'on est pas connecté, on peut voir les séries et leurs informations mais pas interagir avec.
- Lorsque l'on est connecté, on peut voir les séries et leurs informations, on peut noter les séries, laisser des commentaires et modifier ou supprimer ses notes et commentaires.


# Utilisation

J'utilise Visual Studio pour pouvoir faire tourner l'API. 

Pour le frontend, une fois dans le dossier front, utiliser npm install puis npm run dev pour lancer le serveur.

Si l'API a changé de port, il faut modifier le fichier `src/API/main.ts` pour mettre le bon port de l'API.

# Exemple d'écran

Voici un exemple sur la série Frieren.
![Exemple de ce que l'on peux voir](/front/public/exemple_ecran_note_commentaire.png "Exemple de ce que l'on peux voir").