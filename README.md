# Comuni Italiani Search

This is a test project to explore _Servant_ and _Servant To Elm_ conversion.
It is composed of 3 parts:
1. The Servant Server that expose a very basic API
2. An Elm client that query the API
3. A generator command, that converts the API Servant Types into the Elm client Types

## How To Use It

To run the server, from the root of the project:
```stack run comuni-italiani-exe```
[You need stack to run this]


To run the client, from the elm-client/comuni-client:
```elm-app start```
[You need elm to run this]

## How to generate the client Types
From the root of the project:
```stack run comuni-italiani-code-gen```

This will generate the client in the src/Generated folder.
