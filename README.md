# ImageCompressor

Epitech Project, 2022
The goal of this project is to compress a list of pixels of the format below using the k-means algorithm.
```
(x, y) (r, g, b)
...
```

The produced output respects the following format:
```Makefile
OUT ::= CLUSTER *
CLUSTER ::= '- -\n' COLOR '\n -\ n' ( POINT ' ' COLOR '\n ') *
POINT ::= '(' int ',' int ')'
COLOR ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT ::= '0 '.. '255 '
```

## How to start
You will need a [stack]() installation to run this project.\
Then you can build using:
```bash
make
```
And start the `imageCompressor` binary that comes out.
