# Explainable decision support system based on argument search

As described in section IV.B. of [the report](https://github.com/explainable-reasoning/explainable-reasoning.github.io/blob/main/report.pdf).

## Usage

Visit the [online interface](https://explainable-reasoning.github.io/argument-search/), or locally open `index.html` in the browser.

## Compiling

[Install Elm.](https://guide.elm-lang.org/install/elm.html)

```bash
elm make src/Main.elm --output index.html
```

## Testing

[Install NPM,](https://nodejs.org/en/download/) then `npm install --global elm-test`.

```bash
elm-test
```