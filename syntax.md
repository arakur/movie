# スクリプト構文(仮)

## BNF(仮)

```bnf
Block ::= Statement*

Statement ::=
    | '#' CommandName Expression* ':' (Block)?

Expression ::= (...)
```

## エイリアス

- '@' は '#set-speaker' のエイリアス
- '>' は '#talk' のエイリアス
  - '>' の後の文字列は空白含め全てまとめて単一の文字列として扱われる
- 'A <- B' は '#set A B' のエイリアス
