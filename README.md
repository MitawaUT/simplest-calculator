# simplest-calculator

## モチベ
なんか年越しに新しいことをやろうと思って純粋関数型言語である Elm で書いてみました (しかも 60 分縛りという謎企画) 。

フロントエンドには苦手意識があるんですが，これならやってもいいかなと思いました (小並感) 。

[こちら](https://mitawaut.github.io/simplest-calculator/) で遊べます。

2022/01/01 追記：リファクタしました (60 分縛りが破れる)

## 補遺

電卓って実は実装工夫しないといけない点があって楽しいんです。
1. 演算子 (`+`, `-` など) が押された時にすぐに演算をできない。
    Elm のような純粋関数型言語だと
    ```elm
    add : number -> number -> number
    add a b = a + b
    ```
    に対して，
    ```elm
    add1 = Add 1
    ```
    は `add1: number -> number` という型を持ちます。これを用いると後から入力されたものを演算するというのをスマートに実装できますね。
2. `0123` とか `00` みたいな表示にならないようにする。
    これは `String -> Float -> String` にすることで楽に正規化しました。
    ```elm
    -- 抜粋
    normalizeDigits : String -> String 
    normalizeDigits s =
        (
        case String.toFloat s of
        Nothing -> s 
        Just v -> String.fromFloat v
        )
    ```
3. `=` や画面消去は演算子 (`+`, `-`など) とは仕様を分ける 。
    演算子と違い `Float -> Float` として，また状態 (`Model` の attribution の 1 つ) を変えるようにしました。これは，例えば `=` を入力して結果を表示した後に数字が押される (= 今の結果を消して次の計算を始める) か演算子が押される (今の結果を使ってさらに計算を続ける) かで処理を分けたいためです。

