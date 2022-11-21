# [迷路小説](https://tannakaken.github.io/mazenovel/)

途中まで共通で途中からが異なっている複数の小説から
迷路を作る

とりあえず固定の小説で動くようになった。

## TODO

次は小説を投稿できるようにしたい。

## 実装

迷路生成アルゴリズムをいくつか調べても、ちょうどいいのが見つからなかったのでアルゴリズムはフルスクラッチで作った。

- 小説の中から一つ選ぶ
- 壁からスタートして、選んだ小説の文字数分ランダムに道を伸ばしていく。自己交差したら、いくつか戻る。
- 選んだ小説の文字数分の道ができたら、到達地点が逆に小説のスタートになる。
- 他の小説との分岐地点から、他の小説をランダムに道を伸ばしていく。自己交差したらそこで終了。

## ローカルでの開発方法・起動方法

Makefileを見ればわかる。

## 作者

[淡中圏](https://tannakaken.xyz) ([@tannakaken](https://twitter.com/tannakaken)）
