# Github Workspace

このリポジトリにデータ解析のスクリプトと実際に用いたデータを保管します。
ここでは、オープンソースのデータを使った解析を紹介します。

## 解析を始める前に

開発環境は Microsoft Windows 11 Pro を想定しています。
OS などシステムに関する情報はコマンドプロンプトから確認することができます。
コマンドプロンプトを起動し、**systeminfo**
と入力すると、システムに関する情報が出力されます。
出力結果の **OS Name** がデバイスの OS です。

R と RStudio のダウンロードは以下のサイトから実行します。

[RCRAN](https://cran.rstudio.com/)

[RStudio Desktop Posit](https://posit.co/download/rstudio-desktop/)

### パッケージのインストール
パッケージは基本的には以下の関数を用いることでデバイスにインストールすることができます。

```
install.packages("package_name")
```

Windows 11 では今のところこの関数でインストールできないパッケージを確認していませんが、
Debian系のUbuntu 24.04LTS ではいくつかのパッケージがこの関数でインストールすることができません。

Ubuntu 24.04LTS に R
をインストールして解析する際、パッケージのインストールを Terminal
で実行する必要があります。

はじめに、 Ubuntu に新しいものをインストールする前に、
以下のコマンドを実行します。

```
sudo apt update
```

次に、以下のコマンドでパッケージをインストールします。
今回は、構造方程式モデリングに用いられる lavaan
パッケージをインストールしていきます。

```
sudo apt install r-cran-lavaan
```

これでパッケージのインストールは完了です。

しかし、このコマンドでも showtext パッケージはインストールできなかったので、
別の方法が思いつき次第、追記していこうと思います。

## Data Analysis

このフォルダには、多変量解析のスクリプトファイルをアップロードしています。

### Principal Component Analysis (PCA)

### Redundancy Analysis (RDA)

RDA は制約付き序列化解析です。つまり、条件付きの PCA です。
多変量解析の際は、データの標準化を行うことが一般的です。
種数のデータには一般的にへリンガー変換 (Hellinger Transformation) が用いられます。
そのため、ここではへリンガー変換します。

RDA は、群集生態学で用いられる解析手法であり、
環境要因の行列データを説明変数として、
生物群集の行列データを説明します。

ここでは、分散共分散行列 (Variance-Covariance matrix)
の群集データに関する固有値解析を行います。

