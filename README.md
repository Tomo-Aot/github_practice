---
title: |
       | Github Workspace
       |
       |
       |
date: today
author: Aota Tomoyuki
institute: Institute for East China Sea Research
---

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

## Data_analysis

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

