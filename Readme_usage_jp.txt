%================================================================
%      Readme of 特定温位面以下の寒気質量解析ツール
%================================================================
1.動作環境
  Linux + intel fortran (ifort 10.1)で確認。Linux + 富士通コンパイラ(f90)でも確認。

2.ディレクトリ構成
 ${isen_cam}/
	     src/  ソースコード
	     Doc/  ドキュメント
	　　 work/ gradsのctlファイルが入っている。
	　　 Makefile   メイクファイル
   	     Readme_namelist_eng.txt　namelistの説明
	     sample.sh　　サンプルデータ用のシェルスクリプト

3.入力データ
　grads形式のデータを入力。　　
　・u-wind
　・v-wind
　・geopotential height
　・air temperature
　・surface pressure
  ・surface u-wind
　・surface v-wind
　・surface temperature
  ・surface altitude
 surface pressureはsea level pressureでも代用可。

4.使用方法
　4.1 コンパイル
    はじめに、${isen_cam}/Makefile の使用するコンパイラをチェックする。
    $cd ${isen_cam}/
    $make
    成功すれば、isen_cold_airが作成される。

　4.2 ネームリストの作成
　　sample.shを参考にしてnamelistを作成。
   namelistの詳細はReadme_namelist_eng.txtを参照。

　4.3 解析
    作成したネームリスト(namelist)を使う場合、
　　$./isen_cold_air < namelist

  GrADS用のctlファイルはwork/isen.ctlを
  修正して使用してください。
　具体的な計算についてはドキュメント(${isen_cam}/Doc/document.pdf)をご覧ください。


5.注意点

地表付近の逆転層は、鉛直に見て温位が最少となる点の値を使って除去している。
特定温位は対流圏中～下層を想定してプログラムを作成しているため、
対流圏界面付近にできる逆転層については考慮していない。

特定温位面の気圧、高度、寒気容量とそのフラックス、生成率は
namelistの設定で出力させないことも可能。
出力ファイルのサイズを減らすことはできるが、計算はされている。

入力ファイルの内、特定の期間だけを計算したい場合
(例えば入力ファイルが1年分で1月だけを計算したいとき)、
namelistのINITIAL_TIME、END_TIMEをうまく使って調整して下さい。

1つ前のtime stepの情報が無い場合、同様生成量は寒気質量の発散だけで計算されることに注意。
INPUT_TDEF_DAYNUMは時間変化項の分母を決定するときに必要。




