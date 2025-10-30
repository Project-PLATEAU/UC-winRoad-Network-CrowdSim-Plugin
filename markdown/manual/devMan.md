# 環境構築手順書

## 1 本書について

本書は、輸送計画検討に向けた人流シミュレーションプラグイン(UC-win/Roadプラグイン)の利用環境構築手順について記載しています。

> [!Note]
> 本システムの構成や仕様の詳細については技術検証レポートも参考にしてください(**技術検証レポート公開後、リンクを追記します**)。

> [!Important]
> 本システムを実行するにはUC-win/Roadおよびそのライセンスが必要です。詳細は株式会社フォーラムエイトにお問い合わせ下さい。<br>
> [フォーラムエイト(HP)][Forum8HP]<br>
> [フォーラムエイト(サポートページ)][Forum8Support]

## 2 動作環境

本システムの動作環境は以下のとおりです。

|項目|最小動作環境|推奨動作環境|
| - | - | - |
|OS|Microsoft Windows10 / 11(64bit)|同左|
|CPU|Intel Core i5 以上|同左|
|GPU|NVIDIA製GPU|NVIDIA Geforce GTX1650以上(4GB以上のメモリ)|
|メモリ|16GB以上|32GB以上|
|ストレージ|最低30GB以上の空き容量|SSDドライブ<br>最低60GB以上の空き容量|
|ディスプレイ解像度|1920×1080以上|同左|
|ネットワーク|必須|同左|

## 3 セットアップ

### 共通手順

1. [GitHubページ][CrowdSimGitHub]からソースコードをダウンロードします。
2. WindowsのエクスプローラでUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダを開きます。
    > [!Note]
    > UC-win/Roadをインストールすると、デフォルト設定ではデスクトップにショートカットが作成されます。<br>
    > ショートカットを右クリックし、「ファイルの場所を開く」を選択するとUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダを開けます。<br>
    > ![UCwinRoad_ショートカット][UCwinRoad_Shortcut]
3. Shadersフォルダを開きます。
4. Pluginsフォルダを開きます。
5. ダウンロードしたソースファイルのsrc\F8CrowdSimPlugin\Shadersフォルダ内のCrowdSimMeshフォルダを前項のPluginsフォルダにコピーします。

次に、目的に合わせて該当する手順を実行します。

# [本システムを利用したい(開発はしない)](#tab/useOnly)

1. [GitHubページ][CrowdSimGitHub]から最新のRelease版をダウンロードします。
![GitHubリリース画面][GitHubRelease]
2. WindowsのエクスプローラでUC-win/Roadのデータディレクトリを開きます。
    > [!Note]
    > データディレクトリの場所は、UC-win/Roadで確認できます。<br>
    > UC-win/Roadを起動して、メイン画面上部の「ファイル」タブ>>「アプリケーションオプション」>>「デフォルト設定」を押下します。<br>
    > 「アプリケーションデフォルト」画面が開くので、画面左側のタブから「フォルダ、ファイル関連」を選択します。「データディレクトリ」項目に記載されている場所を確認します。<br>
    > ![アプリケーションデフォルト画面][ApplicationDefaultForm]
3. データディレクトリ直下の「Plugins」フォルダを開きます(もし「Plugins」フォルダが無ければ作成してください)。
![データディレクトリ_Plugins][DataDirectory_Plugins]
4. 「Plugins」フォルダ直下にダウンロードしたF8CrowdSimPlugin.bplを設置します。

# [本システムを元に開発したい](#tab/develop)

ソースコードからプラグインをビルドして利用します。

> [!Important]
> F8CrowdSimPluginをビルドするにはUC-win/Road SDKが必要です。詳細は株式会社フォーラムエイトにお問い合わせ下さい。<br>
> [フォーラムエイト(HP)][Forum8HP]<br>
> [フォーラムエイト(サポートページ)][Forum8Support]
> [!Important]
> F8CrowdSimPluginをビルドするにはDelphi 10.4.2が必要です。IDE Patchesは全て適用済みの状態にして下さい。詳しくは[こちら][DelphiPatch]をご参照ください。

ソースファイルからF8CrowdSimPluginを生成することができます。<br>
ソースファイルは[こちら][CrowdSimGitHub]からダウンロード可能です。<br>
ダウンロードしたsrcフォルダ内のF8CrowdSimPluginフォルダ内にソースコード一式があります。<br>
F8CrowdSimPluginフォルダのフォルダ構成は次のようになっています。

```bash
├─Importer
│  ├─CityGML
│  ├─MFJSON
│  └─TrafficSensor
├─PeopleFlowAnalysis
│  ├─BusTransportation
│  ├─ExportsForm
│  ├─FlowLog
│  ├─HeatMap
│  ├─ODTrip
│  └─WaitingQueue
├─PeopleFlowSimulation
│  ├─PedestrianMap
│  │  └─PedestrianMapUtil
│  └─Renderer
├─PlayerForm
│  ├─imgs
│  ├─PlayerFormFrame
│  └─SidePanel
├─resources
├─Shaders
│  └─CrowdSimMesh
│      └─Sources
├─SimulationInput
│  ├─Dialog
│  ├─Manager
│  └─PedestrianPopOut
└─WalkingRoute
```

ビルド方法は次のとおりです。

1. UC-win/Road SDKのヘルプファイルに従って開発環境の初期設定を行います。
2. 本システムのプロジェクトファイル（F8CrowdSimPlugin.dproj）をDelphi 10.4.2で開きます。
3. プラグインの出力先を確認します。
   1. プロジェクト画面で「F8CrowdSimPlugin.bpl」を右クリックし、「オプション」を選択します。
   2. 「ビルド」-「Delphiコンパイラ」を開きます。
   3. 「ターゲット」を「すべての構成 - Windows64ビット プラットフォーム」に変更します。
   4. 「パッケージの出力ディレクトリ」が「(UC-win/Roadのデータディレクトリ)\Plugins」になってない場合は変更して下さい。
   ![Delphiプロジェクトオプション][delphiProjectOptionForm]
4. ビルド構成を「Release」、ターゲットプラットフォームを「Windows 64ビット」にします。
![F8CrowdSimPluginビルド時の構成][buildSetting_F8CrowdSimPlugin]
5. ビルドします。
6. ビルドに成功すると、前項で確認した「パッケージの出力ディレクトリ」の場所に「F8CrowdSim.bpl」が出力されます。

> [!Note]
> Shadersフォルダ内のファイルを変更した場合は、インストール手順を参考にCrowdSimMeshフォルダを更新して下さい。

---

### セットアップ完了時のプログラム構成

UC-win/Roadをデフォルト設定でインストールし、上記手順でセットアップ完了した場合、最終的にフォルダ構成は次のようになります。

```bash
C:
├─Program Files
│  └─FORUM8
│      └─UCwinRoad 17.2 <------ UCwin/Roadの実行ファイルがあるフォルダ
│          └─shaders
│              └─Plugins
│                  └─CrowdSimMesh <------ ダウンロードしたフォルダを設置
└─UCwinRoad Data 17.2 <------ データディレクトリ
│  └─Plugins
│      └─F8CrowdSimPlugin.bpl <------ ダウンロードしたbplを設置
```

## 4 入力するデータ

アプリケーションを利用するために以下のデータを入手します。<br>
データの入力方法については操作マニュアルをご参照下さい。

|# | データ種別 | 機能| 用途| 入力方法 |
| - | - | - | - | - |
| 1| 3D都市モデル(CityGML)<br>[G空間情報センター][geospatial]から取得します。| 全般| 全般| 格納フォルダパス指定|

本システムでは、歩行領域生成・編集において3D都市モデルの道路、建築物、都市設備モデルの形状を活用します。

> [!TIP]
> 建築物LOD1～3を3次元空間上に配置、表示することは可能ですが、歩行エリア生成には利用できません。
> これは、建築物LOD4で定義されている部屋(bldg:Room)の情報を使用しているためです。

> [!TIP]
> 広場モデル等、道路モデルと同じ仕様のモデルについては道路モデルと同様の扱いとなります。

| 地物| 地物型| 属性区分| 属性名| 内容|
| - | - | - | - | - |
| 道路LOD1 |tran:Road|空間属性|tran:lod1MultiSurface|道路のLOD1の形状|
| 道路LOD3|tran:Road|空間属性|tran:lod3MultiSurface|道路のLOD3の形状|
| 部屋(建築物LOD4の子要素)|bldg:Room|空間属性|bldg:lod4Solid|部屋の形状|
| 都市設備LOD3 |frn:CityFurniture|空間属性|frn:lod3Geometry|都市設備の形状|

<!---GitHubページなどは確定次第修正します-->
<!--URL-->
[TechnicalReport]: https://www.mlit.go.jp/plateau/use-case/uc25-07/
[Forum8HP]: https://www.forum8.co.jp/index.html
[Forum8Support]: https://www.forum8.co.jp/tech/tech.htm
[CrowdSimGitHub]: https://github.com/Project-PLATEAU/UC-winRoad-Network-CrowdSim-Plugin
[DelphiPatch]: https://blogs.embarcadero.com/ja/rad-studio-10-4-2patch-general-patchdelphi-compiler-patch-ja/
[geospatial]: https://front.geospatial.jp/
<!--画像-->
[ApplicationDefaultForm]: ../resources/devMan/applicationDefaultForm.png
[delphiProjectOptionForm]: ../resources/devMan/delphiProjectOption.png
[buildSetting_F8CrowdSimPlugin]: ../resources/devMan/BuildSettings_Plugin.png
[buildSetting_CrowdSim]: ../resources/devMan/BuildSettings_FrowdSim.png
[DataDirectory_Plugins]: ../resources/devMan/RoadData_Plugins.png
[UCwinRoad_Shortcut]: ../resources/devMan/UCwinRoad_shortcut.png
[GitHubRelease]: ../resources/devMan/GitHubPage_Release.png