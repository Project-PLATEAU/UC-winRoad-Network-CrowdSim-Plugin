unit SensorDataLoaderSettingForm;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.StrUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.Samples.Spin,
    Vcl.ComCtrls,
    PluginCore,
    TrafficSensor,
    TrafficSensorDat;

type
    ImportSensorsDataProc      = procedure of Object;
    ImportSensorDetailsProc    = procedure of Object;
    ImportInOutDatDataProc     = procedure of Object;
    ImportGenderOldDatDataProc = procedure of Object;

    /// <summary>
    ///    断面交通流データの読み込み設定および読み込みを行うフォームとその機能を定義するクラス
    /// </summary>
    TFormSensorDataLoaderSetting = class(TForm)
        PanelFutter: TPanel;
        BevelFutter: TBevel;
        ButtonCancel: TButton;
        ButtonOK: TButton;
        PanelImportJson: TPanel;
        ButtonImportJson: TButton;
        LabelImportedJson: TLabel;
        CheckBoxAddedData: TCheckBox;
        ButtonImportdetailJson: TButton;
        LabelImporteddetailJson: TLabel;
        OpenDialogJson: TOpenDialog;
        OpenDialogDetailJson: TOpenDialog;
        OpenDialogDat: TOpenDialog;
        OpenDialogGenderOldDat: TOpenDialog;
        PageControlImport: TPageControl;
        TabSheetJson: TTabSheet;
        TabSheetDat: TTabSheet;
        PanelImportDat: TPanel;
        LabelImportedDat: TLabel;
        LabelImportedGenderOldDat: TLabel;
        LabelImportDatData: TLabel;
        ButtonImportDat: TButton;
        ButtonImportGenderOldDat: TButton;
        LabelDataRetrievedTime: TLabel;
        MemoImportDataJson: TMemo;
        LabelID: TLabel;
        LabelImportDataIndex: TLabel;
        SpinEditIndex: TSpinEdit;
        MemoImportDataDat: TMemo;
        SpinEditHour: TSpinEdit;
        SpinEditMinute: TSpinEdit;
        ComboBoxID: TComboBox;
        PanelJsonDataMemo: TPanel;
        PanelDatDataMemo: TPanel;
        procedure ButtonImportJsonClick(Sender: TObject);
        procedure ButtonImportdetailJsonClick(Sender: TObject);
        procedure ButtonImportDatClick(Sender: TObject);
        procedure ButtonImportGenderOldDatClick(Sender: TObject);
        procedure CheckBoxAddedDataClick(Sender: TObject);
        procedure ButtonOKClick(Sender: TObject);
        procedure SpinEditTimeChange(Sender: TObject);
        procedure IDorIndexChange(Sender: TObject);
        private
            p_project : IF8ProjectForRoad;
            p_app     : IF8ApplicationServices;
            p_addData : boolean;

            p_SensorData         : TrafficSensorListClass;
            p_DatData            : TrafficSensorDatClass;
            p_ImportedSensorData : boolean;
            p_ImportedDetailData : boolean;
            p_ImportedDatData    : boolean;

            F_OnImportSensorsData   : ImportSensorsDataProc;
            F_OnImportSensorDetails : ImportSensorDetailsProc;
            F_OnImportInOutDat      : ImportInOutDatDataProc;
            F_OnImportGenderOldDat  : ImportGenderOldDatDataProc;

            procedure UpdateComboBoxIDItems(const SensorData: TrafficSensorListClass);
            procedure UpdateAppliedJsonData;
            procedure UpdateAppliedDatData;

            function  GetSensorData : TrafficSensorListClass;
            function  GetDatData    : TrafficSensorDatClass;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure SetSensorData(aData: TrafficSensorListClass);
            procedure SetDatData(aData: TrafficSensorDatClass);

            property app        : IF8ApplicationServices  read p_app     write p_app;
            property project    : IF8ProjectForRoad       read p_project write p_project;
            property SensorData : TrafficSensorListClass  read GetSensorData write SetSensorData;
            property DatData    : TrafficSensorDatClass   read GetDatData write SetDatData;

            property ImportedSensorData : boolean read p_ImportedSensorData write p_ImportedSensorData;
            property ImportedDetailData : boolean read p_ImportedDetailData write p_ImportedDetailData;
            property ImportedDatData    : boolean read p_ImportedDatData    write p_ImportedDatData;

            property OnImportSensorsData   : ImportSensorsDataProc      read F_OnImportSensorsData   write F_OnImportSensorsData;
            property OnImportSensorDetails : ImportSensorDetailsProc    read F_OnImportSensorDetails write F_OnImportSensorDetails;
            property OnImportInOutDat      : ImportInOutDatDataProc     read F_OnImportInOutDat      write F_OnImportInOutDat;
            property OnImportGenderOldDat  : ImportGenderOldDatDataProc read F_OnImportGenderOldDat  write F_OnImportGenderOldDat;
        end;

implementation

uses
    TrafficSensorLoader;

{$R *.dfm}

{ TFormSensorDataLoaderSetting }
//==============================================================================
procedure TFormSensorDataLoaderSetting.AfterConstruction;
    begin
    inherited;
    p_addData := false;

    LabelImportedJson.Caption := 'センサデータ：未選択';
    LabelImporteddetailJson.Caption := 'センサ詳細データ：未選択';
    LabelImportedDat.Caption := '入退場人流データ：未選択';
    LabelImportedGenderOldDat.Caption := '性別・年齢別人流データ：未選択';

    p_SensorData := nil;
    p_DatData    := nil;
    p_ImportedSensorData := false;
    p_ImportedDetailData := false;
    p_ImportedDatData := false;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.BeforeDestruction;
    begin
    inherited;
    p_SensorData := nil;
    p_DatData    := nil;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.ButtonImportJsonClick(Sender: TObject);
    begin
    OpenDialogJson.DefaultExt := 'json';
    OpenDialogJson.Filter := 'Jsonファイル|*.json';
    if OpenDialogJson.Execute then
        begin
        OnImportSensorsData;
        UpdateComboBoxIDItems(p_SensorData);
        UpdateAppliedJsonData;
        LabelImportedJson.Caption := 'センサデータ：' + OpenDialogJson.Files.Count.ToString + '個のファイルを追加';
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.ButtonImportdetailJsonClick(Sender: TObject);
    begin
    OpenDialogdetailJson.DefaultExt := 'json';
    OpenDialogdetailJson.Filter := 'Jsonファイル|*.json';
    if OpenDialogdetailJson.Execute then
        begin
        OnImportSensorDetails;
        UpdateAppliedJsonData;
        LabelImporteddetailJson.Caption := 'センサ詳細データ：' + OpenDialogdetailJson.Files.Count.ToString + '個のファイルを追加';
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.ButtonImportDatClick(Sender: TObject);
    var
        name : string;
    begin
    OpenDialogDat.DefaultExt := 'dat';
    OpenDialogDat.Filter := 'Datファイル|*.dat';
    if OpenDialogDat.Execute then
        begin
        OnImportInOutDat;
        name := OpenDialogDat.FileName;
        LabelImportedDat.Caption := '入退場人流データ：' + name.Split(['\'])[Length(name.Split(['\'])) - 1];
        UpdateAppliedDatData;
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.ButtonImportGenderOldDatClick(Sender: TObject);
    var
        name : string;
    begin
    OpenDialogGenderOldDat.DefaultExt := 'dat';
    OpenDialogGenderOldDat.Filter := 'Datファイル|*.dat';
    if OpenDialogGenderOldDat.Execute then
        begin
        OnImportGenderOldDat;
        name := OpenDialogGenderOldDat.FileName;
        LabelImportedGenderOldDat.Caption := '性別・年齢別人流データ：' + name.Split(['\'])[Length(name.Split(['\'])) - 1];
        UpdateAppliedDatData;
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.UpdateComboBoxIDItems(const SensorData: TrafficSensorListClass);
    begin
    if ComboBoxID.Items.Count > 0 then
        ComboBoxID.Items.Clear;

    if SensorData.numberOfTrafficSensorData > 0 then
        begin
        for var i := 0 to SensorData.numberOfTrafficSensorData - 1 do
            begin
            if SensorData.TrafficSensorData[i].ID <> 'null' then
                ComboBoxID.Items.Add(SensorData.TrafficSensorData[i].ID);
            end;

        ComboBoxID.ItemIndex := 0;
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.IDorIndexChange(Sender: TObject);
    begin
    UpdateAppliedJsonData;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.SpinEditTimeChange(Sender: TObject);
    begin
    UpdateAppliedDatData;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.UpdateAppliedJsonData;

    //--------------------------------------------------------------------------
    procedure AddBlesensorData(const i, ind: integer);
        begin
        MemoImportDataJson.lines.Add('DataType：Blesensor');
        MemoImportDataJson.lines.Add('ID：'+p_SensorData.TrafficSensorData[i].BleSensorData.id);
        if (p_SensorData.TrafficSensorData[i].BleSensorData.identifcation.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.identifcation.Count) then
            MemoImportDataJson.lines.Add('identifcation：'+p_SensorData.TrafficSensorData[i].BleSensorData.identifcation[ind].attrvalue);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedFrom.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedFrom.Count) then
            MemoImportDataJson.lines.Add('dateObservedFrom：'+DateTimeToStr(p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedFrom[ind].attrValue));

        if (p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedTo.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedTo.Count) then
            MemoImportDataJson.lines.Add('dateObservedTo：'+DateTimeToStr(p_SensorData.TrafficSensorData[i].BleSensorData.dateObservedTo[ind].attrValue));

        if (p_SensorData.TrafficSensorData[i].BleSensorData.dateRetrieved.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.dateRetrieved.Count) then
            MemoImportDataJson.lines.Add('dateRetrieved：'+DateTimeToStr(p_SensorData.TrafficSensorData[i].BleSensorData.dateRetrieved[ind].attrValue));

        MemoImportDataJson.lines.Add('latitude：'+p_SensorData.TrafficSensorData[i].BleSensorData.latitude.ToString);
        MemoImportDataJson.lines.Add('longitude：'+p_SensorData.TrafficSensorData[i].BleSensorData.longitude.ToString);
        MemoImportDataJson.lines.Add('locationName：'+p_SensorData.TrafficSensorData[i].BleSensorData.locationName);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountFar.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountFar.Count) then
            MemoImportDataJson.lines.Add('peopleCountFar：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountFar[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountNear.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountNear.Count) then
            MemoImportDataJson.lines.Add('peopleCountNear：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountNear[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountImmedate.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountImmedate.Count) then
            MemoImportDataJson.lines.Add('peopleCountImmedate：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleCountImmedate[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyFar.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyFar.Count) then
            MemoImportDataJson.lines.Add('peopleOccupancyFar：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyFar[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyNear.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyNear.Count) then
            MemoImportDataJson.lines.Add('peopleOccupancyNear：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyNear[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyImmedate.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyImmedate.Count) then
            MemoImportDataJson.lines.Add('peopleOccupancyImmedate：'+p_SensorData.TrafficSensorData[i].BleSensorData.peopleOccupancyImmedate[ind].attrValue.tostring);
        end;

    //--------------------------------------------------------------------------
    procedure AddEventData(const i, ind: integer);
        begin
        MemoImportDataJson.lines.Add('DataType：Event');
        MemoImportDataJson.lines.Add('ID：'+p_SensorData.TrafficSensorData[i].EventData.id);

        if (p_SensorData.TrafficSensorData[i].EventData.dataCreated.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.dataCreated.Count) then
            MemoImportDataJson.lines.Add('dataCreated：'+DateTimeToStr(p_SensorData.TrafficSensorData[i].EventData.dataCreated[ind].attrValue));

        if (p_SensorData.TrafficSensorData[i].EventData.dateModified.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.dateModified.Count) then
            MemoImportDataJson.lines.Add('dateModified：'+DateTimeToStr(p_SensorData.TrafficSensorData[i].EventData.dateModified[ind].attrValue));

        if (p_SensorData.TrafficSensorData[i].EventData.detailedUrl.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.detailedUrl.Count) then
            MemoImportDataJson.lines.Add('detailedUrl：'+p_SensorData.TrafficSensorData[i].EventData.detailedUrl[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.startDate.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.startDate.Count) then
            MemoImportDataJson.lines.Add('startDate：'+p_SensorData.TrafficSensorData[i].EventData.startDate[ind].attrvalue);

        if (p_SensorData.TrafficSensorData[i].EventData.endDate.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.endDate.Count) then
            MemoImportDataJson.lines.Add('endDate：'+p_SensorData.TrafficSensorData[i].EventData.endDate[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.image.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.image.Count) then
            MemoImportDataJson.lines.Add('image：'+p_SensorData.TrafficSensorData[i].EventData.image[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.name.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.name.Count) then
            MemoImportDataJson.lines.Add('name：'+p_SensorData.TrafficSensorData[i].EventData.name[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.no.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.no.Count) then
            MemoImportDataJson.lines.Add('no：'+p_SensorData.TrafficSensorData[i].EventData.no[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.summary.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.summary.Count) then
            MemoImportDataJson.lines.Add('summary：'+p_SensorData.TrafficSensorData[i].EventData.summary[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.latitude.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.latitude.Count) then
            MemoImportDataJson.lines.Add('latitude：'+p_SensorData.TrafficSensorData[i].EventData.latitude[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].EventData.longitude.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.longitude.Count) then
            MemoImportDataJson.lines.Add('longitude：'+p_SensorData.TrafficSensorData[i].EventData.longitude[ind].attrValue.tostring);

        if (p_SensorData.TrafficSensorData[i].EventData.locationName.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.locationName.Count) then
            MemoImportDataJson.lines.Add('locationName：'+p_SensorData.TrafficSensorData[i].EventData.locationName[ind].attrValue);

        if (p_SensorData.TrafficSensorData[i].EventData.locationAddress.Count > 0) and (ind < p_SensorData.TrafficSensorData[i].EventData.locationAddress.Count) then
            MemoImportDataJson.lines.Add('locationAddress：'+p_SensorData.TrafficSensorData[i].EventData.locationAddress[ind].attrValue);
        end;

    //--------------------------------------------------------------------------
    begin
    if not Assigned(p_SensorData) then
        Exit;

    MemoImportDataJson.lines.Clear;
    var ind := SpinEditIndex.Value;
    if p_SensorData.numberOfTrafficSensorData > 0 then
        begin
        for var i := 0 to p_SensorData.numberOfTrafficSensorData - 1 do
            begin
            if (p_SensorData.TrafficSensorData[i].ID = ComboBoxID.Text) and (ContainsStr(p_SensorData.TrafficSensorData[i].ID, 'Blesensor')) then
                AddBlesensorData(i, ind)
            else if (p_SensorData.TrafficSensorData[i].ID = ComboBoxID.Text) and (ContainsStr(p_SensorData.TrafficSensorData[i].ID, 'Event')) then
                AddEventData(i, ind);
            end;

        MemoImportDataJson.SelStart := MemoImportDataJson.Perform(EM_LINEINDEX, 0, 0);
        MemoImportDataJson.Perform(WM_VSCROLL,SB_TOP,0);
        end;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.UpdateAppliedDatData;

    //--------------------------------------------------------------------------
    procedure AddInOutData(const i: integer);
        begin
        MemoImportDataDat.lines.Add('peopleflow In Out');
        MemoImportDataDat.lines.Add('time,in,out,all');
        MemoImportDataDat.lines.Add(p_DatData.InOutData[i].time+','+p_DatData.InOutData[i].peoplein.tostring+','+
                                    p_DatData.InOutData[i].peopleout.tostring+','+p_DatData.InOutData[i].peopleall.tostring);
        MemoImportDataDat.lines.Add(' ');
        end;

    //--------------------------------------------------------------------------
    procedure AddGenderOldData(const i: integer);
        begin
        MemoImportDataDat.lines.Add('peopleflow Gender Old');
        MemoImportDataDat.lines.Add('time,0m,0f,10m,10f,20m,20f,30m,30f,40m,40f,50m,50f,60m,60f');
        MemoImportDataDat.lines.Add(p_DatData.GenderOldData[i].time+','+p_DatData.GenderOldData[i].maleover0.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover0.tostring+','+p_DatData.GenderOldData[i].maleover10.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover10.tostring+','+p_DatData.GenderOldData[i].maleover20.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover20.tostring+','+p_DatData.GenderOldData[i].maleover30.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover30.tostring+','+p_DatData.GenderOldData[i].maleover40.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover40.tostring+','+p_DatData.GenderOldData[i].maleover50.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover50.tostring+','+p_DatData.GenderOldData[i].maleover60.tostring+','+
                                    p_DatData.GenderOldData[i].femaleover60.tostring+','+p_DatData.GenderOldData[i].peopleall.tostring);
        end;

    //--------------------------------------------------------------------------
    begin
    if not Assigned(p_DatData) then
        Exit;

    MemoImportDataDat.lines.Clear;
    var hour := Format('%.2u', [SpinEditHour.Value]);
    var minute := Format('%.2u', [SpinEditMinute.Value]);
    if p_DatData.InOutData.Count > 0 then
        begin
        for var i := 0 to p_DatData.InOutData.Count - 1 do
            begin
            if p_DatData.InOutData[i].time = (hour+':'+minute) then
                AddInOutData(i);
            end;
        end;

    if p_DatData.GenderOldData.Count > 0 then
        begin
        for var i := 0 to p_DatData.GenderOldData.Count - 1 do
            begin
            if p_DatData.GenderOldData[i].time = (hour+':'+minute) then
                AddGenderOldData(i);
            end;
        end;

    MemoImportDataDat.SelStart := MemoImportDataDat.Perform(EM_LINEINDEX, 0, 0);
    MemoImportDataDat.Perform(WM_VSCROLL,SB_TOP,0);
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.CheckBoxAddedDataClick(Sender: TObject);
    begin
    p_addData := CheckBoxAddedData.checked;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.ButtonOKClick(Sender: TObject);
    begin
    var ImportedSensorsData := (p_ImportedSensorData and p_ImportedDetailData);
    if ImportedSensorsData or p_ImportedDatData then
        begin
        ModalResult := mrOk;
        end
    else
        begin
        if (ImportedSensorsData = false) and (p_ImportedDatData = false) then
            begin
            if (p_ImportedSensorData = false) and (p_ImportedDetailData = false) then
                ShowMessage('データがいずれもインポートされていません')
            else
                ShowMessage('センサデータまたはセンサ詳細データがインポートされていません')
            end;
        end;
    end;

//==============================================================================
function TFormSensorDataLoaderSetting.GetSensorData : TrafficSensorListClass;
    begin
    Result := p_SensorData;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.SetSensorData(aData: TrafficSensorListClass);
    begin
    p_SensorData := aData;
    end;

//==============================================================================
function TFormSensorDataLoaderSetting.GetDatData : TrafficSensorDatClass;
    begin
    Result := p_DatData;
    end;

//==============================================================================
procedure TFormSensorDataLoaderSetting.SetDatData(aData: TrafficSensorDatClass);
    begin
    p_DatData := aData;
    end;
end.
