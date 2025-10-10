unit F_PedestrianSettings;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.ImageList,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Grids,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.ImgList,
    Vcl.Buttons,
    VirtualTrees,
    VirtualButtonTree,
    VTEditors,
    ComboDrawingHelper,
    PluginCore,
    F8VTEditors,
    PedestrianCategoryData,
    PedestrianProfileOptionData,
    PedestrianCategoryEditorForm,
    PopOutPointList,
    SimulationInputUtils;

const
    NAME_COL      = 0;
    MODEL_COL     = 1;
    MIN_AGE_COL   = 2;
    MAX_AGE_COL   = 3;
    GENDER_COL    = 4;
    CATEGORY_COL  = 5;
    TAKE_BUS_COL  = 6;
    PASS_GATE_COL = 7;

type
    TPedestrianSettingDataType = record
        Name          : string;
        //Model         : IF8QuakeIII;
        ModelIndex    : integer;
        MinAge        : integer;
        MaxAge        : integer;
        Gender        : integer;
        CategoryIndex : integer;
        end;

    PPedestrianSettingDataType = ^TPedestrianSettingDataType;

    TFramePedestrianSettings = class(TFrame)
        PanelMain: TPanel;
        vbtPedestrianSettings: TVirtualButtonTree;
        btnAdd: TSpeedButton;
        btnDel: TSpeedButton;
        ImageList1: TImageList;
        PanelFotter: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        lbCategory: TListBox;
        btnEditCategory: TButton;
        Label1: TLabel;
        btnCateAdd: TSpeedButton;
        btnCateDel: TSpeedButton;
        Panel1: TPanel;
        procedure AfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
        procedure btnAddClick(Sender: TObject);
        procedure btnDelClick(Sender: TObject);
        procedure InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
        procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
        procedure CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
        procedure FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
        procedure Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
        procedure vbtExit(Sender: TObject);
        procedure PedCateEditorOKClick(Sender: TObject);
        procedure PedCateEditorCancelClick(Sender: TObject);
        procedure btnEditCategoryClick(Sender: TObject);
        procedure btnCateAddClick(Sender: TObject);
        procedure btnCateDelClick(Sender: TObject);
        procedure lbCategoryClick(Sender: TObject);
        procedure vbtPedestrianSettingsColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
        private
            p_tmpNode              : PVirtualNode;
            p_tmpCol               : integer;
            p_characterList        : TStringList;
            p_genderList           : TStringList;
            p_comboDrawingHelperIF : IComboCustomDraw;
            p_currentEditLink      : TCustomEditLink;
            p_currentEditLinkIF    : IVTEditLink;
            p_PedestrianProfileOptionList  : TPedestrianProfileOptionList;
            p_EditingProfList              : TPedestrianProfileOptionList;
            p_EditingCategories            : TPedestrianCategoryList;
            p_CategoryList                 : TStringList;
            p_PedestrianCategoryEditorForm : TFormPedestrianCategoryEditor;
            p_EditingLabelList : TList<TPedestrianData>;

            procedure DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean = false);
            procedure ShowCategoryEditor;
            function  GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
            procedure SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
            function  IsSampleCategory(const CateName: string): boolean;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure ResetNodes;
            procedure HideCategoryEditor;
            procedure SetEditPedCategories;
            procedure SetNewCategories;
            procedure SendLatestPedProfOptData;
            procedure ReceiveLatestPedProfOptData;
            procedure ReceivePedestrianLabelList(const LblList: TList<TPedestrianData>);
            procedure SendNewPedestrianLabelList(out LblList: TList<TPedestrianData>);

            property  PedestrianProfileOptionList : TPedestrianProfileOptionList read GetPedestrianProfileOptionList write SetPedestrianProfileOptionList;
            property  EditingCategories           : TPedestrianCategoryList      read p_EditingCategories write p_EditingCategories;
            property  EditingLabelList : TList<TPedestrianData> read p_EditingLabelList write p_EditingLabelList;
        end;

    TComboEditLink = class(TF8GraphicComboEditLink)
        private
            p_selectedObject : TObject;
            p_selectedIndex  : integer;
        protected
            procedure ComboChange(Sender: TObject);
            procedure PrepareEditControl; override;
        public
            property selectedObject : TObject read p_selectedObject write p_selectedObject;
            property selectedIndex  : Integer read p_selectedIndex  write p_selectedIndex;
        end;

implementation

{$R *.dfm}

{ TFramePedestrianSettings }

procedure TFramePedestrianSettings.AfterConstruction;
    var
        i          : integer;
        gridColumn : TVirtualTreeColumn;
        ch         : IF8QuakeIII;

    procedure AddColumn(const ACol: integer; Lbl: string; ColWidth: integer);
        begin
        gridColumn          := vbtPedestrianSettings.Header.Columns.Add();
        gridColumn.Position := ACol;
        gridColumn.Text     := Lbl;
        gridColumn.Width    := ColWidth;
        end;

    begin
    inherited;

    with vbtPedestrianSettings do
        begin
        DefaultNodeHeight := 80;
        EditDelay := 0;
        Header.Height := 20;
        Margin := 0;
        ScrollBarOptions.ScrollBars  := ssVertical;
        TreeOptions.MiscOptions      := TreeOptions.MiscOptions + [toCheckSupport, toGridExtensions];
        TreeOptions.PaintOptions     := TreeOptions.PaintOptions + [toShowHorzGridLines, toShowVertGridLines];
        TreeOptions.PaintOptions     := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines];
        TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus];
        WantTabs := true;
        HintMode := hmTooltip;
        ShowHint := true;
        end;

    vbtPedestrianSettings.NodeDataSize := SizeOf(TPedestrianSettingDataType);
    vbtPedestrianSettings.Header.Options := vbtPedestrianSettings.Header.Options + [hoColumnResize];
    AddColumn(NAME_COL,      '属性名',           60);
    AddColumn(MODEL_COL,     'モデル',           80);
    AddColumn(MIN_AGE_COL,   '年齢(下限)',       60);
    AddColumn(MAX_AGE_COL,   '年齢(上限)',       60);
    AddColumn(GENDER_COL,    '性別',             40);
    AddColumn(CATEGORY_COL,  '動作(歩行速度等)', 100);
    for i := 0 to (vbtPedestrianSettings.Header.Columns.Count - 1) do
        begin
        gridColumn         := vbtPedestrianSettings.Header.Columns[i];
        gridColumn.Options := gridColumn.Options + [coFixed];
        gridColumn.Margin  := 0;
        end;

    vbtPedestrianSettings.Margin := 0;
    vbtPedestrianSettings.TreeOptions.PaintOptions := vbtPedestrianSettings.TreeOptions.PaintOptions - [toFullVertGridLines];
    vbtPedestrianSettings.Header.Options := vbtPedestrianSettings.Header.Options + [hoVisible];
    vbtPedestrianSettings.Header.Options := vbtPedestrianSettings.Header.Options - [hoDrag];
    p_comboDrawingHelperIF := TComboDrawingHelperClass.Create() as IComboCustomDraw;
    p_characterList   := TStringList.Create();
    p_genderList      := TStringList.Create();
    p_EditingProfList := TPedestrianProfileOptionList.Create;
    p_genderList.Add('男');
    p_genderList.Add('女');
    for i := 1 to theApplicationServices.project.numberOfCharacters do
        begin
        ch := theApplicationServices.project.character[i];
        p_characterList.AddObject(ch.name, TObject(ch));
        end;

    p_CategoryList := TStringList.Create();
    p_EditingCategories := TPedestrianCategoryList.Create;
    end;

procedure TFramePedestrianSettings.BeforeDestruction;
    begin
    inherited;

    p_currentEditLinkIF := nil;
    p_currentEditLink   := nil;
    p_comboDrawingHelperIF := nil;

    FreeAndNil(p_EditingProfList);
    FreeAndNil(p_genderList);
    if Assigned(p_characterList) then
        FreeAndNil(p_characterList);

    if Assigned(p_CategoryList) then
        FreeAndNil(p_CategoryList);

    FreeAndNil(p_EditingCategories);

    if Assigned(p_EditingLabelList) then
        FreeAndNil(p_EditingLabelList);
    end;

procedure TFramePedestrianSettings.btnAddClick(Sender: TObject);
    var
        pNode  : PVirtualNode;
        pData  : PPedestrianSettingDataType;
        newOpt : TPedestrianProfileOptionData;
        i : integer;
    begin
    try
        pNode  := vbtPedestrianSettings.AddChild(nil);
        pData  := vbtPedestrianSettings.GetNodeData(pNode);
        newOpt := TPedestrianProfileOptionData.Create;
        newOpt.Name := pData^.Name;
        p_EditingProfList.Data.Add(newOpt);
        p_EditingProfList.Data[p_EditingProfList.Data.Count - 1].CategoryIndex := 0;
        if Assigned(p_EditingLabelList) then
            begin
            for i := 0 to p_EditingLabelList.Count - 1 do
                begin
                if p_EditingLabelList[i].AttrPerList.Count = 0 then
                    p_EditingLabelList[i].AttrPerList.Add(100)
                else
                    p_EditingLabelList[i].AttrPerList.Add(0);
                end;

            end;
    finally
        vbtPedestrianSettings.InvalidateChildren(nil, false);
        pNode := vbtPedestrianSettings.GetFirst();
        vbtPedestrianSettings.FocusedNode := pNode;
        end;
    end;

procedure TFramePedestrianSettings.btnDelClick(Sender: TObject);
    var
        i : integer;
    begin
    if p_tmpNode^.Index = 0 then
        Exit; //デフォルト設定は削除不可

    p_EditingProfList.Data.Delete(p_tmpNode^.Index);
    vbtPedestrianSettings.DeleteSelectedNodes;
    if Assigned(p_EditingLabelList) then
        begin
        for i := 0 to p_EditingLabelList.Count - 1 do
            p_EditingLabelList[i].AttrPerList.Delete(p_tmpNode^.Index);
        end;

    btnDel.Enabled := false;
    end;

procedure TFramePedestrianSettings.btnEditCategoryClick(Sender: TObject);
    begin
    ShowCategoryEditor;
    end;

procedure TFramePedestrianSettings.AfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    var
        itemIndex   : Integer;
        saveColour  : TColor;
        text        : WideString;
        thumbNail   : TBitmap;
        ch          : IF8QuakeIII;
        pData       : PPedestrianSettingDataType;
    begin
    pData := Sender.GetNodeData(Node);
    case Column of
        MODEL_COL:
            begin
            if pData^.ModelIndex < 0 then
                Exit;

            saveColour := TargetCanvas.Font.Color;
            TargetCanvas.Font.Color := clWindowText;
            try
                itemIndex := pData^.ModelIndex;
                if itemIndex <= 0 then
                    begin
                    text := '';
                    thumbnail := nil;
                    end
                else
                    begin
                    ch := theApplicationServices.project.character[itemIndex];
                    text := ch.name;
                    thumbnail := ch.thumbnail;
                    end;

                if Assigned(thumbnail) then
                    DrawBitmapInRect(thumbnail, text, TargetCanvas, CellRect);
            finally
                TargetCanvas.Font.Color := saveColour;
                end;
            end;
        end;
    end;

procedure TFramePedestrianSettings.DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean = false);
    const
        OFFSET = 2;
    var
        displayedName: String;
        lastNameCharacterIndex: Integer;
        maxTextWidth: Integer;
        textExtent: TSize;
        textWidth: Integer;
    begin
    canvas.FillRect(rect);
    if Assigned(thumbnail) then
        BitBlt(canvas.Handle, rect.Left + 1, rect.Top + 1, rect.Right - rect.Left - 1,
                              rect.Bottom - rect.Top - 1, thumbnail.Canvas.Handle, 1, 1, SRCCOPY);

    displayedName := name;
    textExtent := Canvas.TextExtent(displayedName);
    if useEllipsisIfTooLong then
        begin
        maxTextWidth := rect.Right - (rect.Left + OFFSET) - 1;
        textWidth := textExtent.cx;
        if textWidth > maxTextWidth then
            begin
            lastNameCharacterIndex := Length(displayedName);
            displayedName := displayedName + '...';
            repeat
                Delete(displayedName, lastNameCharacterIndex, 1);
                Dec(lastNameCharacterIndex);
                textWidth := Canvas.TextWidth(displayedName);
            until (textWidth <= maxTextWidth) or (lastNameCharacterIndex = 1);
            end;
        end;

    Canvas.TextOut(rect.Left + OFFSET, rect.Bottom - textExtent.cy - 2, displayedName);
    end;

procedure TFramePedestrianSettings.InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    var
        pIndex  : Integer;
        pData   : PPedestrianSettingDataType;
    begin
    pData := Sender.GetNodeData(Node);
    pIndex := Node^.Index;

    if pIndex <= p_EditingProfList.Data.Count - 1 then
        begin
        pData^.Name          := p_EditingProfList.Data[pIndex].Name;
        pData^.ModelIndex         := p_EditingProfList.Data[pIndex].ModelIndex;
        pData^.MinAge        := p_EditingProfList.Data[pIndex].MinAge;
        pData^.MaxAge        := p_EditingProfList.Data[pIndex].MaxAge;
        pData^.Gender        := p_EditingProfList.Data[pIndex].Gender;
        pData^.CategoryIndex := p_EditingProfList.Data[pIndex].CategoryIndex;
        end
    else
        begin
        pData^.Name          := '歩行者'+ (pIndex + 1).ToString;
        pData^.ModelIndex         := -1;
        pData^.MinAge        := 10;
        pData^.MaxAge        := 60;
        pData^.Gender        := 0;
        pData^.CategoryIndex := -1;
        end;
    end;

procedure TFramePedestrianSettings.NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    var
        pData   : PPedestrianSettingDataType;
        newModel: IF8QuakeIII;
    begin
    pData := Sender.GetNodeData(Node);
    case Column of
        NAME_COL:
            begin
            pData^.Name := NewText;
            p_EditingProfList.Data[Node^.Index].Name := NewText;
            end;
        MODEL_COL:
            begin
            if Supports(TComboEditLink(p_currentEditLink).selectedObject, IF8QuakeIII, newModel) then
                begin
                for var i := 1 to theApplicationServices.project.numberOfCharacters do
                    begin
                    if theApplicationServices.project.character[i].GUID = newModel.GUID then
                        begin
                        if (pData^.ModelIndex < 0) or (pData^.ModelIndex <> i) then
                            begin
                            pData^.ModelIndex := i;
                            p_EditingProfList.Data[Node^.Index].ModelIndex := i;
                            end;
                        end;
                    end;
                end;
            end;
        MIN_AGE_COL:
            begin
            pData^.MinAge := StrToIntDef(NewText, 10);
            p_EditingProfList.Data[Node^.Index].MinAge := StrToIntDef(NewText, 10);
            end;
        MAX_AGE_COL:
            begin
            pData^.MaxAge := StrToIntDef(NewText, 60);
            p_EditingProfList.Data[Node^.Index].MaxAge := StrToIntDef(NewText, 60);
            end;
        GENDER_COL:
            begin
            pData^.Gender := TComboEditLink(p_currentEditLink).selectedIndex;
            p_EditingProfList.Data[Node^.Index].Gender := TComboEditLink(p_currentEditLink).selectedIndex;
            end;
        CATEGORY_COL:
            begin
            if (Node^.Index = 0) then
                Exit;

            pData^.CategoryIndex := TComboEditLink(p_currentEditLink).selectedIndex;
            p_EditingProfList.Data[Node^.Index].CategoryIndex := TComboEditLink(p_currentEditLink).selectedIndex;
            end;
        end;
    end;

procedure TFramePedestrianSettings.vbtExit(Sender: TObject);
    begin
    vbtPedestrianSettings.Invalidate;
    end;

procedure TFramePedestrianSettings.vbtPedestrianSettingsColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
    begin
    case Column of
        NAME_COL, MIN_AGE_COL, MAX_AGE_COL, GENDER_COL, CATEGORY_COL:
            begin
            if not Assigned(vbtPedestrianSettings.FocusedNode) then
                Exit;

            if (vbtPedestrianSettings.FocusedNode.Index = 0) then
                begin
                vbtPedestrianSettings.FocusedNode := nil;
                Exit;
                end;
            end;
        end;
    end;

procedure TFramePedestrianSettings.Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    begin
    Allowed := True;
    end;

procedure TFramePedestrianSettings.FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    begin
    if not Self.Enabled then
        Exit;

    if Assigned(Node) then
        begin
        p_tmpNode := Node;
        p_tmpCol  := Column;
        Sender.EditNode( Node, Column );
        if Node^.Index = 0 then
            btnDel.Enabled := false
        else
            btnDel.Enabled := true;
        end;
    end;

procedure TFramePedestrianSettings.btnCateAddClick(Sender: TObject);
    var
        newData : TPedestrianCategoryData;
        MaxIdx  : integer;
    begin
    newData      := TPedestrianCategoryData.Create;
    newData.Name := '歩行者設定' + (lbCategory.Items.Count).ToString;
    lbCategory.Items.Add('歩行者設定' + (lbCategory.Items.Count).ToString);
    p_EditingCategories.Data.Add(newData);
    MaxIdx := p_EditingCategories.Data.Count - 1;
    p_CategoryList.AddObject(p_EditingCategories.Data[MaxIdx].Name, TObject(p_EditingCategories.Data[MaxIdx]));
    end;

procedure TFramePedestrianSettings.btnCateDelClick(Sender: TObject);
    begin
    if lbCategory.ItemIndex < 0 then
        Exit;

    p_EditingCategories.Data.Delete(lbCategory.ItemIndex);
    lbCategory.Items.Delete(lbCategory.ItemIndex);
    btnEditCategory.Enabled := false;
    btnCateDel.Enabled := false;
    end;

procedure TFramePedestrianSettings.ShowCategoryEditor;
    var
        SelIdx : integer;
    begin
    SelIdx := lbCategory.ItemIndex;
    if SelIdx < 0 then
        Exit;

    p_PedestrianCategoryEditorForm := TFormPedestrianCategoryEditor.Create(nil);
    p_PedestrianCategoryEditorForm.F_PedCateEditor.SetEditData(p_EditingCategories, SelIdx);
    p_PedestrianCategoryEditorForm.F_PedCateEditor.btnOK.OnClick     := PedCateEditorOKClick;
    p_PedestrianCategoryEditorForm.F_PedCateEditor.btnCancel.OnClick := PedCateEditorCancelClick;
    p_PedestrianCategoryEditorForm.Show;
    end;

procedure TFramePedestrianSettings.PedCateEditorOKClick(Sender: TObject);

    procedure UpdateCategoryData;
        var
            SelIdx : integer;
        begin
        SelIdx := lbCategory.ItemIndex;
        p_EditingCategories.Data[SelIdx].Name                := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.Name;
        p_EditingCategories.Data[SelIdx].StairUpPer          := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.StairUpPer;
        p_EditingCategories.Data[SelIdx].StairDownPer        := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.StairDownPer;
        p_EditingCategories.Data[SelIdx].EscaUpPer           := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.EscaUpPer;
        p_EditingCategories.Data[SelIdx].EscaUpWalkPer       := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.EscaUpWalkPer;
        p_EditingCategories.Data[SelIdx].EscaDownPer         := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.EscaDownPer;
        p_EditingCategories.Data[SelIdx].EscaDownWalkPer     := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.EscaDownWalkPer;
        p_EditingCategories.Data[SelIdx].ElevUpPer           := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.ElevUpPer;
        p_EditingCategories.Data[SelIdx].ElevDownPer         := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.ElevDownPer;
        p_EditingCategories.Data[SelIdx].CrosswalkBluePer    := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.CrosswalkBluePer;
        p_EditingCategories.Data[SelIdx].CrosswalkRedPer     := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.CrosswalkRedPer;
        p_EditingCategories.Data[SelIdx].StopCollision       := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.StopCollision;
        p_EditingCategories.Data[SelIdx].WalkCollision       := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.WalkCollision;
        p_EditingCategories.Data[SelIdx].WQWalkCollision     := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.WQWalkCollision;
        p_EditingCategories.Data[SelIdx].DoorOrGateCollision := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.DoorOrGateCollision;
        p_EditingCategories.Data[SelIdx].NormalSpeed         := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.NormalSpeed;
        p_EditingCategories.Data[SelIdx].StairUpSpeed        := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.StairUpSpeed;
        p_EditingCategories.Data[SelIdx].StairDownSpeed      := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.StairDownSpeed;
        p_EditingCategories.Data[SelIdx].EscaWalkSpeed       := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.EscaWalkSpeed;
        p_EditingCategories.Data[SelIdx].CrosswalkBlueSpeed  := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.CrosswalkBlueSpeed;
        p_EditingCategories.Data[SelIdx].CrosswalkRedSpeed   := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.CrosswalkRedSpeed;
        p_EditingCategories.Data[SelIdx].TakeBusTime         := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.TakeBusTime;
        p_EditingCategories.Data[SelIdx].PassGateTime        := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.PassGateTime;
        lbCategory.Items[SelIdx] := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.Name;
        p_CategoryList[SelIdx] := p_PedestrianCategoryEditorForm.F_PedCateEditor.EditingCategory.Name;
        end;

    begin
    UpdateCategoryData;
    p_PedestrianCategoryEditorForm.Close;
    FreeAndNil(p_PedestrianCategoryEditorForm);
    end;

procedure TFramePedestrianSettings.PedCateEditorCancelClick(Sender: TObject);
    begin
    p_PedestrianCategoryEditorForm.Close;
    FreeAndNil(p_PedestrianCategoryEditorForm);
    end;

procedure TFramePedestrianSettings.CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    var
        comboEditLink : TComboEditLink;
    begin
    EditLink            := nil;
    p_currentEditLinkIF := nil;
    p_currentEditLink   := nil;
    case Column of
        MODEL_COL:
            begin
            comboEditLink := TComboEditLink.Create(p_comboDrawingHelperIF);
            comboEditLink.PickList := p_characterList;
            EditLink               := comboEditLink;
            p_currentEditLink      := comboEditLink;
            p_currentEditLinkIF    := comboEditLink;
            end;
        GENDER_COL:
            begin
            comboEditLink := TComboEditLink.Create(nil);
            comboEditLink.PickList := p_genderList;
            EditLink               := comboEditLink;
            p_currentEditLink      := comboEditLink;
            p_currentEditLinkIF    := comboEditLink;
            end;
        CATEGORY_COL:
            begin
            comboEditLink := TComboEditLink.Create(nil);
            comboEditLink.PickList := p_CategoryList;
            EditLink               := comboEditLink;
            p_currentEditLink      := comboEditLink;
            p_currentEditLinkIF    := comboEditLink;
            end;
        end;
    end;

procedure TFramePedestrianSettings.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    var
        itemIndex : Integer;
        pData     : PPedestrianSettingDataType;
    begin
    pData := Sender.GetNodeData(Node);
    case Column of
        NAME_COL:
            CellText := pData^.Name;
        MODEL_COL:
            if pData^.ModelIndex < 0 then
                CellText := ''
            else
                begin
                itemIndex := pData^.ModelIndex;
                if itemIndex = -1 then
                    CellText := ''
                else
                    begin
                    CellText := p_characterList[itemIndex-1];
                    if CellText = '' then
                        begin
                        if pData^.ModelIndex > 0 then
                            CellText := theApplicationServices.project.character[pData^.ModelIndex].name
                        else
                            CellText := '';
                        end;
                    end;
                end;
        MIN_AGE_COL:
            CellText := pData^.MinAge.ToString;
        MAX_AGE_COL:
            CellText := pData^.MaxAge.ToString;
        GENDER_COL:
            begin
            if pData^.Gender = 0 then
                CellText := '男'
            else
                CellText := '女';
            end;
        CATEGORY_COL:
            begin
            if p_EditingCategories.Data.Count < 1 then
                CellText := '未設定'
            else
                begin
                if p_EditingProfList.Data[Node^.Index].CategoryIndex < 0 then
                    CellText := '未設定'
                else
                    CellText := p_EditingCategories.Data[p_EditingProfList.Data[Node^.Index].CategoryIndex].Name;
                end;
            end;
    else
        CellText := '';
        end;
    end;

function TFramePedestrianSettings.GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
    begin
    result := p_PedestrianProfileOptionList;
    end;

procedure TFramePedestrianSettings.SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
    var
        i       : integer;
        newData : TPedestrianProfileOptionData;
    begin
    p_PedestrianProfileOptionList := aValue;

    if p_EditingProfList.Data.Count > 0 then
        p_EditingProfList.Data.Clear;

    if not Assigned(aValue) then
        Exit;

    for i := 0 to aValue.Data.Count - 1 do
        begin
        newData := TPedestrianProfileOptionData.Create;
        newData.Name          := aValue.Data[i].Name;
        newData.Weight        := aValue.Data[i].Weight;
        newData.ModelIndex         := aValue.Data[i].ModelIndex;
        newData.MinAge        := aValue.Data[i].MinAge;
        newData.MaxAge        := aValue.Data[i].MaxAge;
        newData.Gender        := aValue.Data[i].Gender;
        newData.CategoryIndex := aValue.Data[i].CategoryIndex;
        p_EditingProfList.Data.Add(newData);
        end;
    end;

procedure TFramePedestrianSettings.SendLatestPedProfOptData;
    var
        pedProf : IF8PedestrianProfile;
        newData : TPedestrianProfileOptionData;
        i, j : integer;
    begin
    pedProf := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile];
    if p_EditingProfList.Data.Count > 0 then
        p_EditingProfList.Data.Clear;

    if not Assigned(pedProf) then
        Exit;

    for i := 1 to pedProf.numberOfOptions do
        begin
        newData := TPedestrianProfileOptionData.Create;
        if pedProf.option[i].Name = '' then
            pedProf.option[i].Name := 'デフォルト';

        newData.Name := pedProf.option[i].Name;
        for j := 1 to theApplicationServices.project.numberOfCharacters do
            begin
            if theApplicationServices.project.character[j].GUID = pedProf.option[i].GetModel.GUID then
                begin
                newData.ModelIndex := j;
                Break;
                end;
            end;

        newData.MinAge        := pedProf.option[i].YoungerAge;
        newData.MaxAge        := pedProf.option[i].OlderAge;
        if pedProf.option[i].Gender = _pdgMale then
            newData.Gender    := 0
        else
            newData.Gender    := 1;

        if IsSampleCategory(theApplicationServices.project.PedestrianCategoryIF[pedProf.option[i].PedestrianCategoryIndex].name) then
            newData.CategoryIndex := 0
        else
            newData.CategoryIndex := pedProf.option[i].PedestrianCategoryIndex;

        p_EditingProfList.Data.Add(newData);
        end;
    end;

procedure TFramePedestrianSettings.ReceiveLatestPedProfOptData;
    var
        pedProf : IF8PedestrianProfile;
        i, pedPIdx : integer;
        preWeights : TList<integer>;
    begin
    for pedPIdx := 1 to theApplicationServices.project.NumberOfPedestrianProfile do //属性の種類はすべてのProfileで一致する
        begin
        pedProf := theApplicationServices.project.PedestrianProfileIF[pedPIdx];
        if not Assigned(pedProf) then
            Exit;

        preWeights := TList<integer>.Create;
        for i := 1 to pedProf.numberOfOptions do
            preWeights.Add(pedProf.option[i].Weight);

        while pedProf.numberOfOptions > 0 do //既存のoptionにModelを渡す手段がないため一度初期化する必要がある
            pedProf.DeleteOption(1);

        for i := 0 to p_EditingProfList.Data.Count - 1 do
            begin
            pedProf.AddOptionFromGUID(theApplicationServices.project.character[p_EditingProfList.Data[i].ModelIndex].GUID);   //p_EditingProfList.Data[i].Model.CloneIF.GUID);
            pedProf.option[i+1].Name       := p_EditingProfList.Data[i].Name;
            pedProf.option[i+1].YoungerAge := p_EditingProfList.Data[i].MinAge;
            pedProf.option[i+1].OlderAge   := p_EditingProfList.Data[i].MaxAge;
            if p_EditingProfList.Data[i].Gender = 0 then
                pedProf.option[i+1].Gender := _pdgMale
            else
                pedProf.option[i+1].Gender := _pdgFemale;

            pedProf.option[i+1].maxSpeed := p_EditingCategories.Data[p_EditingProfList.Data[i].CategoryIndex].NormalSpeed;
            pedProf.option[i+1].TakeBusTime := p_EditingCategories.Data[p_EditingProfList.Data[i].CategoryIndex].TakeBusTime;
            pedProf.option[i+1].PassThroughTicketGateSec := p_EditingCategories.Data[p_EditingProfList.Data[i].CategoryIndex].PassGateTime;
            pedProf.option[i+1].pedestrianCategoryIndex := p_EditingProfList.Data[i].CategoryIndex;
            if i <= preWeights.Count - 1 then
                pedProf.option[i+1].Weight := preWeights[i]
            else
                pedProf.option[i+1].Weight := 0;
            end;

        theApplicationServices.project.PedestrianProfileIF[pedPIdx] := pedProf;
        FreeAndNil(preWeights);
        end;
    end;

function TFramePedestrianSettings.IsSampleCategory(const CateName: string): boolean;
    begin
    Result := false;
    if (CateName = SAMPLE_PEDESTRIAN_CATEGORY_1)
        or (CateName = SAMPLE_PEDESTRIAN_CATEGORY_2)
        or (CateName = SAMPLE_PEDESTRIAN_CATEGORY_3)
        or (CateName = SAMPLE_PEDESTRIAN_CATEGORY_4) then
        Result := true;
    end;

procedure TFramePedestrianSettings.lbCategoryClick(Sender: TObject);
    var
        SelIdx : integer;
    begin
    SelIdx := lbCategory.ItemIndex;
    if SelIdx <= 0 then
        begin
        btnEditCategory.Enabled := false;
        btnCateDel.Enabled := false;
        end
    else
        begin
        btnEditCategory.Enabled := true;
        btnCateDel.Enabled := true;
        end;
    end;

procedure TFramePedestrianSettings.SetEditPedCategories;
    var
        i, j, k : integer;
        newData : TPedestrianCategoryData;
        pedCate : IF8PedestrianCategory;
        pedProf : IF8PedestrianProfile;
        Breaked : boolean;
    begin
    p_EditingCategories.Data.Clear;
    if lbCategory.Items.Count > 0 then
        lbCategory.Items.Clear;

    for i := 0 to theApplicationServices.project.NumberOfPedestrianCategory - 1 do
        begin
        pedCate := theApplicationServices.project.PedestrianCategoryIF[i];
        if IsSampleCategory(pedCate.name) then
            Continue;

        newData                     := TPedestrianCategoryData.Create;
        newData.Name                := pedCate.Name;
        newData.StairUpPer          := Round(pedCate.adjustRate[_nlStairs, _nldUp, _ccaWalking].percentage);
        newData.StairDownPer        := Round(pedCate.adjustRate[_nlStairs, _nldDown, _ccaWalking].percentage);
        newData.EscaUpPer           := Round(pedCate.adjustRate[_nlEscalator, _nldUp, _ccaStanding].percentage);
        newData.EscaUpWalkPer       := Round(pedCate.adjustRate[_nlEscalator, _nldUp, _ccaWalking].percentage);
        newData.EscaDownPer         := Round(pedCate.adjustRate[_nlEscalator, _nldDown, _ccaStanding].percentage);
        newData.EscaDownWalkPer     := Round(pedCate.adjustRate[_nlEscalator, _nldDown, _ccaWalking].percentage);
        newData.ElevUpPer           := Round(pedCate.adjustRate[_nlElevator, _nldUp, _ccaStanding].percentage);
        newData.ElevDownPer         := Round(pedCate.adjustRate[_nlElevator, _nldDown, _ccaStanding].percentage);
        newData.CrosswalkBluePer    := Round(pedCate.adjustRate[_nlCrosswalkBlue, _nldNone, _ccaWalking].percentage);
        newData.CrosswalkRedPer     := Round(pedCate.adjustRate[_nlCrosswalkRed, _nldNone, _ccaWalking].percentage);
        newData.StopCollision       := pedCate.thicknessAction[_ccaStanding];
        newData.WalkCollision       := pedCate.thicknessAction[_ccaWalking];
        newData.WQWalkCollision     := pedCate.thicknessAction[_ccaWalkingQueue];
        newData.DoorOrGateCollision := pedCate.thicknessAction[_ccaFlush];
        newData.NormalSpeed         := pedCate.horizontalSpeed[_nlFloor, _nldNone, _ccaWalking];
        newData.StairUpSpeed        := pedCate.horizontalSpeed[_nlStairs, _nldUp, _ccaWalking];
        newData.StairDownSpeed      := pedCate.horizontalSpeed[_nlStairs, _nldDown, _ccaWalking];
        newData.EscaWalkSpeed       := pedCate.horizontalSpeed[_nlEscalator, _nldUp, _ccaWalking];
        newData.CrosswalkBlueSpeed  := pedCate.horizontalSpeed[_nlCrosswalkBlue, _nldNone, _ccaWalking];
        newData.CrosswalkRedSpeed   := pedCate.horizontalSpeed[_nlCrosswalkRed, _nldNone, _ccaWalking];
        Breaked := false;
        for j := 1 to theApplicationServices.project.NumberOfPedestrianProfile do
            begin
            pedProf := theApplicationServices.project.PedestrianProfileIF[j];
            for k := 1 to pedProf.numberOfOptions do
                begin
                if pedProf.option[k].pedestrianCategoryIndex = i then
                    begin
                    newData.TakeBusTime  := pedProf.option[k].TakeBusTime;
                    newData.PassGateTime := pedProf.option[k].PassThroughTicketGateSec;
                    Breaked := true;
                    Break;
                    end;
                end;

            if Breaked then
                Break;
            end;

        p_EditingCategories.Data.Add(newData);
        lbCategory.Items.Add(newData.Name);
        end;

    if p_CategoryList.Count > 0 then
        p_CategoryList.Clear;

    for i := 0 to p_EditingCategories.Data.Count - 1 do
        p_CategoryList.AddObject(p_EditingCategories.Data[i].Name, TObject(p_EditingCategories.Data[i]));
    end;

procedure TFramePedestrianSettings.SetNewCategories;
    var
        i : integer;
        newAdjustRate : AdjustRateType;
        pedCate : IF8PedestrianCategory;

    procedure SendAdjustRate(const linkType: NetworkLinkType; linkDirectionType : NetworkLinkDirectionType; actionType: CrowdCharacterActionType; percentage: integer);
        begin
        newAdjustRate.rate := DEFAULT_ADJUST_RATE;
        newAdjustRate.percentage := percentage;
        pedCate.adjustRate[linkType, linkDirectionType, actionType] := newAdjustRate;
        end;

    begin
    for i := 0 to p_EditingCategories.Data.Count - 1 do
        begin
        if i >= theApplicationServices.project.NumberOfPedestrianCategory then
            theApplicationServices.project.AddPedestrianCategory;

        pedCate := theApplicationServices.project.PedestrianCategoryIF[i];
        pedCate.name := p_EditingCategories.Data[i].Name;
        SendAdjustRate(_nlStairs, _nldUp, _ccaWalking, p_EditingCategories.Data[i].StairUpPer);
        SendAdjustRate(_nlStairs, _nldDown, _ccaWalking, p_EditingCategories.Data[i].StairDownPer);
        SendAdjustRate(_nlEscalator, _nldUp, _ccaStanding, p_EditingCategories.Data[i].EscaUpPer);
        SendAdjustRate(_nlEscalator, _nldUp, _ccaWalking, p_EditingCategories.Data[i].EscaUpWalkPer);
        SendAdjustRate(_nlEscalator, _nldDown, _ccaStanding, p_EditingCategories.Data[i].EscaDownPer);
        SendAdjustRate(_nlEscalator, _nldDown, _ccaWalking, p_EditingCategories.Data[i].EscaDownWalkPer);
        SendAdjustRate(_nlElevator, _nldUp, _ccaStanding, p_EditingCategories.Data[i].ElevUpPer);
        SendAdjustRate(_nlElevator, _nldDown, _ccaStanding, p_EditingCategories.Data[i].ElevDownPer);
        SendAdjustRate(_nlCrosswalkBlue, _nldNone, _ccaWalking, p_EditingCategories.Data[i].CrosswalkBluePer);
        SendAdjustRate(_nlCrosswalkRed, _nldNone, _ccaWalking, p_EditingCategories.Data[i].CrosswalkRedPer);
        pedCate.widthAction[_ccaStanding] := p_EditingCategories.Data[i].StopCollision;
        pedCate.widthAction[_ccaWalking] := p_EditingCategories.Data[i].WalkCollision;
        pedCate.widthAction[_ccaWalkingQueue] := p_EditingCategories.Data[i].WQWalkCollision;
        pedCate.thicknessAction[_ccaStanding] := p_EditingCategories.Data[i].StopCollision;
        pedCate.thicknessAction[_ccaWalking] := p_EditingCategories.Data[i].WalkCollision;
        pedCate.thicknessAction[_ccaWalkingQueue] := p_EditingCategories.Data[i].WQWalkCollision;
        pedCate.thicknessAction[_ccaFlush] := p_EditingCategories.Data[i].DoorOrGateCollision;
        pedCate.horizontalSpeed[_nlFloor, _nldNone, _ccaWalking] := p_EditingCategories.Data[i].NormalSpeed;
        pedCate.horizontalSpeed[_nlStairs, _nldUp, _ccaWalking] := p_EditingCategories.Data[i].StairUpSpeed;
        pedCate.horizontalSpeed[_nlStairs, _nldDown, _ccaWalking] := p_EditingCategories.Data[i].StairDownSpeed;
        pedCate.horizontalSpeed[_nlEscalator, _nldUp, _ccaWalking] := p_EditingCategories.Data[i].EscaWalkSpeed;
        pedCate.horizontalSpeed[_nlEscalator, _nldDown, _ccaWalking] := p_EditingCategories.Data[i].EscaWalkSpeed;
        pedCate.horizontalSpeed[_nlCrosswalkBlue, _nldNone, _ccaWalking] := p_EditingCategories.Data[i].CrosswalkBlueSpeed;
        pedCate.horizontalSpeed[_nlCrosswalkRed, _nldNone, _ccaWalking] := p_EditingCategories.Data[i].CrosswalkRedSpeed;
        end;
    end;

procedure TFramePedestrianSettings.ResetNodes;
    var
        i      : integer;
    begin
    vbtPedestrianSettings.Clear;
    for i := 0 to p_EditingProfList.Data.Count - 1 do
        begin
        try
            vbtPedestrianSettings.AddChild(nil);
        finally
            vbtPedestrianSettings.InvalidateChildren(nil, false);
            vbtPedestrianSettings.FocusedNode := nil;
            end;
        end;
    end;

procedure TFramePedestrianSettings.HideCategoryEditor;
    begin
    if Assigned(p_PedestrianCategoryEditorForm) then
        begin
        p_PedestrianCategoryEditorForm.Close;
        FreeAndNil(p_PedestrianCategoryEditorForm);
        end;
    end;

procedure TFramePedestrianSettings.ReceivePedestrianLabelList(const LblList: TList<TPedestrianData>);
    var
        i, j : integer;
        newData : TPedestrianData;
    begin
    if LblList.Count < 1 then
        Exit;

    p_EditingLabelList := TList<TPedestrianData>.Create;
    for i := 0 to LblList.Count - 1 do
        begin
        newData := TPedestrianData.Create;
        newData.IsActive             := LblList[i].IsActive;
        newData.LabelName            := LblList[i].LabelName;
        newData.PopNum               := LblList[i].PopNum;
        newData.PopPer               := LblList[i].PopPer;
        for j := 0 to LblList[i].AttrPerList.Count - 1 do
            newData.AttrPerList.Add(LblList[i].AttrPerList[j]);

        for j := 0 to LblList[i].DestPerList.Count - 1 do
            newData.DestPerList.Add(LblList[i].DestPerList[j]);

        p_EditingLabelList.Add(newData);
        end;
    end;

procedure TFramePedestrianSettings.SendNewPedestrianLabelList(out LblList: TList<TPedestrianData>);
    var
        i, j : integer;
        newData : TPedestrianData;
    begin
    if not Assigned(p_EditingLabelList) then
        Exit;

    LblList.Clear;
    for i := 0 to p_EditingLabelList.Count - 1 do
        begin
        newData := TPedestrianData.Create;
        newData.IsActive             := p_EditingLabelList[i].IsActive;
        newData.LabelName            := p_EditingLabelList[i].LabelName;
        newData.PopNum               := p_EditingLabelList[i].PopNum;
        newData.PopPer               := p_EditingLabelList[i].PopPer;
        for j := 0 to p_EditingLabelList[i].AttrPerList.Count - 1 do
            newData.AttrPerList.Add(p_EditingLabelList[i].AttrPerList[j]);

        for j := 0 to p_EditingLabelList[i].DestPerList.Count - 1 do
            newData.DestPerList.Add(p_EditingLabelList[i].DestPerList[j]);

        LblList.Add(newData);
        end;

    FreeAndNil(p_EditingLabelList);
    end;

{ TComboEditLink }

procedure TComboEditLink.ComboChange(Sender: TObject);
    var
        comboBox : TComboBox;
        indexCombo : Integer;
    begin
    comboBox := TComboBox(EditControl);
    indexCombo := comboBox.ItemIndex;
    p_selectedIndex := comboBox.ItemIndex;
    if indexCombo >= 0 then
        p_selectedObject := PickList.Objects[indexCombo]
    else
        p_selectedObject := nil;

    inherited ComboChange(Sender);
    end;

procedure TComboEditLink.PrepareEditControl;
    var
        comboBox : TComboBox;
    begin
    inherited;
    comboBox := TComboBox(EditControl);
    comboBox.OnChange := ComboChange;
    comboBox.ItemIndex := PickList.IndexOfObject(p_selectedObject);
    end;
end.
