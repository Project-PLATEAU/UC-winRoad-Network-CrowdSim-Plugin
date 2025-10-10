unit XMLPointType;

interface

uses
    XML.XMLIntf,
    F8Utils,
    PedestrianUtil;

type
    /// <summary>
    ///    TPoint3DデータをUC-win/RoadのプロジェクトファイルにXML形式で保存するクラス
    /// </summary>
    TPoint3DXMLType = class
        strict private
            const
                TAG_X = 'X';
                TAG_Y = 'Y';
                TAG_Z = 'Z';
        public
            class procedure ExportToXML(const aData: TPoint3D; const aNodeName: String; const aParentNode: IXMLNode);
            class function  ImportFromXML(const aDataNode: IXMLNode; out aRes: TPoint3D): Boolean;
        end;

    /// <summary>
    ///    TPoint3DのリストデータをUC-win/RoadのプロジェクトファイルにXML形式で保存するクラス
    /// </summary>
    TPoint3DListXMLType = class
        strict private
            const
                NODE_POINT      = 'Point_';
                TAG_POINT_COUNT = 'PointCount';
        public
            class procedure ExportToXML(const aData: TPoint3DListType; const aNodeName: String; const aParentNode: IXMLNode);
            class function  ImportFromXML(const aDataNode: IXMLNode; out aRes: TPoint3DListType): Boolean;
        end;

    /// <summary>
    ///    TParametricLine2DデータをUC-win/RoadのプロジェクトファイルにXML形式で保存するクラス
    /// </summary>
    TParametricLine2DXMLType = class
        strict private
            const
                NODE_A = 'pointA';
                NODE_B = 'pointB';
        public
            class procedure ExportToXML(const aData: TParametricLine2D; const aNodeName: String; const aParentNode: IXMLNode);
            class function  ImportFromXML(const aDataNode: IXMLNode; out aRes: TParametricLine2D): Boolean;
        end;

    /// <summary>
    ///    TParametricLine2DのリストデータをUC-win/RoadのプロジェクトファイルにXML形式で保存するクラス
    /// </summary>
    TParametricLineListXMLType = class
        strict private
            const
                NODE_VERTEX     = 'Vertex_';
                TAG_VERTEX_COUNT = 'VertexCount';
        public
            class procedure ExportToXML(const aData: TParametricLineList; const aNodeName: String; const aParentNode: IXMLNode);
            class function  ImportFromXML(const aDataNode: IXMLNode; out aRes: TParametricLineList): Boolean;
        end;

    /// <summary>
    ///    TVertexPositionsデータをUC-win/RoadのプロジェクトファイルにXML形式で保存するクラス
    /// </summary>
    TVertexPositionsXMLType = class
        strict private
            const
                NODE_LEFT_TOP     = 'LeftTop';
                NODE_RIGHT_TOP    = 'RightTop';
                NODE_RIGHT_BOTTOM = 'RightBottom';
                NODE_LEFT_BOTTOM  = 'LeftBottom';
        public
            class procedure ExportToXML(const aData: VertexPositions; const aNodeName: String; const aParentNode: IXMLNode);
            class function  ImportFromXML(const aDataNode: IXMLNode; out aRes: VertexPositions): Boolean;
        end;

implementation

uses
    System.SysUtils;

{ TPoint3DXMLType }
class procedure TPoint3DXMLType.ExportToXML(const aData: TPoint3D; const aNodeName: String; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
    begin
    node := aParentNode.AddChild(aNodeName);
    node.Attributes[TAG_X] := aData.X.ToString;
    node.Attributes[TAG_Y] := aData.Y.ToString;
    node.Attributes[TAG_Z] := aData.Z.ToString;
    end;

class function TPoint3DXMLType.ImportFromXML(const aDataNode: IXMLNode; out aRes: TPoint3D): Boolean;
    begin
    Result := False;
    if not aDataNode.HasAttribute(TAG_X) then
        Exit;

    if not aDataNode.HasAttribute(TAG_Y) then
        Exit;

    if not aDataNode.HasAttribute(TAG_Z) then
        Exit;

    aRes.X := StrToFloatDef(aDataNode.Attributes[TAG_X], 0.0);
    aRes.Y := StrToFloatDef(aDataNode.Attributes[TAG_Y], 0.0);
    aRes.Z := StrToFloatDef(aDataNode.Attributes[TAG_Z], 0.0);
    Result := True;
    end;

{ TPoint3DListXMLType }
class procedure TPoint3DListXMLType.ExportToXML(const aData: TPoint3DListType; const aNodeName: String; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
        i   : Integer;
        len : Integer;
        itemNodeName: String;
    begin
    node := aParentNode.AddChild(aNodeName);
    len := Length(aData);

    node.Attributes[TAG_POINT_COUNT] := len.ToString;
    for i := 0 to len - 1 do
        begin
        itemNodeName := Format('%s%d', [NODE_POINT, i]);
        TPoint3DXMLType.ExportToXML(aData[i], itemNodeName, node);
        end;
    end;

class function TPoint3DListXMLType.ImportFromXML(const aDataNode: IXMLNode; out aRes: TPoint3DListType): Boolean;
    var
        len: Integer;
        i  : Integer;
        itemNodeName: String;
        itemNode: IXMLNode;
        itemData: TPoint3D;
        itemcount: Integer;
    begin
    Result := False;

    if not aDataNode.HasAttribute(TAG_POINT_COUNT) then
        Exit;

    len := StrToIntDef(aDataNode.Attributes[TAG_POINT_COUNT], 0);

    if len < 0 then
        Exit;

    SetLength(aRes, len);
    itemCount := 0;
    for i := 0 to aDataNode.ChildNodes.Count - 1 do
        begin
        itemNodeName := Format('%s%d', [NODE_POINT, itemCount]);
        itemNode := aDataNode.ChildNodes.FindNode(itemNodeName);
        if not Assigned(itemNode) then
            Break;

        if not TPoint3DXMLType.ImportFromXML(itemNode, itemData) then
            Break;

        aRes[itemCount] := itemData;
        Inc(itemCount);
        end;

    Result := (len = itemCount);
    end;

{ TParametricLine2DXMLType }
class procedure TParametricLine2DXMLType.ExportToXML(const aData: TParametricLine2D; const aNodeName: String; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
    begin
    node := aParentNode.AddChild(aNodeName);
    TPoint3DXMLType.ExportToXML(aData.A, NODE_A, node);
    TPoint3DXMLType.ExportToXML(aData.B, NODE_B, node);
    end;

class function TParametricLine2DXMLType.ImportFromXML(const aDataNode: IXMLNode; out aRes: TParametricLine2D): Boolean;
    function ImportPoint(const aTag: String; out aRes: TPoint3D): Boolean;
        var
            node: IXMLNode;
        begin
        Result := False;
        node := aDataNode.ChildNodes.FindNode(aTag);
        if not Assigned(node) then
            Exit;

        Result := TPoint3DXMLType.ImportFromXML(node, aRes);
        end;
    var
        point: TPoint3D;
    begin
    Result := False;

    if not ImportPoint(NODE_A, point) then
        Exit;

    aRes.A := point;

    if not ImportPoint(NODE_B, point) then
        Exit;

    aRes.B := point;

    Result := True;
    end;

{ TParametricLineListXMLType }
class procedure TParametricLineListXMLType.ExportToXML(const aData: TParametricLineList; const aNodeName: String; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
        i   : Integer;
        len : Integer;
        itemNodeName: String;
    begin
    node := aParentNode.AddChild(aNodeName);
    len := Length(aData);

    node.Attributes[TAG_VERTEX_COUNT] := len.ToString;
    for i := 0 to len - 1 do
        begin
        itemNodeName := Format('%s%d', [NODE_VERTEX, i]);
        TParametricLine2DXMLType.ExportToXML(aData[i], itemNodeName, node);
        end;
    end;

class function TParametricLineListXMLType.ImportFromXML(const aDataNode: IXMLNode; out aRes: TParametricLineList): Boolean;
    var
        len: Integer;
        i  : Integer;
        itemNodeName: String;
        itemNode: IXMLNode;
        itemData: TParametricLine2D;
        itemcount: Integer;
    begin
    Result := False;

    if not aDataNode.HasAttribute(TAG_VERTEX_COUNT) then
        Exit;

    len := StrToIntDef(aDataNode.Attributes[TAG_VERTEX_COUNT], 0);

    if len < 0 then
        Exit;

    SetLength(aRes, len);
    itemCount := 0;
    for i := 0 to aDataNode.ChildNodes.Count - 1 do
        begin
        itemNodeName := Format('%s%d', [NODE_VERTEX, itemCount]);
        itemNode := aDataNode.ChildNodes.FindNode(itemNodeName);
        if not Assigned(itemNode) then
            Break;

        if not TParametricLine2DXMLType.ImportFromXML(itemNode, itemData) then
            Break;

        aRes[itemCount] := itemData;
        Inc(itemCount);
        end;

    Result := (len = itemCount);
    end;

{ TVertexPositionsXMLType }
class procedure TVertexPositionsXMLType.ExportToXML(const aData: VertexPositions; const aNodeName: String; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
    begin
    node := aParentNode.AddChild(aNodeName);

    TPoint3DXMLType.ExportToXML(aData[VertexPositionType._LeftTop], NODE_LEFT_TOP, node);
    TPoint3DXMLType.ExportToXML(aData[VertexPositionType._RightTop], NODE_RIGHT_TOP, node);
    TPoint3DXMLType.ExportToXML(aData[VertexPositionType._RightBottom], NODE_RIGHT_BOTTOM, node);
    TPoint3DXMLType.ExportToXML(aData[VertexPositionType._LeftBottom], NODE_LEFT_BOTTOM, node);
    end;

class function TVertexPositionsXMLType.ImportFromXML(const aDataNode: IXMLNode; out aRes: VertexPositions): Boolean;
    function ImportPoint(const aTag: String; const aTagType: VertexPositionType): Boolean;
        var
            node: IXMLNode;
            point: TPoint3D;
        begin
        Result := False;

        node := aDataNode.ChildNodes.FindNode(aTag);
        if not Assigned(node) then
            Exit;

        if not TPoint3DXMLType.ImportFromXML(node, point) then
            Exit;

        aRes[aTagType] := point;

        Result := True;
        end;
    begin
    Result := ImportPoint(NODE_LEFT_TOP,     VertexPositionType._LeftTop)
          and ImportPoint(NODE_RIGHT_TOP,    VertexPositionType._RightTop)
          and ImportPoint(NODE_RIGHT_BOTTOM, VertexPositionType._RightBottom)
          and ImportPoint(NODE_LEFT_BOTTOM,  VertexPositionType._LeftBottom);
    end;
end.
