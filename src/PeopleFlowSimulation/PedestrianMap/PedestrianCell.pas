unit PedestrianCell;

interface

uses
    F8Utils,
    MapOrigin,
    CellID,
    PedestrianUtil;
type
    /// <summary>
    ///    歩行マップ内を正方形に区切った範囲を表すクラス
    ///    UC-win/Roadが使用するOpenGL座標系、ローカル座標系および緯度経度座標系で表される四角形の4隅の点の位置を持つ
    ///    (座標系変換処理に時間がかかるので、初期化時に各座標系での位置情報を生成・保持しておく)
    /// </summary>
    TPedestrianCell = class
        private
            p_Vertexs     : VertexPositions;
            p_VertexsLocal: VertexPositions;
            p_VertexsGL   : VertexPositions;
            p_Center : TPointPositionData;
            p_CellID : TCellID;

            p_Status: PedestrianAreaStatus;

            procedure SetStatus(const aStatus: PedestrianAreaStatus);
            function  GetArea: VertexPositions;
            function  GetAreaLocal: VertexPositions;
            function  GetAreaGL: VertexPositions;
            function  GetCenter: TPointPositionData;
            function  GetCorner(const aIdx: VertexPositionType): TPointPositionData;
        public
            constructor Create(const aCellID: TCellID;
                               const aMatrix, aMatrixLocal, aMatrixGL: VertexPositions; const aCenter: TPointPositionData;
                               const aStatus: PedestrianAreaStatus = PedestrianAreaStatus._pasNoEntry);

            function  Clone: TPedestrianCell;

            property  Status                                : PedestrianAreaStatus read p_Status write SetStatus;
            property  Area                                  : VertexPositions      read GetArea;
            property  AreaLocal                             : VertexPositions      read GetAreaLocal;
            property  AreaGL                                : VertexPositions      read GetAreaGL;
            property  Corner[const aIdx: VertexPositionType]: TPointPositionData   read GetCorner;
            property  Center                                : TPointPositionData   read GetCenter;
            property  CellID                                : TCellID              read p_CellID;
        end;

    ChangeCellNotifyProc = procedure(const aCell: TPedestrianCell) of Object;

implementation

uses
    LatLonHelper;

{ TPedestrianCell }
constructor TPedestrianCell.Create(const aCellID: TCellID;
                                   const aMatrix, aMatrixLocal, aMatrixGL: VertexPositions; const aCenter: TPointPositionData;
                                   const aStatus: PedestrianAreaStatus = PedestrianAreaStatus._pasNoEntry);
    begin
    p_CellID       := aCellID;
    p_Vertexs      := aMatrix;
    p_VertexsLocal := aMatrixLocal;
    p_VertexsGL    := aMatrixGL;
    p_Center       := aCenter;
    p_Status       := aStatus;
    end;

function TPedestrianCell.Clone: TPedestrianCell;
    begin
    Result := TPedestrianCell.Create(p_CellID, p_Vertexs, p_VertexsLocal, p_VertexsGL, p_Center, p_Status);
    end;

procedure TPedestrianCell.SetStatus(const aStatus: PedestrianAreaStatus);
    begin
    if p_Status <> aStatus then
        p_Status := aStatus;
    end;

function TPedestrianCell.GetArea: VertexPositions;
    begin
    Result := p_Vertexs;
    end;

function TPedestrianCell.GetAreaLocal: VertexPositions;
    begin
    Result := p_VertexsLocal;
    end;

function TPedestrianCell.GetAreaGL: VertexPositions;
    begin
    Result := p_VertexsGL;
    end;

function TPedestrianCell.GetCenter: TPointPositionData;
    begin
    Result := p_Center;
    end;

function TPedestrianCell.GetCorner(const aIdx: VertexPositionType): TPointPositionData;
    begin
    Result.LatLon := p_Vertexs[aIdx];
    Result.Local  := p_VertexsLocal[aIdx];
    Result.OpenGL := p_VertexsGL[aIdx];
    end;
end.
