unit WaitingQueueAreaRebuilder;

interface

uses
    PluginCore,
    F8Utils,
    F8GLUtils,
    WaitingQueueAreaList;

type
    ///    待機列形成範囲の範囲内にあるノードを削除する
    ///    ノードインデックスを更新する
    TWaitingQueueAreaRebuilder = class
        private
        public
            class procedure Rebuild(const aAreaList: TWaitingQueueAreaList);
        end;

implementation

uses
    System.Generics.Collections,
    System.SysUtils,
    PedestrianUtil,
    SimulationInputUtils;

{ TWaitingQueueAreaRebuilder }
// aAreaList.Data.Lastを新規に追加された待機列形成範囲設定をして、その設定範囲内のノードを削除する
// 他の待機列形成範囲設定のノードは現状判定の対象外(本来、そのような形成範囲は作成できてはいけない)
class procedure TWaitingQueueAreaRebuilder.Rebuild(const aAreaList: TWaitingQueueAreaList);
    var
        network: IF8Networks;

    procedure UpdateAreaNodeIndex(const aNetworkIndex: Integer; const aRemoveNodeIndex: Integer);
        var
            item: TWaitingQueueArea;
            newSetting: TNodeNumArray;
        begin
        for item in aAreaList.Data do
            begin
            if aNetworkIndex = item.LinkedNodeIdx[0] then
                begin
                newSetting := item.LinkedNodeIdx;
                if aRemoveNodeIndex = item.LinkedNodeIdx[1] then
                    newSetting[1] := 0
                else if aRemoveNodeIndex < item.LinkedNodeIdx[1] then
                    newSetting[1] := newSetting[1] - 1;

                item.LinkedNodeIdx := newSetting;
                end;

            if aNetworkIndex = item.WaitingQueueNodeIdx[0] then
                begin
                newSetting := item.WaitingQueueNodeIdx;
                if aRemoveNodeIndex = item.WaitingQueueNodeIdx[1] then
                    newSetting[1] := 0
                else if aRemoveNodeIndex < item.WaitingQueueNodeIdx[1] then
                    newSetting[1] := newSetting[1] - 1;

                item.WaitingQueueNodeIdx := newSetting;
                end;
            end;
        end;

    var
        newArea: TWaitingQueueArea;
        newWQNode: IF8NetworkNode;
        node: IF8NetworkNode;
        link: IF8NetworkLink;

        rect: TPoint3DListType;
        deleteNodes: TList<IF8NetworkNode>; // 削除するノード
        deleteNodeIndexs: TList<Integer>; // 削除するノードのインデックス
        reconnectNodes: TList<IF8NetworkNode>; // 再接続するノード

        isDeleteStartNode, isDeleteEndNode: Boolean;
        point: TPoint3D;
        i: Integer;
    begin
    if not Assigned(aAreaList) then
        Exit;

    if aAreaList.Data.Count < 1 then
        Exit;

    newArea := aAreaList.Data.Last;

    if not Assigned(theApplicationServices.project) then
        Exit;

    if theApplicationServices.project.NumberOfFlightWayNwks < newArea.WaitingQueueNodeIdx[0] then
        Exit;

    network := theApplicationServices.project.Flightwaynwk[newArea.WaitingQueueNodeIdx[0]];

    newWQNode := network.Node[newArea.WaitingQueueNodeIdx[1]];

    SetLength(rect, 4);
    rect[0] := Point3D(newArea.OriginPoint[_x], newArea.OriginPoint[_z], newArea.OriginPoint[_y]);
    rect[1] := Point3D(newArea.EndPoint[_x],    newArea.OriginPoint[_z], newArea.OriginPoint[_y]);
    rect[2] := Point3D(newArea.EndPoint[_x],    newArea.EndPoint[_z],    newArea.OriginPoint[_y]);
    rect[3] := Point3D(newArea.OriginPoint[_x], newArea.EndPoint[_z],    newArea.OriginPoint[_y]);


    deleteNodes      := TList<IF8NetworkNode>.Create;
    deleteNodeIndexs := TList<Integer>.Create;
    reconnectNodes   := TList<IF8NetworkNode>.Create;
    try
        // 削除するノードを探索
        for i := 0 to network.NodeCount - 1 do
            begin
            node := network.Node[i];

            if node = newWQNode then
                Continue;

            point := Point3D(node.position[_x], node.position[_z], node.position[_y]);
            if IsOverlay(point, rect) then
                begin
                deleteNodes.Add(node);
                deleteNodeIndexs.Add(i);
                end;
            end;
        // 削除するノードがStart/Endのどちらかであるリンクを削除
        // リンクの一方が削除しないノードだった場合、リストに記録する
        // Notice: RemoveLinkを実行した時、Link数が0になったノードは自動で破棄されている
        for i := network.LinkCount - 1 downto 0 do
            begin
            link := network.Link[i];

            isDeleteStartNode := deleteNodes.Contains(link.StartNode);
            isDeleteEndNode   := deleteNodes.Contains(link.EndNode);

            if isDeleteStartNode and isDeleteEndNode then
                network.RemoveLink(i)
            else if isDeleteStartNode then
                begin
                if link.endNode <> newWQNode then
                    reconnectNodes.Add(link.EndNode);
                network.RemoveLink(i);
                end
            else if isDeleteEndNode then
                begin
                if link.startNode <> newWQNode then
                    reconnectNodes.Add(link.StartNode);
                network.RemoveLink(i);
                end;
            end;

        deleteNodes.Clear;

        // インデックスの更新
        for i := 0 to deleteNodeIndexs.Count - 1 do
            UpdateAreaNodeIndex(newArea.WaitingQueueNodeIdx[0], deleteNodeIndexs[i]);

        // 再接続
        for i := 0 to reconnectNodes.Count - 1 do
            begin
            if Assigned(reconnectNodes[i]) and Assigned(newWQNode) then
                network.CreateNewLinkFromNode(reconnectNodes[i], newWQNode, True, False);
            end;

    finally
        FreeAndNil(deleteNodes);
        FreeAndNil(deletenodeIndexs);
        FreeAndNil(reconnectNodes);
        end;
    network.Clean;
    end;
end.
