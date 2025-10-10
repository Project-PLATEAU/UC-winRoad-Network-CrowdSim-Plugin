unit ComboDrawingHelper;

interface

uses
    System.SysUtils,
    VTEditors,
    Vcl.StdCtrls,
    Winapi.Windows,
    Vcl.Graphics;

type
    /// <summary>
    ///    Agentのキャラクターモデルを選択するUIの機能を定義するクラス
    /// </summary>
    TComboDrawingHelperClass = class(TInterfacedObject, IComboCustomDraw)
        private
            procedure   DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean = false);
        public
            procedure   ComboDrawItem(Sender : TComboEditLink; Control : TComboBox; Index : Integer; Rect : TRect; State : TOwnerDrawState);
            procedure   ComboMeasureItem(Sender : TComboEditLink; Control : TComboBox; Index : Integer; var Height : Integer);
        end;



implementation

uses
    F8Utils,
    PluginCore;

{ TComboDrawingHelperClass }

procedure TComboDrawingHelperClass.ComboDrawItem(Sender: TComboEditLink; Control: TComboBox; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    var
        iq  : IF8QuakeIII;
    begin
    if Supports(Sender.PickList.Objects[Index], IF8QuakeIII, iq) then
        DrawBitmapInRect(iq.GetThumbnail,
                         iq.name,
                         Control.Canvas,
                         Rect);
    end;


procedure TComboDrawingHelperClass.ComboMeasureItem(Sender: TComboEditLink; Control: TComboBox; Index: Integer; var Height: Integer);
    begin
    end;

procedure TComboDrawingHelperClass.DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean);
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

    if useEllipsisIfTooLong then     // Equivalent to Windows' DT_END_ELLIPSIS
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

end.
