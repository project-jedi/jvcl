{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReport.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgReport;

interface
uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  JvgTypes,
  JvgCommClasses,
  JvComponent,
  JvgUtils,
  Forms,
  OleCtnrs,
  ExtCtrls,
  SysUtils,
  Printers;
type

  TJvgReport = class;

  TglRepParamType = (gptUnknown, gptEdit, gptRadio, gptCheck);

  TJvgRepScrollBox = class(TScrollBox)
  private
    GridImage: TBitmap;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    OnDraw: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TJvgReportItem = class(TJvGraphicControl) //TCustomPanel) //TGraphicControl)
  private
    FSelected: boolean;
    FBkColor: integer;
    FBvColor: integer;
    FTransparent: integer;
    FAlignment: word; //..1-left,2-right,3-center,4-boadwise
    FSideLeft,
      FSideTop,
      FSideRight,
      FSideBottom: word;
    FPenStyle: integer;
    FPenWidth: word;
    FText: string;
    PrintText: string;
    FCompName: string;
    FFName: string;
    FFSize,
      FFColor,
      FFStyle: integer;
    FContainOLE: boolean;
    FFixed: word;
    FOLELinkToFile: string;
    FOLESizeMode: word;
    fSizing, fRepaintOnlyBorder: boolean;
    R: array[1..8] of TRect;
    DownPos: TPoint;
    SizeDirection: integer;
    FExternalCanvas: TCanvas;
    Cursors: array[1..8] of TCursor;
    bmp: TBitmap;
    Report: TJvgReport;

    procedure SetSelected(Value: boolean);
    procedure SetBkColor(Value: integer);
    procedure SetBvColor(Value: integer);
    procedure SetTransparent(Value: integer);
    procedure SetAlignment(Value: word);
    procedure SetSideLeft(Value: word);
    procedure SetSideTop(Value: word);
    procedure SetSideRight(Value: word);
    procedure SetSideBottom(Value: word);
    procedure SetPenStyle(Value: integer);
    procedure SetPenWidth(Value: word);
    procedure SetText(Value: string);
    procedure SetFName(Value: string);
    procedure SetFSize(Value: integer);
    procedure SetFColor(Value: integer);
    procedure SetFStyle(Value: integer);
    procedure SetContainOLE(Value: boolean);
    procedure SetOLELinkToFile(Value: string);
    procedure SetOLESizeMode(Value: word);
    procedure SetFixed(Value: word);
    function IsContainOLE: boolean;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMLMouseDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLMouseUP(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  public
    procedure Paint; override;
    procedure PaintTo(Canvas: TCanvas);
  protected
    procedure SetParent(Value: TWinControl); override;

  public
    ResText: string;
    OLEContainer: TOLEContainer;
    property Selected: boolean read FSelected write SetSelected default false;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //    property OnResize;
    property ExternalCanvas: TCanvas read FExternalCanvas write
      FExternalCanvas;
    //    procedure RepaintBorder;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

  published
    property BkColor: integer read FBkColor write SetBkColor default clWhite;
    property BvColor: integer read FBvColor write SetBvColor default clBlack;
    property Transparent: integer read FTransparent write SetTransparent
      default 0;
    property Alignment: word read FAlignment write SetAlignment
      default 1;
    property SideLeft: word read FSideLeft write SetSideLeft
      default 1;
    property SideTop: word read FSideTop write SetSideTop
      default 1;
    property SideRight: word read FSideRight write SetSideRight
      default 1;
    property SideBottom: word read FSideBottom write SetSideBottom
      default 1;
    property PenStyle: integer read FPenStyle write SetPenStyle
      default integer(psSolid);
    property PenWidth: word read FPenWidth write SetPenWidth
      default 1;
    property Text: string read FText write SetText;
    property CompName: string read FCompName write FCompName;
    property FName: string read FFName write SetFName;
    property FSize: integer read FFSize write SetFSize;
    property FColor: integer read FFColor write SetFColor;
    property FStyle: integer read FFStyle write SetFStyle;
    property ContainOLE: boolean read FContainOLE write SetContainOLE default
      false;
    property OLELinkToFile: string read FOLELinkToFile write SetOLELinkToFile
      stored IsContainOLE;
    property OLESizeMode: word read FOLESizeMode write SetOLESizeMode
      stored IsContainOLE default 2;
    property Fixed: word read FFixed write SetFixed
      default 0;
  end;

  TBeforePrintEvent = procedure(Sender: TJvgReport) of object;

  TJvgReport = class(TJvComponent)
  private
    procedure ValidateWnds;
    function GetReportText: TStringList;
    procedure SetReportText(Value: TStringList);
  public
    OwnerWnd,
      ParentWnd: TWinControl;
    ParamNames: TStringList;
    ParamValues: TStringList;
    ParamMasks: TStringList;
    ParamTypes: TList;
    FReportList: TStringList;
    ComponentList: TList;
    FBeforePrint: TBeforePrintEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure PaintTo(Canvas: TCanvas);
    procedure PreviewTo(Window: TWinControl);
    procedure Print;
    procedure CreateReport(ParentWnd: TWinControl; fNeedClearOwner: boolean);
    function SetParam(const sParamName, sParamValue: string): boolean;
    function GetParam(const sParamName: string; var sParamValue: string):
      boolean;
    function AddComponent: TJvgReportItem;
    procedure AnalyzeParams(Item: TJvgReportItem; DefName: string);
  private
    procedure SetUnicalName(laBevel: TJvgReportItem);
  protected
    procedure Loaded; override;
    procedure ClearReport;
  published
    property Report: TStringList read FReportList;
    property ReportText: TStringList read GetReportText write SetReportText;
    property BeforePrint: TBeforePrintEvent read FBeforePrint write
      FBeforePrint;
  end;

procedure Register;
implementation

const
  S = 2;
  DS = 2 * S + 1;
  {~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgRepScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GridImage := TBitmap.Create;
  GridImage.Width := 8;
  GridImage.Height := 8;
  GridImage.Canvas.Brush.Color := clWhite; //clWindow;
  GridImage.Canvas.FillRect(Rect(0, 0, 8, 8));
  GridImage.Canvas.Pixels[7, 7] := 0;
end;

destructor TJvgRepScrollBox.Destroy;
begin
  GridImage.Free;
  inherited;
end;

procedure TJvgRepScrollBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  DC: HDC;
  Canvas: TCanvas;
begin
  with TCanvas.Create do
  begin
    Handle := Msg.DC;
    //    Pen.Color := clWindow;
    //    Brush.Color := clWindow;
    //    Brush.Style := bsCross;
    Brush.Bitmap := GridImage;
    FillRect(ClientRect);
    Handle := 0;
    Free;
  end;
  Msg.Result := 1;
  if Assigned(OnDraw) then
    OnDraw(self);
end;

constructor TJvgReportItem.Create(AOwner: TComponent);
var
  Msg: TMessage;
begin
  inherited Create(AOwner);
  //..defaults
  Width := 50;
  Height := 50;
  Color := clWhite;
  FBkColor := clWhite;
  FBvColor := clBlack;
  FAlignment := 1;
  FSideLeft := 1;
  FSideTop := 1;
  FSideRight := 1;
  FSideBottom := 1;
  FPenStyle := integer(psSolid);
  FPenWidth := 1;
  FOLESizeMode := 2;
  Cursors[1] := crSizeNWSE;
  Cursors[2] := crSizeNS;
  Cursors[3] := crSizeNESW;

  Cursors[4] := crSizeNESW;
  Cursors[5] := crSizeNS;
  Cursors[6] := crSizeNWSE;

  Cursors[7] := crSizeWE;
  Cursors[8] := crSizeWE;
  ParentFont := false;
  {$IFDEF GL_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF}
  CMFontChanged(Msg);
end;

destructor TJvgReportItem.Destroy;
begin
  if Assigned(bmp) then
    bmp.Free;
  if Assigned(OLEContainer) then
  begin
    OLEContainer.DestroyObject;
    if not (csDestroying in ComponentState) then
    begin
      OLEContainer.Free;
      OLEContainer := nil;
    end;
  end;
  inherited;
end;

{procedure TJvgReportItem.RepaintBorder;
var R: TRect;
begin
  R := ClientRect;
  OffsetRect( R, Left, Top );
  InvalidateRect( Parent.Handle, @R, false );
  InflateRect( R, -DS, -DS );
//  ValidateRect( Parent.Handle, @R );
  fRepaintOnlyBorder := true;
  Paint;
  //fRepaintOnlyBorder := false;
end;
}

procedure TJvgReportItem.Paint;
begin
  PaintTo(Canvas);
end;

procedure TJvgReportItem.PaintTo(Canvas: TCanvas);
const
  Alignments: array[1..4] of TglAlignment = (ftaLeftJustify,
    ftaRightJustify, ftaCenter, ftaBroadwise);
  //  SysAlignments: array[TglAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
var
  i, L, T: integer;
  sPrintText: string;
  R_, Client_Rect, RCalc: TRect;
begin
  FFColor := 0;
  with Canvas do
  begin

    if Canvas = Self.Canvas then
      Client_Rect := Rect(0, 0, Width, height)
    else
    begin
      Client_Rect := Bounds(Left, Top, Width, height);
      Canvas.Font := Self.Canvas.Font;
      Canvas.Font.Color := 0;
    end;
    R_ := Client_Rect;
    L := Client_Rect.Left;
    T := Client_Rect.Top;
    InflateRect(R_, -DS, -S);
    RCalc := R_;

    if Transparent = 0 then
    begin
      Brush.Color := BkColor;
      FillRect(Client_Rect);
    end;

    if Canvas = Self.Canvas then
    begin
      Pen.Style := psDot;
      Pen.Width := 1;
      Pen.Color := clSilver;
      Brush.Style := bsClear;
      Rectangle(L, T, L + Width, T + Height);
      sPrintText := Text;
    end
    else
      sPrintText := PrintText;
    if sPrintText = '' then
      sPrintText := Text;

    Pen.Style := TPenStyle(PenStyle);
    Pen.Width := PenWidth;
    Pen.Color := BvColor;
    if bool(SideLeft) then
    begin
      MoveTo(L + PenWidth div 2, T + Height - 1);
      LineTo(L + PenWidth div 2, T);
    end;
    if bool(SideTop) then
    begin
      MoveTo(L + PenWidth div 2, T + PenWidth div 2);
      LineTo(L + Width - PenWidth, T + PenWidth div 2);
    end;
    if bool(SideRight) then
    begin
      MoveTo(L + Width - 1, T);
      LineTo(L + Width - 1, T + Height - 1);
    end;
    if bool(SideBottom) then
    begin
      MoveTo(L + Width - 1, T + Height - 1);
      LineTo(L, T + Height - 1);
    end;

    if not ContainOLE then
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      SetTextColor(Canvas.Handle, FColor);
      DrawText(Canvas.Handle, PChar(sPrintText), Length(sPrintText), RCalc,
        DT_CALCRECT or DT_WORDBREAK);
      R_.Top := R_.Top + max(0, (R_.Bottom - R_.Top - (RCalc.Bottom -
        RCalc.Top)) div 2);
      DrawTextExtAligned(Canvas, sPrintText, R_, Alignments[Alignment],
        true);
    end
    else if (OLELinkToFile <> '') and (ExtractFileExt(OLELinkToFile) = '.bmp') then
    begin
      if Assigned(OLEContainer) then
        OLEContainer.Visible := false;
      if bmp = nil then
      begin
        bmp := TBitmap.Create;
        bmp.LoadFromFile(OLELinkToFile);
      end;
      BitBlt(Canvas.Handle, L, T, bmp.width, bmp.height, bmp.Canvas.handle,
        0, 0, SRCCOPY);
    end;

    if Selected then
    begin
      Pen.Style := psSolid;
      Pen.Width := 1;
      Pen.Color := 0;
      Brush.Style := bsSolid;
      if bool(Fixed) then
        Brush.Color := clBtnFace
      else
        Brush.Color := clWhite;
      R[1] := Rect(0, 0, DS, DS); //...top-left
      R[2] := Rect(Width div 2 - S, 0, Width div 2 + S + 1, DS); //...top-center
      R[3] := Rect(Width - DS, 0, Width, DS); //...top-right

      R[4] := Rect(0, Height - DS, DS, Height); //...bottom-left
      R[5] := Rect(Width div 2 - S, Height - DS, Width div 2 + S + 1, Height); //...bottom-center
      R[6] := Rect(Width - DS, Height - DS, Width, Height); //...bottom-right

      R[7] := Rect(0, Height div 2 - S, DS, Height div 2 + S + 1); //...left-center
      R[8] := Rect(Width - DS, Height div 2 - S, Width, Height div 2 + S + 1); //...right-center

      for i := 1 to 8 do
        Rectangle(R[i].Left, R[i].Top, R[i].Right, R[i].Bottom);
    end;
  end;
  if Assigned(OLEContainer) then
    OLEContainer.SetBounds(Left + DS, Top + DS, Width - 2 * DS, Height - 2 *
      DS);
end;

procedure TJvgReportItem.SetParent(Value: TWinControl);
begin
  inherited;
  if Assigned(OLEContainer) and Assigned(Value) then
    OLEContainer.Parent := Value;
end;

procedure TJvgReportItem.CMMouseEnter(var Message: TMessage);
begin
  //Cursor := crCross;
//  SetCursor( Screen.Cursors[crCross] );
end;

procedure TJvgReportItem.CMMouseLeave(var Message: TMessage);
begin
  Cursor := crDefault;
  //  SetCursor( Screen.Cursors[crDefault] );
end;

procedure TJvgReportItem.WMMouseMove(var Message: TWMMouse);
var
  i, dX, dY, nLeft, nTop, nWidth, nHeight: integer;
  pt: TPoint;
begin
  inherited;
  if not bool(Fixed) then
    with Message do
    begin
      pt.x := Pos.x;
      pt.y := Pos.y;
      if fSizing then
      begin
        dX := Pos.x - DownPos.x;
        dY := Pos.y - DownPos.y;
        inc(pos.x, 4);
        inc(pos.y, 4);
        nLeft := Left;
        nTop := Top;
        nWidth := Width;
        nHeight := Height;
        case SizeDirection of
          1:
            begin
              nLeft := Left + dX;
              nWidth := Width - dX;
              nTop := Top + dY;
              nHeight := Height - dY;
            end;
          2:
            begin
              nTop := Top + dY;
              nHeight := Height - dY;
            end;
          3:
            begin
              nWidth := Pos.x;
              nTop := Top + dY;
              nHeight := Height - dY;
            end;

          4:
            begin
              nLeft := Left + dX;
              nWidth := Width - dX;
              nHeight := Pos.y;
            end;

          5:
            begin
              nHeight := Pos.y;
            end;

          6:
            begin
              nWidth := Pos.x;
              nHeight := Pos.y;
            end;
          7:
            begin
              nLeft := Left + dX;
              nWidth := Width - dX;
            end;
          8:
            begin
              nWidth := Pos.x;
            end;
        end;
        Left := min(nLeft, nLeft + nWidth);
        Top := min(nTop, nTop + nHeight);
        Width := abs(nWidth);
        Height := abs(nHeight);
        if nWidth < 0 then
        begin
          case SizeDirection of
            1: SizeDirection := 3;
            3: SizeDirection := 1;
            4: SizeDirection := 6;
            6: SizeDirection := 4;
            8: SizeDirection := 7;
            7: SizeDirection := 8;
          end;
          DownPos.x := Pos.x;
        end;
        if nHeight < 0 then
        begin
          case SizeDirection of
            1: SizeDirection := 4;
            2: SizeDirection := 5;
            3: SizeDirection := 6;
            4: SizeDirection := 1;
            5: SizeDirection := 2;
            6: SizeDirection := 3;
          end;
          DownPos.y := Pos.y;
        end;
      end
      else
        for i := 1 to 8 do
          if PtInRect(R[i], pt) then
          begin
            Cursor := Cursors[i];
            SizeDirection := i;
            exit;
          end;
    end;
  Cursor := crDefault;
  //  SetCursor( Screen.Cursors[crDefault] );
end;

procedure TJvgReportItem.WMLMouseDown(var Message: TWMMouse);
begin
  DownPos.x := Message.Pos.x;
  DownPos.y := Message.Pos.y;
  //DownPos := ClientToScreen(DownPos);
  fSizing := Cursor <> crDefault;
  inherited;
end;

{procedure TJvgReportItem.WMRMouseDown(var Message: TWMMouse);
begin
  DownPos.x := Message.Pos.x; DownPos.y := Message.Pos.y;
  if Assigned(PopupMenu)
  inherited;
end;}

procedure TJvgReportItem.WMLMouseUp(var Message: TWMMouse);
begin
  fSizing := false;
  inherited;
end;

procedure TJvgReportItem.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FName := Font.Name;
  FFSize := Font.Size;
  FFColor := Font.Color;
  FFStyle := 0;
  if fsBold in Font.Style then
    FFStyle := FFStyle or 1;
  if fsItalic in Font.Style then
    FFStyle := FFStyle or (1 shl 1);
  if fsUnderline in Font.Style then
    FFStyle := FFStyle or (1 shl 2);
  Invalidate;
end;

procedure TJvgReportItem.WMSize(var Message: TWMSize);
begin
  inherited;
  // if Assigned(OnResize) then OnResize(self);
end;

procedure TJvgReportItem.SetSelected(Value: boolean);
begin
  FSelected := Value;
  Repaint;
end;

procedure TJvgReportItem.SetBkColor(Value: integer);
begin
  FBkColor := Value;
  Color := BkColor;
  Repaint;
end;

procedure TJvgReportItem.SetBvColor(Value: integer);
begin
  FBvColor := Value;
  Repaint;
end;

procedure TJvgReportItem.SetTransparent(Value: integer);
begin
  FTransparent := Value;
  Repaint;
end;

procedure TJvgReportItem.SetAlignment(Value: word);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideLeft(Value: word);
begin
  FSideLeft := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideTop(Value: word);
begin
  FSideTop := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideRight(Value: word);
begin
  FSideRight := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideBottom(Value: word);
begin
  FSideBottom := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetPenStyle(Value: integer);
begin
  FPenStyle := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetPenWidth(Value: word);
begin
  FPenWidth := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetText(Value: string);
begin
  if FText = Value then
    exit;
  FText := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFName(Value: string);
begin
  FFName := Value;
  Canvas.Font.Name := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFSize(Value: integer);
begin
  FFSize := Value;
  Canvas.Font.Size := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFColor(Value: integer);
begin
  FFColor := Value;
  Canvas.Font.Color := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFStyle(Value: integer);
begin
  FFStyle := Value;
  with Canvas.Font do
  begin
    if bool(Value and 1) then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
    if bool(Value and (1 shl 1)) then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
    if bool(Value and (1 shl 2)) then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  end;
  Invalidate;
end;

procedure TJvgReportItem.SetContainOLE(Value: boolean);
begin
  FContainOLE := Value;
  if FContainOLE and (not Assigned(OLEContainer)) then
  begin
    if not Assigned(Parent) then
      exit;
    OLEContainer := TOLEContainer.Create(parent.parent); //{$IFDEF COMPILER3_UP} Parent {$ELSE} Owner {$ENDIF} );
    OLEContainer.AutoVerbMenu := false;
    OLEContainer.BorderStyle := bsNone;
    OLEContainer.Color := clWhite;
    OLEContainer.SizeMode := smScale;
    OLEContainer.Parent := Parent;
    if (OLEContainer.State = osEmpty) and (OLELinkToFile <> '') then
      SetOLELinkToFile(OLELinkToFile);
  end;
end;

procedure TJvgReportItem.SetOLELinkToFile(Value: string);
begin
  FOLELinkToFile := Value;
  if not Assigned(OLEContainer) then
    exit;
  OLEContainer.CreateLinkToFile(Value, False);
  //OLEContainer.LoadFromFile( Value );
end;

procedure TJvgReportItem.SetFixed(Value: word);
begin
  FFixed := Value;
  Repaint;
end;

procedure TJvgReportItem.SetOLESizeMode(Value: word);
begin
  if FOLESizeMode = Value then
    exit;
  FOLESizeMode := Value;
  if Assigned(OLEContainer) then
    OLEContainer.SizeMode := TSizeMode(Value);
end;

function TJvgReportItem.IsContainOLE: boolean;
begin
  Result := FContainOLE;
end;

//===========================================================================

constructor TJvgReport.Create(AOwner: TComponent);
begin
  inherited;
  ParamNames := TStringList.Create;
  ParamValues := TStringList.Create;
  ParamMasks := TStringList.Create;
  FReportList := TStringList.Create;
  ParamTypes := TList.Create;
  ComponentList := TList.Create;
end;

destructor TJvgReport.Destroy;
begin
  FReportList.Free;
  ParamNames.Free;
  ParamValues.Free;
  ParamMasks.Free;
  ParamTypes.Free;
  ClearReport;
  ComponentList.Free;
  inherited;
end;

procedure TJvgReport.Loaded;
begin
  inherited;
  CreateReport(nil, false);
end;

procedure TJvgReport.Save;
var
  msS, msT: TMemoryStream;
begin
  ValidateWnds;
  msS := TMemoryStream.Create;
  msT := TMemoryStream.Create;
  try
    msS.WriteComponent(ParentWnd);
    msS.Position := 0;
    ObjectBinaryToText(msS, msT);
    msT.Position := 0;
    FReportList.LoadFromStream(msT);
  finally
    msS.Free;
    msT.Free;
  end;
end;

procedure TJvgReport.SaveToFile(FileName: string);
var
  fs, fs2: TFileStream;
  W: TWriter;
  i: integer;
begin
  ValidateWnds;
  fs := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    fs.WriteComponent(ParentWnd);
  finally
    fs.Free;
  end;
end;

procedure TJvgReport.LoadFromFile(FileName: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  ms := TMemoryStream.Create;
  try
    ObjectBinaryToText(fs, ms);
    ms.Position := 0;
    FReportList.LoadFromStream(ms);
  finally
    fs.Free;
    ms.Free;
  end;
end;

{procedure TJvgReport.Edit;
begin
  CreateReport(true);
end;}

procedure TJvgReport.PaintTo(Canvas: TCanvas);
var
  i: integer;
begin
  OwnerWnd := nil;
  ParentWnd := nil;
  //  ParamNames.Clear;
  //  ParamMasks.Clear;
  //  ParamValues.Clear;
  //  ParamTypes.Clear;
  ComponentList.Clear;
  CreateReport(ParentWnd, false);
  for i := 0 to ComponentList.Count - 1 do
    TJvgReportItem(ComponentList[i]).PaintTo(Canvas);
end;

procedure TJvgReport.PreviewTo(Window: TWinControl);
begin
  OwnerWnd := Window;
  ParentWnd := OwnerWnd;
  ParamNames.Clear;
  ParamMasks.Clear;
  ParamValues.Clear;
  ParamTypes.Clear;
  ComponentList.Clear;
  CreateReport(ParentWnd, false);
  //  ProcessParams;
end;

procedure TJvgReport.Print;
var
  i: integer;
  ScreenDC: HDC;
  HS, WS, HP, WP: integer;
begin
  if Assigned(BeforePrint) then
    BeforePrint(self);
  OwnerWnd := TForm.Create(nil);
  TForm(OwnerWnd).WindowState := wsMaximized;
  ParentWnd := OwnerWnd;
  //OwnerWnd.Show;
  try
    CreateReport(ParentWnd, true);
    if ComponentList.Count = 0 then
      exit;

    Printer.BeginDoc;
    ScreenDC := GetDC(0);

    HS := SantimsToPixels(ScreenDC, 21, true);
    WS := SantimsToPixels(ScreenDC, 21, false);
    HP := SantimsToPixels(Printer.Canvas.Handle, 21, true);
    WP := SantimsToPixels(Printer.Canvas.Handle, 21, false);

    ReleaseDC(0, ScreenDC);

    for i := 0 to ComponentList.Count - 1 do
    begin
      TJvgReportItem(ComponentList[i]).Left :=
        MulDiv(TJvgReportItem(ComponentList[i]).Left, WP, WS);
      TJvgReportItem(ComponentList[i]).Top :=
        MulDiv(TJvgReportItem(ComponentList[i]).Top, HP, HS);
      TJvgReportItem(ComponentList[i]).Width :=
        MulDiv(TJvgReportItem(ComponentList[i]).Width, WP, WS);
      TJvgReportItem(ComponentList[i]).Height :=
        MulDiv(TJvgReportItem(ComponentList[i]).Height, HP, HS);
      TJvgReportItem(ComponentList[i]).PenWidth :=
        MulDiv(TJvgReportItem(ComponentList[i]).PenWidth, HP, HS);
    end;

    for i := 0 to ComponentList.Count - 1 do
      with TJvgReportItem(ComponentList[i]) do
      begin
        PaintTo(Printer.Canvas);
        if ContainOle then
          OLEContainer.PaintTo(Printer.Canvas.Handle, Left, Top);
      end;
    Printer.EndDoc;

    repeat Application.ProcessMessages;
    until not TForm(OwnerWnd).Active;
  finally
    OwnerWnd.Free;
  end;
end;

procedure TJvgReport.ClearReport;
var
  i: integer;
begin
  for i := 0 to ComponentList.Count - 1 do
    TJvgReportItem(ComponentList[i]).Free;
  ComponentList.Count := 0;
end;

procedure TJvgReport.CreateReport(ParentWnd: TWinControl; fNeedClearOwner:
  boolean);
var
  fs, fs2: TFileStream;
  ms: TMemoryStream;
  p: TParser;
  c: char;
  i: integer;
  Compon: TComponent;
  sName, sClassName: string;

  procedure N2T;
  begin
    p.NextToken;
    p.NextToken;
  end;

  procedure Create_Object(sClassName, sName: string);
  var
    B: TJvgReportItem;
  begin
    B := nil;
    if sClassName = 'TJvgReportItem' then //...process only TJvgReportItem class
    begin
      B := TJvgReportItem.Create(OwnerWnd);
      B.Report := self;
    end;
    if B = nil then
      exit;
    ComponentList.Add(B);
    c := p.NextToken;
    while not p.TokenSymbolIs('end') do
      with p do
      begin
        case c of
          '+':
            begin
              p.NextToken;
              b.Text := b.Text + TokenString;
            end;
          toSymbol:
            begin
              if TokenString = 'Left' then
              begin
                N2T;
                b.Left := TokenInt;
              end;
              if TokenString = 'Top' then
              begin
                N2T;
                b.Top := TokenInt;
              end;
              if TokenString = 'Width' then
              begin
                N2T;
                b.Width := TokenInt;
              end;
              if TokenString = 'Height' then
              begin
                N2T;
                b.Height := TokenInt;
              end;
              if TokenString = 'Text' then
              begin
                N2T;
                b.Text := TokenString;
              end;
              if TokenString = 'BkColor' then
              begin
                N2T;
                b.BkColor := TokenInt;
              end;
              if TokenString = 'BvColor' then
              begin
                N2T;
                b.BvColor := TokenInt;
              end;
              if TokenString = 'Transparent' then
              begin
                N2T;
                b.Transparent := TokenInt;
              end;
              if TokenString = 'Alignment' then
              begin
                N2T;
                b.Alignment := TokenInt;
              end;
              if TokenString = 'SideLeft' then
              begin
                N2T;
                b.SideLeft := TokenInt;
              end;
              if TokenString = 'SideTop' then
              begin
                N2T;
                b.SideTop := TokenInt;
              end;
              if TokenString = 'SideRight' then
              begin
                N2T;
                b.SideRight := TokenInt;
              end;
              if TokenString = 'SideBottom' then
              begin
                N2T;
                b.SideBottom := TokenInt;
              end;
              if TokenString = 'PenStyle' then
              begin
                N2T;
                b.PenStyle := TokenInt;
              end;
              if TokenString = 'PenWidth' then
              begin
                N2T;
                b.PenWidth := TokenInt;
              end;
              if TokenString = 'CompName' then
              begin
                N2T;
                b.CompName := TokenString;
              end;
              if TokenString = 'FName' then
              begin
                N2T;
                b.FName := TokenString;
              end;
              if TokenString = 'FSize' then
              begin
                N2T;
                b.FSize := TokenInt;
              end;
              if TokenString = 'FColor' then
              begin
                N2T;
                b.FColor := TokenInt;
              end;
              if TokenString = 'FStyle' then
              begin
                N2T;
                b.FStyle := TokenInt;
              end;
              if TokenString = 'OLELinkToFile' then
              begin
                N2T;
                b.OLELinkToFile := TokenString;
              end;
              if TokenString = 'OLESizeMode' then
              begin
                N2T;
                b.OLESizeMode := TokenInt;
              end;
              if TokenString = 'Fixed' then
              begin
                N2T;
                b.Fixed := TokenInt;
              end;
            end;
        end;
        c := NextToken;
      end;

    B.Parent := ParentWnd;
    try
      B.ContainOLE := B.OLELinkToFile <> '';
    except
      Application.MessageBox('OLE: Linked object not found.', 'Error',
        MB_APPLMODAL or MB_OK or MB_ICONSTOP);
    end;
    B.Name := sName;
    if B.CompName = '' then
      SetUnicalName(B);
    AnalyzeParams(B, B.CompName);
  end;

  procedure ClearOwner;
  var
    i: integer;
  begin
    //    ParamNames.Clear;
    //    ParamMasks.Clear;
    //    ParamValues.Clear;
    //    ParamTypes.Clear;
    ComponentList.Clear;
    if Assigned(ParentWnd) then
    begin
      with ParentWnd do
        for i := ControlCount - 1 downto 0 do
          if Controls[i] is TJvgReportItem then
            RemoveControl(Controls[i]);
      with OwnerWnd do
        for i := ComponentCount - 1 downto 0 do
        begin
          if Components[i] is TJvgReportItem then
          begin
            Compon := Components[i];
            RemoveComponent(Compon);
            Compon.Free;
          end;
        end;
    end;
  end;

begin
  ValidateWnds;
  if fNeedClearOwner then
    ClearOwner
  else
    ClearReport;
  ms := TMemoryStream.Create;
  FReportList.SaveToStream(ms);
  ms.Position := 0;
  p := TParser.Create(ms);
  c := p.Token;
  with p do
    repeat
      if TokenSymbolIs('object') then //...only noname objects!
      begin
        NextToken;
        sClassName := TokenString;
        try
          Create_Object(sClassName, sName);
        except
          Application.MessageBox('Error reading component.', 'Error',
            MB_APPLMODAL or MB_OK or MB_ICONSTOP);
        end;
      end;
      c := NextToken;
    until c = toEOF;

  p.Free;
  ms.Free;
end;

function TJvgReport.AddComponent: TJvgReportItem;
begin
  //AnalyzeParams( ReportComponent );
  ValidateWnds;
  Result := TJvgReportItem.Create(OwnerWnd);
  Result.Report := self;
  SetUnicalName(Result);
  Result.Parent := ParentWnd;
  ComponentList.Add(Result);
end;

procedure TJvgReport.SetUnicalName(laBevel: TJvgReportItem);
var
  i: integer;

  function ComponentExists(No: integer): boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := 0 to OwnerWnd.ComponentCount - 1 do
      if OwnerWnd.Components[i] is TJvgReportItem then
        if TJvgReportItem(OwnerWnd.Components[i]).CompName = 'Component' +
          IntToStr(No) then
        begin
          Result := true;
          break;
        end;
  end;
begin
  i := 0;
  repeat
    inc(i);
  until not ComponentExists(i);
  laBevel.CompName := 'Component' + IntToStr(i);
end;

procedure TJvgReport.AnalyzeParams(Item: TJvgReportItem; DefName: string);
var
  ParamIndex, LastPos: integer;
  SList: TStringList;
  ParamType: TglRepParamType;
  ParamText, ParamName, ParamMask, ParamValue: string;

  function ExtractParam(Item: TJvgReportItem; var SrchPos: integer; var
    ParamName: string; var ParamType: TglRepParamType): boolean;
  var
    i, j, p: integer;
    f: boolean;
    Text: string;
  begin
    Result := false;
    Text := Item.Text;
    if Length(Text) = 0 then
      exit;
    f := false;
    for i := SrchPos to Length(Text) - 1 do
      if Text[i] = '#' then
      begin
        f := true;
        break;
      end;

    if not f then
      exit;
    if Text[i - 1] = '{' then
      ParamType := gptEdit
    else if Text[i - 1] = '<' then
      ParamType := gptRadio
    else if Text[i - 1] = '[' then
      ParamType := gptCheck
    else
      ParamType := gptUnknown;

    if not f or (ParamType = gptUnknown) then
      exit;
    SrchPos := i + 1;
    f := false;
    for i := SrchPos to Length(Text) do
      if (Text[i] = '}') or (Text[i] = ']') or (Text[i] = '>') then
      begin
        f := true;
        break;
      end;
    if not f then
      exit;
    ParamName := copy(Text, SrchPos, i - SrchPos);

    j := ParamNames.IndexOf(ParamName);
    if j <> -1 then
      Item.PrintText := copy(Text, 0, SrchPos - 3) + ParamValues[j] +
        copy(Text, i + 1, 255);

    Result := true;
  end;
begin

  LastPos := 0;
  SList := TStringList.Create;
  repeat

    if ExtractParam(Item, LastPos, ParamText, ParamType) then
    begin
      ParamMask := '';
      ParamValue := '';
      ParamTypes.Add(Pointer(ParamType));
      if ParamType = gptEdit then
      begin
        if ParamText = '' then
          ParamText := DefName;
        SList.CommaText := ParamText;
        if SList.Count = 0 then
          continue;
        ParamName := SList[0];
        if SList.Count > 1 then
          ParamMask := SList[1];
        if SList.Count > 2 then
          ParamValue := SList[2];
      end
      else
        ParamName := ParamText;
      if ParamNames.IndexOf(ParamName) <> -1 then
        continue; //...already exists
      ParamNames.Add(ParamName);
      ParamMasks.Add(ParamMask);
      ParamValues.Add(ParamValue);
      // else ParamValues[ParamIndex] := sParamValue;
    end
    else
      break;
  until false;
  SList.Free;
end;

function TJvgReport.SetParam(const sParamName, sParamValue: string): boolean;
var
  i: integer;
begin
  Result := false;
  i := ParamNames.IndexOf(sParamName);
  if i <> -1 then
  begin
    Result := true;
    ParamValues[i] := sParamValue;
  end;
end;

function TJvgReport.GetParam(const sParamName: string; var sParamValue: string):
  boolean;
var
  i, ParamIndex: integer;
begin
  ParamIndex := ParamNames.IndexOf(sParamName);
  if ParamIndex = -1 then
    Result := false
  else
  begin
    Result := true;
    sParamValue := ParamValues[ParamIndex];
  end;
end;

procedure TJvgReport.ValidateWnds;
begin
  {$IFDEF COMPILER3_UP}OwnerWnd := ParentWnd;
  {$ENDIF}
  //  if (OwnerWnd=nil)or(ParentWnd=nil) then raise Exception.Create('TJvgReport: Unassigned Owner or Parent window.');
end;

function TJvgReport.GetReportText: TStringList;
begin
  Result := FReportList;
end;

procedure TJvgReport.SetReportText(Value: TStringList);
begin
  FReportList.Assign(Value);
end;

end.
