{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReport.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgReport;

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  Forms, OleCtnrs, ExtCtrls, SysUtils, Printers,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvgUtils, JvgTypes, JvgCommClasses;

type
  TJvgReport = class;

  TJvgReportParamKind = (gptUnknown, gptEdit, gptRadio, gptCheck);

  TJvgReportScrollBox = class(TScrollBox)
  private
    FGridImage: TBitmap;
    FOnDraw: TNotifyEvent;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
  end;

  {$IFDEF USEJVCL}
  TJvgReportItem = class(TJvGraphicControl)
  {$ELSE}
  TJvgReportItem = class(TGraphicControl)
  {$ENDIF USEJVCL}
  private
    FSelected: Boolean;
    FBkColor: Integer;
    FBvColor: Integer;
    FTransparent: Integer;
    FAlignment: Word; //..1-left,2-right,3-center,4-boadwise
    FSideLeft, FSideTop, FSideRight, FSideBottom: Word;
    FPenStyle: Integer;
    FPenWidth: Word;
    FText: string;
    PrintText: string;
    FCompName: string;
    FFName: string;
    FFSize, FFColor, FFStyle: Integer;
    FContainOLE: Boolean;
    FFixed: Word;
    FOLELinkToFile: string;
    FOLESizeMode: Word;
    //    fRepaintOnlyBorder,
    fSizing: Boolean;
    R: array[1..8] of TRect;
    DownPos: TPoint;
    SizeDirection: Integer;
    FExternalCanvas: TCanvas;
    Cursors: array[1..8] of TCursor;
    bmp: TBitmap;
    Report: TJvgReport;

    procedure SetSelected(Value: Boolean);
    procedure SetBkColor(Value: Integer);
    procedure SetBvColor(Value: Integer);
    procedure SetTransparent(Value: Integer);
    procedure SetAlignment(Value: Word);
    procedure SetSideLeft(Value: Word);
    procedure SetSideTop(Value: Word);
    procedure SetSideRight(Value: Word);
    procedure SetSideBottom(Value: Word);
    procedure SetPenStyle(Value: Integer);
    procedure SetPenWidth(Value: Word);
    procedure SetText(Value: string);
    procedure SetFName(Value: string);
    procedure SetFSize(Value: Integer);
    procedure SetFColor(Value: Integer);
    procedure SetFStyle(Value: Integer);
    procedure SetContainOLE(Value: Boolean);
    procedure SetOLELinkToFile(Value: string);
    procedure SetOLESizeMode(Value: Word);
    procedure SetFixed(Value: Word);
    function IsContainOLE: Boolean;

    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMLMouseDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLMouseUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  {$IFDEF USEJVCL}
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure FontChanged; override;
  {$ELSE}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    procedure FontChanged; dynamic;
  {$ENDIF USEJVCL}
  public
    procedure Paint; override;
    procedure PaintTo(Canvas: TCanvas);
  protected
    procedure SetParent(Value: TWinControl); override;
  public
    ResText: string;
    OLEContainer: TOLEContainer;
    property Selected: Boolean read FSelected write SetSelected default False;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //    property OnResize;
    property ExternalCanvas: TCanvas read FExternalCanvas write FExternalCanvas;
    //    procedure RepaintBorder;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BkColor: Integer read FBkColor write SetBkColor default clWhite;
    property BvColor: Integer read FBvColor write SetBvColor default clBlack;
    property Transparent: Integer read FTransparent write SetTransparent default 0;
    property Alignment: Word read FAlignment write SetAlignment default 1;
    property SideLeft: Word read FSideLeft write SetSideLeft default 1;
    property SideTop: Word read FSideTop write SetSideTop default 1;
    property SideRight: Word read FSideRight write SetSideRight default 1;
    property SideBottom: Word read FSideBottom write SetSideBottom default 1;
    property PenStyle: Integer read FPenStyle write SetPenStyle default Integer(psSolid);
    property PenWidth: Word read FPenWidth write SetPenWidth default 1;
    property Text: string read FText write SetText;
    property CompName: string read FCompName write FCompName;
    property FName: string read FFName write SetFName;
    property FSize: Integer read FFSize write SetFSize;
    property FColor: Integer read FFColor write SetFColor;
    property FStyle: Integer read FFStyle write SetFStyle;
    property ContainOLE: Boolean read FContainOLE write SetContainOLE default False;
    property OLELinkToFile: string read FOLELinkToFile write SetOLELinkToFile stored IsContainOLE;
    property OLESizeMode: Word read FOLESizeMode write SetOLESizeMode stored IsContainOLE default 2;
    property Fixed: Word read FFixed write SetFixed default 0;
  end;

  TJvgReportBeforePrintEvent = procedure(Sender: TJvgReport) of object;

  {$IFDEF USEJVCL}
  TJvgReport = class(TJvComponent)
  {$ELSE}
  TJvgReport = class(TComponent)
  {$ENDIF USEJVCL}
  private
    procedure ValidateWnds;
    function GetReportText: TStringList;
    procedure SetReportText(Value: TStringList);
  public
    OwnerWnd, ParentWnd: TWinControl;
    ParamNames: TStringList;
    ParamValues: TStringList;
    ParamMasks: TStringList;
    ParamTypes: TList;
    FReportList: TStringList;
    ComponentList: TList;
    FBeforePrint: TJvgReportBeforePrintEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure PaintTo(Canvas: TCanvas);
    procedure PreviewTo(Window: TWinControl);
    procedure Print;
    procedure CreateReport(ParentWnd: TWinControl; fNeedClearOwner: Boolean);
    function SetParam(const sParamName, sParamValue: string): Boolean;
    function GetParam(const sParamName: string; var sParamValue: string): Boolean;
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
    property BeforePrint: TJvgReportBeforePrintEvent read FBeforePrint write FBeforePrint;
  end;

implementation

uses
  Math,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts;

const
  S = 2;
  DS = 2 * S + 1;

{$IFNDEF USEJVCL}
resourcestring
  RsOLELinkedObjectNotFound = 'OLE: Linked object not found.';
  RsErrorText = 'Error';
  RsErrorReadingComponent = 'Error reading component.';
{$ENDIF USEJVCL}

//=== TJvgReportScrollBox ====================================================

constructor TJvgReportScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGridImage := TBitmap.Create;
  FGridImage.Width := 8;
  FGridImage.Height := 8;
  FGridImage.Canvas.Brush.Color := clWhite; //clWindow;
  FGridImage.Canvas.FillRect(Rect(0, 0, 8, 8));
  FGridImage.Canvas.Pixels[7, 7] := 0;
end;

destructor TJvgReportScrollBox.Destroy;
begin
  FGridImage.Free;
  inherited Destroy;
end;

procedure TJvgReportScrollBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
  if csDestroying in ComponentState then
    Exit;
  with TCanvas.Create do
    try
      Handle := Msg.DC;
      //    Pen.Color := clWindow;
      //    Brush.Color := clWindow;
      //    Brush.Style := bsCross;
      Brush.Bitmap := FGridImage;
      FillRect(ClientRect);
      Handle := 0;
    finally
      Free;
    end;
  if Assigned(FOnDraw) then
    FOnDraw(Self);
end;

//=== TJvgReportItem =========================================================

constructor TJvgReportItem.Create(AOwner: TComponent);
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
  FPenStyle := Integer(psSolid);
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
  ParentFont := False;
  {$IFDEF GL_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF GL_RUS}
  FontChanged;
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
var
  R: TRect;
begin
  R := ClientRect;
  OffsetRect( R, Left, Top );
  InvalidateRect( Parent.Handle, @R, False );
  InflateRect( R, -DS, -DS );
//  ValidateRect( Parent.Handle, @R );
  fRepaintOnlyBorder := True;
  Paint;
  //fRepaintOnlyBorder := False;
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
  i, L, T: Integer;
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
    if SideLeft <> 0 then
    begin
      MoveTo(L + PenWidth div 2, T + Height - 1);
      LineTo(L + PenWidth div 2, T);
    end;
    if SideTop <> 0 then
    begin
      MoveTo(L + PenWidth div 2, T + PenWidth div 2);
      LineTo(L + Width - PenWidth, T + PenWidth div 2);
    end;
    if SideRight <> 0 then
    begin
      MoveTo(L + Width - 1, T);
      LineTo(L + Width - 1, T + Height - 1);
    end;
    if SideBottom <> 0 then
    begin
      MoveTo(L + Width - 1, T + Height - 1);
      LineTo(L, T + Height - 1);
    end;

    if not ContainOLE then
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      SetTextColor(Canvas.Handle, FColor);
      DrawText(Canvas.Handle, PChar(sPrintText), Length(sPrintText), RCalc,
        DT_CALCRECT or DT_WordBREAK);
      R_.Top := R_.Top + max(0, (R_.Bottom - R_.Top - (RCalc.Bottom -
        RCalc.Top)) div 2);
      DrawTextExtAligned(Canvas, sPrintText, R_, Alignments[Alignment],
        True);
    end
    else
    if (OLELinkToFile <> '') and (ExtractFileExt(OLELinkToFile) = '.bmp') then
    begin
      if Assigned(OLEContainer) then
        OLEContainer.Visible := False;
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
      if Fixed <> 0 then
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

procedure TJvgReportItem.MouseEnter(Control: TControl);
begin
  {$IFDEF USEJVCL}
  inherited MouseEnter(Control);
  {$ENDIF USEJVCL}
  if csDesigning in ComponentState then
    Exit;
  //Cursor := crCross;
//  SetCursor( Screen.Cursors[crCross] );
end;

procedure TJvgReportItem.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  Cursor := crDefault;
  {$IFDEF USEJVCL}
  inherited MouseLeave(Control);
  {$ENDIF USEJVCL}
  //  SetCursor( Screen.Cursors[crDefault] );
end;

procedure TJvgReportItem.WMMouseMove(var Message: TWMMouse);
var
  i, dX, dY, nLeft, nTop, nWidth, nHeight: Integer;
  pt: TPoint;
begin
  inherited;
  if Fixed = 0 then
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
            Exit;
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
  fSizing := False;
  inherited;
end;

procedure TJvgReportItem.FontChanged;
begin
  {$IFDEF USEJVCL}
  inherited FontChanged;
  {$ENDIF USEJVCL}
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
  // if Assigned(OnResize) then OnResize(Self);
end;

{$IFNDEF USEJVCL}

procedure TJvgReportItem.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter(TControl(Message.LParam));
end;

procedure TJvgReportItem.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave(TControl(Message.LParam));
end;

procedure TJvgReportItem.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FontChanged;
end;

{$ENDIF USEJVCL}

procedure TJvgReportItem.SetSelected(Value: Boolean);
begin
  FSelected := Value;
  Repaint;
end;

procedure TJvgReportItem.SetBkColor(Value: Integer);
begin
  FBkColor := Value;
  Color := BkColor;
  Repaint;
end;

procedure TJvgReportItem.SetBvColor(Value: Integer);
begin
  FBvColor := Value;
  Repaint;
end;

procedure TJvgReportItem.SetTransparent(Value: Integer);
begin
  FTransparent := Value;
  Repaint;
end;

procedure TJvgReportItem.SetAlignment(Value: Word);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideLeft(Value: Word);
begin
  FSideLeft := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideTop(Value: Word);
begin
  FSideTop := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideRight(Value: Word);
begin
  FSideRight := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetSideBottom(Value: Word);
begin
  FSideBottom := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetPenStyle(Value: Integer);
begin
  FPenStyle := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetPenWidth(Value: Word);
begin
  FPenWidth := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetText(Value: string);
begin
  if FText = Value then
    Exit;
  FText := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFName(Value: string);
begin
  FFName := Value;
  Canvas.Font.Name := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFSize(Value: Integer);
begin
  FFSize := Value;
  Canvas.Font.Size := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFColor(Value: Integer);
begin
  FFColor := Value;
  Canvas.Font.Color := Value;
  Invalidate;
end;

procedure TJvgReportItem.SetFStyle(Value: Integer);
begin
  FFStyle := Value;
  with Canvas.Font do
  begin
    if (Value and 1) <> 0 then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];
    if (Value and (1 shl 1)) <> 0 then
      Style := Style + [fsItalic]
    else
      Style := Style - [fsItalic];
    if (Value and (1 shl 2)) <> 0 then
      Style := Style + [fsUnderline]
    else
      Style := Style - [fsUnderline];
  end;
  Invalidate;
end;

procedure TJvgReportItem.SetContainOLE(Value: Boolean);
begin
  FContainOLE := Value;
  if FContainOLE and (not Assigned(OLEContainer)) then
  begin
    if not Assigned(Parent) then
      Exit;
    OLEContainer := TOLEContainer.Create(parent.parent);
    OLEContainer.AutoVerbMenu := False;
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
    Exit;
  OLEContainer.CreateLinkToFile(Value, False);
  //OLEContainer.LoadFromFile( Value );
end;

procedure TJvgReportItem.SetFixed(Value: Word);
begin
  FFixed := Value;
  Repaint;
end;

procedure TJvgReportItem.SetOLESizeMode(Value: Word);
begin
  if FOLESizeMode = Value then
    Exit;
  FOLESizeMode := Value;
  if Assigned(OLEContainer) then
    OLEContainer.SizeMode := TSizeMode(Value);
end;

function TJvgReportItem.IsContainOLE: Boolean;
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
  CreateReport(nil, False);
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
  fs: TFileStream;
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
  CreateReport(True);
end;}

procedure TJvgReport.PaintTo(Canvas: TCanvas);
var
  i: Integer;
begin
  OwnerWnd := nil;
  ParentWnd := nil;
  //  ParamNames.Clear;
  //  ParamMasks.Clear;
  //  ParamValues.Clear;
  //  ParamTypes.Clear;
  ComponentList.Clear;
  CreateReport(ParentWnd, False);
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
  CreateReport(ParentWnd, False);
  //  ProcessParams;
end;

procedure TJvgReport.Print;
var
  i: Integer;
  ScreenDC: HDC;
  HS, WS, HP, WP: Integer;
begin
  if Assigned(BeforePrint) then
    BeforePrint(Self);
  OwnerWnd := TForm.Create(nil);
  TForm(OwnerWnd).WindowState := wsMaximized;
  ParentWnd := OwnerWnd;
  //OwnerWnd.Show;
  try
    CreateReport(ParentWnd, True);
    if ComponentList.Count = 0 then
      Exit;

    Printer.BeginDoc;
    ScreenDC := GetDC(0);

    HS := CentimetersToPixels(ScreenDC, 21, True);
    WS := CentimetersToPixels(ScreenDC, 21, False);
    HP := CentimetersToPixels(Printer.Canvas.Handle, 21, True);
    WP := CentimetersToPixels(Printer.Canvas.Handle, 21, False);

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
  i: Integer;
begin
  for i := 0 to ComponentList.Count - 1 do
    TJvgReportItem(ComponentList[i]).Free;
  ComponentList.Count := 0;
end;

procedure TJvgReport.CreateReport(ParentWnd: TWinControl; fNeedClearOwner:
  Boolean);
var
  ms: TMemoryStream;
  p: TParser;
  c: char;
  Compon: TComponent;
  sName, sClassName: string;
  S1, S2: string;

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
      B.Report := Self;
    end;
    if B = nil then
      Exit;
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
      S1 := RsOLELinkedObjectNotFound;
      S2 := RsErrorText;
      Application.MessageBox(PChar(S1), PChar(S2),
        MB_APPLMODAL or MB_OK or MB_ICONSTOP);
    end;
    B.Name := sName;
    if B.CompName = '' then
      SetUnicalName(B);
    AnalyzeParams(B, B.CompName);
  end;

  procedure ClearOwner;
  var
    i: Integer;
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
          S1 := RsErrorReadingComponent;
          S2 := RsErrorText;
          Application.MessageBox(PChar(S1), PChar(S2),
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
  Result.Report := Self;
  SetUnicalName(Result);
  Result.Parent := ParentWnd;
  ComponentList.Add(Result);
end;

procedure TJvgReport.SetUnicalName(laBevel: TJvgReportItem);
var
  i: Integer;

  function ComponentExists(No: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to OwnerWnd.ComponentCount - 1 do
      if OwnerWnd.Components[i] is TJvgReportItem then
        if TJvgReportItem(OwnerWnd.Components[i]).CompName = 'Component' +
          IntToStr(No) then
        begin
          Result := True;
          Break;
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
  LastPos: Integer;
  SList: TStringList;
  ParamType: TJvgReportParamKind;
  ParamText, ParamName, ParamMask, ParamValue: string;

  function ExtractParam(Item: TJvgReportItem; var SrchPos: Integer; var
    ParamName: string; var ParamType: TJvgReportParamKind): Boolean;
  var
    i, j: Integer;
    f: Boolean;
    Text: string;
  begin
    Result := False;
    Text := Item.Text;
    if Length(Text) = 0 then
      Exit;
    f := False;
    for i := SrchPos to Length(Text) - 1 do
      if Text[i] = '#' then
      begin
        f := True;
        Break;
      end;

    if not f then
      Exit;
    if Text[i - 1] = '{' then
      ParamType := gptEdit
    else
    if Text[i - 1] = '<' then
      ParamType := gptRadio
    else
    if Text[i - 1] = '[' then
      ParamType := gptCheck
    else
      ParamType := gptUnknown;

    if not f or (ParamType = gptUnknown) then
      Exit;
    SrchPos := i + 1;
    f := False;
    for i := SrchPos to Length(Text) do
      if (Text[i] = '}') or (Text[i] = ']') or (Text[i] = '>') then
      begin
        f := True;
        Break;
      end;
    if not f then
      Exit;
    ParamName := copy(Text, SrchPos, i - SrchPos);

    j := ParamNames.IndexOf(ParamName);
    if j <> -1 then
      Item.PrintText := copy(Text, 0, SrchPos - 3) + ParamValues[j] +
        copy(Text, i + 1, 255);

    Result := True;
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
      Break;
  until False;
  SList.Free;
end;

function TJvgReport.SetParam(const sParamName, sParamValue: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := ParamNames.IndexOf(sParamName);
  if i <> -1 then
  begin
    Result := True;
    ParamValues[i] := sParamValue;
  end;
end;

function TJvgReport.GetParam(const sParamName: string; var sParamValue: string):
  Boolean;
var
  ParamIndex: Integer;
begin
  ParamIndex := ParamNames.IndexOf(sParamName);
  if ParamIndex = -1 then
    Result := False
  else
  begin
    Result := True;
    sParamValue := ParamValues[ParamIndex];
  end;
end;

procedure TJvgReport.ValidateWnds;
begin
  OwnerWnd := ParentWnd;
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

