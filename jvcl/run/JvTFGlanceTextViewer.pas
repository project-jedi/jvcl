{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFGlanceTextViewer.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTFGlanceTextViewer;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvTFManager, JvTFGlance, JvTFUtils;

type
  TJvTFGlanceTextViewer = class;

  TJvTFGlTxtVwDrawInfo = record
    Cell: TJvTFGlanceCell;
    Font: TFont;
    Color: TColor;
    aRect: TRect;
  end;

  TJvTFGlTxtVwPointInfo = record
    AbsX: Integer;
    AbsY: Integer;
    AbsLineNum: Integer;
    RelLineNum: Integer;
  end;

  TJvTFGVTxtEditor = class(TMemo)
  private
    FLinkedAppt: TJvTFAppt;
  protected
    FCancelEdit: Boolean;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LinkedAppt: TJvTFAppt read FLinkedAppt write FLinkedAppt;
  end;

  {$IFDEF USEJVCL}
  TJvTFGVTextControl = class(TJvCustomControl)
  {$else}
  TJvTFGVTextControl = class(TCustomControl)
  {$ENDIF USEJVCL}
  private
    FViewer: TJvTFGlanceTextViewer;
    FReplicating: Boolean;
    FMouseLine: Integer;
    function GetGlanceControl: TJvTFCustomGlance;
    procedure SetTopLine(Value: Integer);
    function GetTopLine: Integer;
  protected
    FMousePtInfo: TJvTFGlTxtVwPointInfo;
    FDDBtnRect: TRect;
    FMouseInControl: Boolean;
    FScrollUpBtnBMP: TBitmap;
    FScrollDnBtnBMP: TBitmap;
    FEditor: TJvTFGVTxtEditor;
    {$IFDEF USEJVCL}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    {$ENDIF USEJVCL}

    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure DoEnter; override;
    procedure DoExit; override;

    procedure SetMouseLine(Value: Integer);
    property MouseLine: Integer read FMouseLine write SetMouseLine;
    procedure UpdateDDBtnRect;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseAccel(X, Y: Integer);

    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;

    property Replicating: Boolean read FReplicating;
    procedure Paint; override;
    procedure DrawDDButton(ACanvas: TCanvas);
    procedure DrawArrow(ACanvas: TCanvas; aRect: TRect; Direction: TJvTFDirection);
    procedure DrawScrollUpBtn(ACanvas: TCanvas; aCellRect: TRect);
    procedure DrawScrollDnBtn(ACanvas: TCanvas; aCellRect: TRect);
    function GetStartEndString(Appt: TJvTFAppt): string;

    function CalcLineHeight: Integer;
    function CalcAbsLineNum(Y: Integer): Integer;
    function LineRect(AbsLineNum: Integer): TRect;
    function CalcPointInfo(X, Y: Integer): TJvTFGlTxtVwPointInfo;
    function RelToAbs(Rel: Integer): Integer;
    function AbsToRel(Abs: Integer): Integer;
    function FindApptAtLine(RelLineNum: Integer): TJvTFAppt;
    function GetApptRelLineNum(Appt: TJvTFAppt): Integer;

    procedure Scroll(ScrollBy: Integer);
    function ScrollUpBtnRect(aCellRect: TRect): TRect;
    function ScrollDnBtnRect(aCellRect: TRect): TRect;
    procedure InitScrollUpBtnBMP;
    procedure InitScrollDnBtnBMP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PaintTo(ACanvas: TCanvas; DrawInfo: TJvTFGlTxtVwDrawInfo); overload;

    property Viewer: TJvTFGlanceTextViewer read FViewer;
    property GlanceControl: TJvTFCustomGlance read GetGlanceControl;

    // editor management routines
    //procedure EditAppt(Col, Row: Integer; Appt: TJvTFAppt);
    procedure EditAppt(ACell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
    procedure FinishEditAppt;
    function Editing: Boolean;
    function CanEdit: Boolean;

    function LineCount: Integer;
    function AbsLineCount: Integer;
    function ViewableLines: Integer;
    function FullViewableLines: Integer;
    property TopLine: Integer read GetTopLine write SetTopLine;

    function GetApptAt(X, Y: Integer): TJvTFAppt;
    function GetApptAccel(X, Y: Integer): TJvTFAppt;
  end;

  TJvTFLineDDClickEvent = procedure(Sender: TObject; LineNum: Integer) of object;

  TJvTFTxtVwApptAttr = class(TPersistent)
  private
    FColor: TColor;
    FFontColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetFontColor(Value: TColor);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property FontColor: TColor read FFontColor write SetFontColor default clWhite;
  end;

  TJvTFGlTxtVwEditorAlign = (eaLine, eaCell);

  TJvTFGlanceTextViewer = class(TJvTFGlanceViewer)
  private
    FViewControl: TJvTFGVTextControl;
    FLineSpacing: Integer;
    FEditorAlign: TJvTFGlTxtVwEditorAlign;
    FOnLineDDClick: TJvTFLineDDClickEvent;
    FShowStartEnd: Boolean;
    FTopLines: TStringList;
    FSelApptAttr: TJvTFTxtVwApptAttr;
    FSelAppt: TJvTFAppt;
    procedure SetLineSpacing(Value: Integer);
    procedure SetSelApptAttr(Value: TJvTFTxtVwApptAttr);
    procedure SetEditorAlign(Value: TJvTFGlTxtVwEditorAlign);
    procedure SetShowStartEnd(Value: Boolean);
    function GetCellString(ACell: TJvTFGlanceCell): string;
  protected
    procedure SetVisible(Value: Boolean); override;
    procedure SetGlanceControl(Value: TJvTFCustomGlance); override;
    procedure SelApptAttrChange(Sender: TObject);
    procedure Change; virtual;
    procedure LineDDClick(LineNum: Integer); virtual;
    procedure ParentReconfig; override;
    procedure SetSelAppt(Value: TJvTFAppt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); override;
    procedure MouseAccel(X, Y: Integer); override;

    procedure Refresh; override;
    procedure Realign; override;
    procedure PaintTo(ACanvas: TCanvas; ACell: TJvTFGlanceCell); override;
    function GetDrawInfo(ACell: TJvTFGlanceCell): TJvTFGlTxtVwDrawInfo;
    procedure ResetTopLines;
    property SelAppt: TJvTFAppt read FSelAppt;

    procedure SetTopLine(ACell: TJvTFGlanceCell; Value: Integer);
    function GetTopLine(ACell: TJvTFGlanceCell): Integer;
    function GetApptAt(X, Y: Integer): TJvTFAppt; override;

    // editor management routines
    procedure EditAppt(ACell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
    procedure FinishEditAppt; override;
    function Editing: Boolean; override;
    function CanEdit: Boolean; override;
  published
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property OnLineDDClick: TJvTFLineDDClickEvent read FOnLineDDClick write FOnLineDDClick;
    property SelApptAttr: TJvTFTxtVwApptAttr read FSelApptAttr write SetSelApptAttr;
    property EditorAlign: TJvTFGlTxtVwEditorAlign read FEditorAlign
      write SetEditorAlign default eaLine;
    property ShowStartEnd: Boolean read FShowStartEnd
      write SetShowStartEnd default True;
  end;

implementation

{$IFDEF USEJVCL}
uses
  JvJVCLUtils, JvResources;
{$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
  RsEGlanceControlNotAssigned = 'GlanceControl not assigned';
{$ENDIF USEJVCL}

//=== { TJvTFGVTextControl } =================================================

constructor TJvTFGVTextControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvTFGlanceTextViewer then
    FViewer := TJvTFGlanceTextViewer(AOwner);

  DoubleBuffered := True;

  FReplicating := True;
  FMouseLine := -1;

  FScrollUpBtnBMP := TBitmap.Create;
  InitScrollUpBtnBMP;
  FScrollDnBtnBMP := TBitmap.Create;
  InitScrollDnBtnBMP;

  FEditor := TJvTFGVTxtEditor.Create(Self);
  FEditor.Visible := False;
  FEditor.Parent := Self;
  //FEditor.Parent := Viewer.GlanceControl;
  // (rom) deactivated seems of no use
  // if FEditor.Parent = nil then
  //   Beep;
end;

function TJvTFGVTextControl.CalcAbsLineNum(Y: Integer): Integer;
begin
  Result := Y div CalcLineHeight;
end;

procedure TJvTFGVTextControl.DrawDDButton(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(FDDBtnRect);

    DrawArrow(ACanvas, FDDBtnRect, dirDown);

    Pen.Color := clBlack;
    Polyline([FDDBtnRect.TopLeft, Point(FDDBtnRect.Right, FDDBtnRect.Top),
      FDDBtnRect.BottomRight, Point(FDDBtnRect.Left, FDDBtnRect.Bottom),
      FDDBtnRect.TopLeft]);
    {
    if Windows.PtInRect(aRect, FMouseLoc) then
      begin
        Pen.Color := clBtnHighlight;
        MoveTo(aRect.Left, aRect.Top);
        LineTo(aRect.Left, aRect.Bottom);
        MoveTo(aRect.Left, aRect.Top);
        LineTo(aRect.Right, aRect.Top);

        Pen.Color := clBtnShadow;
        MoveTo(aRect.Right - 1, aRect.Top);
        LineTo(aRect.Right - 1, aRect.Bottom);
        MoveTo(aRect.Right, aRect.Bottom - 1);
        LineTo(aRect.Left, aRect.Bottom - 1);
      end;
    }
  end;
end;

function TJvTFGVTextControl.GetGlanceControl: TJvTFCustomGlance;
begin
  Result := nil;
  if Assigned(Viewer) then
    Result := Viewer.GlanceControl;
end;

function TJvTFGVTextControl.CalcLineHeight: Integer;
begin
  Result := Canvas.TextHeight('Wq') + Viewer.LineSpacing;
end;

function TJvTFGVTextControl.LineRect(AbsLineNum: Integer): TRect;
var
  LineHt: Integer;
begin
  LineHt := CalcLineHeight;
  Result := ClientRect;
  Result.Top := LineHt * AbsLineNum;
  Result.Bottom := Lesser(Result.Top + LineHt, Result.Bottom);
end;

procedure TJvTFGVTextControl.Paint;
var
  DrawInfo: TJvTFGlTxtVwDrawInfo;
begin
  {
  All drawing should be done in a PaintTo method.  PaintTo should have ACanvas
  and aRect Params.  All drawing code within PaintTo should rely solely on
  the ACanvas and aRect parameters given.

  This method (Paint) should then call PaintTo(Canvas, ClientRect) to draw the
  info on the viewer control.  TJvTFCustomGlance.DrawCell should call
  PaintTo(PaintBuffer, CellBodyRect(Col, Row, Selected, False)) to draw the
  info on the GlanceControl.
  }

  Viewer.SetTo(Viewer.PhysicalCell);
  DrawInfo := Viewer.GetDrawInfo(Viewer.Cell);
  DrawInfo.aRect := ClientRect;

  FReplicating := False;
  try
    PaintTo(Canvas, DrawInfo);
  finally
    FReplicating := True;
  end;

{
  // for TESTING PURPOSES ONLY!!
  with Canvas do
    begin
      Pen.Color := clBlack;
      MoveTo(0, 0);
      LineTo(ClientWidth, ClientHeight);
    end;
}
end;

procedure TJvTFGVTextControl.PaintTo(ACanvas: TCanvas; DrawInfo: TJvTFGlTxtVwDrawInfo);
var
  I, NextLineTop, LastLine: Integer;
  aRect, LineRect, TxtRect, BtnRect: TRect;
  Flags: UINT;
  Txt: string;
  PTxt: PChar;
  Appt: TJvTFAppt;
  RegFontColor,
  RegBrushColor: TColor;
begin
  Viewer.SetTo(DrawInfo.Cell);

  with ACanvas do
  begin
    aRect := DrawInfo.aRect;

    //Brush.Color := Viewer.Color;
    Brush.Color := DrawInfo.Color;
    FillRect(aRect);

    //Font.Assign(Viewer.Font);
    Font.Assign(DrawInfo.Font);
    Self.Canvas.Font.Assign(DrawInfo.Font);

    RegBrushColor := Brush.Color;
    RegFontColor := Font.Color;

    NextLineTop := aRect.Top;
    LineRect.Left := aRect.Left;
    LineRect.Right := aRect.Right;

    //Flags := DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER;
    Flags := DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_TOP;

    if csDesigning in ComponentState then
      LastLine := 2
    else
      LastLine := Lesser(ViewableLines - 1, LineCount - TopLine - 1);

    for I := 0 to LastLine do
    begin
      Brush.Color := RegBrushColor;
      Font.Color := RegFontColor;

      LineRect.Top := NextLineTop;
      LineRect.Bottom := Lesser(NextLineTop + CalcLineHeight, aRect.Bottom);

      if csDesigning in ComponentState then
        Txt := 'Appt ' + IntToStr(I)
      else
      begin
        Appt := Viewer.Appts[AbsToRel(I)];

        Txt := '';
        if Viewer.ShowStartEnd then
          Txt := GetStartEndString(Appt) + ': ';
        Txt := Txt + Appt.Description;

        if Appt = Viewer.SelAppt then
        begin
          Brush.Color := Viewer.SelApptAttr.Color;
          Font.Color := Viewer.SelApptAttr.FontColor;

          FillRect(LineRect);

          if I <> 0 then
          begin
            MoveTo(aRect.Left, LineRect.Top);
            LineTo(aRect.Right, LineRect.Top);
          end;
          if I <> AbsLineCount - 1 then
          begin
            MoveTo(aRect.Left, LineRect.Bottom - 1);
            LineTo(aRect.Right, LineRect.Bottom - 1);
          end;
        end;
      end;

      TxtRect := LineRect;
      Windows.InflateRect(TxtRect, -1, -1);

      PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
      StrPCopy(PTxt, Txt);
      Windows.DrawText(ACanvas.Handle, PTxt, -1, TxtRect, Flags);
      StrDispose(PTxt);

      Inc(NextLineTop, CalcLineHeight);
    end;
  end;

  if not (csDesigning in ComponentState) then
  begin
    if not Replicating and (FMousePtInfo.RelLineNum < Viewer.ApptCount) and
       FMouseInControl then
      DrawDDButton(ACanvas);

    BtnRect := ScrollUpBtnRect(DrawInfo.aRect);
    if not Windows.IsRectEmpty(BtnRect) then
      DrawScrollUpBtn(ACanvas, DrawInfo.aRect);

    BtnRect := ScrollDnBtnRect(DrawInfo.aRect);
    if not Windows.IsRectEmpty(BtnRect) then
      DrawScrollDnBtn(ACanvas, DrawInfo.aRect);

    {
    if TopLine > 0 then
      DrawScrollUpBtn(ACanvas, DrawInfo.aRect);

    BottomLine := TopLine + FullViewableLines - 1;
    LastLine := LineCount - 1;
    if BottomLine < LastLine then
      DrawScrollDnBtn(ACanvas, DrawInfo.aRect);
    }
  end;
end;

procedure TJvTFGVTextControl.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := LRESULT(False);
end;

procedure TJvTFGVTextControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  GlancePt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  FMousePtInfo := CalcPointInfo(X, Y);
  MouseLine := FMousePtInfo.AbsLineNum;
  //SetFocus;

  GlancePt := Point(X, Y);
  GlancePt := Viewer.GlanceControl.ScreenToClient(ClientToScreen(Point(X, Y)));
  Viewer.GlanceControl.CheckViewerApptHint(GlancePt.X, GlancePt.Y);

  // for TESTING ONLY!!!
  //Invalidate;
  ////////////////////
end;

procedure TJvTFGVTextControl.SetMouseLine(Value: Integer);
begin
  if Value <> FMouseLine then
  begin
    FMouseLine := Value;
    UpdateDDBtnRect;
    Invalidate;
  end;
end;

procedure TJvTFGVTextControl.DrawArrow(ACanvas: TCanvas; aRect: TRect;
  Direction: TJvTFDirection);
var
  I, ArrowHeight, ArrowWidth, BaseX, BaseY: Integer;
begin
  ArrowWidth := RectWidth(aRect) - 2;
  if not Odd(ArrowWidth) then
    Dec(ArrowWidth);
  ArrowHeight := (ArrowWidth + 1) div 2;

  case Direction of
    dirUp:
      begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowWidth div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 + ArrowHeight div 2 - 1;

        for I := ArrowHeight downto 1 do
          with ACanvas do
          begin
            MoveTo(BaseX, BaseY);
            LineTo(BaseX + I * 2 - 1, BaseY);
            Inc(BaseX);
            Dec(BaseY);
          end;
      end;
    dirDown:
      begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowWidth div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowHeight div 2 + 1;

        for I := ArrowHeight downto 1 do
        with ACanvas do
          begin
            MoveTo(BaseX, BaseY);
            LineTo(BaseX + I * 2 - 1, BaseY);
            Inc(BaseX);
            Inc(BaseY);
          end;
      end;
    dirLeft:
      begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 + ArrowHeight div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowWidth div 2;

        for I := ArrowHeight downto 1 do
          with ACanvas do
          begin
            MoveTo(BaseX, BaseY);
            LineTo(BaseX, BaseY + I * 2 - 1);
            Dec(BaseX);
            Inc(BaseY);
          end;
      end;
  else
    BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowHeight div 2;
    BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowWidth div 2;

    for I := ArrowHeight downto 1 do
      with ACanvas do
      begin
        MoveTo(BaseX, BaseY);
        LineTo(BaseX, BaseY + I * 2 - 1);
        Inc(BaseX);
        Inc(BaseY);
      end;
  end;
end;

procedure TJvTFGVTextControl.UpdateDDBtnRect;
begin
  FDDBtnRect := LineRect(FMousePtInfo.AbsLineNum);
  FDDBtnRect.Right := ClientRect.Right - 1;
  FDDBtnRect.Left := FDDBtnRect.Right - 10;
  Inc(FDDBtnRect.Top, 2);
  Dec(FDDBtnRect.Bottom, 1);
end;

procedure TJvTFGVTextControl.DoEnter;
begin
  inherited DoEnter;
  Viewer.SetSelAppt(FindApptAtLine(FMousePtInfo.RelLineNum));
end;

procedure TJvTFGVTextControl.DoExit;
begin
  inherited DoExit;
  FMouseLine := -1; 
end;

{
function TJvTFGVTextControl.LineCount: Integer;
var
  ACell: TJvTFGlanceCell;
  I: Integer;
begin
  Result := 0;
  ACell := Viewer.GlanceControl.Cells.Cells[Viewer.Col, Viewer.Row];

  for I := 0 to ACell.ScheduleCount - 1 do
    Inc(Result, ACell.Schedules[I].ApptCount);
end;
}

function TJvTFGVTextControl.LineCount: Integer;
begin
  Result := Viewer.ApptCount;
end;

procedure TJvTFGVTextControl.SetTopLine(Value: Integer);
begin
  Viewer.SetTopLine(Viewer.Cell, Value);
end;

function TJvTFGVTextControl.CalcPointInfo(X, Y: Integer): TJvTFGlTxtVwPointInfo;
begin
  with Result do
  begin
    AbsX := X;
    AbsY := Y;
    AbsLineNum := CalcAbsLineNum(Y);
    RelLineNum := TopLine + AbsLineNum;
  end;
end;

function TJvTFGVTextControl.ViewableLines: Integer;
var
  aRect: TRect;
begin
  aRect := GlanceControl.CalcCellBodyRect(Viewer.Cell,
    GlanceControl.CellIsSelected(Viewer.Cell), False);

  Result := RectHeight(aRect) div CalcLineHeight;
  if RectHeight(aRect) mod CalcLineHeight > 0 then
    Inc(Result);
end;

function TJvTFGVTextControl.AbsToRel(Abs: Integer): Integer;
begin
  Result := TopLine + Abs;
end;

function TJvTFGVTextControl.RelToAbs(Rel: Integer): Integer;
begin
  Result := Rel - TopLine;
end;

procedure TJvTFGVTextControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Appt: TJvTFAppt;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;

  if Windows.PtInRect(ScrollDnBtnRect(ClientRect), Point(X, Y)) then
    Scroll(1)
  else
  if Windows.PtInRect(ScrollUpBtnRect(ClientRect), Point(X, Y)) then
    Scroll(-1)
  else
  begin
    Appt := FindApptAtLine(FMousePtInfo.RelLineNum);
    if Assigned(Appt) then
      Viewer.SetSelAppt(Appt);

    if Windows.PtInRect(FDDBtnRect, Point(X, Y)) and Assigned(Viewer) then
    begin
      EditAppt(Viewer.Cell, FMousePtInfo.RelLineNum, Appt);
      Viewer.LineDDClick(MouseLine);
    end
    else
    if not Windows.PtInRect(FDDBtnRect, Point(X, Y)) and Assigned(Appt) then
      Viewer.GlanceControl.BeginDrag(False);
  end;
end;

{$IFDEF USEJVCL}

procedure TJvTFGVTextControl.MouseEnter(Control: TControl);
begin
  FMouseInControl := True;
  inherited MouseEnter(Control);
  Invalidate;
end;

procedure TJvTFGVTextControl.MouseLeave(Control: TControl);
begin
  FMouseInControl := False;
  inherited MouseLeave(Control);
  Invalidate;
end;

{$ENDIF USEJVCL}

procedure TJvTFGVTextControl.Scroll(ScrollBy: Integer);
var
  CurrTop: Integer;
begin
  CurrTop := Viewer.GetTopLine(Viewer.Cell);
  Viewer.SetTopLine(Viewer.Cell, CurrTop + ScrollBy);
end;

function TJvTFGVTextControl.GetTopLine: Integer;
begin
  Result := Viewer.GetTopLine(Viewer.Cell);
end;

function TJvTFGVTextControl.ScrollDnBtnRect(aCellRect: TRect): TRect;
var
  BtnLeft,
  BtnTop: Integer;
begin
  if TopLine + FullViewableLines - 1 < LineCount - 1 then
  begin
    Result := Rect(0, 0, FScrollDnBtnBMP.Width, FScrollDnBtnBMP.Height);
    BtnLeft := aCellRect.Right - 10 - RectWidth(Result);
    BtnTop := aCellRect.Bottom - RectHeight(Result);
    Windows.OffsetRect(Result, BtnLeft, BtnTop);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvTFGVTextControl.ScrollUpBtnRect(aCellRect: TRect): TRect;
var
  BtnLeft: Integer;
begin
  if TopLine > 0 then
  begin
    Result := Rect(0, 0, FScrollUpBtnBMP.Width, FScrollUpBtnBMP.Height);
    BtnLeft := aCellRect.Right - 10 - RectWidth(Result);
    Windows.OffsetRect(Result, BtnLeft, aCellRect.Top);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

destructor TJvTFGVTextControl.Destroy;
begin
  FEditor.Free;
  FScrollUpBtnBMP.Free;
  FScrollDnBtnBMP.Free;

  inherited Destroy;
end;

procedure TJvTFGVTextControl.InitScrollDnBtnBMP;
begin
  with FScrollDnBtnBMP do
  begin
    Height := 9;
    Width := 16;

    with Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, Width, Height));

      Pen.Color := clBlack;
      Polyline([Point(0, 0), Point(Width - 1, 0),
        Point(Width - 1, Height - 1), Point(0, Height - 1),
        Point(0, 0)]);

      MoveTo(2, 2);
      LineTo(14, 2);
      MoveTo(2, 3);
      LineTo(14, 3);
      MoveTo(7, 4);
      LineTo(13, 4);
      MoveTo(8, 5);
      LineTo(12, 5);
      MoveTo(9, 6);
      LineTo(11, 6);
    end;
  end;
end;

procedure TJvTFGVTextControl.InitScrollUpBtnBMP;
begin
  with FScrollUpBtnBMP do
  begin
    Height := 9;
    Width := 16;

    with Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, Width, Height));

      Pen.Color := clBlack;
      Polyline([Point(0, 0), Point(Width - 1, 0),
        Point(Width - 1, Height - 1), Point(0, Height - 1),
        Point(0, 0)]);

      MoveTo(9, 2);
      LineTo(11, 2);
      MoveTo(8, 3);
      LineTo(12, 3);
      MoveTo(7, 4);
      LineTo(13, 4);
      MoveTo(2, 5);
      LineTo(14, 5);
      MoveTo(2, 6);
      LineTo(14, 6);
    end;
  end;
end;

procedure TJvTFGVTextControl.DrawScrollDnBtn(ACanvas: TCanvas; aCellRect: TRect);
var
  aRect: TRect;
begin
  aRect := ScrollDnBtnRect(aCellRect);
  Windows.BitBlt(ACanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
    RectHeight(aRect), FScrollDnBtnBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJvTFGVTextControl.DrawScrollUpBtn(ACanvas: TCanvas; aCellRect: TRect);
var
  aRect: TRect;
begin
  aRect := ScrollUpBtnRect(aCellRect);
  Windows.BitBlt(ACanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
    RectHeight(aRect), FScrollUpBtnBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TJvTFGVTextControl.FullViewableLines: Integer;
var
  aRect: TRect;
begin
  aRect := GlanceControl.CalcCellBodyRect(Viewer.Cell,
    GlanceControl.CellIsSelected(Viewer.Cell), False);

  Result := RectHeight(aRect) div CalcLineHeight;
end;

function TJvTFGVTextControl.CanEdit: Boolean;
begin
  Result := True;
end;

{
procedure TJvTFGVTextControl.EditAppt(Col, Row: Integer; Appt: TJvTFAppt);
var
  EditLine: Integer;
  EditorRect: TRect;
begin
  EditLine := RelToAbs(GetApptRelLineNum(Appt));
  if not Assigned(Appt) or not CanEdit or
     ((EditLine < 0) or (EditLine > AbsLineCount)) then
    Exit;

  Viewer.EnsureCol(Col);
  Viewer.EnsureRow(Row);
  if (Viewer.Col <> Col) or (Viewer.Row <> Row) then
    Viewer.MoveTo(Col, Row);

  if Viewer.EditorAlign = eaLine then
  begin
    EditorRect := LineRect(EditLine);
    FEditor.WordWrap := False;
    FEditor.BorderStyle := bsSingle;
  end
  else
  begin
    EditorRect := ClientRect;
    FEditor.WordWrap := True;
    FEditor.BorderStyle := bsNone;
  end;

  with FEditor do
  begin
    LinkedAppt := Appt;
    Color := Viewer.SelApptAttr.Color;
    Font := Viewer.GlanceControl.SelCellAttr.Font;
    Font.Color := Viewer.SelApptAttr.FontColor;
    BoundsRect := EditorRect;

    Text := Appt.Description;
    {
    if agoFormattedDesc in Options then
      Text := Appt.Description
    else
      Text := StripCRLF(Appt.Description);
    }

{      //Self.Update;  // not calling update here increases flicker
    Visible := True;
    SetFocus;
    SelLength := 0;
    SelStart := 0;
  end;
end;
}

procedure TJvTFGVTextControl.EditAppt(ACell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
var
  EditLine: Integer;
  EditorRect: TRect;
begin
  //EditLine := RelToAbs(GetApptRelLineNum(Appt));
  EditLine := RelToAbs(RelLine);
  if not Assigned(Appt) or not CanEdit or
     ((EditLine < 0) or (EditLine > AbsLineCount)) then
    Exit;

  Viewer.MoveTo(ACell);

  if Viewer.EditorAlign = eaLine then
  begin
    EditorRect := LineRect(EditLine);
    FEditor.WordWrap := False;
    FEditor.BorderStyle := bsSingle;
  end
  else
  begin
    EditorRect := ClientRect;
    FEditor.WordWrap := True;
    FEditor.BorderStyle := bsNone;
  end;

  with FEditor do
  begin
    LinkedAppt := Appt;
    Color := Viewer.SelApptAttr.Color;
    Font := Viewer.GlanceControl.SelCellAttr.Font;
    Font.Color := Viewer.SelApptAttr.FontColor;
    BoundsRect := EditorRect;

    Text := Appt.Description;
    {
    if agoFormattedDesc in Options then
      Text := Appt.Description
    else
      Text := StripCRLF(Appt.Description);
    }

    //Self.Update;  // not calling update here increases flicker
    Visible := True;
    SetFocus;
    SelLength := 0;
    SelStart := 0;
  end;
end;


function TJvTFGVTextControl.Editing: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TJvTFGVTextControl.FinishEditAppt;
begin
  if Assigned(FEditor.LinkedAppt) then
    FEditor.LinkedAppt.Description := FEditor.Text;
  FEditor.Visible := False;
end;

function TJvTFGVTextControl.FindApptAtLine(RelLineNum: Integer): TJvTFAppt;
begin
  if Assigned(Viewer) and
     (RelLineNum >= 0) and (RelLineNum < Viewer.ApptCount) then
    Result := Viewer.Appts[RelLineNum]
  else
    Result := nil;
end;

function TJvTFGVTextControl.GetApptRelLineNum(Appt: TJvTFAppt): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(Appt) then
    Exit;

  I := 0;
  while (I < Viewer.ApptCount) and (Result = -1) do
    if Viewer.Appts[I] = Appt then
      Result := I
    else
      Inc(I);
end;

function TJvTFGVTextControl.AbsLineCount: Integer;
begin
  //Result := Lesser(ViewableLines - 1, LineCount - TopLine - 1);
  Result := RectHeight(ClientRect) div CalcLineHeight;
  if RectHeight(ClientRect) mod CalcLineHeight > 0 then
    Inc(Result);
end;

procedure TJvTFGVTextControl.MouseAccel(X, Y: Integer);
var
  Appt: TJvTFAppt;
begin
  Appt := GetApptAccel(X, Y);
  if Assigned(Appt) then
    Viewer.SetSelAppt(Appt);
end;

function TJvTFGVTextControl.GetStartEndString(Appt: TJvTFAppt): string;
var
  ShowDates: Boolean;
  DateFormat,
  TimeFormat: string;
begin
  ShowDates := (Trunc(Appt.StartDate) <> Trunc(Viewer.Date)) or
               (Trunc(Appt.EndDate) <> Trunc(Viewer.Date));
  DateFormat := Viewer.GlanceControl.DateFormat;
  TimeFormat := Viewer.GlanceControl.TimeFormat;

  Result := '';
  if ShowDates then
    Result := FormatDateTime(DateFormat, Appt.StartDate) + ' ';

  Result := Result + FormatDateTime(TimeFormat, Appt.StartTime) + ' - ';

  if ShowDates then
    Result := Result + FormatDateTime(DateFormat, Appt.EndDate) + ' ';

  Result := Result + FormatDateTime(TimeFormat, Appt.EndTime);
end;

function TJvTFGVTextControl.GetApptAccel(X, Y: Integer): TJvTFAppt;
var
  LocalPt: TPoint;
begin
  LocalPt := ScreenToClient(Viewer.GlanceControl.ClientToScreen(Point(X, Y)));
  Result := GetApptAt(LocalPt.X, LocalPt.Y);
end;

function TJvTFGVTextControl.GetApptAt(X, Y: Integer): TJvTFAppt;
var
  PtInfo: TJvTFGlTxtVwPointInfo;
begin
  PtInfo := CalcPointInfo(X, Y);
  Result := FindApptAtLine(PtInfo.RelLineNum);
end;

procedure TJvTFGVTextControl.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if Source is TJvTFControl then
    Viewer.Visible := False;
end;

//=== { TJvTFGlanceTextViewer } ==============================================

constructor TJvTFGlanceTextViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTopLines := TStringList.Create;
  FViewControl := TJvTFGVTextControl.Create(Self);
  FSelApptAttr := TJvTFTxtVwApptAttr.Create(Self);
  FSelApptAttr.OnChange := SelApptAttrChange;
  FEditorAlign := eaLine;
  FShowStartEnd := True;
end;

destructor TJvTFGlanceTextViewer.Destroy;
begin
  FViewControl.Free;
  FTopLines.Free;
  FSelApptAttr.OnChange := nil;
  FSelApptAttr.Free;
  inherited Destroy;
end;

procedure TJvTFGlanceTextViewer.Change;
begin
  Refresh;
end;

procedure TJvTFGlanceTextViewer.SetEditorAlign(Value: TJvTFGlTxtVwEditorAlign);
begin
  FEditorAlign := Value;
end;

function TJvTFGlanceTextViewer.GetDrawInfo(ACell: TJvTFGlanceCell): TJvTFGlTxtVwDrawInfo;
var
  Attr: TJvTFGlanceCellAttr;
begin
  if not Assigned(GlanceControl) then
    raise EGlanceViewerError.CreateRes(@RsEGlanceControlNotAssigned);

  with Result do
  begin
    Cell := ACell;
    Attr := GlanceControl.GetCellAttr(ACell);
    Font := Attr.Font;
    Color := Attr.Color;
    aRect := GlanceControl.CalcCellBodyRect(ACell,
      GlanceControl.CellIsSelected(ACell), False);
  end;
end;

function TJvTFGlanceTextViewer.GetTopLine(ACell: TJvTFGlanceCell): Integer;
var
  I: Integer;
begin
  I := FTopLines.IndexOf(GetCellString(ACell));
  if I > -1 then
    Result := Integer(FTopLines.Objects[I])
  else
    Result := 0;
end;

procedure TJvTFGlanceTextViewer.LineDDClick(LineNum: Integer);
begin
  if Assigned(FOnLineDDClick) then
    FOnLineDDClick(Self, LineNum);
end;

procedure TJvTFGlanceTextViewer.MouseAccel(X, Y: Integer);
begin
  inherited MouseAccel(X, Y);
  FViewControl.MouseAccel(X, Y);
end;

procedure TJvTFGlanceTextViewer.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  inherited Notify(Sender, Code);
end;

procedure TJvTFGlanceTextViewer.PaintTo(ACanvas: TCanvas; ACell: TJvTFGlanceCell);
begin
  FViewControl.PaintTo(ACanvas, GetDrawInfo(ACell));
end;

procedure TJvTFGlanceTextViewer.ParentReconfig;
begin
  inherited ParentReconfig;
  FTopLines.Clear;
end;

procedure TJvTFGlanceTextViewer.Realign;
begin
  if not Assigned(GlanceControl) then
    Exit;

  FViewControl.BoundsRect := CalcBoundsRect(Cell);
  if not FViewControl.Replicating then
    SetSelAppt(nil);
end;

procedure TJvTFGlanceTextViewer.Refresh;
begin
  FViewControl.Invalidate;
end;

procedure TJvTFGlanceTextViewer.ResetTopLines;
begin
  FTopLines.Clear;
  GlanceControl.Invalidate;
end;

procedure TJvTFGlanceTextViewer.SelApptAttrChange(Sender: TObject);
begin
  //Change;
  FViewControl.Invalidate;
end;

procedure TJvTFGlanceTextViewer.SetGlanceControl(Value: TJvTFCustomGlance);
begin
  inherited SetGlanceControl(Value);
  FViewControl.Parent := Value;
end;

procedure TJvTFGlanceTextViewer.SetLineSpacing(Value: Integer);
begin
  //Value := Greater(Value, 0);
  if Value <> FLineSpacing then
  begin
    FLineSpacing := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTextViewer.SetSelAppt(Value: TJvTFAppt);
begin
  FSelAppt := Value;
  FViewControl.Invalidate;
end;

procedure TJvTFGlanceTextViewer.SetSelApptAttr(Value: TJvTFTxtVwApptAttr);
begin
  FSelApptAttr.Assign(Value);
end;

procedure TJvTFGlanceTextViewer.SetTopLine(ACell: TJvTFGlanceCell; Value: Integer);
var
  I: Integer;
  CellStr: string;
begin
  Value := Greater(Value, 0);
  Value := Lesser(Value, ApptCount - 1);

  // bug fix - this effectively hides the hint window.  The showing/hiding
  // of the hint window was causing the viewer to be positioned at the
  // wrong cell due to repainting as the hint window would hide/show.
  GlanceControl.CheckViewerApptHint(-1, -1);

  CellStr := GetCellString(ACell);
  I := FTopLines.IndexOf(CellStr);
  if I > -1 then
    if Value = 0 then
      FTopLines.Delete(I)
    else
      FTopLines.Objects[I] := TObject(Value)
  else
  if Value <> 0 then
    FTopLines.AddObject(CellStr, TObject(Value));
  Refresh;
end;

procedure TJvTFGlanceTextViewer.SetVisible(Value: Boolean);
begin
  // MORE STUFF NEEDS TO BE ADDED HERE!
  FViewControl.Visible := Value;
end;

procedure TJvTFGlanceTextViewer.SetShowStartEnd(Value: Boolean);
begin
  if Value <> FShowStartEnd then
  begin
    FShowStartEnd := Value;
    if not (csLoading in ComponentState) then
    begin
      GlanceControl.Invalidate;
      FViewControl.Invalidate;
    end;
  end;
end;

function TJvTFGlanceTextViewer.GetApptAt(X, Y: Integer): TJvTFAppt;
begin
  Result := FViewControl.GetApptAt(X, Y);
end;

function TJvTFGlanceTextViewer.CanEdit: Boolean;
begin
  Result := FViewControl.CanEdit;
end;

procedure TJvTFGlanceTextViewer.EditAppt(ACell: TJvTFGlanceCell; RelLine: Integer;
  Appt: TJvTFAppt);
begin
  FViewControl.EditAppt(ACell, RelLine, Appt);
end;

function TJvTFGlanceTextViewer.Editing: Boolean;
begin
  Result := FViewControl.Editing;
end;

procedure TJvTFGlanceTextViewer.FinishEditAppt;
begin
  FViewControl.FinishEditAppt;
end;

function TJvTFGlanceTextViewer.GetCellString(ACell: TJvTFGlanceCell): string;
begin
  Result := '';
  if Assigned(ACell) then
  begin
    Result := IntToStr(ACell.ColIndex) + ',' + IntToStr(ACell.RowIndex);
    if ACell.IsSubcell then
      Result := Result + 'S';
  end;
end;

//=== { TJvTFGVTxtEditor } ===================================================

constructor TJvTFGVTxtEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csNoDesignVisible];

  ParentCtl3D := False;
  Ctl3D := False;
end;

destructor TJvTFGVTxtEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TJvTFGVTxtEditor.DoExit;
begin
  inherited DoExit;
  try
    if not FCancelEdit then
      TJvTFGVTextControl(Owner).FinishEditAppt;
  finally
    FCancelEdit := False;
    Parent.SetFocus;
  end;
end;

procedure TJvTFGVTxtEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_ESCAPE then
  begin
    FCancelEdit := True;
    Key := 0;
    Visible := False;
  end
  else
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    TJvTFGVTextControl(Owner).FinishEditAppt;
end;

//=== { TJvTFTxtVwApptAttr } =================================================

constructor TJvTFTxtVwApptAttr.Create(AOwner: TComponent);
begin
  inherited Create;
  FColor := clBlue;
  FFontColor := clWhite;
end;

procedure TJvTFTxtVwApptAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFTxtVwApptAttr then
  begin
    FColor := TJvTFTxtVwApptAttr(Source).Color;
    FFontColor := TJvTFTxtVwApptAttr(Source).FontColor;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFTxtVwApptAttr.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTFTxtVwApptAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFTxtVwApptAttr.SetFontColor(Value: TColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;
    Change;
  end;
end;

end.
