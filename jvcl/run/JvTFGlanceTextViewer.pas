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

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvTFGlanceTextViewer;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
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
  {$ELSE}
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

    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure DoEnter; override;
    procedure DoExit; override;

    procedure SetMouseLine(Value: Integer);
    property MouseLine: Integer read FMouseLine write SetMouseLine;
    procedure UpdateDDBtnRect;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseAccel(X, Y: Integer);

    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;

    property Replicating: Boolean read FReplicating;
    procedure Paint; override;
    procedure DrawDDButton(aCanvas: TCanvas);
    procedure DrawArrow(aCanvas: TCanvas; aRect: TRect; Direction: TJvTFDirection);
    procedure DrawScrollUpBtn(aCanvas: TCanvas; aCellRect: TRect);
    procedure DrawScrollDnBtn(aCanvas: TCanvas; aCellRect: TRect);
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

    procedure PaintTo(aCanvas: TCanvas; DrawInfo: TJvTFGlTxtVwDrawInfo); overload;

    property Viewer: TJvTFGlanceTextViewer read FViewer;
    property GlanceControl: TJvTFCustomGlance read GetGlanceControl;

    // editor management routines
    //procedure EditAppt(Col, Row: Integer; Appt: TJvTFAppt);
    procedure EditAppt(aCell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
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
  published
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
    property FontColor: TColor read FFontColor write SetFontColor
      default clWhite;
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
    function GetCellString(aCell: TJvTFGlanceCell): string;
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
    procedure PaintTo(aCanvas: TCanvas; aCell: TJvTFGlanceCell); override;
    function GetDrawInfo(aCell: TJvTFGlanceCell): TJvTFGlTxtVwDrawInfo;
    procedure ResetTopLines;
    property SelAppt: TJvTFAppt read FSelAppt;

    procedure SetTopLine(aCell: TJvTFGlanceCell; Value: Integer);
    function GetTopLine(aCell: TJvTFGlanceCell): Integer;
    function GetApptAt(X, Y: Integer): TJvTFAppt; override;

    // editor management routines
    procedure EditAppt(aCell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
    procedure FinishEditAppt; override;
    function Editing: Boolean; override;
    function CanEdit: Boolean; override;
  published
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing
      default 0;
    property OnLineDDClick: TJvTFLineDDClickEvent read FOnLineDDClick
      write FOnLineDDClick;
    property SelApptAttr: TJvTFTxtVwApptAttr read FSelApptAttr
      write SetSelApptAttr;
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

function TJvTFGVTextControl.CalcAbsLineNum(Y: Integer): Integer;
begin
  Result := Y div CalcLineHeight;
end;

constructor TJvTFGVTextControl.Create(AOwner: TComponent);
begin
  inherited;
  If AOwner is TJvTFGlanceTextViewer Then
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
  // If FEditor.Parent = nil then
  //   Beep;
end;

procedure TJvTFGVTextControl.DrawDDButton(aCanvas: TCanvas);
begin
  With aCanvas do
    Begin
      Brush.Color := clBtnFace;
      FillRect(FDDBtnRect);

      DrawArrow(aCanvas, FDDBtnRect, dirDown);

      Pen.Color := clBlack;
      PolyLine([FDDBtnRect.TopLeft, Point(FDDBtnRect.Right, FDDBtnRect.Top),
               FDDBtnRect.BottomRight, Point(FDDBtnRect.Left, FDDBtnRect.Bottom),
               FDDBtnRect.TopLeft]);
      {
      If Windows.PtInRect(aRect, FMouseLoc) Then
        Begin
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
        End;
      }
    End;
end;

function TJvTFGVTextControl.GetGlanceControl: TJvTFCustomGlance;
begin
  Result := nil;
  If Assigned(Viewer) Then
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
All drawing should be done in a PaintTo method.  PaintTo should have aCanvas
and aRect Params.  All drawing code within PaintTo should rely solely on
the aCanvas and aRect parameters given.

This method (Paint) should then call PaintTo(Canvas, ClientRect) to draw the
info on the viewer control.  TJvTFCustomGlance.DrawCell should call
PaintTo(PaintBuffer, CellBodyRect(Col, Row, Selected, False)) to draw the
info on the GlanceControl.
}

  Viewer.SetTo(Viewer.PhysicalCell);
  DrawInfo := Viewer.GetDrawInfo(Viewer.Cell);
  DrawInfo.aRect := ClientRect;

  FReplicating := False;
  Try
    PaintTo(Canvas, DrawInfo);
  Finally
    FReplicating := True;
  End;

{
  // FOR TESTING PURPOSES ONLY!!
  With Canvas do
    Begin
      Pen.Color := clBlack;
      MoveTo(0, 0);
      LineTo(ClientWidth, ClientHeight);
    End;
}
end;

procedure TJvTFGVTextControl.PaintTo(aCanvas: TCanvas; DrawInfo: TJvTFGlTxtVwDrawInfo);
var
  I,
  NextLineTop,
  LastLine: Integer;
  aRect,
  LineRect,
  TxtRect,
  BtnRect: TRect;
  Flags: UINT;
  Txt: string;
  PTxt: PChar;
  Appt: TJvTFAppt;
  RegFontColor,
  RegBrushColor: TColor;
begin
  Viewer.SetTo(DrawInfo.Cell);

  With aCanvas do
    Begin
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

      If csDesigning in ComponentState Then
        LastLine := 2
      Else
        LastLine := Lesser(ViewableLines - 1, LineCount - TopLine - 1);

      For I := 0 to LastLine do
        Begin
          Brush.Color := RegBrushColor;
          Font.Color := RegFontColor;

          LineRect.Top := NextLineTop;
          LineRect.Bottom := Lesser(NextLineTop + CalcLineHeight, aRect.Bottom);

          If csDesigning in ComponentState Then
            Txt := 'Appt ' + IntToStr(I)
          Else
            Begin
              Appt := Viewer.Appts[AbsToRel(I)];

              Txt := '';
              If Viewer.ShowStartEnd Then
                Txt := GetStartEndString(Appt) + ': ';
              Txt := Txt + Appt.Description;

              If Appt = Viewer.SelAppt Then
                Begin
                  Brush.Color := Viewer.SelApptAttr.Color;
                  Font.Color := Viewer.SelApptAttr.FontColor;

                  FillRect(LineRect);

                  If I <> 0 Then
                    Begin
                      MoveTo(aRect.Left, LineRect.Top);
                      LineTo(aRect.Right, LineRect.Top);
                    End;
                  If I <> AbsLineCount - 1 Then
                    Begin
                      MoveTo(aRect.Left, LineRect.Bottom - 1);
                      LineTo(aRect.Right, LineRect.Bottom - 1);
                    End;
                End;
            End;

          TxtRect := LineRect;
          Windows.InflateRect(TxtRect, -1, -1);

          PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
          StrPCopy(PTxt, Txt);
          Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
          StrDispose(PTxt);

          Inc(NextLineTop, CalcLineHeight);
        End;
    End;

  If not (csDesigning in ComponentState) Then
    Begin
      If not Replicating and (FMousePtInfo.RelLineNum < Viewer.ApptCount) and
         FMouseInControl Then
        DrawDDButton(aCanvas);

      BtnRect := ScrollUpBtnRect(DrawInfo.aRect);
      If not Windows.IsRectEmpty(BtnRect) Then
        DrawScrollUpBtn(aCanvas, DrawInfo.aRect);

      BtnRect := ScrollDnBtnRect(DrawInfo.aRect);
      If not Windows.IsRectEmpty(BtnRect) Then
        DrawScrollDnBtn(aCanvas, DrawInfo.aRect);

      {
      If TopLine > 0 Then
        DrawScrollUpBtn(aCanvas, DrawInfo.aRect);

      BottomLine := TopLine + FullViewableLines - 1;
      LastLine := LineCount - 1;
      If BottomLine < LastLine Then
        DrawScrollDnBtn(aCanvas, DrawInfo.aRect);
      }
    End;
end;

procedure TJvTFGVTextControl.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := LRESULT(False);
end;

procedure TJvTFGVTextControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  GlancePt: TPoint;
begin
  inherited;
  FMousePtInfo := CalcPointInfo(X, Y);
  MouseLine := FMousePtInfo.AbsLineNum;
  //SetFocus;

  GlancePt := Point(X, Y);
  GlancePt := Viewer.GlanceControl.ScreenToClient(ClientToScreen(Point(X, Y)));
  Viewer.GlanceControl.CheckViewerApptHint(GlancePt.X, GlancePt.Y);

  // FOR TESTING ONLY!!!
  //Invalidate;
  ////////////////////
end;

procedure TJvTFGVTextControl.SetMouseLine(Value: Integer);
begin
  If Value <> FMouseLine Then
    Begin
      FMouseLine := Value;
      UpdateDDBtnRect;
      Invalidate;
    End;
end;

procedure TJvTFGVTextControl.DrawArrow(aCanvas: TCanvas; aRect: TRect;
  Direction: TJvTFDirection);
var
  I,
  ArrowHeight,
  ArrowWidth,
  BaseX,
  BaseY: Integer;
begin
  ArrowWidth := RectWidth(aRect) - 2;
  If not Odd(ArrowWidth) Then
    Dec(ArrowWidth);
  ArrowHeight := (ArrowWidth + 1) div 2;

  Case Direction of
    dirUp :
      Begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowWidth div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 + ArrowHeight div 2 - 1;

        For I := ArrowHeight downto 1 do
          With aCanvas do
            Begin
              MoveTo(BaseX, BaseY);
              LineTo(BaseX + I * 2 - 1, BaseY);
              Inc(BaseX);
              Dec(BaseY);
            End;
      End;
    dirDown :
      Begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowWidth div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowHeight div 2 + 1;

        For I := ArrowHeight downto 1 do
          With aCanvas do
            Begin
              MoveTo(BaseX, BaseY);
              LineTo(BaseX + I * 2 - 1, BaseY);
              Inc(BaseX);
              Inc(BaseY);
            End;
      End;
    dirLeft :
      Begin
        BaseX := aRect.Left + RectWidth(aRect) div 2 + ArrowHeight div 2;
        BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowWidth div 2;

        For I := ArrowHeight downto 1 do
          With aCanvas do
            Begin
              MoveTo(BaseX, BaseY);
              LineTo(BaseX, BaseY + I * 2 - 1);
              Dec(BaseX);
              Inc(BaseY);
            End;
      End;
  Else
    Begin
      BaseX := aRect.Left + RectWidth(aRect) div 2 - ArrowHeight div 2;
      BaseY := aRect.Top + RectHeight(aRect) div 2 - ArrowWidth div 2;

      For I := ArrowHeight downto 1 do
        With aCanvas do
          Begin
            MoveTo(BaseX, BaseY);
            LineTo(BaseX, BaseY + I * 2 - 1);
            Inc(BaseX);
            Inc(BaseY);
          End;
    End;
  End;
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
  inherited;
  Viewer.SetSelAppt(FindApptAtLine(FMousePtInfo.RelLineNum));
end;

procedure TJvTFGVTextControl.DoExit;
begin
  inherited;
  FMouseLine := -1; 
end;
{
function TJvTFGVTextControl.LineCount: Integer;
var
  aCell: TJvTFGlanceCell;
  I: Integer;
begin
  Result := 0;
  aCell := Viewer.GlanceControl.Cells.Cells[Viewer.Col, Viewer.Row];

  For I := 0 to aCell.ScheduleCount - 1 do
    Inc(Result, aCell.Schedules[I].ApptCount);
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
  With Result do
    Begin
      AbsX := X;
      AbsY := Y;
      AbsLineNum := CalcAbsLineNum(Y);
      RelLineNum := TopLine + AbsLineNum;
    End;
end;

function TJvTFGVTextControl.ViewableLines: Integer;
var
  aRect: TRect;
begin
  aRect := GlanceControl.CalcCellBodyRect(Viewer.Cell,
    GlanceControl.CellIsSelected(Viewer.Cell), False);

  Result := RectHeight(aRect) div CalcLineHeight;
  If RectHeight(aRect) mod CalcLineHeight > 0 Then
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
  inherited;
  SetFocus;

  If Windows.PtInRect(ScrollDnBtnRect(ClientRect), Point(X, Y)) Then
    Scroll(1)
  Else If Windows.PtInRect(ScrollUpBtnRect(ClientRect), Point(X, Y)) Then
    Scroll(-1)
  Else
    Begin
      Appt := FindApptAtLine(FMousePtInfo.RelLineNum);
      If Assigned(Appt) Then
        Viewer.SetSelAppt(Appt);

      If Windows.PtInRect(FDDBtnRect, Point(X, Y)) and Assigned(Viewer) Then
        Begin
          EditAppt(Viewer.Cell, FMousePtInfo.RelLineNum, Appt);
          Viewer.LineDDClick(MouseLine);
        End
      Else If not Windows.PtInRect(FDDBtnRect, Point(X, Y)) and
                  Assigned(Appt) Then
        Viewer.GlanceControl.BeginDrag(False);
    End;
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
  If TopLine + FullViewableLines - 1 < LineCount - 1 Then
    Begin
      Result := Rect(0, 0, FScrollDnBtnBMP.Width, FScrollDnBtnBMP.Height);
      BtnLeft := aCellRect.Right - 10 - RectWidth(Result);
      BtnTop := aCellRect.Bottom - RectHeight(Result);
      Windows.OffsetRect(Result, BtnLeft, BtnTop);
    End
  Else
    Result := Rect(0, 0, 0, 0);
end;

function TJvTFGVTextControl.ScrollUpBtnRect(aCellRect: TRect): TRect;
var
  BtnLeft: Integer;
begin
  If TopLine > 0 Then
    Begin
      Result := Rect(0, 0, FScrollUpBtnBMP.Width, FScrollUpBtnBMP.Height);
      BtnLeft := aCellRect.Right - 10 - RectWidth(Result);
      Windows.OffsetRect(Result, BtnLeft, aCellRect.Top);
    End
  Else
    Result := Rect(0, 0, 0, 0);
end;

destructor TJvTFGVTextControl.Destroy;
begin
  FEditor.Free;
  FScrollUpBtnBMP.Free;
  FScrollDnBtnBMP.Free;
  
  inherited;
end;

procedure TJvTFGVTextControl.InitScrollDnBtnBMP;
begin
  With FScrollDnBtnBMP do
    Begin
      Height := 9;
      Width := 16;

      With Canvas do
        Begin
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
        End;
    End;
end;

procedure TJvTFGVTextControl.InitScrollUpBtnBMP;
begin
  With FScrollUpBtnBMP do
    Begin
      Height := 9;
      Width := 16;

      With Canvas do
        Begin
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
        End;
    End;
end;

procedure TJvTFGVTextControl.DrawScrollDnBtn(aCanvas: TCanvas; aCellRect: TRect);
var
  aRect: TRect;
begin
  aRect := ScrollDnBtnRect(aCellRect);
  Windows.BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
    RectHeight(aRect), FScrollDnBtnBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJvTFGVTextControl.DrawScrollUpBtn(aCanvas: TCanvas; aCellRect: TRect);
var
  aRect: TRect;
begin
  aRect := ScrollUpBtnRect(aCellRect);
  Windows.BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
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
  If not Assigned(Appt) or not CanEdit or
     ((EditLine < 0) or (EditLine > AbsLineCount)) Then
    Exit;

  Viewer.EnsureCol(Col);
  Viewer.EnsureRow(Row);
  If (Viewer.Col <> Col) or (Viewer.Row <> Row) Then
    Viewer.MoveTo(Col, Row);

  If Viewer.EditorAlign = eaLine Then
    Begin
      EditorRect := LineRect(EditLine);
      FEditor.WordWrap := False;
      FEditor.BorderStyle := bsSingle;
    End
  Else
    Begin
      EditorRect := ClientRect;
      FEditor.WordWrap := True;
      FEditor.BorderStyle := bsNone;
    End;

  With FEditor do
    Begin
      LinkedAppt := Appt;
      Color := Viewer.SelApptAttr.Color;
      Font := Viewer.GlanceControl.SelCellAttr.Font;
      Font.Color := Viewer.SelApptAttr.FontColor;
      BoundsRect := EditorRect;

      Text := Appt.Description;
      {
      If agoFormattedDesc in Options Then
        Text := Appt.Description
      Else
        Text := StripCRLF(Appt.Description);
      }

{      //Self.Update;  // not calling update here increases flicker
      Visible := True;
      SetFocus;
      SelLength := 0;
      SelStart := 0;
    End;
end;
}

procedure TJvTFGVTextControl.EditAppt(aCell: TJvTFGlanceCell; RelLine: Integer; Appt: TJvTFAppt);
var
  EditLine: Integer;
  EditorRect: TRect;
begin
  //EditLine := RelToAbs(GetApptRelLineNum(Appt));
  EditLine := RelToAbs(RelLine);
  If not Assigned(Appt) or not CanEdit or
     ((EditLine < 0) or (EditLine > AbsLineCount)) Then
    Exit;

  Viewer.MoveTo(aCell);

  If Viewer.EditorAlign = eaLine Then
    Begin
      EditorRect := LineRect(EditLine);
      FEditor.WordWrap := False;
      FEditor.BorderStyle := bsSingle;
    End
  Else
    Begin
      EditorRect := ClientRect;
      FEditor.WordWrap := True;
      FEditor.BorderStyle := bsNone;
    End;

  With FEditor do
    Begin
      LinkedAppt := Appt;
      Color := Viewer.SelApptAttr.Color;
      Font := Viewer.GlanceControl.SelCellAttr.Font;
      Font.Color := Viewer.SelApptAttr.FontColor;
      BoundsRect := EditorRect;

      Text := Appt.Description;
      {
      If agoFormattedDesc in Options Then
        Text := Appt.Description
      Else
        Text := StripCRLF(Appt.Description);
      }

      //Self.Update;  // not calling update here increases flicker
      Visible := True;
      SetFocus;
      SelLength := 0;
      SelStart := 0;
    End;
end;


function TJvTFGVTextControl.Editing: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TJvTFGVTextControl.FinishEditAppt;
begin
  If Assigned(FEditor.LinkedAppt) Then
    FEditor.LinkedAppt.Description := FEditor.Text;
  FEditor.Visible := False;
end;

function TJvTFGVTextControl.FindApptAtLine(RelLineNum: Integer): TJvTFAppt;
begin
  If Assigned(Viewer) and
     (RelLineNum >= 0) and (RelLineNum < Viewer.ApptCount) Then
    Result := Viewer.Appts[RelLineNum]
  Else
    Result := nil;
end;

function TJvTFGVTextControl.GetApptRelLineNum(Appt: TJvTFAppt): Integer;
var
  I: Integer;
begin
  Result := -1;
  If not Assigned(Appt) Then
    Exit;

  I := 0;
  While (I < Viewer.ApptCount) and (Result = -1) do
    If Viewer.Appts[I] = Appt Then
      Result := I
    Else
      Inc(I);
end;

function TJvTFGVTextControl.AbsLineCount: Integer;
begin
  //Result := Lesser(ViewableLines - 1, LineCount - TopLine - 1);
  Result := RectHeight(ClientRect) div CalcLineHeight;
  If RectHeight(ClientRect) mod CalcLineHeight > 0 Then
    Inc(Result); 
end;

procedure TJvTFGVTextControl.MouseAccel(X, Y: Integer);
var
  Appt: TJvTFAppt;
begin
  Appt := GetApptAccel(X, Y);
  If Assigned(Appt) Then
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
  If ShowDates Then
    Result := FormatDateTime(DateFormat, Appt.StartDate) + ' ';

  Result := Result + FormatDateTime(TimeFormat, Appt.StartTime) + ' - ';

  If ShowDates Then
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
  inherited;
  If Source is TJvTFControl Then
    Viewer.Visible := False;
end;

{ TJvTFGlanceTextViewer }

procedure TJvTFGlanceTextViewer.Change;
begin
  Refresh;
end;

constructor TJvTFGlanceTextViewer.Create(AOwner: TComponent);
begin
  inherited;

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
  inherited;
end;

procedure TJvTFGlanceTextViewer.SetEditorAlign(Value: TJvTFGlTxtVwEditorAlign);
begin
  FEditorAlign := Value;
end;

function TJvTFGlanceTextViewer.GetDrawInfo(aCell: TJvTFGlanceCell): TJvTFGlTxtVwDrawInfo;
var
  Attr: TJvTFGlanceCellAttr;
begin
  If not Assigned(GlanceControl) Then
    Raise EGlanceViewerError.Create(RsEGlanceControlNotAssigned);

  With Result do
    Begin
      Cell := aCell;
      Attr := GlanceControl.GetCellAttr(aCell);
      Font := Attr.Font;
      Color := Attr.Color;
      aRect := GlanceControl.CalcCellBodyRect(aCell,
        GlanceControl.CellIsSelected(aCell), False);
    End;
end;

function TJvTFGlanceTextViewer.GetTopLine(aCell: TJvTFGlanceCell): Integer;
var
  I: Integer;
begin
  I := FTopLines.IndexOf(GetCellString(aCell));
  If I > -1 Then
    Result := Integer(FTopLines.Objects[I])
  Else
    Result := 0;
end;

procedure TJvTFGlanceTextViewer.LineDDClick(LineNum: Integer);
begin
  If Assigned(FOnLineDDClick) Then
    FOnLineDDClick(Self, LineNum);
end;

procedure TJvTFGlanceTextViewer.MouseAccel(X, Y: Integer);
begin
  inherited;
  FViewControl.MouseAccel(X, Y);
end;

procedure TJvTFGlanceTextViewer.Notify(Sender: TObject;
  Code: TJvTFServNotifyCode);
begin
  inherited;
end;

procedure TJvTFGlanceTextViewer.PaintTo(aCanvas: TCanvas; aCell: TJvTFGlanceCell);
begin
  FViewControl.PaintTo(aCanvas, GetDrawInfo(aCell));
end;

procedure TJvTFGlanceTextViewer.ParentReconfig;
begin
  inherited;
  FTopLines.Clear;
end;

procedure TJvTFGlanceTextViewer.Realign;
begin
  If not Assigned(GlanceControl) Then
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
  inherited;
  FViewControl.Parent := Value;
end;

procedure TJvTFGlanceTextViewer.SetLineSpacing(Value: Integer);
begin
  //Value := Greater(Value, 0);
  If Value <> FLineSpacing Then
    Begin
      FLineSpacing := Value;
      Change;
    End;
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

procedure TJvTFGlanceTextViewer.SetTopLine(aCell: TJvTFGlanceCell; Value: Integer);
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

  CellStr := GetCellString(aCell);
  I := FTopLines.IndexOf(CellStr);
  If I > -1 Then
    If Value = 0 Then
      FTopLines.Delete(I)
    Else
      FTopLines.Objects[I] := TObject(Value)
  Else If Value <> 0 Then
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
  If Value <> FShowStartEnd Then
    Begin
      FShowStartEnd := Value;
      If not (csLoading in ComponentState) Then
        Begin
          GlanceControl.Invalidate;
          FViewControl.Invalidate;
        End;
    End;
end;

function TJvTFGlanceTextViewer.GetApptAt(X, Y: Integer): TJvTFAppt;
begin
  Result := FViewControl.GetApptAt(X, Y);
end;

function TJvTFGlanceTextViewer.CanEdit: Boolean;
begin
  Result := FViewControl.CanEdit;
end;

procedure TJvTFGlanceTextViewer.EditAppt(aCell: TJvTFGlanceCell; RelLine: Integer;
  Appt: TJvTFAppt);
begin
  FViewControl.EditAppt(aCell, RelLine, Appt);
end;

function TJvTFGlanceTextViewer.Editing: Boolean;
begin
  Result := FViewControl.Editing;
end;

procedure TJvTFGlanceTextViewer.FinishEditAppt;
begin
  FViewControl.FinishEditAppt;
end;

function TJvTFGlanceTextViewer.GetCellString(aCell: TJvTFGlanceCell): string;
begin
  Result := '';
  if Assigned(aCell) then
  begin
    Result := IntToStr(aCell.ColIndex) + ',' + IntToStr(aCell.RowIndex);
    If aCell.IsSubcell Then
      Result := Result + 'S';
  end;
end;

{ TJvTFGVTxtEditor }

constructor TJvTFGVTxtEditor.Create(AOwner: TComponent);
begin
  Inherited;

  ControlStyle := ControlStyle + [csNoDesignVisible];

  ParentCtl3D := False;
  Ctl3D := False;
end;

destructor TJvTFGVTxtEditor.Destroy;
begin
  inherited;
end;

procedure TJvTFGVTxtEditor.DoExit;
begin
  Inherited;
  Try
    If not FCancelEdit Then
      TJvTFGVTextControl(Owner).FinishEditAppt;
  Finally
    FCancelEdit := False;
    Parent.SetFocus;
  End;
end;

procedure TJvTFGVTxtEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  If Key = VK_ESCAPE Then
    Begin
      FCancelEdit := True;
      Key := 0;
      Visible := False;
    End
  Else If (Key = VK_RETURN) and (ssCtrl in Shift) Then
    TJvTFGVTextControl(Owner).FinishEditAppt;
end;

{ TJvTFTxtVwApptAttr }

procedure TJvTFTxtVwApptAttr.Assign(Source: TPersistent);
begin
  If Source is TJvTFTxtVwApptAttr Then
    Begin
      FColor := TJvTFTxtVwApptAttr(Source).Color;
      FFontColor := TJvTFTxtVwApptAttr(Source).FontColor;
      Change;
    End
  Else
    Inherited Assign(Source);
end;

procedure TJvTFTxtVwApptAttr.Change;
begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

constructor TJvTFTxtVwApptAttr.Create(AOwner: TComponent);
begin
  inherited Create;
  FColor := clBlue;
  FFontColor := clWhite;
end;

procedure TJvTFTxtVwApptAttr.SetColor(Value: TColor);
begin
  If Value <> FColor Then
    Begin
      FColor := Value;
      Change;
    End;
end;

procedure TJvTFTxtVwApptAttr.SetFontColor(Value: TColor);
begin
  If Value <> FFontColor Then
    Begin
      FFontColor := Value;
      Change;
    End;
end;

end.
