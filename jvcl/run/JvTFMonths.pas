{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFMonths.PAS, released on 2003-08-01.

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

unit JvTFMonths;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, Types,
  {$ENDIF VisualCLX}
  {$IFDEF USEJVCL}
  JvTypes,
  {$ENDIF USEJVCL}
  JvTFGlance, JvTFUtils, JvTFManager;

type
  TJvTFMonthsScrollSize = (mssMonth, mssWeek);

  TJvTFMonths = class(TJvTFCustomGlance)
  private
    FDisplayDate: TDate;
    FDWNames: TJvTFDWNames;
    FDWTitleAttr: TJvTFGlanceTitle;
    FOnDrawDWTitle: TJvTFDrawDWTitleEvent;
    FOnUpdateTitle: TJvTFUpdateTitleEvent;
    FOffDays: TTFDaysOfWeek;
    FExtraDayCellAttr: TJvTFGlanceCellAttr;
    FOffDayCellAttr: TJvTFGlanceCellAttr;
    FScrollSize: TJvTFMonthsScrollSize;
    FSplitSatSun: Boolean;
    function GetMonth: Word;
    procedure SetMonth(Value: Word);
    function GetYear: Word;
    procedure SetYear(Value: Word);
    procedure SetDisplayDate(Value: TDate);
    procedure SetDWNames(Value: TJvTFDWNames);
    procedure SetDWTitleAttr(Value: TJvTFGlanceTitle);
    procedure SetOffDays(Value: TTFDaysOfWeek);
    procedure SetExtraDayCellAttr(Value: TJvTFGlanceCellAttr);
    procedure SetOffDayCellAttr(Value: TJvTFGlanceCellAttr);
    procedure SetSplitSatSun(Value: Boolean);
  protected
    procedure SetStartOfWeek(Value: TTFDayOfWeek); override;
    procedure SetColCount(Value: Integer); override;
    procedure ConfigCells; override;
    procedure DWNamesChange(Sender: TObject);
    procedure Navigate(AControl: TJvTFControl; SchedNames: TStringList;
      Dates: TJvTFDateList); override;
    // draws the DWTitles
    procedure DrawTitle(ACanvas: TCanvas); override;
    procedure UpdateTitle;
    procedure NextMonth;
    procedure PrevMonth;
    procedure NextWeek;
    procedure PrevWeek;
    function GetCellTitleText(Cell: TJvTFGlanceCell): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDataTop: Integer; override;
    function GetCellAttr(ACell: TJvTFGlanceCell): TJvTFGlanceCellAttr; override;
    function CellIsExtraDay(ACell: TJvTFGlanceCell): Boolean;
    function CellIsOffDay(ACell: TJvTFGlanceCell): Boolean;
    function DOWShowing(DOW: TTFDayOfWeek): Boolean;
    procedure ScrollPrev;
    procedure ScrollNext;
  published
    property ScrollSize: TJvTFMonthsScrollSize read FScrollSize write FScrollSize default mssMonth;
    property Month: Word read GetMonth write SetMonth;
    property Year: Word read GetYear write SetYear;
    property DisplayDate: TDate read FDisplayDate write SetDisplayDate;
    property DWNames: TJvTFDWNames read FDWNames write SetDWNames;
    property DWTitleAttr: TJvTFGlanceTitle read FDWTitleAttr write SetDWTitleAttr;
    property OffDays: TTFDaysOfWeek read FOffDays write SetOffDays default [dowSunday, dowSaturday];
    property ExtraDayCellAttr: TJvTFGlanceCellAttr read FExtraDayCellAttr write SetExtraDayCellAttr;
    property OffDayCellAttr: TJvTFGlanceCellAttr read FOffDayCellAttr write SetOffDayCellAttr;
    property SplitSatSun: Boolean read FSplitSatSun write SetSplitSatSun default False;
    property OnDrawDWTitle: TJvTFDrawDWTitleEvent read FOnDrawDWTitle write FOnDrawDWTitle;
    property OnUpdateTitle: TJvTFUpdateTitleEvent read FOnUpdateTitle write FOnUpdateTitle;
    property StartOfWeek;
    property ColCount;
//    property Navigator;
//    property OnNavigate;
  end;

implementation

constructor TJvTFMonths.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisplayDate := Date;

  FOffDays := [dowSunday, dowSaturday];
  FScrollSize := mssMonth;

  FDWNames := TJvTFDWNames.Create;
  FDWNames.OnChange := DWNamesChange;

  FExtraDayCellAttr := TJvTFGlanceCellAttr.Create(Self);
  FOffDayCellAttr := TJvTFGlanceCellAttr.Create(Self);

  CellAttr.TitleAttr.Color := clWhite;
  FExtraDayCellAttr.TitleAttr.Color := clWhite;
  FOffDayCellAttr.TitleAttr.Color := clWhite;

  FDWTitleAttr := TJvTFGlanceTitle.Create(Self);
  with FDWTitleAttr do
  begin
//      Assign(TitleAttr);
    TxtAttr.Font.Size := 8;
    TxtAttr.Font.Style := [];
    Height := 20;
    Visible := True;
    FrameAttr.Style := fs3DRaised;
    OnChange := GlanceTitleChange;
  end;
end;

destructor TJvTFMonths.Destroy;
begin
  FDWNames.OnChange := nil;
  FDWNames.Free;
  FDWTitleAttr.Free;
  FExtraDayCellAttr.Free;
  FOffDayCellAttr.Free;

  inherited Destroy;
end;

function TJvTFMonths.CellIsExtraDay(ACell: TJvTFGlanceCell): Boolean;
var
  Y, M, D: Word;
begin
  DecodeDate(ACell.CellDate, Y, M, D);
  Result := (Y <> Self.Year) or (M <> Self.Month);
end;

function TJvTFMonths.CellIsOffDay(ACell: TJvTFGlanceCell): Boolean;
begin
  Result := DateToDOW(ACell.CellDate) in OffDays
end;

procedure TJvTFMonths.ConfigCells;
var
  Row, Col, SplitCount: Integer;
  Cell: TJvTFGlanceCell;
begin
{
  For Row := 0 to RowCount - 1 do
    For Col := 0 to ColCount - 1 do
      begin
        Cell := Cells.Cells[Col, Row];
        if SplitSatSun and (DateToDow(Cell.CellDate) = dowSaturday) Then
          SplitCell(Cell)
        else
          Cell.Combine;
      end;

{
  Found := False;
  Col := 0;
  While (Col < ColCount) and not Found do
    if DateToDOW(Cells.Cells[Col, 0].CellDate) = dowSaturday Then
      Found := True
    else
      Inc(Col);

  if Found Then
    For Row := 0 to RowCount - 1 do
      if SplitSatSun Then
        SplitCell(Cells.Cells[Col, Row])
      else
        Cells.Cells[Col, Row].Combine;
}

  for Row := 0 to RowCount - 1 do
  begin
    SplitCount := 0;

    for Col := 0 to ColCount - 1 do
    begin
      Cell := Cells.Cells[Col, Row];
      SetCellDate(Cell, OriginDate + Row * 7 + Col + SplitCount);

      if SplitSatSun and (DateToDOW(Cell.CellDate) = dowSaturday) then
        SplitCell(Cell)
      else
        CombineCell(Cell);

      if Cell.IsSplit then
      begin
        Inc(SplitCount);
        SetCellDate(Cell.SubCell, OriginDate + Row * 7 + Col + SplitCount);
      end;
    end;
  end;

  inherited ConfigCells;
end;

function TJvTFMonths.DOWShowing(DOW: TTFDayOfWeek): Boolean;
var
  I: Integer;
  TestDOW: TTFDayOfWeek;
begin
  // THIS ROUTINE SUPPORTS ONLY SAT/SUN SPLITS
  if (DOW = dowSunday) and SplitSatSun then
    Result := DOWShowing(dowSaturday)
  else
  begin
    I := 0;
    Result := False;
    TestDOW := StartOfWeek;
    while (I < ColCount) and not Result do
      if TestDOW = DOW then
        Result := True
      else
        IncDOW(TestDOW, 1);
  end;
end;

procedure TJvTFMonths.DrawTitle(ACanvas: TCanvas);
var
  I, Col, LineBottom: Integer;
  CurrDOW: TTFDayOfWeek;
  R, TempRect, TxtRect, TextBounds: TRect;
  OldPen: TPen;
  OldBrush: TBrush;
  OldFont: TFont;
  Txt: string;
begin
  inherited DrawTitle(ACanvas);
  if not DWTitleAttr.Visible then
    Exit;

  with ACanvas do
  begin
    OldPen := TPen.Create;
    OldPen.Assign(Pen);
    OldBrush := TBrush.Create;
    OldBrush.Assign(Brush);
    OldFont := TFont.Create;
    OldFont.Assign(Font);
  end;

  // draw the DWTitles
  R.Top := inherited GetDataTop;
  R.Bottom := GetDataTop;

  CurrDOW := StartOfWeek;

  for Col := 0 to ColCount - 1 do
  begin
    TempRect := WholeCellRect(Col, 0);
    R.Left := TempRect.Left;
    R.Right := TempRect.Right;
    TxtRect := R;
    Windows.InflateRect(TxtRect, -1, -1);

    with ACanvas do
    begin
      Brush.Color := DWTitleAttr.Color;
      FillRect(R);

      case DWTitleAttr.FrameAttr.Style of
        fs3DRaised:
          Draw3DFrame(ACanvas, R, clBtnHighlight, clBtnShadow);
        fs3DLowered:
          Draw3DFrame(ACanvas, R, clBtnShadow, clBtnHighlight);
        fsFlat:
          begin
            Pen.Color := DWTitleAttr.FrameAttr.Color;
            Pen.Width := DWTitleAttr.FrameAttr.Width;
            if Col = 0 then
            begin
              MoveTo(R.Left, R.Top);
              LineTo(R.Left, R.Bottom);
            end;
            PolyLine([Point(R.Right - 1, R.Top),
              Point(R.Right - 1, R.Bottom - 1),
                Point(R.Left - 1, R.Bottom - 1)]);
          end;
        fsNone:
          begin
            Pen.Color := DWTitleAttr.FrameAttr.Color;
            Pen.Width := 1;
            LineBottom := R.Bottom - 1;
            for I := 1 to DWTitleAttr.FrameAttr.Width do
            begin
              MoveTo(R.Left, LineBottom);
              LineTo(R.Right, LineBottom);
              Dec(LineBottom);
            end;
          end;
      end;

      Txt := DWNames.GetDWName(DOWToBorl(CurrDOW));
      if SplitSatSun and (CurrDOW = dowSaturday) then
      begin
        IncDOW(CurrDOW, 1);
        Txt := Txt + '/' + DWNames.GetDWName(DOWToBorl(CurrDOW));
      end;

      Font := DWTitleAttr.TxtAttr.Font;
      DrawAngleText(ACanvas, TxtRect, TextBounds,
        DWTitleAttr.TxtAttr.Rotation,
        DWTitleAttr.TxtAttr.AlignH,
        DWTitleAttr.TxtAttr.AlignV, Txt);
    end;

    if Assigned(FOnDrawDWTitle) then
      FOnDrawDWTitle(Self, ACanvas, R, CurrDOW, Txt);

    IncDOW(CurrDOW, 1);
  end;

  with ACanvas do
  begin
    Pen.Assign(OldPen);
    Brush.Assign(OldBrush);
    Font.Assign(OldFont);
    OldPen.Free;
    OldBrush.Free;
    OldFont.Free;
  end;
end;

procedure TJvTFMonths.DWNamesChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvTFMonths.GetCellAttr(ACell: TJvTFGlanceCell): TJvTFGlanceCellAttr;
begin
  if CellIsSelected(ACell) then
    Result := SelCellAttr
  else
  if CellIsExtraDay(ACell) then
    Result := ExtraDayCellAttr
  else
  if CellIsOffDay(ACell) then
    Result := OffDayCellAttr
  else
    Result := CellAttr;
end;

function TJvTFMonths.GetCellTitleText(Cell: TJvTFGlanceCell): string;
begin
  if CellIsExtraDay(Cell) and (IsFirstOfMonth(Cell.CellDate) or
    EqualDates(Cell.CellDate, OriginDate)) then
    Result := FormatDateTime('mmm d', Cell.CellDate)
  else
    Result := FormatDateTime('d', Cell.CellDate);
end;

function TJvTFMonths.GetDataTop: Integer;
begin
  Result := inherited GetDataTop;
  if DWTitleAttr.Visible then
    Inc(Result, DWTitleAttr.Height);
end;

function TJvTFMonths.GetMonth: Word;
begin
  Result := ExtractMonth(DisplayDate);
end;

function TJvTFMonths.GetYear: Word;
begin
  Result := ExtractYear(DisplayDate);
end;

procedure TJvTFMonths.Navigate(AControl: TJvTFControl;
  SchedNames: TStringList; Dates: TJvTFDateList);
begin
  inherited Navigate(AControl, SchedNames, Dates);
  if Dates.Count > 0 then
    DisplayDate := Dates[0];
end;

procedure TJvTFMonths.NextMonth;
var
  Temp: TDateTime;
begin
  Temp := DisplayDate;
  IncMonths(Temp, 1);
  DisplayDate := Temp;
end;

procedure TJvTFMonths.NextWeek;
var
  Temp: TDateTime;
begin
  Temp := DisplayDate;
  IncWeeks(Temp, 1);
  DisplayDate := Temp;
end;

procedure TJvTFMonths.PrevMonth;
var
  Temp: TDateTime;
begin
  Temp := DisplayDate;
  IncMonths(Temp, -1);
  DisplayDate := Temp;
end;

procedure TJvTFMonths.PrevWeek;
var
  Temp: TDateTime;
begin
  Temp := DisplayDate;
  IncWeeks(Temp, -1);
  DisplayDate := Temp;
end;

procedure TJvTFMonths.ScrollNext;
begin
  if ScrollSize = mssMonth then
    NextMonth
  else
    NextWeek;
end;

procedure TJvTFMonths.ScrollPrev;
begin
  if ScrollSize = mssMonth then
    PrevMonth
  else
    PrevWeek;
end;

procedure TJvTFMonths.SetColCount(Value: Integer);
begin
  Value := Lesser(Value, 7);
  inherited SetColCount(Value);
end;

procedure TJvTFMonths.SetDisplayDate(Value: TDate);
begin
  FDisplayDate := Value;
  if ScrollSize = mssMonth then
    StartDate := FirstOfMonth(Value)
  else
    StartDate := Value;
  UpdateTitle;
end;

procedure TJvTFMonths.SetDWNames(Value: TJvTFDWNames);
begin
  FDWNames.Assign(Value);
end;

procedure TJvTFMonths.SetDWTitleAttr(Value: TJvTFGlanceTitle);
begin
  FDWTitleAttr.Assign(Value);
end;

procedure TJvTFMonths.SetExtraDayCellAttr(Value: TJvTFGlanceCellAttr);
begin
  FExtraDayCellAttr.Assign(Value);
end;

procedure TJvTFMonths.SetMonth(Value: Word);
var
  Y, M, D: Word;
begin
  EnsureMonth(Value);

  DecodeDate(DisplayDate, Y, M, D);
  if Value <> M then
    DisplayDate := EncodeDate(Y, Value, D);
end;

procedure TJvTFMonths.SetOffDayCellAttr(Value: TJvTFGlanceCellAttr);
begin
  FOffDayCellAttr.Assign(Value);
end;

procedure TJvTFMonths.SetOffDays(Value: TTFDaysOfWeek);
begin
  if Value <> FOffDays then
  begin
    FOffDays := Value;
    Invalidate;
  end;
end;

procedure TJvTFMonths.SetSplitSatSun(Value: Boolean);
begin
  if Value <> FSplitSatSun then
  begin
    if DOWShowing(dowSunday) or DOWShowing(dowSaturday) then
      if Value then
      begin
        if StartOfWeek = dowSunday then
          StartOfWeek := dowMonday;
        ColCount := ColCount - 1;
      end
      else
        ColCount := ColCount + 1;

    FSplitSatSun := Value;
    Cells.ReconfigCells;
  end;
end;

procedure TJvTFMonths.SetStartOfWeek(Value: TTFDayOfWeek);
begin
  if SplitSatSun and (Value = dowSunday) then
    Value := dowSaturday;
  inherited SetStartOfWeek(Value);
end;

procedure TJvTFMonths.SetYear(Value: Word);
var
  Y, M, D: Word;
begin
  DecodeDate(DisplayDate, Y, M, D);
  if Value <> Y then
    DisplayDate := EncodeDate(Value, M, D);
end;

procedure TJvTFMonths.UpdateTitle;
var
  NewTitle: string;
begin
  NewTitle := FormatDateTime('mmmm yyyy', DisplayDate);
  if NewTitle <> TitleAttr.Title then
  begin
    if Assigned(FOnUpdateTitle) then
      FOnUpdateTitle(Self, NewTitle);
    TitleAttr.Title := NewTitle;
  end;
end;

end.

