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

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvTFMonths;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvTFGlance, JvTFUtils, JvTFManager{$IFDEF USEJVCL}, JvTypes{$ENDIF};

{$HPPEMIT '#define TDate Controls::TDate'}
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
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
      Dates: TJvTFDateList); override;

    // draws the DWTitles
    procedure DrawTitle(aCanvas: TCanvas); override;

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
    function GetCellAttr(aCell: TJvTFGlanceCell): TJvTFGlanceCellAttr; override;

    function CellIsExtraDay(aCell: TJvTFGlanceCell): Boolean;
    function CellIsOffDay(aCell: TJvTFGlanceCell): Boolean;
    function DOWShowing(DOW: TTFDayOfWeek): Boolean;

    procedure ScrollPrev;
    procedure ScrollNext;
  published
    property ScrollSize: TJvTFMonthsScrollSize read FScrollSize
      write FScrollSize default mssMonth;
    property Month: Word read GetMonth write SetMonth;
    property Year: Word read GetYear write SetYear;
    property DisplayDate: TDate read FDisplayDate write SetDisplayDate;
    property DWNames: TJvTFDWNames read FDWNames write SetDWNames;
    property DWTitleAttr: TJvTFGlanceTitle read FDWTitleAttr write SetDWTitleAttr;
    property OffDays: TTFDaysOfWeek read FOffDays write SetOffDays
      default [dowSunday, dowSaturday];
    property ExtraDayCellAttr: TJvTFGlanceCellAttr read FExtraDayCellAttr
      write SetExtraDayCellAttr;
    property OffDayCellAttr: TJvTFGlanceCellAttr read FOffDayCellAttr
      write SetOffDayCellAttr;
    property SplitSatSun: Boolean read FSplitSatSun write SetSplitSatSun
      default False;
    property OnDrawDWTitle: TJvTFDrawDWTitleEvent read FOnDrawDWTitle
      write FOnDrawDWTitle;
    property OnUpdateTitle: TJvTFUpdateTitleEvent read FOnUpdateTitle
      write FOnUpdateTitle;

    property StartOfWeek;
    property ColCount;
//    property Navigator;
//    property OnNavigate;
  end;

{$HPPEMIT '#undef TDate'}

implementation

{ TJvTFMonths }

function TJvTFMonths.CellIsExtraDay(aCell: TJvTFGlanceCell): Boolean;
var
  Y, M, D: Word;
begin
  DecodeDate(aCell.CellDate, Y, M, D);
  Result := (Y <> Self.Year) or (M <> Self.Month);
end;

function TJvTFMonths.CellIsOffDay(aCell: TJvTFGlanceCell): Boolean;
begin
  Result := DateToDOW(aCell.CellDate) in OffDays
end;

procedure TJvTFMonths.ConfigCells;
var
  Row,
  Col,
  SplitCount: Integer;
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

  For Row := 0 to RowCount - 1 do
    begin
      SplitCount := 0;

      For Col := 0 to ColCount - 1 do
        begin
          Cell := Cells.Cells[Col, Row];
          SetCellDate(Cell, OriginDate + Row * 7 + Col + SplitCount);

          if SplitSatSun and (DateToDOW(Cell.CellDate) = dowSaturday) Then
            SplitCell(Cell)
          else
            CombineCell(Cell);

          if Cell.IsSplit Then
            begin
              Inc(SplitCount);
              SetCellDate(Cell.SubCell, OriginDate + Row * 7 + Col + SplitCount);
            end;
        end;
    end;

  inherited;
end;

constructor TJvTFMonths.Create(AOwner: TComponent);
begin
  inherited;
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
  With FDWTitleAttr do
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

  inherited;
end;

function TJvTFMonths.DOWShowing(DOW: TTFDayOfWeek): Boolean;
var
  I: Integer;
  TestDOW: TTFDayOfWeek;
begin
  // THIS ROUTINE SUPPORTS ONLY SAT/SUN SPLITS
  if (DOW = dowSunday) and SplitSatSun Then
    Result := DOWShowing(dowSaturday)
  else
    begin
      I := 0;
      Result := False;
      TestDOW := StartOfWeek;
      While (I < ColCount) and not Result do
        if TestDOW = DOW Then
          Result := True
        else
          IncDOW(TestDOW, 1);
    end;
end;

procedure TJvTFMonths.DrawTitle(aCanvas: TCanvas);
var
  I,
  Col,
  LineBottom: Integer;
  CurrDOW: TTFDayOfWeek;
  aRect,
  TempRect,
  TxtRect,
  TextBounds: TRect;
  OldPen: TPen;
  OldBrush: TBrush;
  OldFont: TFont;
  Txt: string;
begin
  inherited;

  if not DWTitleAttr.Visible Then
    Exit;

  With aCanvas do
    begin
      OldPen := TPen.Create;
      OldPen.Assign(Pen);
      OldBrush := TBrush.Create;
      OldBrush.Assign(Brush);
      OldFont := TFont.Create;
      OldFont.Assign(Font);
    end;

  // draw the DWTitles
  aRect.Top := inherited GetDataTop;
  aRect.Bottom := GetDataTop;

  CurrDOW := StartOfWeek;

  For Col := 0 to ColCount - 1 do
    begin
      TempRect := WholeCellRect(Col, 0);
      aRect.Left := TempRect.Left;
      aRect.Right := TempRect.Right;
      TxtRect := aRect;
      Windows.InflateRect(TxtRect, -1, -1);

      With aCanvas do
        begin
          Brush.Color := DWTitleAttr.Color;
          FillRect(aRect);

          Case DWTitleAttr.FrameAttr.Style of
            fs3DRaised :
              Draw3DFrame(aCanvas, aRect, clBtnHighlight, clBtnShadow);
            fs3DLowered :
              Draw3DFrame(aCanvas, aRect, clBtnShadow, clBtnHighlight);
            fsFlat :
              begin
                Pen.Color := DWTitleAttr.FrameAttr.Color;
                Pen.Width := DWTitleAttr.FrameAttr.Width;
                if Col = 0 Then
                  begin
                    MoveTo(aRect.Left, aRect.Top);
                    LineTo(aRect.Left, aRect.Bottom);
                  end;
                PolyLine([Point(aRect.Right - 1, aRect.Top),
                          Point(aRect.Right - 1, aRect.Bottom - 1),
                          Point(aRect.Left - 1, aRect.Bottom - 1)]);
              end;
            fsNone :
              begin
                Pen.Color := DWTitleAttr.FrameAttr.Color;
                Pen.Width := 1;
                LineBottom := aRect.Bottom - 1;
                For I := 1 to DWTitleAttr.FrameAttr.Width do
                  begin
                    MoveTo(aRect.Left, LineBottom);
                    LineTo(aRect.Right, LineBottom);
                    Dec(LineBottom);
                  end;
              end;
          end;

          Txt := DWNames.GetDWName(DOWToBorl(CurrDOW));
          if SplitSatSun and (CurrDow = dowSaturday) Then
            begin
              IncDOW(CurrDOW, 1);
              Txt := Txt + '/' + DWNames.GetDWName(DOWToBorl(CurrDOW));
            end;
            
          Font := DWTitleAttr.TxtAttr.Font;
          DrawAngleText(aCanvas, TxtRect, TextBounds,
                        DWTitleAttr.TxtAttr.Rotation,
                        DWTitleAttr.TxtAttr.AlignH,
                        DWTitleAttr.TxtAttr.AlignV, Txt);
        end;

      if Assigned(FOnDrawDWTitle) Then
        FOnDrawDWTitle(Self, aCanvas, aRect, CurrDOW, Txt);

      IncDOW(CurrDOW, 1);
    end;

  With aCanvas do
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

function TJvTFMonths.GetCellAttr(aCell: TJvTFGlanceCell): TJvTFGlanceCellAttr;
begin
  if CellIsSelected(aCell) Then
    Result := SelCellAttr
  else if CellIsExtraDay(aCell) Then
    Result := ExtraDayCellAttr
  else if CellIsOffDay(aCell) Then
    Result := OffDayCellAttr
  else
    Result := CellAttr;
end;

function TJvTFMonths.GetCellTitleText(Cell: TJvTFGlanceCell): string;
begin
  if CellIsExtraDay(Cell) and (IsFirstOfMonth(Cell.CellDate) or
     EqualDates(Cell.CellDate, OriginDate)) Then
    Result := FormatDateTime('mmm d', Cell.CellDate)
  else
    Result := FormatDateTime('d', Cell.CellDate);
end;

function TJvTFMonths.GetDataTop: Integer;
begin
  Result := inherited GetDataTop;
  if DWTitleAttr.Visible Then
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

procedure TJvTFMonths.Navigate(aControl: TJvTFControl;
  SchedNames: TStringList; Dates: TJvTFDateList);
begin
  inherited;
  if Dates.Count > 0 Then
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
  if ScrollSize = mssMonth Then
    NextMonth
  else
    NextWeek;
end;

procedure TJvTFMonths.ScrollPrev;
begin
  if ScrollSize = mssMonth Then
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
  if ScrollSize = mssMonth Then
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
  if Value <> M Then
    DisplayDate := EncodeDate(Y, Value, D);
end;

procedure TJvTFMonths.SetOffDayCellAttr(Value: TJvTFGlanceCellAttr);
begin
  FOffDayCellAttr.Assign(Value);
end;

procedure TJvTFMonths.SetOffDays(Value: TTFDaysOfWeek);
begin
  if Value <> FOffDays Then
    begin
      FOffDays := Value;
      Invalidate;
    end;
end;

procedure TJvTFMonths.SetSplitSatSun(Value: Boolean);
begin
  if Value <> FSplitSatSun Then
    begin
      if DOWShowing(dowSunday) or DOWShowing(dowSaturday) Then
        if Value Then
          begin
            if StartOfWeek = dowSunday Then
              StartOfWeek := dowMonday;
            ColCount := ColCount - 1;
          end
        else
          begin
            ColCount := ColCount + 1;
          end;

      FSplitSatSun := Value;
      Cells.ReconfigCells;
    end;
end;

procedure TJvTFMonths.SetStartOfWeek(Value: TTFDayOfWeek);
begin
  if SplitSatSun and (Value = dowSunday) Then
    Value := dowSaturday;

  inherited;
end;

procedure TJvTFMonths.SetYear(Value: Word);
var
  Y, M, D: Word;
begin
  DecodeDate(DisplayDate, Y, M, D);
  if Value <> Y Then
    DisplayDate := EncodeDate(Value, M, D);
end;

procedure TJvTFMonths.UpdateTitle;
var
  NewTitle: string;
begin
  NewTitle := FormatDateTime('mmmm yyyy', DisplayDate);
  if NewTitle <> TitleAttr.Title Then
    begin
      if Assigned(FOnUpdateTitle) Then
        FOnUpdateTitle(Self, NewTitle);
      TitleAttr.Title := NewTitle;
    end;
end;


end.
