{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvYearGrid.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvYearGrid;

interface

uses
  {$IFDEF VCL}
  Windows, ShellAPI, Messages, Graphics, Controls, Forms,
  Dialogs, Grids, Menus, Clipbrd,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QGrids, QMenus, QClipbrd, Types,
  QWindows,
  {$ENDIF VisualCLX}
  SysUtils, Classes;

{$HPPEMIT '#define TDate Controls::TDate'}

type
  TYearData = record
    DisplayText: string;
    InfoText: string;
    DefaultColor: TColor;
    CustomColor: TColor;
    Custom: Boolean;
    BookMark: Boolean; // this is not saved
  end;

  TOnYearChanged = procedure(Sender: TObject; AYear: Integer) of object;
  TOnSelectDate = procedure(Sender: TObject; ADate: TDate; InfoText: string; InfoColor: TColor) of object;
  TOnInfoChanging = procedure(Sender: TObject; var InfoText: string; var CanChange: Boolean) of object;
  TJvYearGrid = class(TDrawGrid)
  private
    FGridPop: TPopupMenu;
    FCurrentYear: Word;
    FCurrentMonth: Word;
    FCurrentDay: Word;
    FHTMLBorder: Boolean;
    FGridYear: Integer;
    FOnYearChanged: TOnYearChanged;
    FHTMLFontName: string;
    FOnSelectDate: TOnSelectDate;
    FBorderColor: TColor;
    FOnInfoChanging: TOnInfoChanging;
    FBookMarkColor: TColor;
    FYearData: array [0..37, 0..12] of TYearData;
    FYearFile: string;
    {$IFDEF VCL}
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure DoShowHint(var HintStr: widestring; var CanShow: Boolean;
      var HintInfo: THintInfo);
    {$ENDIF VisualCLX}
    procedure MakeHTML(AList: TStringList; Border, Filter: Boolean);
    procedure SetHTMLBorder(const Value: Boolean);
    procedure SetGridYear(const Value: Integer);
    procedure SetYearChanged(const Value: TOnYearChanged);
    procedure SetYear(AYear: Word);
    procedure LoadYear;
    procedure SaveYear;
    procedure SetupYearData;
    procedure SetupMonths;
    function GetCellData(var S: string): Boolean;
    function SetCellData(S: string): Boolean;
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure CreatePopup;
    procedure Edit1Click(Sender: TObject);
    procedure Year1Click(Sender: TObject);
    procedure Color1Click(Sender: TObject);
    procedure NoColor1Click(Sender: TObject);
    procedure SetupGridPop(Sender: TObject);
    procedure SaveAsHTML(Sender: TObject);
    procedure Launch(AFile: string);
    procedure SetHTMLFontName(const Value: string);
    procedure SetSelectDate(const Value: TOnSelectDate);
    procedure SetBorderColor(const Value: TColor);
    procedure BorderColor1Click(Sender: TObject);
    procedure SetInfoChanging(const Value: TOnInfoChanging);
    function DateToCell(ADate: TDate; var ACol, ARow: Integer): Boolean;
    procedure ClearBookMarks;
    procedure SetBookMarkColor(const Value: TColor);
    procedure BookMarkColor1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure ClearFind1Click(Sender: TObject);
    procedure SaveFound(Sender: TObject);
  protected
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    function SelectCell(ACol, ARow: Integer): Boolean; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSelDateText: string;
    procedure SetSelDateText(AText: string);
    function GetDateInfo(ADate: TDate; var AText: string): Boolean;
    function SetDateInfo(ADate: TDate; AText: string): Boolean;
    procedure Find;
  published
    property HTMLBorder: Boolean read FHTMLBorder write SetHTMLBorder;
    property HTMLFontName: string read FHTMLFontName write SetHTMLFontName;
    property GridYear: Integer read FGridYear write SetGridYear;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $EEF5FF;
    property BookMarkColor: TColor read FBookMarkColor write SetBookMarkColor default clYellow;
    property OnYearChanged: TOnYearChanged read FOnYearChanged write SetYearChanged;
    property OnSelectDate: TOnSelectDate read FOnSelectDate write SetSelectDate;
    property OnInfoChanging: TOnInfoChanging read FOnInfoChanging write SetInfoChanging;
    property Width default 746;
    property Height default 353;
    property DefaultColWidth default 16;
    property DefaultRowHeight default 24;
    property ColCount default 38;
    property RowCount default 13;
  end;

{$HPPEMIT '#undef TDate'}

implementation

uses
  JvConsts, JvTypes, JvResources, JvYearGridEditForm;

const
  TodayFontColor = clWhite;
  TodayBrushColor = clRed;

var
  DaysInMonth: array [1..12] of Integer;
  StartDays: array [1..12] of Integer;
  TheYear: Word;
  IsThisYear: Boolean;

constructor TJvYearGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 746;
  Height := 353;
  DefaultColWidth := 16;
  DefaultRowHeight := 24;
  ColCount := 38;
  RowCount := 13;
  FBorderColor := $EEF5FF;
  FBookMarkColor := clYellow;
  ShowHint := True;
  CreatePopup;
  PopupMenu := FGridPop;
  FGridPop.OnPopup := SetupGridPop;
  ColWidths[0] := 70;
  Application.ShowHint := True;
  Application.OnShowHint := DoShowHint;
  DecodeDate(Now, FCurrentYear, FCurrentMonth, FCurrentDay);
  HTMLFontName := 'Arial';
  Application.HintHidePause := 5000;
  SetYear(0);
end;

destructor TJvYearGrid.Destroy;
begin
  SaveYear;
  FGridPop.Free;
  inherited destroy;
end;

procedure TJvYearGrid.DrawCell(ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  S: string;
begin
  if Assigned(OnDrawCell) then
    OnDrawCell(Self, ACol, ARow, Rect, State);
  S := FYearData[ACol, ARow].DisplayText;
  with Canvas do
  begin
    Font.Color := clBlack;
    Font.Style := Font.Style - [fsBold];
    if (ACol = 0) or ((ARow = 0) and (FYearData[ACol, ARow].DefaultColor = clWhite)) then
      Brush.Color := BorderColor
    else
    if IsThisYear and (ARow = FCurrentMonth) and (S = IntToStr(FCurrentDay)) then
    begin
      Font.Color := TodayFontColor;
      Brush.Color := TodayBrushColor;
      Font.Style := Font.Style + [fsBold];
    end
    else
    if FYearData[ACol, ARow].Custom then
      Brush.Color := FYearData[ACol, ARow].CustomColor
    else
      Brush.Color := FYearData[ACol, ARow].DefaultColor;
    if FYearData[ACol, ARow].BookMark then
      Brush.Color := BookMarkColor;
    TextRect(Rect, Rect.Left, Rect.Top, S);
  end;
end;

{$IFDEF VisualCLX}
procedure TJvYearGrid.DoShowHint(var HintStr: widestring; var CanShow: Boolean;
  var HintInfo: THintInfo);
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvYearGrid.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
{$ENDIF VCL}
var
  ACol, ARow, X, Y: Integer;
  S, DS: string;
begin
  if HintInfo.HintControl = Self then
  begin
    X := HintInfo.CursorPos.X;
    Y := HintInfo.CursorPos.Y;
    MouseToCell(X, Y, ACol, ARow);
    if (ACol < 0) or (ARow < 0) then
      Exit;
    DS := FYearData[ACol, ARow].DisplayText;
    if IsThisYear and (ARow = FCurrentMonth) and (DS = IntToStr(FCurrentDay)) then
      S := RsToday;
    CanShow := False;
    if (ACol >= 0) and (ARow >= 0) then
    begin
      S := S + FYearData[ACol, ARow].InfoText;
      if S <> '' then
      begin
        HintInfo.CursorRect := CellRect(ACol, ARow);
        HintStr := S;
        CanShow := True;
      end;
    end;
  end;
end;

procedure TJvYearGrid.MakeHTML(AList: TStringList; Border, Filter: Boolean);
var
  ACol, ARow, W: Integer;
  DS, Tbs, Infs: string;
  Month, Day: Word;
  ADate: TDate;
  CanAdd: Boolean;
begin
  AList.Clear;
  if Border then
    Tbs := '1'
  else
    Tbs := '0';
  AList.Append('<html><head><title>Year ' + IntToStr(TheYear) + '</title></head>');
  AList.Append('<body>');
  AList.Append('<font size=2 face="' + HTMLFontName + '">');
  AList.Append('<center><h3>Year ' + IntToStr(TheYear) + '</h3></center>');
  AList.Append('<Table width=100% border=' + Tbs + '>');
  for ARow := 1 to 12 do
    for ACol := 1 to 37 do
    begin
      CanAdd := FYearData[ACol, ARow].DisplayText <> '';
      if CanAdd then
        CanAdd := FYearData[ACol, ARow].InfoText <> '';
      if CanAdd and Filter then
        CanAdd := FYearData[ACol, ARow].BookMark;
      if CanAdd then
      begin
        Month := ARow;
        Day := StrToInt(FYearData[ACol, ARow].DisplayText);
        ADate := EncodeDate(TheYear, Month, Day);
        DS := FormatDateTime('d-mmm-yyyy', ADate);
        W := DayOfWeek(ADate);
        DS := ShortDayNames[W] + ' ' + DS;
        AList.Append('<tr>');
        AList.Append('<td width=20%>' + DS + '</td>');
        Infs := FYearData[ACol, ARow].InfoText;
        Infs := StringReplace(Infs, Cr, '<br>', [rfReplaceAll]);
        AList.Append('<td>' + Infs + '</td>');
        AList.Append('</tr>');
      end;
    end;
  AList.Append('</table>');
  AList.Append('</font></body></html>');
end;

procedure TJvYearGrid.SaveAsHTML(Sender: TObject);
var
  List: TStringList;
  FileName: string;
begin
  List := TStringList.Create;
  MakeHTML(List, HTMLBorder, False);
  FileName := ChangeFileExt(FYearFile, '.htm');
  List.SaveToFile(FileName);
  List.Free;
  Launch(FileName);
end;

procedure TJvYearGrid.SetHTMLBorder(const Value: Boolean);
begin
  FHTMLBorder := Value;
end;

procedure TJvYearGrid.SetYearChanged(const Value: TOnYearChanged);
begin
  FOnYearChanged := Value;
end;

procedure TJvYearGrid.SetGridYear(const Value: Integer);
begin
  if Value <> FGridYear then
  begin
    FGridYear := Value;
    SetYear(FGridYear);
    if Assigned(FOnYearChanged) then
      FOnYearChanged(Self, FGridYear);
  end;
end;

procedure TJvYearGrid.SetYear(AYear: Word);
var
  Year, Month, Day: Word;
begin
  if AYear = 0 then
  begin
    DecodeDate(Now, Year, Month, Day);
    TheYear := Year;
    FGridYear := TheYear;
  end
  else
  begin
    SaveYear;
    TheYear := AYear;
  end;
  FYearFile := 'year' + IntToStr(TheYear) + '.csv';
  IsThisYear := TheYear = FCurrentYear;
  if FileExists(FYearFile) then
    LoadYear
  else
    SetupYearData;
end;

procedure TJvYearGrid.SaveYear;
var
  ARow, ACol: Integer;
  YList, DList: TStringList;
  S: string;
begin
  YList := TStringList.Create;
  DList := TStringList.Create;
  for ARow := 0 to 12 do
  begin
    for ACol := 0 to 37 do
    begin
      DList.Clear;
      DList.Append(FYearData[ACol, ARow].DisplayText);
      S := FYearData[ACol, ARow].InfoText;
      S := StringReplace(S, Cr, '||', [rfReplaceAll]);
      DList.Append(S);
      DList.Append(ColorToString(FYearData[ACol, ARow].DefaultColor));
      DList.Append(ColorToString(FYearData[ACol, ARow].CustomColor));
      if FYearData[ACol, ARow].Custom then
        S := 'true'
      else
        S := 'false';
      DList.Append(S);
      YList.Append(DList.CommaText);
    end;
  end;
  YList.SaveToFile(FYearFile);
  DList.Free;
  Ylist.Free;
end;

procedure TJvYearGrid.LoadYear;
var
  ARow, ACol, Index: Integer;
  YList, DList: TStringList;
  S: string;
begin
  YList := TStringList.Create;
  DList := TStringList.Create;
  YList.LoadFromFile(FYearFile);
  Index := 0;
  for ARow := 0 to 12 do
  begin
    for ACol := 0 to 37 do
    begin
      DList.CommaText := YList[Index];
      Inc(Index);
      FYearData[ACol, ARow].DisplayText := DList[0];
      S := DList[1];
      S := StringReplace(S, '||', Cr, [rfReplaceAll]);
      FYearData[ACol, ARow].InfoText := S;
      FYearData[ACol, ARow].DefaultColor := StringToColor(DList[2]);
      FYearData[ACol, ARow].CustomColor := StringToColor(DList[3]);
      FYearData[ACol, ARow].Custom := (DList[4] = 'true');
    end;
  end;
  DList.Free;
  YList.Free;
  Invalidate;
end;

procedure TJvYearGrid.SetupYearData;
var
  S, D: string;
  I, ACol, ARow: Integer;
  AColor: TColor;
begin
  SetupMonths;
  for ARow := 0 to 12 do
    for ACol := 0 to 37 do
    begin
      S := '';
      if ACol > 0 then
      begin
        I := ((ACol - 1) mod 7) + 1;
        D := ShortDayNames[I][1];
      end;
      if (ARow = 0) and (ACol = 0) then
        S := IntToStr(TheYear);
      if (ARow = 0) and (ACol > 0) then
        S := D;
      if (ARow <> 0) and (ACol = 0) then
        S := LongMonthNames[ARow];
      if (ARow <> 0) and (ACol > 0) then
      begin
        if (ACol >= StartDays[ARow]) and (ACol < StartDays[ARow] + DaysInMonth[ARow]) then
          S := IntToStr(ACol - StartDays[ARow] + 1);
      end;

      // AColor might have not been initialized with the following code.
      //if ((ACol>0)and (D='S')) then
      //  AColor:=clsilver;
      //if ((ACol>0)and (D<>'S')) then
      //  AColor:=clwhite;
      //  Change to:
      if (ACol > 0) and (D = 'S') then
        AColor := clSilver
      else
        AColor := clWhite;
      FYearData[ACol, ARow].DisplayText := S;
      FYearData[ACol, ARow].InfoText := '';
      FYearData[ACol, ARow].DefaultColor := AColor;
      FYearData[ACol, ARow].CustomColor := AColor;
      FYearData[ACol, ARow].Custom := False;
      FYearData[ACol, ARow].BookMark := False;
    end;
  Invalidate;
end;

procedure TJvYearGrid.ClearBookMarks;
var
  ACol, ARow: Integer;
  Cleared: Boolean;
begin
  Cleared := False;
  for ARow := 0 to 12 do
    for ACol := 0 to 37 do
    begin
      Cleared := Cleared or FYearData[ACol, ARow].BookMark;
      FYearData[ACol, ARow].BookMark := False;
    end;
  if Cleared then
    Invalidate;
end;

procedure TJvYearGrid.SetupMonths;
var
  Year, Month, Day: Word;
  ADate: TDate;
  I: Integer;
begin
  for I := 1 to 12 do
  begin
    Year := TheYear;
    Month := I + 1;
    if Month = 13 then
    begin
      Year := Year + 1;
      Month := 1;
    end;
    Day := 1;
    ADate := EncodeDate(Year, Month, Day);
    ADate := ADate - 1;
    DecodeDate(ADate, Year, Month, Day);
    DaysInMonth[I] := Day;
    Year := TheYear;
    Month := I;
    Day := 1;
    ADate := EncodeDate(Year, Month, Day);
    StartDays[I] := DayOfWeek(ADate);
  end;
end;

function TJvYearGrid.GetCellData(var S: string): Boolean;
var
  ACol, ARow: Integer;
begin
  ACol := Col;
  ARow := Row;
  Result := False;
  if (ACol > 0) and (ARow > 0) then
    if FYearData[ACol, ARow].DisplayText <> '' then
    begin
      S := FYearData[ACol, ARow].InfoText;
      Result := True;
    end;
end;

function TJvYearGrid.SetCellData(S: string): Boolean;
var
  ACol, ARow: Integer;
begin
  ACol := Col;
  ARow := Row;
  Result := False;
  if (ACol > 0) and (ARow > 0) then
    if FYearData[ACol, ARow].DisplayText <> '' then
    begin
      FYearData[ACol, ARow].InfoText := S;
      Result := True;
    end;
end;

procedure TJvYearGrid.Copy1Click(Sender: TObject);
var
  S: string;
begin
  if GetCellData(S) then
    Clipboard.AsText := S;
end;

procedure TJvYearGrid.Cut1Click(Sender: TObject);
var
  S: string;
begin
  if GetCellData(S) then
  begin
    Clipboard.AsText := S;
    SetCellData('');
  end;
end;

procedure TJvYearGrid.Year1Click(Sender: TObject);
var
  S: string;
  Year: Word;
begin
  S := InputBox(RsYearGrid, RsEnterYear, IntToStr(GridYear));
  try
    if S = '' then
      Exit;
    Year := StrToInt(S);
    if (Year < 1999) or (Year > 2050) then
      Exit;
    GridYear := Year;
  except
    ShowMessage(RsInvalidYear);
  end;
end;

procedure TJvYearGrid.Paste1Click(Sender: TObject);
var
  S: string;
begin
  if GetCellData(S) then
    {$IFDEF VCL}
    if Clipboard.HasFormat(CF_TEXT) then
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    if Clipboard.AsText <> '' then
    {$ENDIF VisualCLX }
      SetCellData(Clipboard.AsText);
end;

procedure TJvYearGrid.Delete1Click(Sender: TObject);
var
  S: string;
begin
  if GetCellData(S) then
    SetCellData('');
end;

procedure TJvYearGrid.CreatePopup;
const
  cMenuBreakCaption = '-';
var
  G: TPopupMenu;
  M: TMenuItem;
begin
  FGridPop := TPopupMenu.Create(Self);
  G := FGridPop;
  M := TMenuItem.Create(G);
  M.Caption := RsYear;
  M.OnClick := Year1Click;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := cMenuBreakCaption;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsEdit;
  M.OnClick := Edit1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsColor;
  M.OnClick := Color1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsNoColor;
  M.OnClick := NoColor1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := cMenuBreakCaption;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsCopyItem;
  M.OnClick := Copy1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsCutItem;
  M.OnClick := Cut1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsPasteItem;
  M.OnClick := Paste1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsDeleteItem;
  M.OnClick := Delete1Click;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := cMenuBreakCaption;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsSaveAllInfo;
  M.OnClick := SaveAsHTML;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsSaveFoundInfo;
  M.OnClick := SaveFound;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := cMenuBreakCaption;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsBorderColor;
  M.OnClick := BorderColor1Click;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsBookMarkColor;
  M.OnClick := BookMarkColor1Click;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := cMenuBreakCaption;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsFindItem;
  M.OnClick := Find1Click;
  M.Tag := 1;
  G.Items.Add(M);
  M := TMenuItem.Create(G);
  M.Caption := RsClearFind;
  M.OnClick := ClearFind1Click;
  M.Tag := 1;
  G.Items.Add(M);
end;

procedure TJvYearGrid.Edit1Click(Sender: TObject);
var
  DS: string;
  ACol, ARow: Integer;
  F: TYearGridEditForm;
  CanChange: Boolean;
  InfoText: string;
begin
  ACol := Col;
  ARow := Row;
  if (ACol < 1) or (ARow < 1) then
    Exit;
  DS := FYearData[Col, Row].DisplayText;
  if DS = '' then
    Exit;
  F := TYearGridEditForm.Create(Application);
  InfoText := FYearData[ACol, ARow].InfoText;
  F.MemoText.Text := InfoText;
  if F.ShowModal = mrOk then
  begin
    InfoText := F.MemoText.Text;
    CanChange := True;
    if Assigned(FOnInfoChanging) then
      FOnInfoChanging(Self, InfoText, CanChange);
    if CanChange then
    begin
      FYearData[Col, Row].InfoText := InfoText;
      if InfoText = '' then
        FYearData[Col, Row].Custom := False
      else
      if not FYearData[Col, Row].Custom then
      begin
        FYearData[Col, Row].Custom := True;
        FYearData[Col, Row].CustomColor := RGB(206, 250, 253);
      end;
    end;
  end;
  F.Free;
end;

procedure TJvYearGrid.Color1Click(Sender: TObject);
var
  CD: TColorDialog;
begin
  if (Col < 1) or (Row < 1) or (FYearData[Col, Row].DisplayText = '') then
    Exit;
  CD := TColorDialog.Create(Application);
  {$IFDEF VCL}
  CD.Options := [cdFullOpen, cdAnyColor];
  {$ENDIF VCL}
  if CD.Execute then
  begin
    FYearData[Col, Row].CustomColor := CD.Color;
    FYearData[Col, Row].Custom := True;
    Invalidate;
  end;
  CD.Free;
end;

procedure TJvYearGrid.NoColor1Click(Sender: TObject);
begin
  if (Col < 1) or (Row < 1) or (FYearData[Col, Row].DisplayText = '') then
    Exit;
  FYearData[Col, Row].Custom := False;
  Invalidate;
end;

procedure TJvYearGrid.SetupGridPop(Sender: TObject);
var
  I: Integer;
begin
  if (Col > 0) and (Row > 0) and (FYearData[Col, Row].DisplayText <> '') then
    for I := 0 to FGridPop.Items.Count - 1 do
      FGridPop.Items[I].Enabled := True
  else
    for I := 0 to FGridPop.Items.Count - 1 do
      FGridPop.Items[I].Enabled := (FGridPop.Items[I].Tag = 1);
end;

procedure TJvYearGrid.Launch(AFile: string);
var
  Command, Params, WorkDir: string;
begin
  Command := AFile;
  Params := '';
  WorkDir := '';
  {$IFDEF VCL}
  ShellExecute(GetForegroundWindow, 'open', PChar(Command),
    PChar(Params), PChar(WorkDir), SW_SHOWNORMAL);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  ShellExecute(HWND_DESKTOP, 'open', PChar(Command),
    PChar(Params), PChar(WorkDir), SW_SHOWNORMAL);
  {$ENDIF LINUX}
end;

procedure TJvYearGrid.SetHTMLFontName(const Value: string);
begin
  FHTMLFontName := Value;
end;

function TJvYearGrid.GetSelDateText: string;
var
  DS: string;
begin
  if (Col < 1) or (Row < 1) then
    Exit;
  DS := FYearData[Col, Row].DisplayText;
  if DS = '' then
    Exit;
  Result := FYearData[Col, Row].InfoText;
end;

procedure TJvYearGrid.SetSelDateText(AText: string);
var
  DS, S: string;
begin
  if (Col < 1) or (Row < 1) then
    Exit;
  DS := FYearData[Col, Row].DisplayText;
  if DS = '' then
    Exit;
  FYearData[Col, Row].InfoText := S;
end;

procedure TJvYearGrid.SetSelectDate(const Value: TOnSelectDate);
begin
  FOnSelectDate := Value;
end;

function TJvYearGrid.SelectCell(ACol, ARow: Longint): Boolean;
var
  DS: string;
  ADate: TDate;
  InfoText: string;
  InfoColor: TColor;
  Month, Day: Word;
  CanSelect: Boolean;
begin
  CanSelect := True;
  if Assigned(OnSelectCell) then
    OnSelectCell(Self, ACol, ARow, CanSelect);
  if not CanSelect then
  begin
    Result := False;
    Exit;
  end;
  Result := False;
  if (ACol < 1) or (ARow < 1) then
    Exit;
  DS := FYearData[ACol, ARow].DisplayText;
  if DS = '' then
    Exit;
  Month := ARow;
  Day := StrToInt(FYearData[ACol, ARow].DisplayText);
  ADate := EncodeDate(TheYear, Month, Day);
  InfoText := FYearData[ACol, ARow].InfoText;
  if FYearData[ACol, ARow].Custom then
    InfoColor := FYearData[ACol, ARow].CustomColor
  else
    InfoColor := FYearData[ACol, ARow].DefaultColor;
  if Assigned(FOnSelectDate) then
    FOnSelectDate(Self, ADate, InfoText, InfoColor);
  Result := True;
end;

procedure TJvYearGrid.DblClick;
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
  if (Col > 0) and (Row > 0) and (FYearData[Col, Row].DisplayText <> '') then
    Edit1Click(nil);
end;

procedure TJvYearGrid.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TJvYearGrid.BorderColor1Click(Sender: TObject);
var
  CD: TColorDialog;
begin
  CD := TColorDialog.Create(Application);
  {$IFDEF VCL}
  CD.Options := [cdFullOpen, cdAnyColor];
  {$ENDIF VCL}
  if CD.Execute then
    BorderColor := CD.Color;
  CD.Free;
end;

procedure TJvYearGrid.BookMarkColor1Click(Sender: TObject);
var
  CD: TColorDialog;
begin
  CD := TColorDialog.Create(Application);
  {$IFDEF VCL}
  CD.Options := [cdFullOpen, cdAnyColor];
  {$ENDIF VCL}
  if CD.Execute then
    BookMarkColor := CD.Color;
  CD.Free;
end;

procedure TJvYearGrid.SetInfoChanging(const Value: TOnInfoChanging);
begin
  FOnInfoChanging := Value;
end;

function TJvYearGrid.DateToCell(ADate: TDate; var ACol, ARow: Integer): Boolean;
var
  Year, Month, Day: Word;
  WD: Integer;
begin
  Result := False;
  DecodeDate(ADate, Year, Month, Day);
  if Year <> GridYear then
    Exit;
  ARow := Month;
  WD := DayOfWeek(EncodeDate(Year, Month, 1));
  ACol := WD + Day - 1;
  Result := True;
end;

function TJvYearGrid.GetDateInfo(ADate: TDate; var AText: string): Boolean;
var
  Col, Row: Integer;
begin
  Result := DateToCell(ADate, Col, Row);
  if Result then
    AText := FYearData[Col, Row].InfoText;
end;

function TJvYearGrid.SetDateInfo(ADate: TDate; AText: string): Boolean;
var
  Col, Row: Integer;
begin
  Result := DateToCell(ADate, Col, Row);
  if Result then
    FYearData[Col, Row].InfoText := AText;
end;

procedure TJvYearGrid.SetBookMarkColor(const Value: TColor);
begin
  if Value <> FBookMarkColor then
  begin
    FBookMarkColor := Value;
    Invalidate;
  end;
end;

procedure TJvYearGrid.Find1Click(Sender: TObject);
var
  S: string;
  Col, Row: Integer;
begin
  ClearBookMarks;
  S := InputBox(RsYearGridFind, RsEnterSeachText, '');
  if S = '' then
    Exit;
  S := LowerCase(S);
  for Row := 0 to 12 do
    for Col := 0 to 37 do
      if Pos(S, LowerCase(FYearData[Col, Row].InfoText)) > 0 then
        FYearData[Col, Row].BookMark := True;
  Invalidate;
end;

procedure TJvYearGrid.ClearFind1Click(Sender: TObject);
begin
  ClearBookMarks;
end;

procedure TJvYearGrid.Find;
begin
  Find1Click(nil);
end;

procedure TJvYearGrid.SaveFound(Sender: TObject);
var
  List: TStringList;
  FileName: string;
begin
  List := TStringList.Create;
  MakeHTML(List, HTMLBorder, True);
  FileName := Format(RsFounds, [ChangeFileExt(FYearFile, '.htm')]);
  List.SaveToFile(FileName);
  List.Free;
  Launch(FileName);
end;

end.

