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

{$I JVCL.INC}

unit JvYearGrid;

interface

uses
  Windows, ShellApi, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, Menus, Clipbrd;

{$HPPEMIT '#define TDate Controls::TDate'}
type
  TYearData = record
    DisPlaytext: string;
    InfoText: string;
    DefaultColor: TColor;
    CustomColor: TColor;
    Custom: Boolean;
    BookMark: Boolean; //this is not saved
  end;

  TOnYearChanged = procedure(Sender: TObject; AYear: Integer) of Object;
  TOnSelectDate = procedure(Sender: TObject; Adate: TDate; InfoText: string; InfoColor: TColor) of Object;
  TOnInfoChanging = procedure(Sender: TObject; var InfoText: string; var CanChange: Boolean) of Object;
  TJvYearGrid = class(TDrawGrid)
  private
    GridPop: TPopupMenu;
    thisyear, thismonth, thisday: Word;
    FHTMLBorder: Boolean;
    FGridYear: Integer;
    FonYearChanged: TOnYearChanged;
    FHTMLFontName: string;
    FonSelectDate: TOnSelectDate;
    FBorderColor: TColor;
    FonInfoChanging: TOnInfoChanging;
    FBookMarkColor: TColor;
    { Private declarations }

    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure MakeHTML(alist: tstringlist; border, filter: Boolean);
    procedure SetHTMLBorder(const Value: Boolean);
    procedure SetGridYear(const Value: Integer);
    procedure SeTOnYearChanged(const Value: TOnYearChanged);
    procedure setYear(AYear: Word);
    procedure LoadYear;
    procedure SaveYear;
    procedure SetupYearData;
    procedure setupmonths;
    function GetCellData(var s: string): Boolean;
    function SetCellData(s: string): Boolean;
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure CreatePopup;
    procedure Edit1Click(Sender: TObject);
    procedure year1Click(Sender: TObject);
    procedure Color1click(Sender: TObject);
    procedure noColor1click(Sender: TObject);
    procedure SetupGridPop(Sender: TObject);
    procedure SaveAsHTML(Sender: TObject);
    procedure Launch(Afile: string);
    procedure SetHTMLFontName(const Value: string);
    procedure SeTOnSelectDate(const Value: TOnSelectDate);
    procedure SetBorderColor(const Value: TColor);
    procedure BorderColor1click(Sender: TObject);
    procedure SeTOnInfoChanging(const Value: TOnInfoChanging);
    function DateToCell(ADate: TDate; var Acol, aRow: Integer): Boolean;
    procedure ClearBookMarks;
    procedure SetBookMarkColor(const Value: TColor);
    procedure BookMarkColor1click(Sender: TObject);
    procedure Find1click(Sender: TObject);
    procedure ClearFind1click(Sender: TObject);
    procedure SaveFound(Sender: TObject);

  protected
    { Protected declarations }
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    function SelectCell(ACol, ARow: Integer): Boolean; override;
    procedure DblClick; override;
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    function GetSelDateText: string;
    procedure SetSelDateText(Atext: string);
    function GetDateInfo(aDate: TDate; var aText: string): Boolean;
    function SetDateInfo(aDate: TDate; aText: string): Boolean;
    procedure Find;
  published
    { Published declarations }
    property HTMLBorder: Boolean read FHTMLBorder write SetHTMLBorder;
    property HTMLFontName: string read FHTMLFontName write SetHTMLFontName;
    property GridYear: Integer read FGridYear write SetGridYear;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BookMarkColor: TColor read FBookMarkColor write SetBookMarkColor;
    property onYearChanged: TOnYearChanged read FonYearChanged write SeTOnYearChanged;
    property onSelectDate: TOnSelectDate read FonSelectDate write SeTOnSelectDate;
    property onInfoChanging: TOnInfoChanging read FonInfoChanging write SeTOnInfoChanging;
  end;

{$HPPEMIT '#undef TDate'}

implementation

uses
  JvConsts, JvTypes, JvResources, JvYearGridEditForm;

const
  todayfontColor = clWhite;
  todaybrushColor = clRed;

var
  appldir: string;
  daysinmonth: array[1..12] of Integer;
  startdays: array[1..12] of Integer;
  theyear: Word;
  thisdaystr: string;
  isthisyear: Boolean;
  YearData: array[0..37, 0..12] of TYearData;
  YearFile: string;

  { TJvYearGrid }

constructor TJvYearGrid.Create(AOwner: Tcomponent);
begin
  inherited create(AOwner);
  width := 746;
  height := 353;
  defaultcolwidth := 16;
  defaultrowheight := 24;
  colcount := 38;
  rowcount := 13;
  FBorderColor := $EEF5FF;
  FBookMarkColor := clyellow;
  CreatePopup;
  popupmenu := GridPop;
  GridPop.OnPopup := SetupGridPop;
  ColWidths[0] := 70;
  showhint := true;
  Application.ShowHint := True;
  Application.OnShowHint := DoShowHint;
  decodedate(now, thisyear, thismonth, thisday);
  thisdaystr := inttostr(thisday);
  HTMLFontName := 'Arial';
  application.HintHidePause := 5000;
  setYear(0);
end;

destructor TJvYearGrid.Destroy;
begin
  SaveYear;
  GridPop.free;
  inherited destroy;
end;

procedure TJvYearGrid.DrawCell(ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  s: string;
begin
  if Assigned(onDrawCell) then
    onDrawCell(Self, acol, arow, rect, state);
  s := YearData[acol, arow].DisplayText;
  with canvas do
  begin
    font.Color := clblack;
    font.style := font.style - [fsbold];
    if ((acol = 0) or ((arow = 0) and (YearData[acol, arow].defaultColor = clwhite))) then
      brush.Color := borderColor
    else if (isthisyear and (arow = thismonth) and (s = thisdaystr)) then
    begin
      font.Color := todayfontColor;
      brush.Color := todaybrushColor;
      font.style := font.style + [fsbold];
    end
    else if YearData[acol, arow].custom then
      brush.Color := YearData[acol, arow].customColor
    else
      brush.Color := YearData[acol, arow].defaultColor;
    if yearData[acol, arow].bookmark then
      brush.Color := BookMarkColor;
    textrect(rect, rect.left, rect.top, s);
  end;
end;

procedure TJvYearGrid.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  acol, arow, x, y: Integer;
  s, ds: string;
begin
  if HintInfo.HintControl = Self then
  begin
    x := HintInfo.CursorPos.x;
    y := HintInfo.cursorPos.y;
    MouseToCell(x, y, acol, arow);
    if ((acol < 0) or (arow < 0)) then Exit;
    ds := YearData[acol, arow].displaytext;
    if (isthisyear and (arow = thismonth) and (ds = thisdaystr)) then
      s := 'Today ';
    canshow := false;
    if ((acol >= 0) and (arow >= 0)) then
    begin
      s := s + YearData[acol, arow].infotext;
      if s <> '' then
      begin
        Hintinfo.CursorRect := CellRect(acol, arow);
        Hintstr := s;
        canshow := true;
      end;
    end;
  end;
end;

procedure TJvYearGrid.MakeHTML(alist: tstringlist; border, Filter: Boolean);
var
  acol, arow, w: Integer;
  ds, tbs, infs: string;
  month, day: Word;
  ADate: tdate;
  CanAdd: Boolean;
begin
  alist.clear;
  if border then
    tbs := '1'
  else
    tbs := '0';
  alist.append('<html><head><title>Year ' + inttostr(theyear) + '</title></head>');
  alist.append('<body>');
  alist.append('<font size=2 face="' + htmlFontName + '">');
  alist.append('<center><h3>Year ' + inttostr(theyear) + '</h3></center>');
  alist.append('<Table width=100% border=' + tbs + '>');
  for arow := 1 to 12 do
    for acol := 1 to 37 do
    begin
      CanAdd := YearData[acol, arow].displaytext <> '';
      if CanAdd then
        CanAdd := YearData[acol, arow].infotext <> '';
      if (CanAdd and Filter) then
        CanAdd := YearData[acol, arow].bookmark;
      if CanAdd then
      begin
        month := arow;
        day := strtoint(YearData[acol, arow].displaytext);
        adate := encodedate(theyear, month, day);
        ds := formatdatetime('d-mmm-yyyy', adate);
        w := dayofweek(adate);
        ds := shortdaynames[w] + ' ' + ds;
        alist.append('<tr>');
        alist.append('<td width=20%>' + ds + '</td>');
        infs := YearData[acol, arow].infotext;
        infs := stringreplace(infs, cr, '<br>', [rfreplaceall]);
        alist.append('<td>' + infs + '</td>');
        alist.append('</tr>');
      end;
    end;
  alist.append('</table>');
  alist.append('</font></body></html>');
end;

procedure TJvYearGrid.SaveAsHTML(Sender: TObject);
var
  alist: Tstringlist;
  afile: string;
begin
  alist := tstringlist.create;
  MakeHTML(alist, HTMLBorder, false);
  afile := changefileext(YearFile, '.htm');
  alist.savetofile(afile);
  alist.free;
  launch(afile);
end;

procedure TJvYearGrid.SetHTMLBorder(const Value: Boolean);
begin
  FHTMLBorder := Value;
end;

procedure TJvYearGrid.SeTOnYearChanged(const Value: TOnYearChanged);
begin
  FonYearChanged := Value;
end;

procedure TJvYearGrid.SetGridYear(const Value: Integer);
begin
  if value <> FGridYear then
  begin
    FGridYear := Value;
    setYear(FGridYear);
    if Assigned(onYearChanged) then
      onYearChanged(Self, FGridYear);
  end;
end;

procedure TJvYearGrid.setYear(AYear: Word);
var
  year, month, day: Word;
begin
  if Ayear = 0 then
  begin
    decodedate(now, year, month, day);
    theyear := year;
    FGridYear := theyear;
  end
  else
  begin
    SaveYear;
    theyear := AYear;
  end;
  YearFile := appldir + 'year' + inttostr(theyear) + '.csv';
  isthisyear := theyear = thisyear;
  if fileexists(YearFile) then
    LoadYear
  else
    setupYearData;
end;

procedure TJvYearGrid.SaveYear;
var
  arow, acol: Integer;
  YList, DList: tstringlist;
  s: string;
begin
  YList := tstringlist.create;
  DList := tstringlist.create;
  for arow := 0 to 12 do
  begin
    for acol := 0 to 37 do
    begin
      Dlist.clear;
      Dlist.append(YearData[acol, arow].DisplayText);
      s := YearData[acol, arow].InfoText;
      s := stringreplace(s, cr, '||', [rfreplaceall]);
      Dlist.append(s);
      Dlist.append(Colortostring(YearData[acol, arow].DefaultColor));
      Dlist.append(Colortostring(YearData[acol, arow].CustomColor));
      if YearData[acol, arow].Custom then
        s := 'true'
      else
        s := 'false';
      Dlist.append(s);
      YList.Append(DList.commatext);
    end;
  end;
  YList.SaveToFile(YearFile);
  Dlist.free;
  Ylist.free;
end;

procedure TJvYearGrid.LoadYear;
var
  arow, acol, index: Integer;
  YList, DList: tstringlist;
  s: string;
begin
  YList := tstringlist.create;
  DList := tstringlist.create;
  YList.LoadFromFile(YearFile);
  index := 0;
  for arow := 0 to 12 do
  begin
    for acol := 0 to 37 do
    begin
      Dlist.commatext := YList[index];
      inc(index);
      YearData[acol, arow].DisplayText := DList[0];
      s := Dlist[1];
      s := stringreplace(s, '||', cr, [rfreplaceall]);
      YearData[acol, arow].InfoText := s;
      YearData[acol, arow].DefaultColor := stringtoColor(DList[2]);
      YearData[acol, arow].CustomColor := stringtoColor(DList[3]);
      YearData[acol, arow].Custom := (Dlist[4] = 'true');
    end;
  end;
  Dlist.free;
  Ylist.free;
  Invalidate;
end;

procedure TJvYearGrid.SetupYearData;
var
  s, d: string;
  i, acol, arow: Integer;
  AColor: TColor;
begin
  setupmonths;
  for arow := 0 to 12 do
    for acol := 0 to 37 do
    begin
      s := '';
      if acol > 0 then
      begin
        i := ((acol - 1) mod 7) + 1;
        d := shortdaynames[i][1];
      end;
      if ((arow = 0) and (acol = 0)) then
        s := inttostr(theyear);
      if ((arow = 0) and (acol > 0)) then
        s := d;
      if ((arow <> 0) and (acol = 0)) then
        s := longmonthnames[arow];
      if ((arow <> 0) and (acol > 0)) then
      begin
        if ((acol >= startdays[arow]) and (acol < (startdays[arow] + daysinmonth[arow]))) then
          s := inttostr(acol - startdays[arow] + 1);
      end;

      // AColor might have not been initialized with the following code.
      //if ((acol>0)and (d='S')) then
      //  AColor:=clsilver;
      //if ((acol>0)and (d<>'S')) then
      //  AColor:=clwhite;
      //  Change to:
      if (acol > 0) and (d = 'S') then
        AColor := clSilver
      else
        AColor := clWhite;
      YearData[acol, arow].DisPlayText := s;
      YearData[acol, arow].InfoText := '';
      YearData[acol, arow].DefaultColor := AColor;
      YearData[acol, arow].CustomColor := AColor;
      YearData[acol, arow].Custom := false;
      YearData[acol, arow].bookmark := false;
    end;
  Invalidate;
end;

procedure TJvYearGrid.ClearBookMarks;
var
  acol, arow: Integer;
begin
  for arow := 0 to 12 do
    for acol := 0 to 37 do
      YearData[acol, arow].bookmark := false;
  invalidate;

end;

procedure TJvYearGrid.setupmonths;
var
  year, month, day: Word;
  Adate: tdate;
  i: Integer;
begin
  for i := 1 to 12 do
  begin
    year := theyear;
    month := i + 1;
    if month = 13 then
    begin
      year := year + 1;
      month := 1;
    end;
    day := 1;
    Adate := encodedate(year, month, day);
    adate := Adate - 1;
    decodedate(Adate, year, month, day);
    daysinmonth[i] := day;
    year := theyear;
    month := i;
    day := 1;
    adate := encodedate(year, month, day);
    startdays[i] := dayofweek(adate);
  end;
end;

function TJvYearGrid.GetCellData(var s: string): Boolean;
var
  acol, arow: Integer;
begin
  acol := col;
  arow := row;
  result := false;
  if ((acol > 0) and (arow > 0)) then
    if YearData[acol, arow].displaytext <> '' then
    begin
      s := YearData[acol, arow].infotext;
      result := true;
    end;
end;

function TJvYearGrid.SetCellData(s: string): Boolean;
var
  acol, arow: Integer;
begin
  acol := col;
  arow := row;
  result := false;
  if ((acol > 0) and (arow > 0)) then
    if YearData[acol, arow].displaytext <> '' then
    begin
      YearData[acol, arow].infotext := s;
      result := true;
    end;
end;

procedure TJvYearGrid.Copy1Click(Sender: TObject);
var
  s: string;
begin
  if getcelldata(s) then
  begin
    Clipboard.AsText := s;
  end;
end;

procedure TJvYearGrid.Cut1Click(Sender: TObject);
var
  s: string;
begin
  if getcelldata(s) then
  begin
    Clipboard.AsText := s;
    setcelldata('');
  end;
end;

procedure TJvYearGrid.year1Click(Sender: TObject);
var
  s: string;
  Ayear: Word;
begin
  s := inputbox(RsYearGrid, RsEnterYear, inttostr(GridYear));
  try
    if s = '' then Exit;
    ayear := strtoint(s);
    if ((ayear < 1999) or (ayear > 2050)) then Exit;
    GridYear := ayear;
  except
    showmessage(RsInvalidYear);
  end;

end;

procedure TJvYearGrid.Paste1Click(Sender: TObject);
var
  s: string;
begin
  if getcelldata(s) then
    if Clipboard.HasFormat(CF_TEXT) then
      setcelldata(Clipboard.asText);
end;

procedure TJvYearGrid.Delete1Click(Sender: TObject);
var
  s: string;
begin
  if getcelldata(s) then
    setcelldata('');
end;

procedure TJvYearGrid.CreatePopup;
const
  cMenuBreakCaption = '-';
var
  g: Tpopupmenu;
  m: tmenuitem;
begin
  GridPop := Tpopupmenu.create(Self);
  g := GridPop;
  m := tmenuitem.Create(g);
  m.Caption := RsYear;
  m.OnClick := year1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := cMenuBreakCaption;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsEdit;
  m.OnClick := edit1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsColor;
  m.OnClick := Color1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsNoColor;
  m.OnClick := noColor1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := cMenuBreakCaption;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsCopyItem;
  m.OnClick := copy1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsCutItem;
  m.OnClick := cut1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsPasteItem;
  m.OnClick := paste1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsDeleteItem;
  m.OnClick := delete1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := cMenuBreakCaption;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsSaveAllInfo;
  m.OnClick := SaveAsHTML;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsSaveFoundInfo;
  m.OnClick := SaveFound;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := cMenuBreakCaption;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsBorderColor;
  m.OnClick := borderColor1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsBookMarkColor;
  m.OnClick := bookmarkColor1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := cMenuBreakCaption;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsFindItem;
  m.OnClick := find1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := RsClearFind;
  m.OnClick := clearfind1click;
  m.tag := 1;
  g.Items.Add(m);
end;

procedure TJvYearGrid.Edit1Click(Sender: TObject);
var
  ds: string;
  acol, arow: Integer;
  f: TYearGridEditForm;
  CanChange: Boolean;
  InfoText: string;
begin
  acol := col;
  arow := row;
  if ((acol < 1) or (arow < 1)) then Exit;
  ds := YearData[col, row].displaytext;
  if (ds = '') then Exit;
  f := TYearGridEditForm.Create(application);
  InfoText := YearData[acol, arow].infotext;
  f.MemoText.Text := Infotext;
  if f.ShowModal = mrOK then
  begin
    InfoText := f.MemoText.Text;
    Canchange := true;
    if Assigned(onInfoChanging) then
      OnInfoChanging(Self, InfoText, Canchange);
    if CanChange then
    begin
      YearData[col, row].infotext := Infotext;
      if InfoText = '' then
        YearData[col, row].custom := false
      else if not YearData[col, row].custom then
      begin
        YearData[col, row].custom := true;
        YearData[col, row].customColor := rgb(206, 250, 253);
      end;
    end;
  end;
  f.free;
end;

procedure TJvYearGrid.Color1click(Sender: TObject);
var
  cd: TColorDialog;
begin
  if (col < 1) or (row < 1) or (YearData[col, row].Displaytext = '') then Exit;
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanyColor];
  if cd.Execute then
  begin
    YearData[col, row].customColor := cd.Color;
    YearData[col, row].custom := true;
  end;
  cd.free;
  invalidate;

end;

procedure TJvYearGrid.noColor1click(Sender: TObject);
begin
  if (col < 1) or (row < 1) or (YearData[col, row].Displaytext = '') then Exit;
  YearData[col, row].custom := false;
  invalidate;
end;

procedure TJvYearGrid.SetupGridPop(Sender: TObject);
var
  i, c: Integer;
begin
  c := GridPop.items.count;
  if (col > 0) and (row > 0) and (YearData[col, row].Displaytext <> '') then
    for i := 0 to c - 1 do
      GridPop.items[i].enabled := true
  else
    for i := 0 to c - 1 do
      GridPop.items[i].enabled := (GridPop.items[i].tag = 1);
end;

procedure TJvYearGrid.Launch(Afile: string);
var
  command, params, workdir: string;
begin
  command := afile;
  params := #0;
  workdir := #0;
{$IFDEF VCL}
  shellexecute(application.handle, 'open', @command[1],
  @params[1], @workdir[1], SW_SHOWNORMAL);
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IFDEF MSWINDOWS}
  shellexecute(0, 'open', @command[1],
    @params[1], @workdir[1], SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$ENDIF VisualCLX}
{$IFDEF LINUX}
  libc.system(PChar(AFile + ' &'));
{$ENDIF}
end;

procedure TJvYearGrid.SetHTMLFontName(const Value: string);
begin
  FHTMLFontName := Value;
end;

function TJvYearGrid.GetSelDateText: string;
var
  ds: string;

begin
  if (col < 1) or (row < 1) then Exit;
  ds := YearData[col, row].displaytext;
  if ds = '' then Exit;
  result := YearData[col, row].infotext;
end;

procedure TJvYearGrid.SetSelDateText(Atext: string);
var
  ds, s: string;
begin
  if (col < 1) or (row < 1) then Exit;
  ds := YearData[col, row].displaytext;
  if ds = '' then Exit;
  YearData[col, row].infotext := s;
end;

procedure TJvYearGrid.SeTOnSelectDate(const Value: TOnSelectDate);
begin
  FonSelectDate := Value;
end;

function TJvYearGrid.SelectCell(ACol, ARow: Longint): Boolean;
var
  ds: string;
  Adate: tdate;
  Infotext: string;
  InfoColor: TColor;
  month, day: Word;
  Canselect: Boolean;
begin
  CanSelect := true;
  if Assigned(onselectcell) then
    onselectcell(Self, acol, arow, canselect);
  if not CanSelect then
  begin
    result := false;
    Exit;
  end;
  Result := False;
  if (Acol < 1) or (Arow < 1) then Exit;
  ds := YearData[acol, arow].displaytext;
  if ds = '' then Exit;
  month := arow;
  day := strtoint(YearData[acol, arow].displaytext);
  adate := encodedate(theyear, month, day);
  infotext := YearData[acol, arow].infotext;
  if YearData[acol, arow].custom = true then
    infoColor := YearData[acol, arow].customColor
  else
    infoColor := YearData[acol, arow].defaultColor;
  if Assigned(onselectDate) then
    onselectdate(Self, Adate, Infotext, infoColor);
  result := true;
end;

procedure TJvYearGrid.DblClick;
begin
  if Assigned(ondblclick) then
    ondblclick(Self);
  if (col > 0) and (row > 0) and (YearData[col, row].Displaytext <> '') then
    edit1click(nil);
end;

procedure TJvYearGrid.SetBorderColor(const Value: TColor);
begin
  if value <> FBorderColor then
  begin
    FBorderColor := Value;
    invalidate;
  end;
end;

procedure TJvYearGrid.BorderColor1click(Sender: TObject);
var
  cd: TColorDialog;
begin
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanyColor];
  if cd.Execute then
    BorderColor := cd.Color;
  cd.free;
end;

procedure TJvYearGrid.BookMarkColor1click(Sender: TObject);
var
  cd: TColorDialog;
begin
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanyColor];
  if cd.Execute then
    BookMarkColor := cd.Color;
  cd.free;
end;

procedure TJvYearGrid.SeTOnInfoChanging(const Value: TOnInfoChanging);
begin
  FonInfoChanging := Value;
end;

function TJvYearGrid.DateToCell(ADate: TDate; var Acol, aRow: Integer): Boolean;
var
  ayear, amonth, aday: Word;
  wd: Integer;
begin
  result := false;
  decodedate(Adate, ayear, amonth, aday);
  if ayear <> GridYear then Exit;
  arow := Amonth;
  wd := dayofweek(encodeDate(ayear, amonth, 1));
  acol := wd + aday - 1;
  result := true;
end;

function TJvYearGrid.GetDateInfo(aDate: TDate; var aText: string): Boolean;
var
  acol, arow: Integer;
begin
  if DateToCell(aDate, acol, arow) then
  begin
    Atext := YearData[acol, arow].infotext;
    result := true;
  end
  else
    result := false;
end;

function TJvYearGrid.SetDateInfo(aDate: TDate; aText: string): Boolean;
var
  acol, arow: Integer;
begin
  if DateToCell(aDate, acol, arow) then
  begin
    YearData[acol, arow].infotext := atext;
    result := true;
  end
  else
    result := false;
end;

procedure TJvYearGrid.SetBookMarkColor(const Value: TColor);
begin
  if value <> FBookMarkColor then
  begin
    FBookMarkColor := Value;
    invalidate;
  end;
end;

procedure TJvYearGrid.Find1click(Sender: TObject);
var
  s: string;
  acol, arow: Integer;
begin
  ClearBookMarks;
  s := inputbox(RsYearGridFind, RsEnterSeachText, '');
  if s = '' then Exit;
  s := lowercase(s);
  for arow := 0 to 12 do
    for acol := 0 to 37 do
      if pos(s, lowercase(YearData[acol, arow].InfoText)) > 0 then
        YearData[acol, arow].Bookmark := true;
  invalidate;

end;

procedure TJvYearGrid.ClearFind1click(Sender: TObject);
begin
  ClearBookMarks;
end;

procedure TJvYearGrid.Find;
begin
  Find1click(nil);
end;

procedure TJvYearGrid.SaveFound(Sender: TObject);
var
  alist: Tstringlist;
  afile: string;
begin
  alist := tstringlist.create;
  MakeHTML(alist, HTMLBorder, true);
  afile := Format(RsFounds, [changefileext(YearFile, '.htm')]);
  alist.savetofile(afile);
  alist.free;
  launch(afile);
end;

end.
