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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit JvYearGrid;

interface

uses
  Windows, shellapi, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, menus, clipbrd;

type
  TYearData = record
    DisPlaytext: string;
    InfoText: string;
    DefaultColor: Tcolor;
    CustomColor: Tcolor;
    Custom: boolean;
    BookMark: boolean; //this is not saved
  end;

  TonYearChanged = procedure(sender: Tobject; AYear: integer) of object;
  TonSelectDate = procedure(sender: Tobject; Adate: TDate; InfoText: string; InfoColor: Tcolor) of object;
  TonInfoChanging = procedure(sender: Tobject; var InfoText: string; var CanChange: boolean) of object;
  TJvYearGrid = class(TDrawGrid)
  private
    GridPop: TPopupMenu;
    thisyear, thismonth, thisday: word;
    FHTMLBorder: boolean;
    FGridYear: integer;
    FonYearChanged: TOnYearChanged;
    FHTMLFontName: string;
    FonSelectDate: TonselectDate;
    FBorderColor: TColor;
    FonInfoChanging: TonInfoChanging;
    FBookMarkColor: Tcolor;
    { Private declarations }

    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure MakeHTML(alist: tstringlist; border, filter: boolean);
    procedure SetHTMLBorder(const Value: boolean);
    procedure SetGridYear(const Value: integer);
    procedure SetonYearChanged(const Value: TOnYearChanged);
    procedure setYear(AYear: word);
    procedure LoadYear;
    procedure SaveYear;
    procedure SetupYearData;
    procedure setupmonths;
    function GetCellData(var s: string): boolean;
    function SetCellData(s: string): boolean;
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure CreatePopup;
    procedure Edit1Click(Sender: TObject);
    procedure year1Click(Sender: TObject);
    procedure color1click(sender: TObject);
    procedure nocolor1click(sender: TObject);
    procedure SetupGridPop(sender: TObject);
    procedure SaveAsHTML(sender: TObject);
    procedure Launch(Afile: string);
    procedure SetHTMLFontName(const Value: string);
    procedure SetonSelectDate(const Value: TonselectDate);
    procedure SetBorderColor(const Value: TColor);
    procedure BorderColor1click(sender: TObject);
    procedure SetonInfoChanging(const Value: TonInfoChanging);
    function DateToCell(ADate: TDate; var Acol, aRow: integer): boolean;
    procedure ClearBookMarks;
    procedure SetBookMarkColor(const Value: Tcolor);
    procedure BookMarkColor1click(sender: TObject);
    procedure Find1click(sender: Tobject);
    procedure ClearFind1click(sender: Tobject);
    procedure SaveFound(sender: Tobject);

  protected
    { Protected declarations }
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    function SelectCell(ACol, ARow: Integer): boolean; override;
    procedure dblclick; override;
  public
    { Public declarations }
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
    function GetSelDateText: string;
    procedure SetSelDateText(Atext: string);
    function GetDateInfo(aDate: TDate; var aText: string): boolean;
    function SetDateInfo(aDate: TDate; aText: string): boolean;
    procedure Find;
  published
    { Published declarations }
    property HTMLBorder: boolean read FHTMLBorder write SetHTMLBorder;
    property HTMLFontName: string read FHTMLFontName write SetHTMLFontName;
    property GridYear: integer read FGridYear write SetGridYear;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BookMarkColor: Tcolor read FBookMarkColor write SetBookMarkColor;
    property onYearChanged: TOnYearChanged read FonYearChanged write SetonYearChanged;
    property onSelectDate: TonselectDate read FonSelectDate write SetonSelectDate;
    property onInfoChanging: TonInfoChanging read FonInfoChanging write SetonInfoChanging;
  end;

implementation

uses
  JvYearGridEdit;

const
  todayfontcolor = clwhite;
  todaybrushcolor = clred;

  cr = chr(13) + chr(10);
  tab = chr(9);

var
  appldir: string;
  daysinmonth: array[1..12] of integer;
  startdays: array[1..12] of integer;
  theyear: word;
  thisdaystr: string;
  isthisyear: boolean;
  YearData: array[0..37, 0..12] of TYearData;
  YearFile: string;

  { TJvYearGrid }

constructor TJvYearGrid.create(AOwner: Tcomponent);
begin
  inherited create(AOwner);
  width := 746;
  height := 353;
  defaultcolwidth := 16;
  defaultrowheight := 24;
  colcount := 38;
  rowcount := 13;
  FBordercolor := $EEF5FF;
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

destructor TJvYearGrid.destroy;
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
  if assigned(onDrawCell) then
    onDrawCell(self, acol, arow, rect, state);
  s := YearData[acol, arow].DisplayText;
  with canvas do
  begin
    font.color := clblack;
    font.style := font.style - [fsbold];
    if ((acol = 0) or ((arow = 0) and (YearData[acol, arow].defaultcolor = clwhite))) then
      brush.color := bordercolor
    else if (isthisyear and (arow = thismonth) and (s = thisdaystr)) then
    begin
      font.color := todayfontcolor;
      brush.color := todaybrushcolor;
      font.style := font.style + [fsbold];
    end
    else if YearData[acol, arow].custom then
      brush.color := YearData[acol, arow].customcolor
    else
      brush.color := YearData[acol, arow].defaultcolor;
    if yearData[acol, arow].bookmark then
      brush.color := BookMarkColor;
    textrect(rect, rect.left, rect.top, s);
  end;
end;

procedure TJvYearGrid.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  acol, arow, x, y: integer;
  s, ds: string;
begin
  if HintInfo.HintControl = self then
  begin
    x := HintInfo.CursorPos.x;
    y := HintInfo.cursorPos.y;
    MouseToCell(x, y, acol, arow);
    if ((acol < 0) or (arow < 0)) then exit;
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

procedure TJvYearGrid.MakeHTML(alist: tstringlist; border, Filter: boolean);
var
  acol, arow, w: integer;
  ds, tbs, infs: string;
  month, day: word;
  ADate: tdate;
  CanAdd: boolean;
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

procedure TJvYearGrid.SaveAsHTML(sender: Tobject);
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

procedure TJvYearGrid.SetHTMLBorder(const Value: boolean);
begin
  FHTMLBorder := Value;
end;

procedure TJvYearGrid.SetonYearChanged(const Value: TOnYearChanged);
begin
  FonYearChanged := Value;
end;

procedure TJvYearGrid.SetGridYear(const Value: integer);
begin
  if value <> FGridYear then
  begin
    FGridYear := Value;
    setYear(FGridYear);
    if assigned(onYearChanged) then
      onYearChanged(self, FGridYear);
  end;
end;

procedure TJvYearGrid.setYear(AYear: word);
var
  year, month, day: word;
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
  arow, acol: integer;
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
      Dlist.append(colortostring(YearData[acol, arow].DefaultColor));
      Dlist.append(colortostring(YearData[acol, arow].CustomColor));
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
  arow, acol, index: integer;
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
      YearData[acol, arow].DefaultColor := stringtocolor(DList[2]);
      YearData[acol, arow].CustomColor := stringtocolor(DList[3]);
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
  i, acol, arow: integer;
  Acolor: Tcolor;
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
      //  Acolor:=clsilver;
      //if ((acol>0)and (d<>'S')) then
      //  Acolor:=clwhite;
      //  Change to:
      if (acol > 0) and (d = 'S') then
        AColor := clSilver
      else
        AColor := clWhite;
      YearData[acol, arow].DisPlayText := s;
      YearData[acol, arow].InfoText := '';
      YearData[acol, arow].DefaultColor := Acolor;
      YearData[acol, arow].CustomColor := Acolor;
      YearData[acol, arow].Custom := false;
      YearData[acol, arow].bookmark := false;
    end;
  Invalidate;
end;

procedure TJvYearGrid.ClearBookMarks;
var
  acol, arow: integer;
begin
  for arow := 0 to 12 do
    for acol := 0 to 37 do
      YearData[acol, arow].bookmark := false;
  invalidate;

end;

procedure TJvYearGrid.setupmonths;
var
  year, month, day: word;
  Adate: tdate;
  i: integer;
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

function TJvYearGrid.GetCellData(var s: string): boolean;
var
  acol, arow: integer;
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

function TJvYearGrid.SetCellData(s: string): boolean;
var
  acol, arow: integer;
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
  Ayear: word;
begin
  s := inputbox('YearGrid', 'Enter year (1999-2050):', inttostr(GridYear));
  try
    if s = '' then exit;
    ayear := strtoint(s);
    if ((ayear < 1999) or (ayear > 2050)) then exit;
    GridYear := ayear;
  except
    showmessage('invalid year');
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
var
  g: Tpopupmenu;
  m: tmenuitem;
begin
  GridPop := Tpopupmenu.create(self);
  g := GridPop;
  m := tmenuitem.Create(g);
  m.Caption := '&Year...';
  m.OnClick := year1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '-';
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Edit';
  m.OnClick := edit1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Color...';
  m.OnClick := color1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&No Color';
  m.OnClick := nocolor1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '-';
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Copy';
  m.OnClick := copy1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := 'Cu&t';
  m.OnClick := cut1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Paste';
  m.OnClick := paste1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Delete';
  m.OnClick := delete1click;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '-';
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Save All Info';
  m.OnClick := SaveAsHTML;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := 'Save Found Info';
  m.OnClick := SaveFound;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '-';
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Border Color...';
  m.OnClick := bordercolor1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := 'Book&Mark Color...';
  m.OnClick := bookmarkcolor1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '-';
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := '&Find...';
  m.OnClick := find1click;
  m.tag := 1;
  g.Items.Add(m);
  m := tmenuitem.Create(g);
  m.Caption := 'Clear Find';
  m.OnClick := clearfind1click;
  m.tag := 1;
  g.Items.Add(m);

end;

procedure TJvYearGrid.Edit1Click(Sender: TObject);
var
  ds: string;
  acol, arow: integer;
  f: TYearGridEditF;
  CanChange: boolean;
  InfoText: string;
begin
  acol := col;
  arow := row;
  if ((acol < 1) or (arow < 1)) then exit;
  ds := YearData[col, row].displaytext;
  if (ds = '') then exit;
  f := TYearGridEditF.Create(application);
  InfoText := YearData[acol, arow].infotext;
  f.Memo1.Text := Infotext;
  if f.ShowModal = mrOK then
  begin
    InfoText := f.memo1.text;
    Canchange := true;
    if assigned(onInfoChanging) then
      onInfoChanging(self, InfoText, Canchange);
    if CanChange then
    begin
      YearData[col, row].infotext := Infotext;
      if InfoText = '' then
        YearData[col, row].custom := false
      else if not YearData[col, row].custom then
      begin
        YearData[col, row].custom := true;
        YearData[col, row].customcolor := rgb(206, 250, 253);
      end;
    end;
  end;
  f.free;
end;

procedure TJvYearGrid.color1click(sender: TObject);
var
  cd: TcolorDialog;
begin
  if (col < 1) or (row < 1) or (YearData[col, row].Displaytext = '') then exit;
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanycolor];
  if cd.Execute then
  begin
    YearData[col, row].customcolor := cd.Color;
    YearData[col, row].custom := true;
  end;
  cd.free;
  invalidate;

end;

procedure TJvYearGrid.nocolor1click(sender: TObject);
begin
  if (col < 1) or (row < 1) or (YearData[col, row].Displaytext = '') then exit;
  YearData[col, row].custom := false;
  invalidate;
end;

procedure TJvYearGrid.SetupGridPop(sender: TObject);
var
  i, c: integer;
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
  shellexecute(application.handle, 'open', @command[1],
    @params[1], @workdir[1], SW_SHOWNORMAL);
end;

procedure TJvYearGrid.SetHTMLFontName(const Value: string);
begin
  FHTMLFontName := Value;
end;

function TJvYearGrid.GetSelDateText: string;
var
  ds: string;

begin
  if (col < 1) or (row < 1) then exit;
  ds := YearData[col, row].displaytext;
  if ds = '' then exit;
  result := YearData[col, row].infotext;
end;

procedure TJvYearGrid.SetSelDateText(Atext: string);
var
  ds, s: string;
begin
  if (col < 1) or (row < 1) then exit;
  ds := YearData[col, row].displaytext;
  if ds = '' then exit;
  YearData[col, row].infotext := s;
end;

procedure TJvYearGrid.SetonSelectDate(const Value: TonselectDate);
begin
  FonSelectDate := Value;
end;

function TJvYearGrid.SelectCell(ACol, ARow: Longint): boolean;
var
  ds: string;
  Adate: tdate;
  Infotext: string;
  Infocolor: TColor;
  month, day: word;
  Canselect: boolean;
begin
  CanSelect := true;
  if assigned(onselectcell) then
    onselectcell(self, acol, arow, canselect);
  if not CanSelect then
  begin
    result := false;
    exit;
  end;
  Result := False;
  if (Acol < 1) or (Arow < 1) then exit;
  ds := YearData[acol, arow].displaytext;
  if ds = '' then exit;
  month := arow;
  day := strtoint(YearData[acol, arow].displaytext);
  adate := encodedate(theyear, month, day);
  infotext := YearData[acol, arow].infotext;
  if YearData[acol, arow].custom = true then
    infocolor := YearData[acol, arow].customcolor
  else
    infocolor := YearData[acol, arow].defaultcolor;
  if assigned(onselectDate) then
    onselectdate(self, Adate, Infotext, infocolor);
  result := true;
end;

procedure TJvYearGrid.dblclick;
begin
  if assigned(ondblclick) then
    ondblclick(self);
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

procedure TJvYearGrid.BorderColor1click(sender: TObject);
var
  cd: TcolorDialog;
begin
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanycolor];
  if cd.Execute then
    BorderColor := cd.Color;
  cd.free;
end;

procedure TJvYearGrid.BookMarkColor1click(sender: TObject);
var
  cd: TcolorDialog;
begin
  cd := TColordialog.Create(application);
  cd.Options := [cdfullopen, cdanycolor];
  if cd.Execute then
    BookMarkColor := cd.Color;
  cd.free;
end;

procedure TJvYearGrid.SetonInfoChanging(const Value: TonInfoChanging);
begin
  FonInfoChanging := Value;
end;

function TJvYearGrid.DateToCell(ADate: TDate; var Acol, aRow: integer): boolean;
var
  ayear, amonth, aday: word;
  wd: integer;
begin
  result := false;
  decodedate(Adate, ayear, amonth, aday);
  if ayear <> GridYear then exit;
  arow := Amonth;
  wd := dayofweek(encodeDate(ayear, amonth, 1));
  acol := wd + aday - 1;
  result := true;
end;

function TJvYearGrid.GetDateInfo(aDate: TDate; var aText: string): boolean;
var
  acol, arow: integer;
begin
  if DateToCell(aDate, acol, arow) then
  begin
    Atext := YearData[acol, arow].infotext;
    result := true;
  end
  else
    result := false;
end;

function TJvYearGrid.SetDateInfo(aDate: TDate; aText: string): boolean;
var
  acol, arow: integer;
begin
  if DateToCell(aDate, acol, arow) then
  begin
    YearData[acol, arow].infotext := atext;
    result := true;
  end
  else
    result := false;
end;

procedure TJvYearGrid.SetBookMarkColor(const Value: Tcolor);
begin
  if value <> FBookMarkColor then
  begin
    FBookMarkColor := Value;
    invalidate;
  end;
end;

procedure TJvYearGrid.Find1click(sender: Tobject);
var
  s: string;
  acol, arow: integer;
begin
  ClearBookMarks;
  s := inputbox('YearGrid Find', 'Enter seach text:', '');
  if s = '' then exit;
  s := lowercase(s);
  for arow := 0 to 12 do
    for acol := 0 to 37 do
      if pos(s, lowercase(YearData[acol, arow].InfoText)) > 0 then
        YearData[acol, arow].Bookmark := true;
  invalidate;

end;

procedure TJvYearGrid.ClearFind1click(sender: Tobject);
begin
  ClearBookMarks;
end;

procedure TJvYearGrid.Find;
begin
  Find1click(nil);
end;

procedure TJvYearGrid.SaveFound(sender: Tobject);
var
  alist: Tstringlist;
  afile: string;
begin
  alist := tstringlist.create;
  MakeHTML(alist, HTMLBorder, true);
  afile := 'Found' + changefileext(YearFile, '.htm');
  alist.savetofile(afile);
  alist.free;
  launch(afile);
end;

end.
