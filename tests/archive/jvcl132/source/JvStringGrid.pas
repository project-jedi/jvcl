{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringGrid.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvStringGrid;

interface

{$OBJEXPORTALL On}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Grids, JvTypes, JVCLVer;

type
  TJvSortType = (stAutomatic, stClassic, stCaseSensitive, stNumeric, stDate, stCurrency);
{$EXTERNALSYM TJvSortType}
  TProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvStringGrid = class(TStringGrid)
  private
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnLoad: TProgress;
    FOnSave: TProgress;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SortGrid(Column: Integer; Ascending: Boolean = True; Fixed: Boolean = False;
      SortType: TJvSortType = stClassic; BlankTop: Boolean = True);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnLoadProgress: TProgress read FOnLoad write FOnLoad;
    property OnSaveProgress: TProgress read FOnSave write FOnSave;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

{**************************************************}

procedure TJvStringGrid.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvStringGrid.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvStringGrid.SortGrid(Column: Integer; Ascending,
  Fixed: Boolean; SortType: TJvSortType; BlankTop: Boolean);
var
  st: string;
  tmpc: Currency;
  tmpf: Double;
  tmpd: TDateTime;
  debut, fin: Integer;

  procedure ExchangeGridRows(i, j: Integer);
  var
    k: Integer;
  begin
    if Fixed then
      for k := 0 to ColCount - 1 do
        Cols[k].Exchange(i, j)
    else
      for k := FixedCols to ColCount - 1 do
        Cols[k].Exchange(i, j);
  end;

  function IsSmaller(First, Second: string): Boolean;
  var
    i, j: Int64;
    f1, f2: Double;
    c1, c2: Currency;
  begin
    try
      i := StrToInt64(First);
      j := StrToInt64(Second);
      Result := i < j;
      Exit;
    except
    end;
    try
      f1 := StrToFloat(First);
      f2 := StrToFloat(Second);
      Result := f1 < f2;
      Exit;
    except
    end;
    try
      c1 := StrToCurr(First);
      c2 := StrToCurr(Second);
      Result := c1 < c2;
      Exit;
    except
    end;
    Result := First > Second;
  end;

  function IsBigger(First, Second: string): Boolean;
  begin
    Result := IsSmaller(Second, First);
  end;

  procedure QuickSort(l, r: Integer);
  var
    i, j, m: Integer;
  begin
    repeat
      i := l;
      j := r;
      m := (l + r) div 2;
      st := Cells[Column, m];
      repeat
        case SortType of
          stClassic:
            begin
              while CompareText(Cells[Column, i], st) < 0 do
                Inc(i);
              while CompareText(Cells[Column, j], st) > 0 do
                Dec(j);
            end;
          stCaseSensitive:
            begin
              while CompareStr(Cells[Column, i], st) < 0 do
                Inc(i);
              while CompareStr(Cells[Column, j], st) > 0 do
                Dec(j);
            end;
          stNumeric:
            begin
              tmpf := StrToFloat(st);
              while StrToFloat(Cells[Column, i]) < tmpf do
                Inc(i);
              while StrToFloat(Cells[Column, j]) > tmpf do
                Dec(j);
            end;
          stDate:
            begin
              tmpd := StrToDateTime(st);
              while StrToDateTime(Cells[Column, i]) < tmpd do
                Inc(i);
              while StrToDateTime(Cells[Column, j]) > tmpd do
                Dec(j);
            end;
          stCurrency:
            begin
              tmpc := StrToCurr(st);
              while StrToCurr(Cells[Column, i]) < tmpc do
                Inc(i);
              while StrToCurr(Cells[Column, j]) > tmpc do
                Dec(j);
            end;
          stAutomatic:
            begin
              while IsSmaller(Cells[Column, i], st) do
                Inc(i);
              while IsBigger(Cells[Column, j], st) do
                Dec(j);
            end;
        end;
        if i <= j then
        begin
          if i <> j then
            ExchangeGridRows(i, j);
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if l < j then
        QuickSort(l, j);
      l := i;
    until i >= r;
  end;

  procedure InvertGrid;
  var
    i, j: Integer;
  begin
    i := FixedRows;
    j := RowCount - 1;
    while i < j do
    begin
      ExchangeGridRows(i, j);
      Inc(i);
      Dec(j);
    end;
  end;

  function MoveBlankTop: Integer;
  var
    i, j: Integer;
  begin
    i := FixedRows;
    Result := i;
    j := RowCount - 1;
    while i <= j do
    begin
      if Trim(Cells[Column, i]) = '' then
      begin
        ExchangeGridRows(Result, i);
        Inc(Result);
      end;
      Inc(i);
    end;
  end;

begin
  if (Column >= 0) and (Column < ColCount) then
  begin
    debut := FixedRows;
    fin := RowCount - 1;
    if BlankTop then
      debut := MoveBlankTop;

    QuickSort(Debut, Fin);
    if not Ascending then
      InvertGrid;
  end;
end;

{**************************************************}

constructor TJvStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{**************************************************}

procedure TJvStringGrid.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvStringGrid.LoadFromCSV(FileName: string; Separator: Char);
var
  st, st2: string;
  i, j, k, l, m, n: Integer;
  fich: TextFile;
  fpos, count: Integer;
  f: file of Byte;
begin
  fpos := 0;
  AssignFile(f, FileName);
  Reset(f);
  count := FileSize(f);
  CloseFile(f);
  if Assigned(FOnLoad) then
    FOnLoad(Self, fpos, count);

  AssignFile(fich, FileName);
  Reset(fich);
  k := 0;
  while not Eof(fich) do
  begin
    Readln(fich, st);
    fpos := fpos + Length(st) + 2;
    if Assigned(FOnLoad) then
      FOnLoad(Self, fpos, count);

    //Analyse st
    j := 0;
    l := 1;
    for i := 1 to Length(st) do
      if st[i] = '"' then
        j := (j + 1) mod 2
      else if st[i] = Separator then
        if j = 0 then
          Inc(l);
    if ColCount < l then
      ColCount := l;
    Inc(k);
    if RowCount < k then
      RowCount := k;

    j := 0;
    m := Pos(Separator, st);
    n := Pos('"', st);
    while m <> 0 do
    begin
      if (n = 0) or (n > m) then
      begin
        Cells[j, k - 1] := Copy(st, 1, m - 1);
        st := Copy(st, m + 1, Length(st));
      end
      else
      begin
        st := Copy(st, n + 1, Length(st));
        n := Pos('"', st);
        st2 := Copy(st, 1, n - 1);
        st := Copy(st, n + 1, Length(st));
        m := Pos(Separator, st);
        if m <> 0 then
          st := Copy(st, m + 1, Length(st))
        else
          st := '';
        Cells[j, k - 1] := st2;
      end;
      Inc(j);

      m := Pos(Separator, st);
      n := Pos('"', st);
    end;
    if st <> '' then
      Cells[j, k - 1] := st;
  end;
  if Assigned(FOnLoad) then
    FOnLoad(Self, count, count);
  CloseFile(fich);
end;

{**************************************************}

procedure TJvStringGrid.LoadFromStream(Stream: TStream);
var
  col, row, i, count: Integer;
  buf: array[0..1024] of Byte;
  st: string;
begin
  col := 0;
  row := 1;
  if Assigned(FOnLoad) then
    FOnLoad(Self, 0, Stream.Size);
  while Stream.Position < Stream.Size do
  begin
    count := Stream.Read(buf, 1024);
    if Assigned(FOnLoad) then
      FOnLoad(Self, Stream.Position, Stream.Size);
    for i := 0 to count - 1 do
      case buf[i] of
        0:
          begin
            Inc(col);
            if row > RowCount then
              RowCount := row;
            if col > ColCount then
              ColCount := col;
            Cells[col - 1, row - 1] := st;
            st := '';
          end;
        1:
          begin
            Inc(col);
            if col > ColCount then
              ColCount := col;
            Cells[col - 1, row - 1] := st;
            Inc(row);
            if row > RowCount then
              RowCount := Row;
            col := 0;
            st := '';
          end;
      else
        st := st + Char(buf[i]);
      end;
  end;
  RowCount := RowCount - 1;
  if Assigned(FOnLoad) then
    FOnLoad(Self, Stream.Size, Stream.Size);
end;

{**************************************************}

procedure TJvStringGrid.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

{**************************************************}

procedure TJvStringGrid.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{**************************************************}

procedure TJvStringGrid.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvStringGrid.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvStringGrid.SaveToFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  SaveToStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvStringGrid.SaveToCSV(FileName: string; Separator: Char);
var
  st: string;
  i, j: Integer;
  fich: TextFile;
begin
  AssignFile(fich, FileName);
  Rewrite(fich);
  if Assigned(FOnSave) then
    FOnSave(Self, 0, RowCount * ColCount);
  for i := 0 to RowCount - 1 do
  begin
    st := '';
    for j := 0 to ColCount - 1 do
    begin
      if Assigned(FOnSave) then
        FOnSave(Self, i * ColCount + j, RowCount * ColCount);
      if Pos(Separator, Cells[j, i]) = 0 then
        st := st + Cells[j, i]
      else
        st := st + '"' + Cells[j, i] + '"';
      if j <> ColCount - 1 then
        st := st + Separator
    end;
    Writeln(fich, st);
  end;
  CloseFile(fich);
  if Assigned(FOnSave) then
    FOnSave(Self, RowCount * ColCount, RowCount * ColCount);
end;

{**************************************************}

procedure TJvStringGrid.SaveToStream(Stream: TStream);
var
  i, j, k: Integer;
  st: array[0..1000] of Char;
  stt: string;
  a, b: Byte;
begin
  a := 0;
  b := 1; //a for end of string, b for end of line
  if Assigned(FOnSave) then
    FOnSave(Self, 0, RowCount * ColCount);
  for i := 0 to RowCount - 1 do
  begin
    for j := 0 to ColCount - 1 do
    begin
      if Assigned(FOnSave) then
        FOnSave(Self, i * ColCount + j, RowCount * ColCount);
      stt := Cells[j, i];
      for k := 1 to Length(stt) do
        st[k - 1] := stt[k];
      Stream.Write(st, Length(Cells[j, i]));
      if j <> ColCount - 1 then
        Stream.Write(a, 1);
    end;
    Stream.Write(b, 1);
  end;
  if Assigned(FOnSave) then
    FOnSave(Self, RowCount * ColCount, RowCount * ColCount);
end;

end.
