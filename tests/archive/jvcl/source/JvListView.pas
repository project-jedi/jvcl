{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListView.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvListView;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  CommCtrl, Menus, JVCLVer, ClipBrd;

type
  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TOnSortMethod = function(Sender: TObject; Column: Integer): TJvSortMethod of object;
  TProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvListItem = class(TListItem)
  private
    FPopupMenu: TPopupMenu;
    FBold: Boolean;
  protected
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
    constructor CreateEnh(AOwner: TListItems);
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;
{$EXTERNALSYM TJvListItem}

  TJvListView = class(TListView)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOver: Boolean;
    FSort: Boolean;
    FLast: Integer;
    FOnSave: TProgress;
    FOnLoad: TProgress;
    FOnSort: TOnSortMethod;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FAutoClipboard: Boolean;
  protected
    function CreateListItem: TListItem; override;
    procedure ColClick(Column: TListColumn); override;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColumnsOrder: string;
    procedure SetColumnsOrder(const Order: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property SortOnClick: Boolean read FSort write FSort default True;
    procedure SetItemPopup(Node: TListItem; Value: TPopupMenu);
    function GetItemPopup(Node: TListItem): TPopupMenu;
    procedure SaveToFile(FileName: string; ForceOldStyle: Boolean = False);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(stream: TStream; ForceOldStyle: Boolean = False);
    procedure LoadFromStream(stream: TStream);
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    property AutoClipboardCopy:Boolean read FAutoClipboard write FAutoClipboard default true;
    property OnLoadProgress: TProgress read FOnLoad write FOnLoad;
    property OnSaveProgress: TProgress read FOnSave write FOnSave;
    property OnAutoSort: TOnSortMethod read FOnSort write FOnSort;
{$IFNDEF COMPILER6_UP}
    procedure SelectAll;
{$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
{$IFNDEF COMPILER6_UP}
    procedure DeleteSelected;
{$ENDIF}
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvListItem
///////////////////////////////////////////////////////////

constructor TJvListItem.CreateEnh(AOwner: TListItems);
begin
  Create(AOwner);
  FBold := False;
  FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

{**************************************************}

procedure TJvListItem.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

///////////////////////////////////////////////////////////
// TJvListView
///////////////////////////////////////////////////////////

constructor TJvListView.Create(AOwner: TComponent);
begin
  FColor := clInfoBk;
  FOver := False;
  FSort := True;
  FLast := -1;
  FAutoClipboard := true;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{**************************************************}

procedure TJvListView.WMNotify(var Msg: TWMNotify);
var
  Node: TListItem;
  Point: TPoint;
begin
  inherited;

  Point := Mouse.CursorPos;
  Point := ScreenToClient(Point);
  with Msg, Point do
  begin
    case NMHdr^.code of
      NM_CLICK, NM_RCLICK:
        begin
          Node := GetItemAt(x, y);
          if Assigned(Node) then
            Selected := Node;
          if (Selected <> nil) and (NMHdr^.code = NM_RCLICK) then
            TJvListItem(Selected).PopupMenu.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
        end;
    end;
  end;
end;

{**************************************************}

procedure TJvListView.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

{**************************************************}

procedure TJvListView.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{***********************************************}

procedure TJvListView.MouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvListView.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvListView.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;
{**************************************************}

procedure TJvListView.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{******************************************************************************}

function FirstNonAlpha(Value: string): Integer;
var
  len: Integer;
  i, j: Integer;
  comma: Boolean;
begin
  len := Length(Value);
  i := 1;
  j := 0;
  comma := False;

  while i <= len do
  begin
    case Value[i] of
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
        j := i;
      ',', '.':
        if not comma then
          comma := True
        else
        begin
          j := i - 1;
          i := len;
        end;
    else
      begin
        j := i - 1;
        i := len;
      end;
    end;
    Inc(i);
  end;

  Result := j;
end;

{******************************************************************************}

function IsBigger(First, Second: string; SortType: TJvSortMethod): Boolean;
var
  i, j: real;
  d, e: TDateTime;
  a, b: Currency;
  l, m: int64;
  st, st2: string;
  int1, int2: Integer;
begin
  Result := False;
  if Trim(First) = '' then
    Result := False
  else if Trim(Second) = '' then
    Result := True
  else
  begin
    case SortType of
      smAlphabetic:
        Result := First > Second;
      smNonCaseSensitive:
        Result := UpperCase(First) > UpperCase(Second);
      smNumeric:
        begin
          try
            i := StrToFloat(First);
            j := StrToFloat(Second);
            Result := i > j;
          except
            try
              l := StrToInt64(First);
            except
              l := 0;
            end;
            try
              m := StrToInt64(Second);
            except
              m := 0;
            end;
            Result := l > m;
          end;
        end;
      smDate:
        begin
          d := StrToDate(First);
          e := StrToDate(Second);
          Result := d > e;
        end;
      smTime:
        begin
          d := StrToTime(First);
          e := StrToTime(Second);
          Result := d > e;
        end;
      smDateTime:
        begin
          d := StrToDateTime(First);
          e := StrToDateTime(Second);
          Result := d > e;
        end;
      smCurrency:
        begin
          a := StrToCurr(First);
          b := StrToCurr(Second);
          Result := a > b;
        end;
      smAutomatic:
        begin
          int1 := FirstNonAlpha(First);
          int2 := FirstNonAlpha(Second);
          if (int1 <> 0) and (int2 <> 0) then
          begin
            st := Copy(First, 1, int1);
            st2 := Copy(Second, 1, int2);
            try
              Result := StrToFloat(st) > StrToFloat(st2);
            except
              Result := First > Second;
            end;
          end
          else
            Result := First > Second;
        end;
    end;
  end;
end;

{******************************************************************************}

function IsSmaller(First, Second: string; SortType: TJvSortMethod): Boolean;
begin
  Result := IsBigger(Second, First, SortType);
end;

{******************************************************************************}

function SortList(item1, item2, Paramsort: Integer): Integer stdcall;
type
  TParamSort = record
    Index: Integer;
    Sender: TObject;
  end;
var
  parm: TParamSort;
  i1, i2: TListItem;
  s1, s2: string;
  i: Integer;
  sortype: TJvSortMethod;
begin
  parm := TParamSort(Pointer(ParamSort)^);
  i1 := TListItem(item1);
  i2 := TListItem(item2);
  i := parm.Index;

  if Assigned(TJvListView(parm.Sender).FOnSort) then
    sortype := TJvListView(parm.Sender).FOnSort(parm.Sender, parm.Index)
  else
    sortype := smAutomatic;

  case i of
    {sort by caption}
    0:
      begin
        s1 := i1.Caption;
        s2 := i2.Caption;

        if IsBigger(s1, s2, sortype) then
          Result := +1
        else if IsSmaller(s1, s2, sortype) then
          Result := -1
        else
          Result := 0;
      end;
    {sort by Column}
  else
    begin
      if i > i1.SubItems.Count then
      begin
        if i > i2.SubItems.Count then
          Result := 0
        else
          Result := -1;
      end
      else if i > i2.SubItems.Count then
        Result := +1
      else
      begin
        s1 := i1.SubItems[i - 1];
        s2 := i2.SubItems[i - 1];
        if IsBigger(s1, s2, sortype) then
          Result := +1
        else if IsSmaller(s1, s2, sortype) then
          Result := -1
        else
          Result := 0;
      end;
    end;
  end;
end;

{******************************************************************************}

function SortList2(item1, item2, Paramsort: Integer): Integer; stdcall;
begin
  Result := -SortList(item1, item2, ParamSort);
end;

{**************************************************}

procedure TJvListView.ColClick(Column: TListColumn);
type
  TParamSort = record
    Index: Integer;
    Sender: TObject;
  end;
var
  parm: TParamSort;
begin
  inherited;
  if Fsort then
  begin
    parm.Index := Column.Index;
    parm.Sender := Self;
    if FLast = Column.Index then
    begin
      FLast := -1;
      CustomSort(sortlist2, Integer(@parm));
    end
    else
    begin
      FLast := Column.Index;
      CustomSort(sortlist, Integer(@parm));
    end;
  end;
end;

{**************************************************}

function TJvListView.CreateListItem: TListItem;
begin
  Result := TJvListItem.CreateEnh(Items);
end;

{**************************************************}

function TJvListView.GetItemPopup(Node: TListItem): TPopupMenu;
begin
  Result := TJvListItem(Node).PopupMenu;
end;

{**************************************************}

procedure TJvListView.SetItemPopup(Node: TListItem; Value: TPopupMenu);
begin
  TJvListItem(Node).PopupMenu := Value;
end;

{**************************************************}

procedure TJvListView.LoadFromFile(FileName: string);
var
  stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvListView.LoadFromStream(stream: TStream);
var
  buf: array[0..100] of Char;
  start: Integer;

  procedure LoadOldStyle(Stream: TStream);
  var
    i, j, k: Integer;
    buf: array[0..100] of Byte;
    st: string;
    ch1, checks: Boolean;
    t: TListItem;
  begin
    i := Stream.Position;
    t := nil;
    st := '';
    Items.Clear;
    if Assigned(FOnLoad) then
      FOnLoad(Self, 0, stream.Size - start);
    checks := False;
    ch1 := CheckBoxes;
    while i < Stream.Size do
    begin
      j := Stream.Read(buf, 100);
      if Assigned(FOnLoad) then
        FOnLoad(Self, j, stream.Size - start);
      i := i + j;
      k := 0;
      while k < j do
      begin
        while (k < j) and (buf[k] <> 0) and (buf[k] <> 1) do
        begin
          st := st + Char(buf[k]);
          Inc(k);
        end;

        if k < j then
        begin
          if t <> nil then
            t.SubItems.Add(st)
          else
          begin
            t := Items.Add;
            checks := checks or (st[1] = 'T');
            t.Checked := st[1] = 'T';
            st := Copy(st, 2, Length(st));
            t.Caption := st;
          end;
          if buf[k] = 1 then
            t := nil;
          st := '';
        end;
        Inc(k);
      end;
    end;
    if (ch1 = False) and (checks = False) then
      CheckBoxes := False;
  end;

  procedure LoadNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    LV_CHECKED = $8000;
  var
    count, i, j: Word;
    options: Byte;
    st: string;
    t: TListItem;
    buf: array[0..2048] of Char;
  begin
    try
      Self.Items.BeginUpdate;
      Self.Items.Clear;
      Self.Items.EndUpdate;

      Stream.Read(options, SizeOf(options));
      CheckBoxes := (options and LV_HASCHECKBOXES) = LV_HASCHECKBOXES;

      //Read all lines
      while Stream.Position < Stream.Size do
      begin
        Stream.Read(count, SizeOf(count));

        //statistics
        if Assigned(FOnLoad) then
          FOnLoad(Self, Stream.Position, stream.Size - start);

        //Read all columns
        t := Self.Items.Add;
        for i := 1 to count do
        begin
          //Read Size of the string
          Stream.Read(j, SizeOf(i));

          //Read the string
          ZeroMemory(@buf, SizeOf(buf));
          Stream.Read(buf, j);
          st := buf;

          if (i = 1) then
          begin
            t.Caption := st;
            if CheckBoxes then
              t.Checked := (i and LV_CHECKED) = LV_CHECKED;
          end
          else
            t.SubItems.Add(st);
        end;
      end;
    except
    end;
  end;

begin
  start := Stream.Position;
  Stream.Read(buf, 10);
  buf[10] := #0;
  if (buf <> 'LISTVIEW01') then
  begin
    Stream.Position := start;
    LoadOldStyle(Stream);
  end
  else
    LoadNewStyle(Stream);
end;
{**************************************************}

procedure TJvListView.SaveToFile(FileName: string; ForceOldStyle: Boolean);
var
  stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream, ForceOldStyle);
  Stream.Free;
end;
{**************************************************}

procedure TJvListView.SaveToStream(stream: TStream; ForceOldStyle: Boolean);

  procedure SaveOldStyle(Stream: TStream);
  var
    i, j, k: Integer;
    b, c, d, e: Byte;
    st: string;
    buf: array[0..1000] of Byte;
  begin
    b := 0;
    c := 1;
    d := ord('T'); //checked
    E := ord('F'); //not checked
    if Assigned(FOnSave) then
      FOnSave(Self, 0, Self.Items.count);
    for i := 0 to Self.Items.count - 1 do
    begin
      if Assigned(FOnSave) then
        FOnSave(Self, i + 1, Self.Items.count);
      st := Self.Items[i].caption;
      for k := 1 to Length(st) do
        buf[k - 1] := Byte(st[k]);
      k := Length(st);
      //write checked,not
      if Self.Items[i].Checked then
        Stream.write(d, 1)
      else
        Stream.write(e, 1);
      stream.Write(buf, k);
      if Self.Items[i].SubItems.Count = 0 then
        Stream.write(c, 1)
      else
      begin
        Stream.Write(b, 1);
        for j := 0 to Self.Items[i].subitems.count - 2 do
        begin
          st := Self.Items[i].subitems[j];
          for k := 1 to Length(st) do
            buf[k - 1] := Byte(st[k]);
          k := Length(st);
          stream.write(buf, k);
          stream.write(b, 1);
        end;
        j := Self.Items[i].subitems.count - 1;
        st := Self.Items[i].subitems[j];
        for k := 1 to Length(st) do
          buf[k - 1] := Byte(st[k]);
        k := Length(st);
        stream.write(buf, k);
        stream.write(c, 1);
      end;
    end;
  end;

  procedure SaveNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    LV_CHECKED = $8000;
  var
    buf: array[0..100] of Char;
    i, j: Word;
    options: Byte;

    procedure WriteString(Txt: string);
    var
      i: Word;
      buf: array[1..2056] of Char;
    begin
      i := Length(Txt);
      CopyMemory(@buf, @Txt[1], i);
      Stream.Write(i, SizeOf(i));
      Stream.Write(buf, i);
    end;

  begin
    buf := 'LISTVIEW01';
    Stream.Write(buf, 10);
    if CheckBoxes then
      options := LV_HASCHECKBOXES
    else
      options := 0;
    Stream.Write(options, SizeOf(options));
    for i := 0 to Items.Count - 1 do
      with Items[i] do
      begin
        j := SubItems.Count + 1;
        Stream.Write(j, SizeOf(j));
        WriteString(Caption);
        for j := 0 to SubItems.Count - 1 do
          WriteString(SubItems[j]);
      end;
  end;

begin
  if ForceOldStyle then
    SaveOldStyle(Stream)
  else
    SaveNewStyle(Stream);
end;

{**************************************************}

procedure TJvListView.LoadFromCSV(FileName: string; Separator: Char);
var
  st, st2: string;
  fich: textfile;
  Size, current: Integer;
  t: TListItem;
  f: file of Byte;
  i, j, k, l: Integer;
begin
  Items.Clear;

  AssignFile(f, FileName);
  Reset(f);
  Size := FileSize(f);
  CloseFile(f);

  AssignFile(fich, FileName);
  Reset(fich);
  if Assigned(FOnLoad) then
    FOnLoad(Self, 0, Size);
  current := 0;
  while not Eof(fich) do
  begin
    Readln(fich, st);
    current := current + Length(st) + 2;
    if Assigned(FOnLoad) then
      FOnLoad(Self, current, Size);
    t := Items.Add;

    j := 0;
    k := 1;
    for i := 1 to Length(st) do
      if st[i] = '"' then
        j := (j + 1) mod 2
      else if st[i] = Separator then
        if j = 0 then
          Inc(k);
    if k <> 1 then
    begin
      i := Pos(Separator, st);
      j := Pos('"', st);
      l := 0;

      while i <> 0 do
      begin
        if (j = 0) or (j > i) then
        begin
          st2 := Copy(st, 1, i - 1);
          st := Copy(st, i + 1, Length(st));
        end
        else
        begin
          st := Copy(st, j + 1, Length(st));
          j := Pos('"', st);
          if j = 0 then
          begin
            st2 := st;
            st := '';
          end
          else
          begin
            st2 := Copy(st, 1, j - 1);
            st := Copy(st, j + 1, Length(st));
            j := Pos(Separator, st);
            st := Copy(st, j + 1, Length(st));
          end;
        end;
        if l = 0 then
        begin
          t.Caption := st2;
          Inc(l);
        end
        else
          t.Subitems.Add(st2);
        Dec(k);

        i := Pos(Separator, st);
        j := Pos('"', st);
      end;

      if k = 1 then
        t.Subitems.Add(st);
    end
    else
    begin
      if Pos('"', st) = 0 then
        t.Caption := st
      else
        st := Copy(st, Pos('"', st) + 1, Length(st));
      if Pos('"', st) = 0 then
        t.Caption := st
      else
        t.Caption := Copy(st, 1, Pos('"', st) - 1);
    end;
  end;
  CloseFile(fich);
end;

{**************************************************}

procedure TJvListView.SaveToCSV(FileName: string; Separator: Char);
var
  st: string;
  fich: textfile;
  i, j: Integer;
begin
  AssignFile(fich, FileName);
  Rewrite(fich);
  if Assigned(FOnLoad) then
    FOnLoad(Self, 0, Items.Count);
  for i := 0 to Items.Count - 1 do
  begin
    if Assigned(FOnLoad) then
      FOnLoad(Self, i + 1, Items.Count);
    st := Items[i].Caption;
    if Pos(Separator, st) <> 0 then
      st := '"' + st + '"';
    for j := 0 to Items[i].Subitems.Count - 1 do
    begin
      if Pos(Separator, Items[i].Subitems[j]) = 0 then
        st := st + Separator + Items[i].Subitems[j]
      else
        st := st + Separator + '"' + Items[i].Subitems[j] + '"';
    end;
    Writeln(fich, st);
  end;
  CloseFile(fich);
end;

{**************************************************}

procedure TJvListView.InvertSelection;
var
  i: Integer;
begin
  Items.BeginUpdate;
  for i := 0 to Items.Count - 1 do
    Items[i].Selected := not Items[i].Selected;
  Items.EndUpdate;
end;

{**************************************************}

{$IFNDEF COMPILER6_UP}

procedure TJvListView.SelectAll;
var
  i: Integer;
begin
  Items.BeginUpdate;
  for i := 0 to Items.Count - 1 do
    Items[i].Selected := True;
  Items.EndUpdate;
end;
{$ENDIF}

{**************************************************}

procedure TJvListView.UnselectAll;
var
  i: Integer;
begin
  Items.BeginUpdate;
  for i := 0 to Items.Count - 1 do
    Items[i].Selected := False;
  Items.EndUpdate;
end;

{**************************************************}

procedure TJvListView.KeyUp(var Key: Word; Shift: TShiftState);
var
 st: string;
 i,j: Integer;
begin
  inherited;
  if AutoClipboardCopy then
    if (Key in [Ord('c'), Ord('C')]) and (ssCtrl in Shift) then
    begin
      for i:=0 to Columns.Count-1 do
        st := st + Columns[i].Caption+#9;
      if st<>'' then
        st := st + #13#10;
      for i:=0 to Items.Count-1 do
        if (SelCount = 0) or (Items[i].Selected) then
        begin
          st := st + Items[i].Caption;
          for j:=0 to Items[i].SubItems.Count-1 do
            st := st + #9 + Items[i].SubItems[j];
          st := st + #13#10;
        end;
      Clipboard.SetTextBuf(PChar(st));
    end;
end;

{**************************************************}

{$IFNDEF COMPILER6_UP}

procedure TJvListView.DeleteSelected;
var
  i: Integer;
begin
  Items.BeginUpdate;
  if SelCount = 1 then
  begin
    i := Selected.Index - 1;
    Selected.Delete;
    if i = -1 then
      i := 0;
    if Items.Count > 0 then
      Selected := Items[i];
  end
  else
  begin
    for i := Items.Count - 1 downto 0 do
      if Items[i].Selected then
        Items[i].Delete;
  end;
  Items.EndUpdate;
end;
{$ENDIF}

{**************************************************}

function TJvListView.GetColumnsOrder: string;
var
  res: array[0..100] of Integer;
  i: Integer;
begin
  ListView_GetColumnOrderArray(Columns.Owner.Handle,Columns.Count,@res);
  result := '';
  for i:=0 to Columns.Count - 1 do
  begin
    if result <> '' then
      result := result+',';
    result := result + IntToStr(res[i]) + '=' + IntToStr(Columns[i].Width);
  end;
end;

{**************************************************}

procedure TJvListView.SetColumnsOrder(const Order: string);
var
  res: array[0..100] of Integer;
  i,j: Integer;
  st: string;
begin
  for i:=0 to 100 do
    res[i] := 0;
  with TStringList.Create do
  try
    CommaText := Order;
    i := 0;
    while Count>0 do
    begin
      st := Strings[0];
      j := pos('=',st);
      if (j<>0) and (i<Columns.Count) then
      begin
        Columns[i].Width := StrToIntDef(Copy(st, j+1, Length(st)),Columns[i].Width);
        st := Copy(st,1,j-1);
      end;
      res[i] := StrToIntDef(st,0);
      Delete(0);
      inc(i);
    end;
    ListView_SetColumnOrderArray(Columns.Owner.Handle,Columns.Count,@res);
  finally
    Free;
  end;
end;

{**************************************************}

end.
