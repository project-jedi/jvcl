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
  CommCtrl, Menus, ClipBrd,
  JVCLVer, JvTypes;

type
  EJvListViewError = EJVCLException;
  TJvListView = class; // forward
  TJvListItemGroup = class;
  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TOnSortMethod = function(Sender: TObject; Column: Integer): TJvSortMethod of object;
  TProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvListItem = class(TListItem)
  private
    FPopupMenu: TPopupMenu;
    FBold: Boolean;
    function GetGroup: TJvListItemGroup;
    procedure SetGroup(const Value: TJvListItemGroup);
  protected
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
    constructor CreateEnh(AOwner: TListItems);
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Group:TJvListItemGroup read GetGroup write SetGroup;
  end;

  TJvListItemGroup = class(TCollectionItem)
  private
    FCaption: string;
    FAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
  published
    property Caption:string read FCaption write SetCaption;
//    property Footer:string;
    property Alignment:TAlignment read FAlignment write SetAlignment;
  end;

  TJvListItemGroups = class(TOwnedCollection)
  private
    FListView:TJvListView;
    function GetItem(Index: integer): TJvListItemGroup;
    procedure SetItem(Index: integer; const Value: TJvListItemGroup);
  public
    constructor Create(AListView:TJvListView);
    function Add:TJvListItemGroup;
    procedure Assign(Source:TPersistent);override;
    property Items[Index:integer]:TJvListItemGroup read GetItem write SetItem;
  end;


  TJvListView = class(TListView)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FAutoClipboardCopy: Boolean;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOver: Boolean;
    FSortOnClick: Boolean;
    FLast: Integer;
    FOnSaveProgress: TProgress;
    FOnLoadProgress: TProgress;
    FOnAutoSort: TOnSortMethod;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
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
    // (rom) why not a property?
    function GetColumnsOrder: string;
    procedure SetColumnsOrder(const Order: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property SortOnClick: Boolean read FSortOnClick write FSortOnClick default True;
    property AutoClipboardCopy: Boolean read FAutoClipboardCopy write FAutoClipboardCopy default True;
    property OnLoadProgress: TProgress read FOnLoadProgress write FOnLoadProgress;
    property OnSaveProgress: TProgress read FOnSaveProgress write FOnSaveProgress;
    property OnAutoSort: TOnSortMethod read FOnAutoSort write FOnAutoSort;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    // (rom) why not a property?
    procedure SetItemPopup(Node: TListItem; Value: TPopupMenu);
    function GetItemPopup(Node: TListItem): TPopupMenu;
    // (rom) why published?
    procedure SaveToFile(FileName: string; ForceOldStyle: Boolean = False);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(Stream: TStream; ForceOldStyle: Boolean = False);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    {$IFNDEF COMPILER6_UP}
    procedure SelectAll;
    {$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
    {$IFNDEF COMPILER6_UP}
    procedure DeleteSelected;
    {$ENDIF}
  end;

implementation

//=== TJvListItem ============================================================

const
  // (rom) increased from 100
  cColumnsHandled = 1024;

constructor TJvListItem.CreateEnh(AOwner: TListItems);
begin
  inherited Create(AOwner);
  FBold := False;
  FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

function TJvListItem.GetGroup: TJvListItemGroup;
begin
  Result := nil;
end;

procedure TJvListItem.SetGroup(const Value: TJvListItemGroup);
begin
  //
end;

procedure TJvListItem.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

//=== TJvListView ============================================================

constructor TJvListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FSortOnClick := True;
  FLast := -1;
  FAutoClipboardCopy := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

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
    case NMHDR^.code of
      NM_CLICK, NM_RCLICK:
        begin
          Node := GetItemAt(X, Y);
          if Assigned(Node) then
            Selected := Node;
          if (Selected <> nil) and (NMHDR^.code = NM_RCLICK) then
            TJvListItem(Selected).PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;
    end;
  end;
end;

procedure TJvListView.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvListView.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvListView.MouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvListView.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvListView.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvListView.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvListView.ColClick(Column: TListColumn);
type
  TParamSort = record
    Index: Integer;
    Sender: TObject;
  end;
var
  Parm: TParamSort;

  function CustomCompare1(Item1, Item2, Paramsort: Integer): Integer stdcall;
  var
    Parm: TParamSort;
    i1, i2: TListItem;
    S1, S2: string;
    I: Integer;
    SortKind: TJvSortMethod;
  
    function IsBigger(First, Second: string; SortType: TJvSortMethod): Boolean;
    var
      I, J: Real;
      d, e: TDateTime;
      a, b: Currency;
      l, m: Int64;
      st, st2: string;
      int1, int2: Integer;

      function FirstNonAlpha(Value: string): Integer;
      var
        Len: Integer;
        I, J: Integer;
        Comma: Boolean;
      begin
        Len := Length(Value);
        I := 1;
        J := 0;
        Comma := False;

        while I <= Len do
        begin
          case Value[I] of
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
              J := I;
            ',', '.':
              if not Comma then
                Comma := True
              else
              begin
                J := I - 1;
                I := Len;
              end;
          else
            begin
              J := I - 1;
              I := Len;
            end;
          end;
          Inc(I);
        end;
      
        Result := J;
      end;

    begin
      Result := False;
      if Trim(First) = '' then
        Result := False
      else
      if Trim(Second) = '' then
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
                I := StrToFloat(First);
                J := StrToFloat(Second);
                Result := I > J;
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
  
  begin
    Parm := TParamSort(Pointer(ParamSort)^);
    i1 := TListItem(Item1);
    i2 := TListItem(Item2);
    I := Parm.Index;
  
    if Assigned(TJvListView(Parm.Sender).FOnAutoSort) then
      SortKind := TJvListView(Parm.Sender).FOnAutoSort(Parm.Sender, Parm.Index)
    else
      SortKind := smAutomatic;
  
    case I of
      {sort by caption}
      0:
        begin
          S1 := i1.Caption;
          S2 := i2.Caption;
  
          if IsBigger(S1, S2, SortKind) then
            Result := +1
          else
          if IsBigger(S2, S1, SortKind) then
            Result := -1
          else
            Result := 0;
        end;
    else
      {sort by Column}
      begin
        if I > i1.SubItems.Count then
        begin
          if I > i2.SubItems.Count then
            Result := 0
          else
            Result := -1;
        end
        else
        if I > i2.SubItems.Count then
          Result := +1
        else
        begin
          S1 := i1.SubItems[I - 1];
          S2 := i2.SubItems[I - 1];
          if IsBigger(S1, S2, SortKind) then
            Result := +1
          else
          if IsBigger(S2, S1, SortKind) then
            Result := -1
          else
            Result := 0;
        end;
      end;
    end;
  end;
  
  function CustomCompare2(Item1, Item2, ParamSort: Integer): Integer; stdcall;
  begin
    Result := -CustomCompare1(Item1, Item2, ParamSort);
  end;

begin
  inherited ColClick(Column);
  if FSortOnClick then
  begin
    Parm.Index := Column.Index;
    Parm.Sender := Self;
    if FLast = Column.Index then
    begin
      FLast := -1;
      CustomSort(TLVCompare(@CustomCompare2), Integer(@Parm));
    end
    else
    begin
      FLast := Column.Index;
      CustomSort(TLVCompare(@CustomCompare1), Integer(@Parm));
    end;
  end;
end;

function TJvListView.CreateListItem: TListItem;
begin
  Result := TJvListItem.CreateEnh(Items);
end;

function TJvListView.GetItemPopup(Node: TListItem): TPopupMenu;
begin
  Result := TJvListItem(Node).PopupMenu;
end;

procedure TJvListView.SetItemPopup(Node: TListItem; Value: TPopupMenu);
begin
  TJvListItem(Node).PopupMenu := Value;
end;

procedure TJvListView.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;

// (rom) a 100 char buffer is silly

procedure TJvListView.LoadFromStream(Stream: TStream);
var
  Buf: array [0..100] of Char;
  Start: Integer;

  procedure LoadOldStyle(Stream: TStream);
  var
    I, J, k: Integer;
    Buf: array [0..100] of Byte;
    st: string;
    ch1, checks: Boolean;
    t: TListItem;
  begin
    I := Stream.Position;
    t := nil;
    st := '';
    Items.Clear;
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, 0, Stream.Size - Start);
    checks := False;
    ch1 := CheckBoxes;
    while I < Stream.Size do
    begin
      J := Stream.Read(Buf, 100);
      if Assigned(FOnLoadProgress) then
        FOnLoadProgress(Self, J, Stream.Size - Start);
      I := I + J;
      k := 0;
      while k < J do
      begin
        while (k < J) and (Buf[k] <> 0) and (Buf[k] <> 1) do
        begin
          st := st + Char(Buf[k]);
          Inc(k);
        end;

        if k < J then
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
          if Buf[k] = 1 then
            t := nil;
          st := '';
        end;
        Inc(k);
      end;
    end;
    if (not ch1) and (not checks) then
      CheckBoxes := False;
  end;

  procedure LoadNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    // hs-    LV_CHECKED = $8000;
  var
    Count, I, J: Word;
    Options: Byte;
    st: string;
    t: TListItem;
    Buf: array [0..2048] of Char;
  begin
    try
      Self.Items.BeginUpdate;
      Self.Items.Clear;
      Self.Items.EndUpdate;

      Stream.Read(Options, SizeOf(Options));
      CheckBoxes := (Options and LV_HASCHECKBOXES) = LV_HASCHECKBOXES;

      //Read all lines
      while Stream.Position < Stream.Size do
      begin
        Stream.Read(Count, SizeOf(Count));

        //statistics
        if Assigned(FOnLoadProgress) then
          FOnLoadProgress(Self, Stream.Position, Stream.Size - Start);

        //Read all columns
        t := Self.Items.Add;
        for I := 1 to Count do
        begin
          // hs-
          if I = 1 then
          begin
            Stream.Read(Options, SizeOf(Options));
            if CheckBoxes then
              t.Checked := Boolean(Options and Ord(True));
          end;
          // -hs

          (* hs-
                    Stream.Read(J, SizeOf(I));
          -hs *)
          Stream.Read(J, SizeOf(J));

          //Read the string
          FillChar(Buf, SizeOf(Buf), #0);
          Stream.Read(Buf, J);
          st := Buf;

          if I = 1 then
          begin
            t.Caption := st;
            (* hs-
                        if CheckBoxes then
                          t.Checked := (I and LV_CHECKED) = LV_CHECKED;
            -hs *)
          end
          else
            t.SubItems.Add(st);
        end;
      end;
    except
    end;
  end;

begin
  Start := Stream.Position;
  Stream.Read(Buf, 10);
  Buf[10] := #0;
  if Buf <> 'LISTVIEW01' then
  begin
    Stream.Position := Start;
    LoadOldStyle(Stream);
  end
  else
    LoadNewStyle(Stream);
end;

procedure TJvListView.SaveToFile(FileName: string; ForceOldStyle: Boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream, ForceOldStyle);
  Stream.Free;
end;

procedure TJvListView.SaveToStream(Stream: TStream; ForceOldStyle: Boolean);

  procedure SaveOldStyle(Stream: TStream);
  var
    I, J, k: Integer;
    b, c, d, e: Byte;
    st: string;
    Buf: array [0..1000] of Byte;
  begin
    b := 0;
    c := 1;
    d := Ord('T'); //checked
    E := Ord('F'); //not checked
    if Assigned(FOnSaveProgress) then
      FOnSaveProgress(Self, 0, Self.Items.Count);
    for I := 0 to Self.Items.Count - 1 do
    begin
      if Assigned(FOnSaveProgress) then
        FOnSaveProgress(Self, I + 1, Self.Items.Count);
      st := Self.Items[I].Caption;
      for k := 1 to Length(st) do
        Buf[k - 1] := Byte(st[k]);
      k := Length(st);
      //write checked,not
      if Self.Items[I].Checked then
        Stream.Write(d, 1)
      else
        Stream.Write(e, 1);
      Stream.Write(Buf, k);
      if Self.Items[I].SubItems.Count = 0 then
        Stream.Write(c, 1)
      else
      begin
        Stream.Write(b, 1);
        for J := 0 to Self.Items[I].subitems.Count - 2 do
        begin
          st := Self.Items[I].subitems[J];
          for k := 1 to Length(st) do
            Buf[k - 1] := Byte(st[k]);
          k := Length(st);
          Stream.Write(Buf, k);
          Stream.Write(b, 1);
        end;
        J := Self.Items[I].subitems.Count - 1;
        st := Self.Items[I].subitems[J];
        for k := 1 to Length(st) do
          Buf[k - 1] := Byte(st[k]);
        k := Length(st);
        Stream.Write(Buf, k);
        Stream.Write(c, 1);
      end;
    end;
  end;

  procedure SaveNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    // hs-    LV_CHECKED = $8000;
  var
    Buf: array [0..100] of Char;
    // hs-    I, J: Word;
    I: Integer;
    J: Word;
    // hs    Options : Byte;
    Options, IsChecked: Byte;

    procedure WriteString(Txt: string);
    var
      I: Word;
      Buf: array [1..2056] of Char;
    begin
      I := Length(Txt);
      Move(Text[1], Buf, I);
      Stream.Write(I, SizeOf(I));
      Stream.Write(Buf, I);
    end;

  begin
    Buf := 'LISTVIEW01';
    Stream.Write(Buf, 10);
    if CheckBoxes then
      Options := LV_HASCHECKBOXES
    else
      Options := 0;
    Stream.Write(Options, SizeOf(Options));
    for I := 0 to Items.Count - 1 do
      with Items[I] do
      begin
        J := SubItems.Count + 1;
        Stream.Write(J, SizeOf(J));
        // hs-
        IsChecked := Options or (Byte(Ord(Checked)));
        Stream.Write(IsChecked, SizeOf(IsChecked));
        // -hs
        WriteString(Caption);
        for J := 0 to SubItems.Count - 1 do
          WriteString(SubItems[J]);
      end;
  end;

begin
  if ForceOldStyle then
    SaveOldStyle(Stream)
  else
    SaveNewStyle(Stream);
end;

// (rom) better reimplement with streams or TStringList

procedure TJvListView.LoadFromCSV(FileName: string; Separator: Char);
var
  st, st2: string;
  fich: textfile;
  Size, Current: Integer;
  t: TListItem;
  f: file of Byte;
  I, J, k, l: Integer;
begin
  Items.Clear;

  AssignFile(f, FileName);
  Reset(f);
  Size := FileSize(f);
  CloseFile(f);

  AssignFile(fich, FileName);
  Reset(fich);
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, 0, Size);
  Current := 0;
  while not Eof(fich) do
  begin
    Readln(fich, st);
    Current := Current + Length(st) + 2;
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, Current, Size);
    t := Items.Add;

    J := 0;
    k := 1;
    for I := 1 to Length(st) do
      if st[I] = '"' then
        J := (J + 1) mod 2
      else
      if st[I] = Separator then
        if J = 0 then
          Inc(k);
    if k <> 1 then
    begin
      I := Pos(Separator, st);
      J := Pos('"', st);
      l := 0;

      while I <> 0 do
      begin
        if (J = 0) or (J > I) then
        begin
          st2 := Copy(st, 1, I - 1);
          st := Copy(st, I + 1, Length(st));
        end
        else
        begin
          st := Copy(st, J + 1, Length(st));
          J := Pos('"', st);
          if J = 0 then
          begin
            st2 := st;
            st := '';
          end
          else
          begin
            st2 := Copy(st, 1, J - 1);
            st := Copy(st, J + 1, Length(st));
            J := Pos(Separator, st);
            st := Copy(st, J + 1, Length(st));
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

        I := Pos(Separator, st);
        J := Pos('"', st);
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

procedure TJvListView.SaveToCSV(FileName: string; Separator: Char);
var
  st: string;
  fich: textfile;
  I, J: Integer;
begin
  AssignFile(fich, FileName);
  Rewrite(fich);
  if Assigned(FOnLoadProgress) then
    FOnLoadProgress(Self, 0, Items.Count);
  for I := 0 to Items.Count - 1 do
  begin
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, I + 1, Items.Count);
    st := Items[I].Caption;
    if Pos(Separator, st) <> 0 then
      st := '"' + st + '"';
    for J := 0 to Items[I].Subitems.Count - 1 do
    begin
      if Pos(Separator, Items[I].Subitems[J]) = 0 then
        st := st + Separator + Items[I].Subitems[J]
      else
        st := st + Separator + '"' + Items[I].Subitems[J] + '"';
    end;
    Writeln(fich, st);
  end;
  CloseFile(fich);
end;

procedure TJvListView.InvertSelection;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := not Items[I].Selected;
  Items.EndUpdate;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvListView.SelectAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := True;
  Items.EndUpdate;
end;
{$ENDIF}

procedure TJvListView.UnselectAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := False;
  Items.EndUpdate;
end;

procedure TJvListView.KeyUp(var Key: Word; Shift: TShiftState);
var
  st: string;
  I, J: Integer;
begin
  inherited KeyUp(Key, Shift);
  if AutoClipboardCopy then
    if (Key in [Ord('c'), Ord('C')]) and (ssCtrl in Shift) then
    begin
      for I := 0 to Columns.Count - 1 do
        st := st + Columns[I].Caption + #9;
      if st <> '' then
        st := st + CrLf;
      for I := 0 to Items.Count - 1 do
        if (SelCount = 0) or (Items[I].Selected) then
        begin
          st := st + Items[I].Caption;
          for J := 0 to Items[I].SubItems.Count - 1 do
            st := st + #9 + Items[I].SubItems[J];
          st := st + CrLf;
        end;
      Clipboard.SetTextBuf(PChar(st));
    end;
end;

{$IFNDEF COMPILER6_UP}
procedure TJvListView.DeleteSelected;
var
  I: Integer;
begin
  Items.BeginUpdate;
  if SelCount = 1 then
  begin
    I := Selected.Index - 1;
    Selected.Delete;
    if I = -1 then
      I := 0;
    if Items.Count > 0 then
      Selected := Items[I];
  end
  else
  begin
    for I := Items.Count - 1 downto 0 do
      if Items[I].Selected then
        Items[I].Delete;
  end;
  Items.EndUpdate;
end;
{$ENDIF}

function TJvListView.GetColumnsOrder: string;
var
  Res: array [0..cColumnsHandled-1] of Integer;
  I: Integer;
begin
  ListView_GetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
  Result := '';
  if Columns.Count > cColumnsHandled then
    raise EJvListViewError.Create('TJvListView.GetColumnsOrder: too many columns');
  for I := 0 to Columns.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + IntToStr(Res[I]) + '=' + IntToStr(Columns[I].Width);
  end;
end;

procedure TJvListView.SetColumnsOrder(const Order: string);
var
  Res: array [0..cColumnsHandled-1] of Integer;
  I, J: Integer;
  st: string;
begin
  FillChar(Res, SizeOf(Res), #0);
  with TStringList.Create do
  try
    CommaText := Order;
    I := 0;
    while Count > 0 do
    begin
      st := Strings[0];
      J := Pos('=', st);
      if (J <> 0) and (I < Columns.Count) then
      begin
        Columns[I].Width := StrToIntDef(Copy(st, J + 1, Length(st)), Columns[I].Width);
        st := Copy(st, 1, J - 1);
      end;
      Res[I] := StrToIntDef(st, 0);
      Delete(0);
      Inc(I);
    end;
    ListView_SetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
  finally
    Free;
  end;
end;

{ TJvListItemGroups }

function TJvListItemGroups.Add: TJvListItemGroup;
begin
  Result := TJvListItemGroup(inherited Add);
end;

procedure TJvListItemGroups.Assign(Source: TPersistent);
begin
  if Source is TJvListItemGroups then
  begin
    Exit;
  end;
  inherited;

end;

constructor TJvListItemGroups.Create(AListView: TJvListView);
begin
  inherited Create(AListView, TJvListItemGroup);
  FListView := AListView;
end;

function TJvListItemGroups.GetItem(Index: integer): TJvListItemGroup;
begin
  Result := TJvListItemGroup(inherited Items[Index]);
end;

procedure TJvListItemGroups.SetItem(Index: integer;
  const Value: TJvListItemGroup);
begin
  inherited Items[Index] := Value;
end;

{ TJvListItemGroup }

procedure TJvListItemGroup.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TJvListItemGroup.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

end.

