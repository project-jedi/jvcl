{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckListBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This is a merging of the code in the original JvCheckListBox.pas and JvFixedCheckListBox.pas
Merging done 2002-06-05 by Peter Thornqvist [peter3 at sourceforge dot net]

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Below <100113 dott 1101 att compuserve dott com>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckListBox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Contnrs, Controls, Graphics, StdCtrls,
  JvExCheckLst, JvDataSourceIntf;

const
  DefaultValueChecked = '1';
  DefaultValueUnchecked = '0';

type
  TJvCheckListBox = class;

  TJvCheckListBoxDataConnector = class(TJvLookupDataConnector)
  private
    FCheckListBox: TJvCheckListBox;
    FValueChecked: string;
    FValueUnchecked: string;
    FMap: TList;
    FRecNumMap: TBucketList;
    procedure SetValueChecked(const Value: string);
    procedure SetValueUnchecked(const Value: string);
    function IsValueCheckedStored: Boolean;
    function IsValueUncheckedStored: Boolean;
  protected
    procedure Popuplate; virtual;
    procedure ActiveChanged; override;
    procedure UpdateData; override;
    procedure RecordChanged; override;
    procedure GetKeyNames(out KeyName, ListKeyName: TDataFieldString);
    procedure GotoCurrent;
  public
    constructor Create(ACheckListBox: TJvCheckListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsValid: Boolean; virtual;
  published
    property ValueChecked: string read FValueChecked write SetValueChecked stored IsValueCheckedStored;
    property ValueUnchecked: string read FValueUnchecked write SetValueUnchecked stored IsValueUncheckedStored;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCheckListBox = class(TJvExCheckListBox)
  private
    FHotTrack: Boolean;
    FOnSelectCancel: TNotifyEvent;
    FMaxWidth: Integer;
    FScroll: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    procedure SetHScroll(const Value: Boolean);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure LBNSelCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure RefreshH;
    procedure SetHotTrack(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  private
    FDataConnector: TJvCheckListBoxDataConnector;
    FOnItemDrawing: TDrawItemEvent;
    procedure SetDataConnector(const Value: TJvCheckListBoxDataConnector);
    procedure CMChanged(var Msg: TMessage); message CM_CHANGED;
  protected
    function CreateDataConnector: TJvCheckListBoxDataConnector; virtual;
    procedure ClickCheck; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DoItemDrawing(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
    procedure SelectAll; override;
    procedure UnselectAll;
    procedure InvertSelection;
    procedure CheckAll;
    procedure UnCheckAll;
    procedure InvertCheck;
    function GetChecked: TStringList;
    function GetUnChecked: TStringList;
    procedure DeleteSelected; override;
    procedure SaveToFile(FileName: TFileName);
    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property OnItemDrawing: TDrawItemEvent read FOnItemDrawing write FOnItemDrawing;

    property MultiSelect;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HorScrollbar: Boolean read FScroll write SetHScroll default True;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;

    property DataConnector: TJvCheckListBoxDataConnector read FDataConnector write SetDataConnector;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF COMPILER10_UP}
  Types,
  {$ENDIF COMPILER10_UP}
  JvItemsSearchs, JvJCLUtils;

//=== { TJvCheckListBoxDataConnector } =======================================

constructor TJvCheckListBoxDataConnector.Create(ACheckListBox: TJvCheckListBox);
begin
  inherited Create;
  FCheckListBox := ACheckListBox;
  FValueChecked := '1';
  FValueUnchecked := '0';
  FMap := TList.Create;
  FRecNumMap := TBucketList.Create(bl256);
end;

destructor TJvCheckListBoxDataConnector.Destroy;
begin
  FMap.Free;
  FRecNumMap.Free;
  inherited Destroy;
end;

procedure TJvCheckListBoxDataConnector.GetKeyNames(out KeyName,
  ListKeyName: TDataFieldString);
begin
  if List.Key.IsValid then
    ListKeyName := List.KeyField;
  if Key.IsValid then
    KeyName := KeyField;
  if ListKeyName = '' then
    ListKeyName := KeyName;
  if KeyName = '' then
    KeyName := ListKeyName;
end;

procedure TJvCheckListBoxDataConnector.GotoCurrent;
var
  RecNo: Integer;
begin
  if IsValid then
  begin
    RecNo := Integer(FMap[FCheckListBox.ItemIndex]);
    if ListSource.RecNo <> RecNo then
    begin
      Active := False;
      try
        ListSource.RecNo := RecNo;
      finally
        Active := True;
      end;
    end;
  end;
end;

function TJvCheckListBoxDataConnector.IsValid: Boolean;
begin
  Result := List.DataSetConnected and List.Field.IsValid and
            DataSetConnected and Field.IsValid and
           (Key.IsValid or List.Key.IsValid or (ListSource.DataSet = DataSource.DataSet));
end;

function TJvCheckListBoxDataConnector.IsValueCheckedStored: Boolean;
begin
  Result := FValueChecked <> DefaultValueChecked;
end;

function TJvCheckListBoxDataConnector.IsValueUncheckedStored: Boolean;
begin
  Result := FValueUnchecked <> DefaultValueUnchecked;
end;

procedure TJvCheckListBoxDataConnector.Popuplate;
var
  IsChecked: TList;
  ListKeyName, KeyName: TDataFieldString;
  I: Integer;
  Index: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF};
begin
  FMap.Clear;
  FRecNumMap.Clear;

  FCheckListBox.Items.BeginUpdate;
  try
    FCheckListBox.Items.Clear;
    Index := -1;
    if IsValid then
    begin
      ListSource.BeginUpdate;
      try
        if DataSource.DataSet <> ListSource.DataSet then
          DataSource.BeginUpdate;
        try
          IsChecked := TList.Create;
          try
            ListSource.First;
            while not ListSource.Eof do
            begin
              Index := FCheckListBox.Items.Add(List.Field.AsString);
              FMap.Add(TObject(ListSource.RecNo));
              FRecNumMap.Add(TObject(ListSource.RecNo), TObject(Index));

              if ListSource.DataSet = DataSource.DataSet then
                IsChecked.Add(TObject(AnsiCompareText(Field.AsString, ValueUnchecked) <> 0))
              else
              begin
                GetKeyNames(KeyName, ListKeyName);
                if DataSource.Locate(KeyName, ListSource.FieldValue[ListSource.FieldByName(ListKeyName)], []) then
                  IsChecked.Add(TObject(AnsiCompareText(Field.AsString, ValueUnchecked) <> 0))
                else
                  IsChecked.Add(TObject(False));
              end;
              ListSource.Next;
            end;
            for I := 0 to IsChecked.Count - 1 do
              FCheckListBox.Checked[I] := Boolean(IsChecked[I]);
          finally
            IsChecked.Free;
          end;
        finally
          if DataSource.DataSet <> ListSource.DataSet then
            DataSource.EndUpdate;
        end;
      finally
        ListSource.EndUpdate;
      end;
      if not FRecNumMap.Find(TObject(ListSource.RecNo), Pointer(Index)) then
        Index := -1;
    end;
    FCheckListBox.ItemIndex := Index;
  finally
    FCheckListBox.Items.EndUpdate;
  end
end;

procedure TJvCheckListBoxDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvCheckListBoxDataConnector then
  begin
    FValueChecked := TJvCheckListBoxDataConnector(Source).ValueChecked;
    FValueUnchecked := TJvCheckListBoxDataConnector(Source).ValueUnchecked;
    Reset;
  end;
end;

procedure TJvCheckListBoxDataConnector.ActiveChanged;
begin
  Popuplate;
end;

procedure TJvCheckListBoxDataConnector.RecordChanged;
var
  Index: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF};
begin
  if IsValid then
  begin
    if ListSource.RecordCount <> FCheckListBox.Items.Count then
      Popuplate
    else
    if ListSource.RecNo <> -1 then
    begin
      if FRecNumMap.Find(TObject(ListSource.RecNo), Pointer(Index)) then
      begin
        FCheckListBox.Items[Index] := List.Field.AsString;
        FCheckListBox.Checked[Index] := AnsiCompareText(Field.AsString, ValueUnchecked) <> 0;
        if Index <> FCheckListBox.ItemIndex then
          FCheckListBox.ItemIndex := Index;
      end;
    end;
  end;
end;

procedure TJvCheckListBoxDataConnector.UpdateData;
var
  KeyName, ListKeyName: TDataFieldString;
  Value: string;
begin
  if Field.CanModify and IsValid and (ValueChecked <> '') and (ValueUnchecked <> '') and
     (FCheckListBox.ItemIndex <> -1) then
  begin
    if FCheckListBox.Checked[FCheckListBox.ItemIndex] then
      Value := ValueChecked
    else
      Value := ValueUnchecked;

    GotoCurrent;
    DataSource.Edit;

    if ListSource.DataSet = DataSource.DataSet then
      Field.AsString := Value
    else
    begin
      GetKeyNames(KeyName, ListKeyName);
      DataSource.BeginUpdate;
      try
        if DataSource.Locate(KeyName, ListSource.FieldValue[ListSource.FieldByName(ListKeyName)], []) then
          Field.AsString := Value;
      finally
        DataSource.EndUpdate;
      end;
    end;
  end;
end;

procedure TJvCheckListBoxDataConnector.SetValueChecked(const Value: string);
begin
  if Value <> FValueChecked then
  begin
    FValueChecked := Value;
    Reset;
  end;
end;

procedure TJvCheckListBoxDataConnector.SetValueUnchecked(const Value: string);
begin
  if Value <> FValueUnchecked then
  begin
    FValueUnchecked := Value;
    Reset;
  end;
end;

//=== { TJvCheckListBox } ====================================================

type
  // Used for the load/save methods
  TCheckListRecord = record
    Checked: Boolean;
    StringSize: Integer;
  end;

constructor TJvCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataConnector := CreateDataConnector;
  FHotTrack := False;
  FMaxWidth := 0;
  FScroll := True;
  {$IFDEF COMPILER14_UP}
  ParentDoubleBuffered := False;
  {$ENDIF COMPILER14_UP}
  // ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TJvCheckListBox.Destroy;
begin
  FDataConnector.Free;
  inherited Destroy;
end;

function TJvCheckListBox.CreateDataConnector: TJvCheckListBoxDataConnector;
begin
  Result := TJvCheckListBoxDataConnector.Create(Self);
end;

procedure TJvCheckListBox.ClickCheck;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    DataConnector.Modify;
    DataConnector.UpdateRecord;
  end;
end;

procedure TJvCheckListBox.SetDataConnector(const Value: TJvCheckListBoxDataConnector);
begin
  if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

procedure TJvCheckListBox.CMChanged(var Msg: TMessage);
begin
  inherited;
  DataConnector.GotoCurrent;
end;

procedure TJvCheckListBox.DoItemDrawing(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(OnItemDrawing) then
    OnItemDrawing(Self, Index, Rect, State);
end;

procedure TJvCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoItemDrawing(Index, Rect, State);
  inherited DrawItem(Index, Rect, State);
end;

procedure TJvCheckListBox.CheckAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := True;
end;

procedure TJvCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FScroll then
      Style := Style or WS_HSCROLL
    else
      Style := Style and not WS_HSCROLL;
end;

function TJvCheckListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

procedure TJvCheckListBox.DeleteSelected;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    for I := Items.Count - 1 downto 0 do
      if Selected[I] then
        Items.Delete(I);
  end
  else
  if ItemIndex <> -1 then
  begin
    I := ItemIndex;
    Items.Delete(I);
    if I > 0 then
      Dec(I);
    if Items.Count > 0 then
      ItemIndex := I;
  end;
end;

function TJvCheckListBox.GetChecked: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Items.Count - 1 do
    if Checked[I] then
      Result.AddObject(Items[I], Items.Objects[I]);
end;

function TJvCheckListBox.GetUnChecked: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Items.Count - 1 do
    if not Checked[I] then
      Result.AddObject(Items[I], Items.Objects[I]);
end;

procedure TJvCheckListBox.InvertCheck;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := not Checked[I];
end;

procedure TJvCheckListBox.InvertSelection;
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := not Selected[I];
end;

procedure TJvCheckListBox.LBNSelCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

procedure TJvCheckListBox.CNDrawItem(var Msg: TWMDrawItem);
begin
  if (Items.Count = 0) or (Msg.DrawItemStruct.itemID >= UINT(Items.Count)) then
    Exit;
  inherited;
end;

procedure TJvCheckListBox.LoadFromFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCheckListBox.LoadFromStream(Stream: TStream);
var
  CheckLst: TCheckListRecord;
  UTF8Item: UTF8String;
begin
  Items.Clear;
  while Stream.Position + SizeOf(TCheckListRecord) <= Stream.Size do
  begin
    Stream.Read(CheckLst, SizeOf(TCheckListRecord));
    if Stream.Position + CheckLst.StringSize <= Stream.Size then
    begin
      SetLength(UTF8Item, CheckLst.StringSize);
      if CheckLst.StringSize > 0 then
        Stream.Read(PAnsiChar(UTF8Item)^, CheckLst.StringSize);
      Checked[Items.Add(UTF8ToString(UTF8Item))] := CheckLst.Checked;
    end;
  end;
end;

procedure TJvCheckListBox.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if HotTrack then
      Ctl3D := True;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCheckListBox.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if HotTrack then
      Ctl3D := False;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCheckListBox.RefreshH;
var
  I: Integer;
  ItemWidth: Word;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
  begin
    ItemWidth := Canvas.TextWidth(Items[I] + ' ');
    Inc(ItemWidth, GetCheckWidth);
    if FMaxWidth < ItemWidth then
      FMaxWidth := ItemWidth;
  end;
  SetHScroll(FScroll);
end;

procedure TJvCheckListBox.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCheckListBox.SaveToStream(Stream: TStream);
var
  I: Integer;
  CheckLst: TCheckListRecord;
  UTF8Item: UTF8String;
begin
  for I := 0 to Items.Count - 1 do
  begin
    UTF8Item := UTF8Encode(Items[I]);
    CheckLst.Checked := Checked[I];
    CheckLst.StringSize := Length(UTF8Item);
    Stream.Write(CheckLst, SizeOf(TCheckListRecord));
    Stream.Write(PAnsiChar(UTF8Item)^, CheckLst.StringSize);
  end;
end;

function TJvCheckListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCheckListBox.SearchPrefix(Value: string; CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

function TJvCheckListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

procedure TJvCheckListBox.SelectAll;
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

procedure TJvCheckListBox.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  if FHotTrack then
    Ctl3D := False;
end;

procedure TJvCheckListBox.SetHScroll(const Value: Boolean);
begin
  FScroll := Value;
  if FScroll then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

procedure TJvCheckListBox.UnCheckAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := False;
end;

procedure TJvCheckListBox.UnselectAll;
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := False;
end;

procedure TJvCheckListBox.WMHScroll(var Msg: TWMHScroll);
var
  ScrollPos: Integer;
  R: TRect;
begin
  inherited;
  // (p3) what does this code do, really?
  if Msg.ScrollCode <> SB_ENDSCROLL then
  begin
    ScrollPos := GetScrollPos(Handle, SB_HORZ);
    if ScrollPos < 20 then
    begin
      R := ClientRect;
      R.Right := R.Left + 20;
      Windows.InvalidateRect(Handle, @R, False);
    end;
  end;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TJvCheckListBox.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

procedure TJvCheckListBox.WndProc(var Msg: TMessage);
var
  ItemWidth: Word;
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
        Inc(ItemWidth, GetCheckWidth);
        if FMaxWidth < ItemWidth then
          FMaxWidth := ItemWidth;
        SetHScroll(FScroll);
      end;
    LB_DELETESTRING:
      begin
        ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
        Inc(ItemWidth, GetCheckWidth);
        if ItemWidth = FMaxWidth then
        begin
          inherited WndProc(Msg);
          RefreshH;
          Exit;
        end;
      end;
    LB_RESETCONTENT:
      begin
        inherited WndProc(Msg);
        FMaxWidth := 0;
        SetHScroll(FScroll);
        Exit;
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Msg);
        Canvas.Font.Assign(Font);
        RefreshH;
        Exit;
      end;
  end;
  inherited WndProc(Msg);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.