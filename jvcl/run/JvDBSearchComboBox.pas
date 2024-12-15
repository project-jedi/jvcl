{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBSearchComboBox.pas, released on 2004-02-28.

The Initial Developer of the Original Code is Lionel Reynaud
Portions created by Sébastien Buysse are Copyright (C) 2004 Lionel Reynaud.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
   DB Component to find record with ComboBox
   Free modified and corrected component TDBViewCombo from ???

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBSearchComboBox;

{$I jvcl.inc}

interface

{$IFDEF COMPILER12}
  {$DEFINE COMPILER_GENERICS_WORKAROUND}
{$ENDIF COMPILER12}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  Generics.Collections,
  {$ENDIF SUPPORTS_GENERICS}
  Windows, Classes, Controls, DB,
  JvCombobox;

type
  TJvDBCustomSearchComboBox = class;

  TJvSearchComboBoxLink = class(TDataLink)
  private
    FJvDBSearchComboBox: TJvDBCustomSearchComboBox;
    FDataFieldName: string;
    FDataField: TField;
    FWasEdited: Boolean;
    FReading: Boolean;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataField;
  protected
    procedure ActiveChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure DataSetChanged; override;
    procedure EditingChanged; override;
  public
    constructor Create(AJvDBSearchComboBox: TJvDBCustomSearchComboBox);
    property DataField: TField read FDataField;
    property DataFieldName: string read FDataFieldName write SetDataFieldName;
  end;

  {$IFDEF SUPPORTS_GENERICS}
    {$IFDEF COMPILER_GENERICS_WORKAROUND}
  // Workaround for QC 70845: Compiler crashes when generating *.lib file with generics in unit
  TBookmarkList = class(TList)
  protected
    function GetItem(Index: Integer): TBookmark;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[Index: Integer]: TBookmark read GetItem; default;
  end;
    {$ELSE}
  TBookmarkList = TList<TBookmark>;
    {$ENDIF COMPILER_GENERICS_WORKAROUND}
  {$ELSE}
  TBookmarkList = TList;
  {$ENDIF SUPPORTS_GENERICS}

  TJvDBCustomSearchComboBox = class(TJvCustomComboBox)
  private
    FDataLink: TJvSearchComboBoxLink;
    FChanging: Boolean;
    FDataResult: string;

    // Mantis 4622: TBookmark are TBytes in D12+ and if we store them inside a
    // simple TList, the compiler will not see the references to the array of
    // bytes, hence will finalize each one of them while we keep them in our
    // list as simple pointers.
    // To avoid this, we could have fiddled with the reference counting
    // ourselves, but we used the new more elegant way of using the generics
    // which makes the compiler do all the work for us.
    FBookmarks: TBookmarkList;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Scroll(Distance: Integer);
    procedure ReadList;
    procedure ClearList;
    procedure Select; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetResult: Variant;
    procedure Refresh;
    property ItemIndex;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataResult: string read FDataResult write FDataResult;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBSearchComboBox = class(TJvDBCustomSearchComboBox)
  published
    property Align;
    property Anchors;
    property AutoComplete default True;
    property AutoSize;
    property AutoDropDown default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property Style;
    property Color;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Flat;
    property ParentFlat;
    property Font;
    property ItemHeight;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
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
  Variants, SysUtils;

{$IFDEF COMPILER_GENERICS_WORKAROUND}
//=== { TBookmarkList } ======================================================

function TBookmarkList.GetItem(Index: Integer): TBookmark;
begin
  Result := TBookmark(inherited Items[Index]);
end;

procedure TBookmarkList.Notify(Ptr: Pointer; Action: TListNotification);
var
  Helper: TBookmark;
begin
  case Action of
    lnAdded:
      begin
        Helper := TBookmark(Ptr); // AddRef
        Pointer(Helper) := nil; // do not call ReleaseRef
      end;
    lnExtracted, lnDeleted:
      TBookmark(Ptr) := nil; // ReleaseRef
  end;
end;
{$ENDIF COMPILER_GENERICS_WORKAROUND}

//=== { TJvSearchComboBoxLink } ==============================================

constructor TJvSearchComboBoxLink.Create(AJvDBSearchComboBox: TJvDBCustomSearchComboBox);
begin
  inherited Create;
  FJvDBSearchComboBox := AJvDBSearchComboBox;
  FDataFieldName := '';
  FDataField := nil;
  FWasEdited := False;
  FReading := False;
end;

procedure TJvSearchComboBoxLink.SetDataField;
begin
  if DataSource = nil then
    Exit;
  if DataSource.DataSet = nil then
    Exit;
  if not DataSource.DataSet.Active then
    Exit;
  if FDataFieldName = '' then
    Exit;
  FDataField := DataSource.DataSet.FieldByName(FDataFieldName);
  if Active then
    FJvDBSearchComboBox.ReadList;
end;

procedure TJvSearchComboBoxLink.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    SetDataField;
  end;
end;

procedure TJvSearchComboBoxLink.ActiveChanged;
begin
  if Active then
  begin
    if FDataField = nil then
      SetDataField;
  end
  else
  begin
    FDataField := nil;
    FJvDBSearchComboBox.ClearList;
  end
end;

procedure TJvSearchComboBoxLink.DataSetChanged;
begin
  if FReading or FJvDBSearchComboBox.FChanging then
    Exit;
  FReading := True;
  try
    if not (DataSource.DataSet.State in dsEditModes) then
    begin
      FJvDBSearchComboBox.ClearList;
      FJvDBSearchComboBox.ReadList;
    end;
  finally
    FReading := False;
  end;
end;

procedure TJvSearchComboBoxLink.DataSetScrolled(Distance: Integer);
begin
  if Distance <> 0 then
    FJvDBSearchComboBox.Scroll(Distance);
end;

procedure TJvSearchComboBoxLink.EditingChanged;
begin
  if Editing then
    FWasEdited := True;
end;

//=== { TJvDBCustomSearchComboBox } ==========================================

constructor TJvDBCustomSearchComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBookmarks := TBookmarkList.Create;
  FDataLink := TJvSearchComboBoxLink.Create(Self);
  FChanging := False;
end;

destructor TJvDBCustomSearchComboBox.Destroy;
begin
  ClearList;
  FDataLink.Free;
  FDataLink := nil; // Notification() is called by inherited Destroy
  inherited Destroy;
  FBookmarks.Free;
end;

procedure TJvDBCustomSearchComboBox.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (FDataLink <> nil) and (Component = DataSource) and (Operation = opRemove) then
    DataSource := nil;
end;

function TJvDBCustomSearchComboBox.GetDataField: string;
begin
  Result := FDataLink.DataFieldName;
end;

function TJvDBCustomSearchComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCustomSearchComboBox.SetDataField(const Value: string);
begin
  FDataLink.DataFieldName := Value;
end;

procedure TJvDBCustomSearchComboBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TJvDBCustomSearchComboBox.Select;
begin
  if not FChanging and (ItemIndex <> -1) and
    (FDataLink.DataSet <> nil) and (FDataLink.DataField <> nil) then
  begin
    FChanging := True;
    try
      FDataLink.DataSet.GotoBookmark({$IFDEF RTL200_UP}TBookmark{$ENDIF RTL200_UP}(Pointer(Items.Objects[ItemIndex])));
    finally
      FChanging := False;
    end;
  end;
  inherited Select;
end;

procedure TJvDBCustomSearchComboBox.Refresh;
begin
  ReadList;
end;

procedure TJvDBCustomSearchComboBox.Scroll(Distance: Integer);
begin
  if FChanging then
    Exit;
  FChanging := True;
  try
    ItemIndex := ItemIndex + Distance;
  finally
    FChanging := False;
  end;
end;

procedure TJvDBCustomSearchComboBox.ReadList;
var
  Bmrk: {$IFDEF RTL200_UP}TBookmark{$ELSE}TBookmarkStr{$ENDIF RTL200_UP};
  N, CurIndex: Integer;
  DataSet: TDataSet;
  Field: TField;
begin
  if (FDataLink.DataField = nil) or (FDataLink.DataSet = nil) or not FDataLink.DataSet.Active then
    Exit;
  Items.BeginUpdate;
  try
    DataSet := FDataLink.DataSet;
    ClearList;
    CurIndex := -1;
    Bmrk := DataSet.Bookmark;
    DataSet.DisableControls;
    N := 0;
    try
      Field := DataSet.FieldByName(FDataLink.FDataFieldName);
      DataSet.First;
      while not DataSet.Eof do
      begin
        FBookmarks.Add(DataSet.GetBookmark);
        Items.AddObject(Field.DisplayText, TObject(FBookmarks[N]));
        if {$IFDEF RTL200_UP}DataSet.CompareBookmarks(DataSet.Bookmark, Bmrk) = 0{$ELSE}DataSet.Bookmark = Bmrk{$ENDIF RTL200} then
          CurIndex := N;
        Inc(N);
        DataSet.Next;
      end;
      DataSet.Bookmark := Bmrk;
    finally
      DataSet.EnableControls;
    end;
  finally
    Items.EndUpdate;
  end;
  ItemIndex := CurIndex;
end;

procedure TJvDBCustomSearchComboBox.ClearList;
var
  I: Integer;
begin
  if Assigned(FDataLink.DataSet) then
    for I := 0 to FBookmarks.Count - 1 do
      FDataLink.DataSet.FreeBookmark(FBookmarks[I]);
  FBookmarks.Clear;
  if HandleAllocated and not (csDestroying in ComponentState) then // TCustomComboBox uses SendMessage(Handle, ...) to clear items
    Items.Clear;
end;

function TJvDBCustomSearchComboBox.GetResult: Variant;
begin
  Result := Null;
  if Assigned(FDataLink.DataSet) and (DataResult <> '') then
    Result := FDataLink.DataSet.Lookup(DataField, Text, DataResult);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
