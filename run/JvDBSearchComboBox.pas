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

Last Modified: 2004-02-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Description:
// DB Component to find record with ComboBox
// Free modified and corrected component TDBViewCombo from ???
-----------------------------------------------------------------------------}


{$I jvcl.inc}

unit JvDBSearchComboBox;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Menus, Dialogs, StdCtrls, DB, DBCtrls, JvComboBox;

type
  TJvDBCustomSearchComboBox = class;

  TJvSearchComboBoxLink = class(TDataLink)
  private
   FJvDBSearchComboBox : TJvDBCustomSearchComboBox;
   FDataFieldName : string;
   FDataField : TField;
   FWasEdited : boolean;
   FReading : boolean;
   procedure SetDataFieldName(const Value : string);
   procedure SetDataField;

  protected
   procedure ActiveChanged;override;
   procedure DataSetScrolled(Distance : integer);override;
   procedure DataSetChanged;override;
   procedure EditingChanged;override;
  public
   constructor Create(aJvDBSearchComboBox : TJvDBCustomSearchComboBox);
   property DataField: TField read FDataField;
   property DataFieldName: string read FDataFieldName write SetDataFieldName;
  end;
 
  TJvDBCustomSearchComboBox = class(TJvCustomComboBox)
  private
    FDataLink : TJvSearchComboBoxLink;
    FChanging : boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Scroll(Distance : Integer);
    procedure ReadList;
    procedure ClearList;
    procedure Change;override;
    procedure Notification(Component : TComponent;Operation : TOperation);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    property ItemIndex;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TJvDBSearchComboBox = class(TJvDBCustomSearchComboBox)
  published
    property Align;
    property Anchors;
    property AutoComplete default True;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF COMPILER6_UP}
    property BiDiMode;
    property CharCase;
    property Constraints;
    property Style;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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
    {$IFDEF COMPILER6_UP}
    property OnCloseUp;
    {$ENDIF COMPILER6_UP}
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
    {$IFDEF COMPILER6_UP}
    property OnSelect;
    {$ENDIF COMPILER6_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;


implementation

// *****************************************************************************
// TJvSearchComboBoxLink
// *****************************************************************************

constructor TJvSearchComboBoxLink.Create(aJvDBSearchComboBox : TJvDBCustomSearchComboBox);
begin
  inherited Create;
  FJvDBSearchComboBox := aJvDBSearchComboBox;
  FDataFieldName := '';
  FDataField     := nil;
  FWasEdited     := false;
  FReading       := false;
end;

procedure TJvSearchComboBoxLink.SetDataField;
begin
  if DataSource = nil then exit;
  if DataSource.DataSet = nil then exit;
  if not DataSource.DataSet.Active then exit;
  if FDataFieldName = '' then exit;
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
  if FReading or FJvDBSearchComboBox.FChanging then Exit;
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

procedure TJvSearchComboBoxLink.DataSetScrolled(Distance : integer);
begin
  if Distance = 0 then Exit;
  FJvDBSearchComboBox.Scroll(Distance);
end;

procedure TJvSearchComboBoxLink.EditingChanged;
begin
  if Editing then
    FWasEdited := True;
end;

// *****************************************************************************
// TJvDBCustomSearchComboBox
// *****************************************************************************

constructor TJvDBCustomSearchComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TJvSearchComboBoxLink.Create(Self);
  Style := csDropDownList;
  FChanging := False;
end;

destructor TJvDBCustomSearchComboBox.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TJvDBCustomSearchComboBox.Notification(Component : TComponent;Operation : TOperation);
begin
  inherited Notification(Component,Operation);
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

procedure TJvDBCustomSearchComboBox.Change;
begin
  if not FChanging and (ItemIndex <> -1) and
    (FDataLink.DataSet <> nil) and (FDataLink.DataField <> nil) then
  begin
    FChanging := True;
    try
      FDataLink.DataSet.GotoBookmark(Pointer(Items.Objects[ItemIndex]));
    finally
      FChanging := False;
    end;
  end;
  inherited Change;
end;

procedure TJvDBCustomSearchComboBox.Refresh;
begin
  ReadList;
end;

procedure TJvDBCustomSearchComboBox.Scroll(Distance : integer);
begin
  if FChanging then Exit;
  FChanging := True;
  try
    ItemIndex := ItemIndex + Distance;
  finally
    FChanging := False;
  end;  
end;

procedure TJvDBCustomSearchComboBox.ReadList;
var
  Bmrk : TBookmarkStr;
  N, CurIndex : integer;
begin
  if (FDataLink.DataField = nil) or (FDataLink.DataSet = nil) or
     (not FDataLink.DataSet.Active) then Exit;
  ClearList;
  CurIndex := -1;
  with FDataLink.DataSet do
  begin
    Bmrk := Bookmark;
    DisableControls;
    N := 0;
    try
      First;
      while not EOF do
      begin
        Items.AddObject(FieldByName(FDatalink.FDataFieldName).AsString, GetBookmark);
        if Bookmark = Bmrk then CurIndex := N;
        inc(N);
        Next;
      end;
      Bookmark := Bmrk;
    finally
      EnableControls;
    end;
    ItemIndex := CurIndex;
  end;
end;

procedure TJvDBCustomSearchComboBox.ClearList;
var
  I : integer;
begin
  if Assigned(FDataLink.DataSet) then
    for I := 0 to Items.Count - 1 do
    begin
      FDataLink.DataSet.FreeBookmark(Pointer(Items.Objects[I]));
    end;
  Items.Clear;
end;


end.

