{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBIndex.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDEIndex;

interface

uses
  Classes, DB,
  JvExStdCtrls;

type
  TJvIdxDisplayMode = (dmFieldLabels, dmFieldNames, dmIndexName);

  TJvDBIndexCombo = class(TJvExCustomComboBox)
  private
    FDataLink: TDataLink;
    FUpdate: Boolean;
    FNoIndexItem: string;
    FEnableNoIndex: Boolean;
    FChanging: Boolean;
    FDisplayMode: TJvIdxDisplayMode;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetIndexFieldName(var AName: string): Boolean;
    procedure SetNoIndexItem(const Value: string);
    function GetNoIndexItem: string;
    procedure SetEnableNoIndex(Value: Boolean);
    procedure SetDisplayMode(Value: TJvIdxDisplayMode);
    procedure ActiveChanged;
  protected
    procedure EnabledChanged; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FillIndexList(List: TStrings);
    procedure Change; override;
    procedure UpdateList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property NoIndexItem: string read GetNoIndexItem write SetNoIndexItem;
    property EnableNoIndex: Boolean read FEnableNoIndex write SetEnableNoIndex default False;
    property DisplayMode: TJvIdxDisplayMode read FDisplayMode write SetDisplayMode default dmFieldLabels;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Color;
    property DropDownCount;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  SysUtils, StdCtrls, DBTables,
  JvBdeUtils;

//=== { TJvKeyDataLink } =====================================================

type
  TJvKeyDataLink = class(TDataLink)
  private
    FCombo: TJvDBIndexCombo;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ACombo: TJvDBIndexCombo);
    destructor Destroy; override;
  end;

constructor TJvKeyDataLink.Create(ACombo: TJvDBIndexCombo);
begin
  inherited Create;
  FCombo := ACombo;
end;

destructor TJvKeyDataLink.Destroy;
begin
  FCombo := nil;
  inherited Destroy;
end;

procedure TJvKeyDataLink.ActiveChanged;
begin
  if FCombo <> nil then
    FCombo.ActiveChanged;
end;

procedure TJvKeyDataLink.DataSetChanged;
begin
  if FCombo <> nil then
    FCombo.ActiveChanged;
end;

procedure TJvKeyDataLink.DataSetScrolled(Distance: Integer);
begin
  { ignore this data event }
end;

//=== { TJvDBIndexCombo } ====================================================

constructor TJvDBIndexCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TJvKeyDataLink.Create(Self);
  Style := csDropDownList;
  FUpdate := False;
  FNoIndexItem := '';
  FEnableNoIndex := False;
  FDisplayMode := dmFieldLabels;
end;

destructor TJvDBIndexCombo.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TJvDBIndexCombo.SetNoIndexItem(const Value: string);
begin
  if Value <> FNoIndexItem then
  begin
    FNoIndexItem := Value;
    if not (csLoading in ComponentState) then
      ActiveChanged;
  end;
end;

procedure TJvDBIndexCombo.SetEnableNoIndex(Value: Boolean);
begin
  if FEnableNoIndex <> Value then
  begin
    FEnableNoIndex := Value;
    if not (csLoading in ComponentState) then
      ActiveChanged;
  end;
end;

procedure TJvDBIndexCombo.SetDisplayMode(Value: TJvIdxDisplayMode);
begin
  if Value <> FDisplayMode then
  begin
    FDisplayMode := Value;
    if not (csLoading in ComponentState) then
      UpdateList;
  end;
end;

function TJvDBIndexCombo.GetNoIndexItem: string;
begin
  Result := FNoIndexItem;
end;

function TJvDBIndexCombo.GetDataSource: TDataSource;
begin
  if FDataLink <> nil then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TJvDBIndexCombo.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TJvDBIndexCombo.ActiveChanged;
begin
  if not (Enabled and FDataLink.Active and
    FDataLink.DataSet.InheritsFrom(TTable)) then
  begin
    Clear;
    ItemIndex := -1;
  end
  else
    UpdateList;
end;

procedure TJvDBIndexCombo.Loaded;
begin
  inherited Loaded;
  ActiveChanged;
end;

procedure TJvDBIndexCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBIndexCombo.EnabledChanged;
begin
  inherited EnabledChanged;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

function TJvDBIndexCombo.GetIndexFieldName(var AName: string): Boolean;
begin
  Result := True;
  if ItemIndex >= 0 then
  begin
    if EnableNoIndex and (Items[ItemIndex] = NoIndexItem) then
      AName := ''
    else
    begin
      AName := TIndexDef(Items.Objects[ItemIndex]).Fields;
      if AName = '' then
      begin
        AName := TIndexDef(Items.Objects[ItemIndex]).Name;
        Result := False;
      end;
    end;
  end
  else
    AName := '';
end;

procedure TJvDBIndexCombo.FillIndexList(List: TStrings);
var
  AFld: string;
  Pos: Integer;
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    if not FDataLink.Active then
      Exit;
    with FDataLink.DataSet as TTable do
    begin
      for I := 0 to IndexDefs.Count - 1 do
        with IndexDefs[I] do
          if not (ixExpression in Options) then
          begin
            if FDisplayMode = dmIndexName then
              AFld := Name
            else
            begin
              AFld := '';
              Pos := 1;
              while Pos <= Length(Fields) do
              begin
                if AFld <> '' then
                  AFld := AFld + '; ';
                case FDisplayMode of
                  dmFieldLabels:
                    AFld := AFld + FieldByName(ExtractFieldName(Fields, Pos)).DisplayLabel;
                  dmFieldNames:
                    AFld := AFld + FieldByName(ExtractFieldName(Fields, Pos)).FieldName;
                end;
              end;
            end;
            if List.IndexOf(AFld) < 0 then
              List.AddObject(AFld, IndexDefs[I]);
          end;
    end;
    if EnableNoIndex then
      if List.IndexOf(NoIndexItem) < 0 then
        List.AddObject(NoIndexItem, nil);
  finally
    List.EndUpdate;
  end;
end;

procedure TJvDBIndexCombo.Change;
var
  ABookmark: TBookmark;
  AName: string;
begin
  if Enabled and FDataLink.Active and not FChanging and
    FDataLink.DataSet.InheritsFrom(TTable) and
    not (csLoading in ComponentState) then
  begin
    ABookmark := nil;
    with FDataLink.DataSet as TTable do
    begin
      if Database.IsSQLBased then
        ABookmark := GetBookmark;
      try
        if GetIndexFieldName(AName) then
        begin
          IndexFieldNames := AName;
          if (AName = '') and (IndexDefs.Count > 0) then
            IndexName := '';
        end
        else
        begin
          if AName = '' then
            IndexFieldNames := '';
          IndexName := AName;
        end;
        if (ABookmark <> nil) then
          SetToBookmark(TTable(Self.FDataLink.DataSet), ABookmark);
      finally
        if ABookmark <> nil then
          FreeBookmark(ABookmark);
      end;
    end;
  end;
  inherited Change;
end;

procedure TJvDBIndexCombo.UpdateList;

  function FindIndex(Table: TTable): Integer;
  var
    I: Integer;
    IdxFields: string;
  begin
    Result := -1;
    IdxFields := '';
    if Table.IndexFieldNames <> '' then
      for I := 0 to Table.IndexFieldCount - 1 do
      begin
        if IdxFields <> '' then
          IdxFields := IdxFields + ';';
        IdxFields := IdxFields + Table.IndexFields[I].FieldName;
      end;
    for I := 0 to Items.Count - 1 do
    begin
      if (Items.Objects[I] <> nil) and
        (((IdxFields <> '') and
        (AnsiCompareText(TIndexDef(Items.Objects[I]).Fields, IdxFields) = 0)) or
        ((Table.IndexName <> '') and
        (AnsiCompareText(TIndexDef(Items.Objects[I]).Name, Table.IndexName) = 0))) then
      begin
        Result := I;
        Exit;
      end;
    end;
    if EnableNoIndex and FDataLink.Active then
      if (Table.IndexFieldNames = '') and (Table.IndexName = '') then
        Result := Items.IndexOf(NoIndexItem);
  end;

begin
  if Enabled and FDataLink.Active then
  try
    Items.BeginUpdate;
    try
      if FDataLink.DataSet.InheritsFrom(TTable) then
      begin
        TTable(FDataLink.DataSet).IndexDefs.Update;
        FillIndexList(Items);
        ItemIndex := FindIndex(TTable(FDataLink.DataSet));
        FChanging := True;
      end
      else
        Items.Clear;
    finally
      Items.EndUpdate;
    end;
  finally
    FChanging := False;
  end;
end;

end.

