{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBTreeView.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : JvDBTreeView
description : db-aware TreeView

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ history
 (JVCL Library versions) :
  1.20:
    - first release;
  1.61:
    - support for non-bde components,
      by Yakovlev Vacheslav (jwe@belkozin.com)
}

unit JvDBTreeView;

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, Graphics,
  CommCtrl, ComCtrls, ExtCtrls, Db;

type
  TJvDBTreeNode = class;
  TJvDBTreeViewDataLink = class;
  TFieldTypes = set of TFieldType;
  TGetDetailValue = function(const AMasterValue: Variant; var DetailValue: Variant): Boolean;

  TCustomJvDBTreeView = class(TCustomTreeView)
  private
    FDataLink: TJvDBTreeViewDataLink;
    FMasterField: string;
    FDetailField: string;
    FItemField: string;
    FIconField: string;
    FStartMasterValue: Variant;
    FGetDetailValue: TGetDetailValue;
    FUseFilter: Boolean;
    FSelectedIndex: Integer;
    {Update flags}
    FUpdateLock: Byte;
    InTreeUpdate: Boolean;
    InDataScrolled: Boolean;
    InAddChild: Boolean;
    InDelete: Boolean;
    Sel: TTreeNode;
    OldRecCount: Integer;
    FPersistentNode: Boolean;
    FMirror: Boolean;
    {**** Drag'n'Drop ****}
    YDragPos: Integer;
    TimerDnD: TTimer;
    procedure InternalDataChanged;
    procedure InternalDataScrolled;
    procedure InternalRecordChanged(Field: TField);
    procedure SetMasterField(Value: string);
    procedure SetDetailField(Value: string);
    procedure SetItemField(Value: string);
    procedure SetIconField(Value: string);
    function GetStartMasterValue: string;
    procedure SetStartMasterValue(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    {$IFDEF COMPILER4_UP}
    procedure SetMirror(Value: Boolean);
    {$ENDIF COMPILER4_UP}
    {**** Drag'n'Drop ****}
    procedure TimerDnDTimer(Sender: TObject);
  protected
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
  protected
    procedure Warning(Msg: string);
    procedure HideEditor;
    function ValidDataSet: Boolean;
    procedure CheckDataSet;
    function ValidField(FieldName: string; AllowFieldTypes: TFieldTypes): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure Change(Node: TTreeNode); override;
    { data }
    procedure DataChanged; dynamic;
    procedure DataScrolled; dynamic;
    procedure Change2(Node: TTreeNode); dynamic;
    procedure RecordChanged(Field: TField); dynamic;

    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Collapse(Node: TTreeNode); override;
    function CreateNode: TTreeNode; override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;
    procedure MoveTo(Source, Destination: TJvDBTreeNode; Mode: TNodeAttachMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure RefreshChild(ANode: TJvDBTreeNode);
    procedure UpdateTree;
    procedure LinkActive(Value: Boolean); virtual;
    procedure UpdateLock;
    procedure UpdateUnLock(const AUpdateTree: Boolean);
    function UpdateLocked: Boolean;
    function AddChildNode(const Node: TTreeNode; const Select: Boolean): TJvDBTreeNode;
    procedure DeleteNode(Node: TTreeNode);
    function FindNextNode(const Node: TTreeNode): TTreeNode;
    function FindNode(AMasterValue: Variant): TJvDBTreeNode;
    function SelectNode(AMasterValue: Variant): TTreeNode;

    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataLink: TJvDBTreeViewDataLink read FDataLink;
    property MasterField: string read FMasterField write SetMasterField;
    // alias for MasterField
    property ParentField: string read FMasterField write SetMasterField;
    property DetailField: string read FDetailField write SetDetailField;
    // alias for DetailField
    property KeyField: string read FDetailField write SetDetailField;

    property ItemField: string read FItemField write SetItemField;
    property IconField: string read FIconField write SetIconField;
    property StartMasterValue: string read GetStartMasterValue write SetStartMasterValue;
    property GetDetailValue: TGetDetailValue read FGetDetailValue write FGetDetailValue;
    property PersistentNode: Boolean read FPersistentNode write FPersistentNode;
    property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex default 1;
    property UseFilter: Boolean read FUseFilter write FUseFilter;
    {$IFDEF COMPILER4_UP}
    property Mirror: Boolean read FMirror write SetMirror;
    {$ENDIF COMPILER4_UP}
    property Items;
  end;

  TJvDBTreeViewDataLink = class(TDataLink)
  private
    FTreeView: TCustomJvDBTreeView;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ATreeView: TCustomJvDBTreeView);
    destructor Destroy; override;
  end;

  TJvDBTreeNode = class(TTreeNode)
  private
    FMasterValue: Variant;
  public
    procedure SetMasterValue(AValue: Variant);
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
      {$IFNDEF COMPILER2} override; {$ENDIF}
    property MasterValue: Variant read FMasterValue;
  end;

  TJvDBTreeView = class(TCustomJvDBTreeView)
  published
    property DataSource;
    property MasterField;
    property DetailField;
    property IconField;
    property ItemField;
    property StartMasterValue;
    property UseFilter;
    property PersistentNode;
    property SelectedIndex;
    property BorderStyle;
    property DragCursor;
    property ShowButtons;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    {$IFDEF COMPILER3_UP}
    property RightClickSelect;
    {$ENDIF COMPILER3_UP}
    property DragMode;
    property HideSelection;
    property Indent;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCompare;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor default False;
    property ParentCtl3D;
    property Ctl3D;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
    property StateImages;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderWidth;
    property ChangeDelay;
    property Constraints;
    property DragKind;
    property HotTrack;
    property ParentBiDiMode;
    property RowSelect;
    property ToolTips;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnEndDock;
    property OnStartDock;
    property Mirror;
    {$ENDIF COMPILER4_UP}
  end;

  EJvDBTreeViewError = class(ETreeViewError);

implementation

uses
  JvDBConst;

{$IFNDEF COMPILER4_UP}
type
  LongWord = Longint;
{$ENDIF}

// (rom) moved to implementation and removed type
// (rom) never rely on assignable consts
const
  DnDScrollArea = 15;
  DnDInterval = 200;

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if V = Null then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
end;

{$IFDEF COMPILER4_UP}
procedure MirrorControl(Control: TWinControl; RightToLeft: Boolean);
var
  OldLong: Longword;
begin
  OldLong := GetWindowLong(Control.Handle, GWL_EXSTYLE);
  if RightToLeft then
  begin
    Control.BiDiMode := bdLeftToRight;
    SetWindowLong(Control.Handle, GWL_EXSTYLE, OldLong or $00400000);
  end
  else
    SetWindowLong(Control.Handle, GWL_EXSTYLE, OldLong and not $00400000);
  Control.Repaint;
end;
{$ENDIF COMPILER4_UP}

//=== TJvDBTreeViewDataLink ==================================================

constructor TJvDBTreeViewDataLink.Create(ATreeView: TCustomJvDBTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
end;

destructor TJvDBTreeViewDataLink.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDBTreeViewDataLink.ActiveChanged;
begin
  FTreeView.LinkActive(Active);
end;

procedure TJvDBTreeViewDataLink.RecordChanged(Field: TField);
begin
  FTreeView.InternalRecordChanged(Field);
end;

procedure TJvDBTreeViewDataLink.DataSetChanged;
begin
  FTreeView.InternalDataChanged;
end;

procedure TJvDBTreeViewDataLink.DataSetScrolled(Distance: Integer);
begin
  FTreeView.InternalDataScrolled;
end;

//=== TJvDBTreeNode ==========================================================

procedure TJvDBTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
var
  PersistNode: Boolean;
  TV: TJvDBTreeView;
begin
  TV := (TreeView as TJvDBTreeView);
  PersistNode := TV.FPersistentNode;
  TV.MoveTo(Self as TJvDBTreeNode, Destination as TJvDBTreeNode, Mode);
  TV.FPersistentNode := True;
  if Destination.HasChildren and (Destination.Count = 0) then
    Free
  else
    inherited MoveTo(Destination, Mode);
  {$IFDEF COMPILER2}
  Destination.HasChildren := True;
  {$ENDIF}
  TV.FPersistentNode := PersistNode;
end;

procedure TJvDBTreeNode.SetMasterValue(AValue: Variant);
begin
  FMasterValue := AValue;
end;

//=== TCustomJvDBTreeView ====================================================

constructor TCustomJvDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TJvDBTreeViewDataLink.Create(Self);
  TimerDnD := TTimer.Create(Self);
  TimerDnD.Enabled := False;
  TimerDnD.Interval := DnDInterval;
  TimerDnD.OnTimer := TimerDnDTimer;
  FStartMasterValue := Null;
  FSelectedIndex := 1;
end;

destructor TCustomJvDBTreeView.Destroy;
begin
  FDataLink.Free;
  TimerDnD.Free;
  inherited Destroy;
end;

procedure TCustomJvDBTreeView.CheckDataSet;
begin
  if not ValidDataSet then
    raise EJvDBTreeViewError.Create(SDataSetNotActive);
end;

procedure TCustomJvDBTreeView.Warning(Msg: string);
begin
  MessageDlg(Name + ': ' + Msg, mtWarning, [mbOk], 0);
end;

function TCustomJvDBTreeView.ValidField(FieldName: string; AllowFieldTypes: TFieldTypes): Boolean;
var
  AField: TField;
begin
  Result := (csLoading in ComponentState) or
    (FDataLink.DataSet = nil) or not FDataLink.DataSet.Active;
  if not Result and (Length(FieldName) > 0) then
  begin
    AField := FDataLink.DataSet.FindField(FieldName); { no exceptions }
    Result := (AField <> nil) and (AField.DataType in AllowFieldTypes);
  end;
end;

procedure TCustomJvDBTreeView.SetMasterField(Value: string);
begin
  if ValidField(Value, [ftSmallInt, ftInteger, ftAutoInc, ftWord, ftString]) then
  begin
    FMasterField := Value;
    RefreshChild(nil);
  end
  else
    Warning(SMasterFieldError);
end;

procedure TCustomJvDBTreeView.SetDetailField(Value: string);
begin
  if ValidField(Value, [ftSmallInt, ftInteger, ftAutoInc, ftWord, ftString]) then
  begin
    FDetailField := Value;
    RefreshChild(nil);
  end
  else
    Warning(SDetailFieldError);
end;

procedure TCustomJvDBTreeView.SetItemField(Value: string);
begin
  if ValidField(Value, [ftString, ftMemo, ftSmallInt, ftInteger, ftAutoInc,
    ftWord, ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime]) then
  begin
    FItemField := Value;
    RefreshChild(nil);
  end
  else
    Warning(SItemFieldError);
end;

procedure TCustomJvDBTreeView.SetIconField(Value: string);
begin
  if ValidField(Value, [ftSmallInt, ftAutoInc, ftInteger, ftWord]) then
  begin
    FIconField := Value;
    RefreshChild(nil);
  end
  else
    Warning(SIconFieldError);
end;

function TCustomJvDBTreeView.GetStartMasterValue: string;
begin
  if FStartMasterValue = Null then
    Result := ''
  else
    Result := FStartMasterValue;
end;

procedure TCustomJvDBTreeView.SetStartMasterValue(Value: string);
begin
  if Length(Value) > 0 then
    FStartMasterValue := Value
  else
    FStartMasterValue := Null;
end;

function TCustomJvDBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomJvDBTreeView.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then
    Exit;
  Items.Clear;
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCustomJvDBTreeView.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TCustomJvDBTreeView.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (FDataLink <> nil) and (Component = DataSource) and (Operation = opRemove) then
    DataSource := nil;
end;

function TCustomJvDBTreeView.CreateNode: TTreeNode;
begin
  Result := TJvDBTreeNode.Create(Items);
end;

procedure TCustomJvDBTreeView.HideEditor;
begin
  if Selected <> nil then
    Selected.EndEdit(True);
end;

function TCustomJvDBTreeView.ValidDataSet: Boolean;
begin
  Result := FDataLink.Active and Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active;
end;

procedure TCustomJvDBTreeView.LinkActive(Value: Boolean);

function AllFieldsValid: Boolean;
  begin
    Result := False;
    if ValidDataSet then
    begin
      if (FMasterField = '') or (FDataLink.DataSet.FindField(FMasterField) = nil) then
      begin
        Warning(SMasterFieldEmpty);
        Exit;
      end;
      if (FDetailField = '') or (FDataLink.DataSet.FindField(FDetailField) = nil) then
      begin
        Warning(SDetailFieldEmpty);
        Exit;
      end;
      if (FItemField = '') or (FDataLink.DataSet.FindField(FItemField) = nil) then
      begin
        Warning(SItemFieldEmpty);
        Exit;
      end;
     { if (FDataLink.DataSet.FindField(FMasterField).DataType <> FDataLink.DataSet.FindField(FDetailField).DataType) then begin
        Warning(SMasterDetailFieldError);
        Exit;
      end; }
      if (FDataLink.DataSet.FindField(FItemField).DataType in
        [ftBytes, ftVarBytes, ftBlob, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary]) then
      begin
        Warning(SItemFieldError);
        Exit;
      end;
      if (FIconField <> '') and not (FDataLink.DataSet.FindField(FIconField).DataType in
        [ftSmallInt, ftInteger, ftWord]) then
      begin
        Warning(SIconFieldError);
        Exit;
      end;
    end;
    Result := True;
  end;
begin
  if not Value then
    HideEditor;
  if not AllFieldsValid then
    Exit;
  //if ( csDesigning in ComponentState ) then Exit;
  if ValidDataSet then
  begin
    RefreshChild(nil);
    OldRecCount := FDataLink.DataSet.RecordCount;
  end
  else
  if FUpdateLock = 0 then
    Items.Clear;
end;

procedure TCustomJvDBTreeView.UpdateLock;
begin
  Inc(FUpdateLock);
end;

procedure TCustomJvDBTreeView.UpdateUnLock(const AUpdateTree: Boolean);
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
  if (FUpdateLock = 0) then
    if AUpdateTree then
      UpdateTree
    else
      OldRecCount := FDataLink.DataSet.RecordCount;
end;

function TCustomJvDBTreeView.UpdateLocked: Boolean;
begin
  Result := FUpdateLock > 0;
end;

procedure TCustomJvDBTreeView.RefreshChild(ANode: TJvDBTreeNode);
var
  ParentValue: Variant;
  BK: TBookmark;
  OldFilter: string;
  OldFiltered: Boolean;
  PV: string;
  I: Integer;
begin
//  CheckDataSet;
  if not ValidDataSet or UpdateLocked then
    Exit;
  Inc(FUpdateLock);
  with FDataLink.DataSet do
  begin
    BK := GetBookmark;
    try
      DisableControls;
      if ANode <> nil then
      begin
        ANode.DeleteChildren;
        ParentValue := ANode.FMasterValue;
      end
      else
      begin
        Items.Clear;
        ParentValue := FStartMasterValue;
      end;
      OldFiltered := False;
      OldFilter := '';
      if FUseFilter then
      begin
        if ParentValue = Null then
          PV := 'Null'
        else
          PV := '''' + Var2Type(ParentValue, varString) + '''';
        OldFilter := Filter;
        OldFiltered := Filtered;
        if Filtered then
          Filter := '(' + OldFilter + ') and (' + FDetailField + '=' + PV + ')'
        else
          Filter := '(' + FDetailField + '=' + PV + ')';
        Filtered := True;
      end;
      try
        First;
        while not Eof do
        begin
          if FUseFilter or
            (((ParentValue = Null) and
            ((Length(FieldByName(FDetailField).AsString) = 0) or
            (Copy(Trim(FieldByName(FDetailField).AsString), 1, 1) = '-'))) or
            (FieldByName(FDetailField).Value = ParentValue)) then
          begin
            with Items.AddChild(ANode, FieldByName(FItemField).Text) as TJvDBTreeNode do
            begin
              FMasterValue := FieldValues[FMasterField];
              if FIconField <> '' then
              begin
                ImageIndex := Var2Type(FieldValues[FIconField], varInteger);
                SelectedIndex := ImageIndex + FSelectedIndex;
              end;
            end;
          end;
          Next;
        end;
      finally
        if FUseFilter then
        begin
          Filtered := OldFiltered;
          Filter := OldFilter;
        end;
      end;
      if ANode = nil then
        for I := 0 to Items.Count - 1 do
          with Items[I] as TJvDBTreeNode do
            HasChildren := Lookup(FDetailField, FMasterValue, FDetailField) <> Null
      else
        for I := 0 to ANode.Count - 1 do
          with ANode[I] as TJvDBTreeNode do
            HasChildren := Lookup(FDetailField, FMasterValue, FDetailField) <> Null
    finally
      try
        GotoBookmark(BK);
        FreeBookmark(BK);
        EnableControls;
      finally
        Dec(FUpdateLock);
      end;
    end;
  end;
end;

function TCustomJvDBTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := inherited CanExpand(Node);
  if Result and (Node.Count = 0) then
    RefreshChild(Node as TJvDBTreeNode);
end;

procedure TCustomJvDBTreeView.Collapse(Node: TTreeNode);
var
  HasChildren: Boolean;
begin
  inherited Collapse(Node);
  if not FPersistentNode then
  begin
    HasChildren := Node.HasChildren;
    Node.DeleteChildren;
    Node.HasChildren := HasChildren;
  end;
end;

function TCustomJvDBTreeView.FindNode(AMasterValue: Variant): TJvDBTreeNode;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    Result := Items[I] as TJvDBTreeNode;
    if Result.FMasterValue = AMasterValue then
      Exit;
  end;
  Result := nil;
end;

function TCustomJvDBTreeView.SelectNode(AMasterValue: Variant): TTreeNode;
var
  V: Variant;
  Node: TJvDBTreeNode;
  Parents: Variant; {varArray}
  I: Integer;

  function GetDetailValue(const AMasterValue: Variant; var DetailValue: Variant): Boolean;
  var
    V: Variant;
  begin
    if Assigned(FGetDetailValue) then
    begin
      Result := FGetDetailValue(AMasterValue, DetailValue);
      if DetailValue = FStartMasterValue then
        raise EJvDBTreeViewError.Create('error value for DetailValue');
    end
    else
    begin
      V := FDataLink.DataSet.Lookup(FMasterField, AMasterValue, FMasterField + ';' + FDetailField);
      Result := ((VarType(V) and varArray) = varArray) and (V[1] <> Null);
      if Result then
      begin
        DetailValue := V[1];
        if DetailValue = FStartMasterValue then
          raise EJvDBTreeViewError.Create('internal error');
      end;
    end;
  end;

begin
  Result := FindNode(AMasterValue);
  if Result = nil then
  try
     // Inc(FUpdateLock);
    Parents := VarArrayCreate([0, 0], varVariant);
    V := AMasterValue;
    I := 0;
    repeat
      if not GetDetailValue(V, V) then
        Exit;
      Node := FindNode(V);
      if Node <> nil then
      begin
        { To open all branches from that found to the necessary [translated] }
        //..
        Node.Expand(False);
        while I > 0 do
        begin
          FindNode(Parents[I]).Expand(False);
          Dec(I);
        end;
        Result := FindNode(AMasterValue);
      end
      else
      begin
        { To add in the array of parents [translated] }
        Inc(I);
        VarArrayRedim(Parents, I);
        Parents[I] := V;
      end;
    until Node <> nil;
  finally
     // Dec(FUpdateLock);
  end;
  if Result <> nil then
    Result.Selected := True;
end;

procedure TCustomJvDBTreeView.UpdateTree;
var
  I: Integer;
  BK: TBookmark;
  AllChecked: Boolean;

  procedure AddRecord;
  var
    Node, ParentNode: TJvDBTreeNode;
  begin
    { If the current record is absent from the tree, but it must be in it, then
      add [translated] }
    Node := FindNode(FDataLink.DataSet[FMasterField]);
    if Node = nil then
    begin
      ParentNode := FindNode(FDataLink.DataSet[FDetailField]);
      if (((ParentNode <> nil) and (not ParentNode.HasChildren or (ParentNode.Count <> 0))) or
        (FDataLink.DataSet[FDetailField] = FStartMasterValue)) then
      begin
        if FDataLink.DataSet[FDetailField] = FStartMasterValue then
          Node := nil
        else
        begin
          Node := FindNode(FDataLink.DataSet[FDetailField]);
          if (Node = nil) or (Node.HasChildren and (Node.Count = 0)) then
            Exit;
        end;
        with FDataLink.DataSet, Items.AddChild(Node, FDataLink.DataSet.FieldByName(FItemField).Text) as TJvDBTreeNode do
        begin
          FMasterValue := FieldValues[FMasterField];
          if FIconField <> '' then
          begin
            ImageIndex := Var2Type(FieldValues[FIconField], varInteger);
            SelectedIndex := ImageIndex + FSelectedIndex;
          end;
          HasChildren := Lookup(FDetailField, FMasterValue, FDetailField) <> Null
        end;
      end;
    end;
  end;

begin
  CheckDataSet;
  if UpdateLocked or (InTreeUpdate) then
    Exit;
  InTreeUpdate := True;
  Items.BeginUpdate;
  try
    with FDataLink.DataSet do
    begin
      BK := GetBookmark;
      DisableControls;
      try
        {*** To delete from a tree the remote/removed records [translated] }
        repeat
          AllChecked := True;
          for I := 0 to Items.Count - 1 do
            if not Locate(FMasterField, (Items[I] as TJvDBTreeNode).FMasterValue, []) then
            begin
              Items[I].Free;
              AllChecked := False;
              Break;
            end
            else
              Items[I].HasChildren := Lookup(FDetailField, (Items[I] as TJvDBTreeNode).FMasterValue, FDetailField) <>
                Null;
        until AllChecked;
       {###}
        {*** To add new [translated]}
        First;
        while not Eof do
        begin
          AddRecord;
          Next;
        end;
       {###}
      finally
        GotoBookmark(BK);
        FreeBookmark(BK);
        EnableControls;
      end;
    end;
  finally
    Items.EndUpdate;
    InTreeUpdate := False;
  end;
end;

procedure TCustomJvDBTreeView.InternalDataChanged;
begin
  if not HandleAllocated or UpdateLocked or InDataScrolled then
    Exit;
//  InDataScrolled := True;
  try
    DataChanged;
  finally
//    InDataScrolled := False;
  end;
end;

procedure TCustomJvDBTreeView.DataChanged;
var
  RecCount: Integer;
begin
  case FDataLink.DataSet.State of
    dsBrowse:
      begin
        RecCount := FDataLink.DataSet.RecordCount;
        if (RecCount = -1) or (RecCount <> OldRecCount) then
          UpdateTree;
        OldRecCount := RecCount;
      end;
    dsInsert:
      OldRecCount := -1; { TQuery don't change RecordCount value after insert new record }
  end;
  Selected := FindNode(FDataLink.DataSet[FMasterField]);
end;

procedure TCustomJvDBTreeView.InternalDataScrolled;
begin
  if not HandleAllocated or UpdateLocked then
    Exit;
  InDataScrolled := True;
  try
    DataScrolled;
  finally
    InDataScrolled := False;
  end;
end;

procedure TCustomJvDBTreeView.DataScrolled;
begin
  Selected := FindNode(FDataLink.DataSet[FMasterField]);
end;

procedure TCustomJvDBTreeView.Change(Node: TTreeNode);
begin
  if ValidDataSet and Assigned(Node) and not InDataScrolled and
    (FUpdateLock = 0) and
    (FDataLink.DataSet.State in [dsBrowse, dsEdit, dsInsert]) then
  begin
    Inc(FUpdateLock);
    try
      Change2(Node);
    finally
      Dec(FUpdateLock);
    end;
  end;
  inherited Change(Node);
end;

procedure TCustomJvDBTreeView.Change2(Node: TTreeNode);
begin
  FDataLink.DataSet.Locate(FMasterField, (Node as TJvDBTreeNode).FMasterValue, []);
end;

procedure TCustomJvDBTreeView.InternalRecordChanged(Field: TField);
begin
  if not (HandleAllocated and ValidDataSet) then
    Exit;
  if (Selected <> nil) and (FUpdateLock = 0) and
    (FDataLink.DataSet.State = dsEdit) then
  begin
    Inc(FUpdateLock);
    try
      RecordChanged(Field);
    finally
      Dec(FUpdateLock);
    end;
  end;
end;

procedure TCustomJvDBTreeView.RecordChanged(Field: TField);
var
  Node: TJvDBTreeNode;
begin
  Selected.Text := FDataLink.DataSet.FieldByName(FItemField).Text;
  with Selected as TJvDBTreeNode do
    if FIconField <> '' then
    begin
      ImageIndex := Var2Type(FDataLink.DataSet[FIconField], varInteger);
      SelectedIndex := ImageIndex + FSelectedIndex;
    end;
 {*** ParentNode changed ?}
  if ((Selected.Parent <> nil) and
    (FDataLink.DataSet[FDetailField] <> (Selected.Parent as TJvDBTreeNode).FMasterValue)) or
    ((Selected.Parent = nil) and
    (FDataLink.DataSet[FDetailField] <> FStartMasterValue)) then
  begin
    Node := FindNode(FDataLink.DataSet[FDetailField]);
    if (FDataLink.DataSet[FDetailField] = FStartMasterValue) or (Node <> nil) then
      (Selected as TJvDBTreeNode).MoveTo(Node, naAddChild)
    else
      Selected.Free;
  end;
  {###}
  {*** MasterValue changed ?}
  if (FDataLink.DataSet[FMasterField] <> (Selected as TJvDBTreeNode).FMasterValue) then
  begin
    with (Selected as TJvDBTreeNode) do
    begin
      FMasterValue := FDataLink.DataSet[FMasterField];
      if FIconField <> '' then
      begin
        ImageIndex := Var2Type(FDataLink.DataSet[FIconField], varInteger);
        SelectedIndex := ImageIndex + FSelectedIndex;
      end;
    end;
    {what have I do with Children ?}
    {if you know, place your code here...}
  end;
  {###}
end;

function TCustomJvDBTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node);
  if FDataLink.DataSet <> nil then
    Result := Result and not FDataLink.ReadOnly;
end;

procedure TCustomJvDBTreeView.Edit(const Item: TTVItem);
begin
  CheckDataSet;
  inherited Edit(Item);
  if Assigned(Selected) then
  begin
    Inc(FUpdateLock);
    try
      if Item.pszText <> nil then
      begin
        if FDataLink.Edit then
          FDataLink.DataSet.FieldByName(FItemField).Text := Item.pszText;
        Change2(Self.Selected); {?}
      end
      else
      begin
        FDataLink.DataSet.Cancel;
        if InAddChild then
        begin
          Self.Selected.Free;
          if Sel <> nil then
            Selected := Sel;
        end;
      end;
    finally
      InAddChild := False;
      Dec(FUpdateLock);
    end;
  end;
end;

function TCustomJvDBTreeView.AddChildNode(const Node: TTreeNode; const Select: Boolean): TJvDBTreeNode;
var
  MV: Variant;
  M: string;
begin
  CheckDataSet;
  if Assigned(Node) then
    MV := (Node as TJvDBTreeNode).FMasterValue
  else
    MV := FStartMasterValue;
  if Assigned(Node) and Node.HasChildren and (Node.Count = 0) then
    RefreshChild(Node as TJvDBTreeNode);
  Inc(FUpdateLock);
  InAddChild := True;
  try
    OldRecCount := FDataLink.DataSet.RecordCount + 1;
    FDataLink.DataSet.Append;
    FDataLink.DataSet[FDetailField] := MV;
    if FDataLink.DataSet.FieldValues[FItemField] = Null then
      M := ''
    else
      M := FDataLink.DataSet.FieldByName(FItemField).Text;
    Result := Items.AddChild(Node, M) as TJvDBTreeNode;
    with Result do
    begin
      FMasterValue := FDataLink.DataSet.FieldValues[FMasterField];
      if FIconField <> '' then
      begin
        ImageIndex := Var2Type(FDataLink.DataSet[FIconField], varInteger);
        SelectedIndex := ImageIndex + FSelectedIndex;
      end;
    end;
    Result.Selected := Select;
    { This line is very necessary, well it(he) does not understand from the first [translated]}
    Result.Selected := Select; 
  finally
    Dec(FUpdateLock);
  end;
end;

procedure TCustomJvDBTreeView.DeleteNode(Node: TTreeNode);
var
  NewSel: TTreeNode;
begin
  CheckDataSet;
  Inc(FUpdateLock);
  InDelete := True;
  try
    NewSel := FindNextNode(Selected);
    FDataLink.DataSet.Delete;
    Selected.Free;
    if NewSel <> nil then
      NewSel.Selected := True;
  finally
    InDelete := False;
    Dec(FUpdateLock);
  end;
end;

function TCustomJvDBTreeView.FindNextNode(const Node: TTreeNode): TTreeNode;
begin
  if (Node <> nil) and (Node.Parent <> nil) then
    if Node.Parent.Count > 1 then
      if Node.Index = Node.Parent.Count - 1 then
        Result := Node.Parent[Node.Index - 1]
      else
        Result := Node.Parent[Node.Index + 1]
    else
      Result := Node.Parent
  else
   { if Items.Count > 1 then
      if Node.Index = Items.Count-1 then
        Result := Items[Node.Index-1] else
        Result := Items[Node.Index+1]
    else}
    Result := nil;
end;

procedure TCustomJvDBTreeView.MoveTo(Source, Destination: TJvDBTreeNode; Mode: TNodeAttachMode);
var
  MV, V: Variant;
begin
  CheckDataSet;
  if FUpdateLock = 0 then
  begin
    Inc(FUpdateLock);
    try
      MV := Source.FMasterValue;
      if FDataLink.DataSet.Locate(FMasterField, MV, []) and FDataLink.Edit then
      begin
        case Mode of
          naAdd:
            if Destination.Parent <> nil then
              V := (Destination.Parent as TJvDBTreeNode).FMasterValue
            else
              V := FStartMasterValue;
          naAddChild:
            V := Destination.FMasterValue;
        else
          raise EJvDBTreeViewError.Create(SMoveToModeError);
        end;
        FDataLink.DataSet[FDetailField] := V;
      end;
    finally
      Dec(FUpdateLock);
    end;
  end;
end;

{******************* Drag'n'Drop ********************}

procedure TCustomJvDBTreeView.TimerDnDTimer(Sender: TObject);
begin
  if YDragPos < DnDScrollArea then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
  if YDragPos > ClientHeight - DnDScrollArea then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TCustomJvDBTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
  HT: THitTests;
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if ValidDataSet and (DragMode = dmAutomatic) and
    not FDataLink.ReadOnly and not Accept then
  begin
    HT := GetHitTestInfoAt(X, Y);
    Node := GetNodeAt(X, Y);
    Accept := (Source = Self) and Assigned(Selected) and
      (Node <> Selected) and Assigned(Node) and
      not Node.HasAsParent(Selected) and
      (HT - [htOnLabel, htOnItem, htOnIcon, htNowhere, htOnIndent, htOnButton] <> HT);
    YDragPos := Y;
    TimerDnD.Enabled := ((Y < DnDScrollArea) or (Y > ClientHeight - DnDScrollArea));
  end;
end;

procedure TCustomJvDBTreeView.DragDrop(Source: TObject; X, Y: Integer);
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
begin
  TimerDnD.Enabled := False;
  inherited DragDrop(Source, X, Y);
  AnItem := GetNodeAt(X, Y);
  if ValidDataSet and (DragMode = dmAutomatic) and Assigned(Selected) and Assigned(AnItem) then
  begin
    HT := GetHitTestInfoAt(X, Y);
    if (HT - [htOnItem, htOnLabel, htOnIcon, htNowhere, htOnIndent, htOnButton] <> HT) then
    begin
      if (HT - [htOnItem, htOnLabel, htOnIcon] <> HT) then
        AttachMode := naAddChild
      else
        AttachMode := naAdd;
      (Selected as TJvDBTreeNode).MoveTo(AnItem, AttachMode);
    end;
  end;
{
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
begin
  if TreeView1.Selected = nil then Exit;
  HT := TreeView1.GetHitTestInfoAt(X, Y);
  AnItem := TreeView1.GetNodeAt(X, Y);
  if (HT - [htOnItem, htOnIcon, htNowhere, htOnIndent] <> HT) then
  begin
    if (htOnItem in HT) or (htOnIcon in HT) then AttachMode := naAddChild
    else if htNowhere in HT then AttachMode := naAdd

    else if htOnIndent in HT then AttachMode := naInsert;
    TreeView1.Selected.MoveTo(AnItem, AttachMode);
  end;
end;
 }
end;

{################### Drag'n'Drop ####################}

procedure TCustomJvDBTreeView.KeyDown(var Key: Word; Shift: TShiftState);

procedure DeleteSelected;
  var
    M: string;
  begin
    if Selected.HasChildren then
      M := SDeleteNode2
    else
      M := SDeleteNode;
    if MessageDlg(Format(M, [Selected.Text]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      DeleteNode(Selected);
  end;
begin
  inherited KeyDown(Key, Shift);
  if not ValidDataSet or (FDataLink.ReadOnly) or ReadOnly then
    Exit;
  case Key of
    VK_DELETE:
      if ([ssCtrl] = Shift) and Assigned(Selected) then
        DeleteSelected;
    VK_INSERT:
      if not IsEditing then
      begin
        Sel := Selected;
        if [ssAlt] = Shift then
          //AddChild
          AddChildNode(Selected, True).EditText
        else
          //Add
          AddChildNode(Selected.Parent, True).EditText;
      end;
    VK_F2:
      if Selected <> nil then
        Selected.EditText;
  end;
end;

{$IFDEF COMPILER4_UP}
procedure TCustomJvDBTreeView.SetMirror(Value: Boolean);
begin
  if Value and SysLocale.MiddleEast and not (csDesigning in ComponentState) then
    MirrorControl(Self, Value);
  FMirror := Value;
end;
{$ENDIF COMPILER4_UP}

end.

