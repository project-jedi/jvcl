{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimLstEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTimerListForm;

interface

uses
  Windows,
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Grids, Menus,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, DesignWindows, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf, DsgnWnds,
  {$ENDIF}
  JvJVCLUtils, JvFormPlacement, JvTimerList, JvComponent;

type
  TJvTimerItemsEditor = class(TDesignWindow)
    BtnPanel: TPanel;
    ClientPanel: TPanel;
    NewBtn: TButton;
    DeleteBtn: TButton;
    DrawGrid: TDrawGrid;
    PopupMenu: TPopupMenu;
    CutMenu: TMenuItem;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    FormStorage: TJvFormStorage;
    DeleteMenu: TMenuItem;
    N1: TMenuItem;
    NewMenu: TMenuItem;
    ClearBtn: TButton;
    Panel1: TPanel;
    CloseBtn: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGridSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FTimersCollection: TJvTimerList;
    function GetForm: TCustomForm;
    function CheckCollection: Boolean;
    function ItemByRow(Row: Integer): TJvTimerEvent;
    procedure SelectItem(Item: TJvTimerEvent);
    procedure UpdateData;
    procedure SetTimersCollection(Value: TJvTimerList);
    procedure Copy;
    procedure Cut;
    procedure Paste;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
  public
    {$IFDEF COMPILER6_UP}
    function EditAction(Action: TEditAction): Boolean; override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    {$ELSE}
    procedure EditAction(Action: TEditAction); override;
    procedure FormModified; override;
    procedure FormClosed(Form: TCustomForm); override;
    {$ENDIF}
    function GetEditState: TEditState; override;
    {$IFDEF COMPILER6_UP}
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    {$ELSE}
    procedure ComponentDeleted(Component: IPersistent); override;
    {$ENDIF}
    property TimersCollection: TJvTimerList read FTimersCollection
      write SetTimersCollection;
    property OwnerForm: TCustomForm read GetForm;
  end;

  TJvTimersItemListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvTimersCollectionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  Consts,
  JvConsts, JvTypes, JvxDConst;

{$R *.DFM}

{$D-}

{$IFDEF COMPILER6_UP}
type
  TDesigner = DesignIntf.IDesigner;
  TFormDesigner = DesignIntf.IDesigner;
{$ELSE}
type
  TDesigner = IDesigner;
  TFormDesigner = IFormDesigner;
{$ENDIF}

function FindEditor(ATimersCollection: TJvTimerList): TJvTimerItemsEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TJvTimerItemsEditor then
      if TJvTimerItemsEditor(Screen.Forms[I]).TimersCollection = ATimersCollection then
      begin
        Result := TJvTimerItemsEditor(Screen.Forms[I]);
        Break;
      end;
end;

procedure ShowItemsEditor(Designer: TDesigner;
  ATimersCollection: TJvTimerList);
var
  Editor: TJvTimerItemsEditor;
begin
  if ATimersCollection = nil then
    Exit;
  Editor := FindEditor(ATimersCollection);
  if Editor = nil then
  begin
    Editor := TJvTimerItemsEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.TimersCollection := ATimersCollection;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end
  else
  begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  end;
end;

//=== TJvTimersItemListProperty ==============================================

function TJvTimersItemListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvTimersItemListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := srNone
  else
    FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvTimersItemListProperty.Edit;
begin
  ShowItemsEditor(Designer, TJvTimerList(GetComponent(0)));
end;

//=== TJvTimersCollectionEditor ==============================================

procedure TJvTimersCollectionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowItemsEditor(Designer, TJvTimerList(Component));
  end;
end;

function TJvTimersCollectionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := srTimerDesigner;
  end;
end;

function TJvTimersCollectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TJvTimerItemsEditor ====================================================

procedure TJvTimerItemsEditor.SetTimersCollection(Value: TJvTimerList);
begin
  if FTimersCollection <> Value then
  begin
    FTimersCollection := Value;
    UpdateData;
  end;
end;

function TJvTimerItemsEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
begin
  if Component <> nil then
    Temp := Component.ClassName
  else
    Temp := TJvTimerEvent.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  Result := Designer.UniqueName(Temp);
end;

function TJvTimerItemsEditor.GetEditState: TEditState;
begin
  Result := [];
  if DeleteBtn.Enabled then
    Result := [esCanDelete, esCanCut, esCanCopy];
  if ClipboardComponents then
    Include(Result, esCanPaste);
end;

{$IFDEF COMPILER6_UP}
procedure TJvTimerItemsEditor.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
{$ELSE}
procedure TJvTimerItemsEditor.FormClosed(Form: TCustomForm);
{$ENDIF}
begin
  if {$IFDEF COMPILER6_UP} ADesigner.Root {$ELSE} Form {$ENDIF} = OwnerForm then
    Free;
end;

{$IFDEF COMPILER6_UP}
procedure TJvTimerItemsEditor.ItemsModified(const Designer: IDesigner);
{$ELSE}
procedure TJvTimerItemsEditor.FormModified;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) then
    UpdateData;
end;

procedure TJvTimerItemsEditor.Activated;
begin
  SelectItem(ItemByRow(DrawGrid.Row - 1));
end;

procedure TJvTimerItemsEditor.UpdateData;
var
  Empty: Boolean;
begin
  if CheckCollection then
  begin
    Caption := Format(srTimerEvents, [TimersCollection.Name]);
    Empty := TimersCollection.Events.Count = 0;
  end
  else
    Empty := True;
  if Empty then
  begin
    DrawGrid.RowCount := 2;
    SelectItem(nil);
  end
  else
    DrawGrid.RowCount := TimersCollection.Events.Count + 1;
  DeleteBtn.Enabled := not Empty;
  ClearBtn.Enabled := not Empty;
  DeleteMenu.Enabled := not Empty;
  CopyMenu.Enabled := not Empty;
  CutMenu.Enabled := not Empty;
  PasteMenu.Enabled := ClipboardComponents;
  DrawGrid.Invalidate;
end;

function TJvTimerItemsEditor.GetForm: TCustomForm;
begin
  Result := {$IFDEF COMPILER6_UP} TCustomForm(Designer.Root) {$ELSE} Designer.Form {$ENDIF};
end;

procedure TJvTimerItemsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TJvTimerItemsEditor.CheckCollection: Boolean;
begin
  Result := (TimersCollection <> nil) and (TimersCollection.Owner <> nil)
    and ({$IFDEF COMPILER6_UP} Designer.Root {$ELSE} Designer.Form {$ENDIF} <> nil);
end;

{$IFDEF COMPILER6_UP}
type
  TDesignerSelectionList = IDesignerSelections;
{$ENDIF}

procedure TJvTimerItemsEditor.SelectItem(Item: TJvTimerEvent);
var
  FComponents: TDesignerSelectionList;
begin
  if CheckCollection and Active then
  begin
    FComponents := {$IFDEF COMPILER6_UP} TDesignerSelections {$ELSE} TDesignerSelectionList {$ENDIF}.Create;
    if Item <> nil then
      FComponents.Add(Item)
    else
      FComponents.Add(TimersCollection);
    SetSelection(FComponents);
  end;
end;

function TJvTimerItemsEditor.ItemByRow(Row: Integer): TJvTimerEvent;
begin
  Result := nil;
  if CheckCollection and (Row >= 0) and
    (Row < TimersCollection.Events.Count) then
  begin
    Result := TJvTimerEvent(TimersCollection.Events[Row]);
  end;
end;

{$IFDEF COMPILER6_UP}
procedure TJvTimerItemsEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = TimersCollection then
  begin
{$ELSE}
procedure TJvTimerItemsEditor.ComponentDeleted(Component: IPersistent);
begin
  if ExtractPersistent(Component) = TimersCollection then
  begin
{$ENDIF}
    TimersCollection := nil;
    Close;
  end;
end;

procedure TJvTimerItemsEditor.DrawGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Item: TJvTimerEvent;
begin
  CellText := '';
  if gdFixed in State then
    CellText := 'Item name'
  else
  begin
    Item := ItemByRow(Row - 1);
    if Item <> nil then
      CellText := Item.DisplayName;
  end;
  DrawCellText(DrawGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenterJustify);
end;

procedure TJvTimerItemsEditor.DrawGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectItem(ItemByRow(Row - 1));
end;

procedure TJvTimerItemsEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvTimerItemsEditor.DeleteClick(Sender: TObject);
var
  Item: TJvTimerEvent;
begin
  Item := ItemByRow(DrawGrid.Row - 1);
  if Item <> nil then
  begin
    {$IFDEF COMPILER6_UP} TCustomForm(Designer.Root).Designer {$ELSE} Designer {$ENDIF}.ValidateRename(Item.TimerList, Item.DisplayName, '');
    TimersCollection.Delete(Item.Handle);
    if TimersCollection.Count > 0 then
    begin
      Item := ItemByRow(DrawGrid.Row - 1);
      SelectItem(Item);
    end
    else
      SelectItem(nil);
    UpdateData;
    Designer.Modified;
  end;
end;

procedure TJvTimerItemsEditor.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_RETURN:
        if ItemByRow(DrawGrid.Row - 1) <> nil then
          ActivateInspector(#0);
      VK_DELETE:
        DeleteClick(nil);
    end;
end;

procedure TJvTimerItemsEditor.FormCreate(Sender: TObject);
begin
  TimersCollection := nil;
  if NewStyleControls then
    Font.Style := [];
  with FormStorage do
  begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
end;

procedure TJvTimerItemsEditor.FormResize(Sender: TObject);
begin
  with DrawGrid do
    ColWidths[0] := ClientWidth;
end;

{$IFDEF COMPILER6_UP}
function TJvTimerItemsEditor.EditAction(Action: TEditAction): Boolean;
begin
  Result := True;
{$ELSE}
procedure TJvTimerItemsEditor.EditAction(Action: TEditAction);
begin
{$ENDIF}
  case Action of
    eaCut:
      Cut;
    eaCopy:
      Copy;
    eaPaste:
      Paste;
    eaDelete:
      DeleteClick(Self);
  end;
end;

procedure TJvTimerItemsEditor.NewClick(Sender: TObject);
var
  I: Integer;
  Item: TJvTimerEvent;
begin
  Item := TJvTimerEvent.Create(TimersCollection.Owner);
  if Item <> nil then
  try
    Item.Name := UniqueName(Item);
    with TimersCollection do
      I := ItemIndexByHandle(AddItem(Item));
    SelectItem(Item);
    Designer.Modified;
    ActivateInspector(#0);
    DrawGrid.Row := I + 1;
  except
    Item.Free;
    raise;
  end
  else
    raise EJVCLException.Create(srEventNotCreate);
end;

procedure TJvTimerItemsEditor.CutClick(Sender: TObject);
begin
  Cut;
  UpdateData;
end;

procedure TJvTimerItemsEditor.CopyClick(Sender: TObject);
begin
  Copy;
  UpdateData;
end;

procedure TJvTimerItemsEditor.PasteClick(Sender: TObject);
begin
  Paste;
  UpdateData;
end;

procedure TJvTimerItemsEditor.Cut;
begin
  Copy;
  DeleteClick(Self);
end;

procedure TJvTimerItemsEditor.Copy;
var
  CompList: TDesignerSelectionList;
  Item: TJvTimerEvent;
begin
  CompList := {$IFDEF COMPILER6_UP} TDesignerSelections {$ELSE} TDesignerSelectionList {$ENDIF}.Create;
  {$IFNDEF COMPILER6_UP}
  try
  {$ENDIF}
    Item := ItemByRow(DrawGrid.Row - 1);
    if Item <> nil then
    begin
      CompList.Add(Item);
      CopyComponents(OwnerForm, CompList);
    end;
  {$IFNDEF COMPILER6_UP}
  finally
    CompList.Free;
  end;
  {$ENDIF}
end;

procedure TJvTimerItemsEditor.Paste;
var
  CompList: TDesignerSelectionList;
begin
  if CheckCollection then
  begin
    CompList := {$IFDEF COMPILER6_UP} TDesignerSelections {$ELSE} TDesignerSelectionList {$ENDIF}.Create;
    {$IFNDEF COMPILER6_UP}
    try
    {$ENDIF}
      PasteComponents(OwnerForm, TimersCollection, CompList);
      UpdateData;
    {$IFNDEF COMPILER6_UP}
    finally
      CompList.Free;
    end;
    {$ENDIF}
  end;
end;

procedure TJvTimerItemsEditor.ClearBtnClick(Sender: TObject);
var
  Item: TJvTimerEvent;
begin
  while TimersCollection.Events.Count > 0 do
  begin
    Item := TJvTimerEvent(TimersCollection.Events[0]);
    if Item <> nil then
      {$IFDEF COMPILER6_UP} TCustomForm(Designer.Root).Designer {$ELSE} Designer {$ENDIF}.ValidateRename(Item,
        Item.Name, '');
    TimersCollection.Events.Delete(0);
    Item.Free;
  end;
  UpdateData;
end;

end.

