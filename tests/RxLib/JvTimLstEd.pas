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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTimLstEd;

interface


uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} SysUtils,
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grids, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Menus, JvCtrls, JvVCLUtils, JvPlacemnt,
  JvTimerLst, DesignWindows;

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
    FormStorage: TJvFormPlacement;
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
    function EditAction(Action: TEditAction):Boolean; override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    function GetEditState: TEditState; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    property TimersCollection: TJvTimerList read FTimersCollection
      write SetTimersCollection;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TJvTimersItemListProperty }

  TJvTimersItemListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TJvTimersCollectionEditor }

  TJvTimersCollectionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses Consts, {$IFDEF WIN32} JvConst, {$ENDIF} JvLConst, JvDsgn;

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$IFDEF Delphi4_Up}
type
  TDesigner = IDesigner;
  TFormDesigner = IDesigner;
{$ENDIF}

function FindEditor(ATimersCollection: TJvTimerList): TJvTimerItemsEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TJvTimerItemsEditor then begin
      if TJvTimerItemsEditor(Screen.Forms[I]).TimersCollection = ATimersCollection then
      begin
        Result := TJvTimerItemsEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowItemsEditor(Designer: TDesigner;
  ATimersCollection: TJvTimerList);
var
  Editor: TJvTimerItemsEditor;
begin
  if ATimersCollection = nil then Exit;
  Editor := FindEditor(ATimersCollection);
  if Editor = nil then begin
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
  else begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  end;
end;

{ TJvTimersItemListProperty }

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
    Result := ResStr(srNone)
  else FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvTimersItemListProperty.Edit;
begin
  ShowItemsEditor(Designer, TJvTimerList(GetComponent(0)));
end;

{ TJvTimersCollectionEditor }

procedure TJvTimersCollectionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowItemsEditor(Designer, TJvTimerList(Component));
  end;
end;

function TJvTimersCollectionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srTimerDesigner);
  end;
end;

function TJvTimersCollectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TJvTimerItemsEditor }

procedure TJvTimerItemsEditor.SetTimersCollection(Value: TJvTimerList);
begin
  if FTimersCollection <> Value then begin
    FTimersCollection := Value;
    UpdateData;
  end;
end;

function TJvTimerItemsEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
{$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
{$ENDIF}
begin
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TJvTimerEvent.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
{$IFDEF WIN32}
  Result := Designer.UniqueName(Temp);
{$ELSE}
  I := 1;
  repeat
    Result := Temp + IntToStr(I);
    Comp := OwnerForm.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Component);
{$ENDIF}
end;

function TJvTimerItemsEditor.GetEditState: TEditState;
begin
  Result := [];
  if DeleteBtn.Enabled then Result := [esCanDelete, esCanCut, esCanCopy];
  if ClipboardComponents then Include(Result, esCanPaste);
end;

procedure TJvTimerItemsEditor.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if Designer.Root = OwnerForm then Free;
end;

procedure TJvTimerItemsEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TJvTimerItemsEditor.Activated;
begin
  SelectItem(ItemByRow(DrawGrid.Row - 1));
end;

procedure TJvTimerItemsEditor.UpdateData;
var
  Empty: Boolean;
begin
  if CheckCollection then begin
    Caption := Format(LoadStr(srTimerEvents), [TimersCollection.Name]);
    Empty := TimersCollection.Count = 0;
  end
  else Empty := True;
  if Empty then begin
    DrawGrid.RowCount := 2;
    SelectItem(nil);
  end
  else DrawGrid.RowCount := TimersCollection.Count + 1;
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
  Result := GetParentForm(ClientPanel); //Designer.Form;
end;

procedure TJvTimerItemsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TJvTimerItemsEditor.CheckCollection: Boolean;
begin
  Result := (TimersCollection <> nil) and (TimersCollection.Owner <> nil)
    and (Designer.Root <> nil);
end;

procedure TJvTimerItemsEditor.SelectItem(Item: TJvTimerEvent);
var
  FComponents: IDesignerSelections;
begin
  if CheckCollection and Active then begin
    FComponents := CreateSelectionList;
    if Item <> nil then FComponents.Add(Item)
    else FComponents.Add(TimersCollection);
    SetSelection(FComponents);
  end;
end;

function TJvTimerItemsEditor.ItemByRow(Row: Integer): TJvTimerEvent;
begin
  Result := nil;
  if CheckCollection and (Row >= 0) and
    (Row < TimersCollection.Count) then
  begin
    Result := TJvTimerEvent(TimersCollection.Events[Row]);
  end;
end;

procedure TJvTimerItemsEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = TimersCollection then begin
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
  if gdFixed in State then CellText := 'Item name'
  else begin
    Item := ItemByRow(Row - 1);
    if Item <> nil then CellText := Item.Name;
  end;
  DrawCellText(DrawGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
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
  if Item <> nil then begin
    Self.ValidateRename(Item, Item.Name, '');
    TimersCollection.Delete(Item.Handle);
    if TimersCollection.Count > 0 then begin
      Item := ItemByRow(DrawGrid.Row - 1);
      SelectItem(Item);
    end
    else SelectItem(nil);
    UpdateData;
    Designer.Modified;
  end;
end;

procedure TJvTimerItemsEditor.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_RETURN: if ItemByRow(DrawGrid.Row - 1) <> nil then ActivateInspector(#0);
      VK_DELETE: DeleteClick(nil);
    end;
end;

procedure TJvTimerItemsEditor.FormCreate(Sender: TObject);
begin
  TimersCollection := nil;
  if NewStyleControls then Font.Style := [];
{$IFDEF WIN32}
  with FormStorage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ENDIF}
end;

procedure TJvTimerItemsEditor.FormResize(Sender: TObject);
begin
  with DrawGrid do ColWidths[0] := ClientWidth;
end;

function TJvTimerItemsEditor.EditAction(Action: TEditAction) : Boolean;
begin
  Result := True;
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: DeleteClick(Self);
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
  else raise Exception.CreateRes(srEventNotCreate);
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
  CompList: IDesignerSelections;
  Item: TJvTimerEvent;
begin
  CompList := CreateSelectionList;
  try
    Item := ItemByRow(DrawGrid.Row - 1);
    if Item <> nil then begin
      CompList.Add(Item);
      CopyComponents(OwnerForm, CompList);
    end;
  finally
    //CompList.Free;
  end;
end;

procedure TJvTimerItemsEditor.Paste;
var
  CompList: IDesignerSelections;
begin
  if CheckCollection then begin
    CompList := CreateSelectionList;
    try
      PasteComponents(OwnerForm, TimersCollection, CompList);
      UpdateData;
    finally
      //CompList.Free;
    end;
  end;
end;

procedure TJvTimerItemsEditor.ClearBtnClick(Sender: TObject);
var
  Item: TJvTimerEvent;
begin
  while TimersCollection.Events.Count > 0 do begin
    Item := TJvTimerEvent(TimersCollection.Events[0]);
    if Item <> nil then Self.ValidateRename(Item, Item.Name, '');
    TimersCollection.Events.Delete(0);
    Item.Free;
  end;
  UpdateData;
end;

end.
