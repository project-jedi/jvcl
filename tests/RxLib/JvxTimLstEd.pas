{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxTimLstEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxTimLstEd;

interface


uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} SysUtils,
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grids, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Menus, JvxCtrls, JvxVCLUtils, JvxPlacemnt,
  JvxTimerLst, DesignWindows;

type
  TJvxTimerItemsEditor = class(TDesignWindow)
    BtnPanel: TPanel;
    ClientPanel: TPanel;
    NewBtn: TButton;
    DeleteBtn: TButton;
    DrawGrid: TDrawGrid;
    PopupMenu: TPopupMenu;
    CutMenu: TMenuItem;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    FormStorage: TJvxFormPlacement;
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
    FTimersCollection: TJvxTimerList;
    function GetForm: TCustomForm;
    function CheckCollection: Boolean;
    function ItemByRow(Row: Integer): TJvxTimerEvent;
    procedure SelectItem(Item: TJvxTimerEvent);
    procedure UpdateData;
    procedure SetTimersCollection(Value: TJvxTimerList);
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
    property TimersCollection: TJvxTimerList read FTimersCollection
      write SetTimersCollection;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TJvxTimersItemListProperty }

  TJvxTimersItemListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TJvxTimersCollectionEditor }

  TJvxTimersCollectionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses Consts, {$IFDEF WIN32} JvxConst, {$ENDIF} JvxLConst, JvxDsgn;

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$IFDEF Delphi4_Up}
type
  TDesigner = IDesigner;
  TFormDesigner = IDesigner;
{$ENDIF}

function FindEditor(ATimersCollection: TJvxTimerList): TJvxTimerItemsEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TJvxTimerItemsEditor then begin
      if TJvxTimerItemsEditor(Screen.Forms[I]).TimersCollection = ATimersCollection then
      begin
        Result := TJvxTimerItemsEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowItemsEditor(Designer: TDesigner;
  ATimersCollection: TJvxTimerList);
var
  Editor: TJvxTimerItemsEditor;
begin
  if ATimersCollection = nil then Exit;
  Editor := FindEditor(ATimersCollection);
  if Editor = nil then begin
    Editor := TJvxTimerItemsEditor.Create(Application);
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

{ TJvxTimersItemListProperty }

function TJvxTimersItemListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvxTimersItemListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := ResStr(srNone)
  else FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TJvxTimersItemListProperty.Edit;
begin
  ShowItemsEditor(Designer, TJvxTimerList(GetComponent(0)));
end;

{ TJvxTimersCollectionEditor }

procedure TJvxTimersCollectionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowItemsEditor(Designer, TJvxTimerList(Component));
  end;
end;

function TJvxTimersCollectionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srTimerDesigner);
  end;
end;

function TJvxTimersCollectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TJvxTimerItemsEditor }

procedure TJvxTimerItemsEditor.SetTimersCollection(Value: TJvxTimerList);
begin
  if FTimersCollection <> Value then begin
    FTimersCollection := Value;
    UpdateData;
  end;
end;

function TJvxTimerItemsEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
{$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
{$ENDIF}
begin
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TJvxTimerEvent.ClassName;
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

function TJvxTimerItemsEditor.GetEditState: TEditState;
begin
  Result := [];
  if DeleteBtn.Enabled then Result := [esCanDelete, esCanCut, esCanCopy];
  if ClipboardComponents then Include(Result, esCanPaste);
end;

procedure TJvxTimerItemsEditor.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if Designer.Root = OwnerForm then Free;
end;

procedure TJvxTimerItemsEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TJvxTimerItemsEditor.Activated;
begin
  SelectItem(ItemByRow(DrawGrid.Row - 1));
end;

procedure TJvxTimerItemsEditor.UpdateData;
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

function TJvxTimerItemsEditor.GetForm: TCustomForm;
begin
  Result := GetParentForm(ClientPanel); //Designer.Form;
end;

procedure TJvxTimerItemsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TJvxTimerItemsEditor.CheckCollection: Boolean;
begin
  Result := (TimersCollection <> nil) and (TimersCollection.Owner <> nil)
    and (Designer.Root <> nil);
end;

procedure TJvxTimerItemsEditor.SelectItem(Item: TJvxTimerEvent);
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

function TJvxTimerItemsEditor.ItemByRow(Row: Integer): TJvxTimerEvent;
begin
  Result := nil;
  if CheckCollection and (Row >= 0) and
    (Row < TimersCollection.Count) then
  begin
    Result := TJvxTimerEvent(TimersCollection.Events[Row]);
  end;
end;

procedure TJvxTimerItemsEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = TimersCollection then begin
    TimersCollection := nil;
    Close;
  end;
end;

procedure TJvxTimerItemsEditor.DrawGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Item: TJvxTimerEvent;
begin
  CellText := '';
  if gdFixed in State then CellText := 'Item name'
  else begin
    Item := ItemByRow(Row - 1);
    if Item <> nil then CellText := Item.Name;
  end;
  DrawCellText(DrawGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
end;

procedure TJvxTimerItemsEditor.DrawGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectItem(ItemByRow(Row - 1));
end;

procedure TJvxTimerItemsEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvxTimerItemsEditor.DeleteClick(Sender: TObject);
var
  Item: TJvxTimerEvent;
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

procedure TJvxTimerItemsEditor.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_RETURN: if ItemByRow(DrawGrid.Row - 1) <> nil then ActivateInspector(#0);
      VK_DELETE: DeleteClick(nil);
    end;
end;

procedure TJvxTimerItemsEditor.FormCreate(Sender: TObject);
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

procedure TJvxTimerItemsEditor.FormResize(Sender: TObject);
begin
  with DrawGrid do ColWidths[0] := ClientWidth;
end;

function TJvxTimerItemsEditor.EditAction(Action: TEditAction) : Boolean;
begin
  Result := True;
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: DeleteClick(Self);
  end;
end;

procedure TJvxTimerItemsEditor.NewClick(Sender: TObject);
var
  I: Integer;
  Item: TJvxTimerEvent;
begin
  Item := TJvxTimerEvent.Create(TimersCollection.Owner);
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

procedure TJvxTimerItemsEditor.CutClick(Sender: TObject);
begin
  Cut;
  UpdateData;
end;

procedure TJvxTimerItemsEditor.CopyClick(Sender: TObject);
begin
  Copy;
  UpdateData;
end;

procedure TJvxTimerItemsEditor.PasteClick(Sender: TObject);
begin
  Paste;
  UpdateData;
end;

procedure TJvxTimerItemsEditor.Cut;
begin
  Copy;
  DeleteClick(Self);
end;

procedure TJvxTimerItemsEditor.Copy;
var
  CompList: IDesignerSelections;
  Item: TJvxTimerEvent;
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

procedure TJvxTimerItemsEditor.Paste;
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

procedure TJvxTimerItemsEditor.ClearBtnClick(Sender: TObject);
var
  Item: TJvxTimerEvent;
begin
  while TimersCollection.Events.Count > 0 do begin
    Item := TJvxTimerEvent(TimersCollection.Events[0]);
    if Item <> nil then Self.ValidateRename(Item, Item.Name, '');
    TimersCollection.Events.Delete(0);
    Item.Free;
  end;
  UpdateData;
end;

end.
