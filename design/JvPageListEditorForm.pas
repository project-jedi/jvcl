{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListEditorForm.PAS, released on 2004-03-28.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageListEditorForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, ComCtrls, StdCtrls, ToolWin, Menus,
  DesignIntf, DesignEditors, DesignWindows,
  JvPageList;

type
  TfrmPageListEditor = class(TDesignWindow)
    ToolBar1: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    ToolButton1: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    lbPages: TListBox;
    alEditor: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    ilButtons: TImageList;
    StatusBar1: TStatusBar;
    popEditor: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acDeleteUpdate(Sender: TObject);
    procedure lbPagesClick(Sender: TObject);
    procedure alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lbPagesKeyPress(Sender: TObject; var Key: Char);
  private
    FPageList: TJvCustomPageList;
    procedure SetPageList(const Value: TJvCustomPageList);
    procedure UpdateList(ItemIndex: Integer);
    procedure SelectPage(const Index: Integer);
    procedure Add(Page: TJvCustomPage);
  public
    property PageList:TJvCustomPageList read FPageList write SetPageList;
    procedure Activated; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    function GetEditState: TEditState; override;
  end;

procedure ShowPageListEditor(Designer: IDesigner; APageList: TJvCustomPageList);

implementation

uses
  JvDsgnConsts;

{$R *.dfm}

procedure ShowPageListEditor(Designer: IDesigner; APageList: TJvCustomPageList);
var
  I: Integer;
  APageListEditor: TfrmPageListEditor;
begin
  APageListEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TfrmPageListEditor then
      if TfrmPageListEditor(Screen.Forms[I]).PageList = APageList then
      begin
        APageListEditor := TfrmPageListEditor(Screen.Forms[I]);
        Break;
      end;
  // Show the editor
  if Assigned(APageListEditor) then
  begin
    APageListEditor.Show;
    if APageListEditor.WindowState = wsMinimized then
      APageListEditor.WindowState := wsNormal;
  end
  else
  begin
    APageListEditor := TfrmPageListEditor.Create(Application);
    try
      APageListEditor.Designer := Designer;
      APageListEditor.PageList := APageList;
      APageListEditor.Show;
    except
      APageListEditor.Free;
      raise;
    end;
  end;
  if APageListEditor <> nil then
    APageListEditor.Caption := Format(RsFmtCaption,[APageList.Name]);
end;

type
  TJvCustomPageAccess = class(TJvCustomPage);

procedure TfrmPageListEditor.acAddExecute(Sender: TObject);
var
  APage: TJvCustomPage;
begin
  APage := PageList.GetPageClass.Create(Designer.GetRoot);
  try
    APage.Name := UniqueName(APage);
    Add(APage);
  except
    APage.Free;
    raise;
  end;
end;

procedure TfrmPageListEditor.acDeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(PageList.ActivePage) then
  begin
    I := lbPages.ItemIndex;
    if lbPages.ItemIndex >= 0 then
      lbPages.Items.Delete(TJvCustomPageAccess(PageList.ActivePage).PageIndex);
    Designer.SelectComponent(PageList);
    PageList.ActivePage.Free;
    if I >= lbPages.Items.Count then
      Dec(I);
    if (I >= 0) and (I < lbPages.Items.Count) then
    begin
      lbPages.ItemIndex := I;
      SelectPage(I);
    end
    else
      Designer.Modified;
  end;
end;

procedure TfrmPageListEditor.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lbPages.ItemIndex;
  lbPages.Items.Move(I, I-1);
  if Assigned(PageList) then
  begin
    TJvCustomPageAccess(PageList.Pages[I]).PageIndex := I - 1;
    lbPages.ItemIndex := I - 1;
  end;
end;

procedure TfrmPageListEditor.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lbPages.ItemIndex;
  lbPages.Items.Move(I, I+1);
  if Assigned(PageList) then
  begin
    TJvCustomPageAccess(PageList.Pages[I]).PageIndex := I + 1;
    lbPages.ItemIndex := I + 1;
  end;
end;

procedure TfrmPageListEditor.Activated;
begin
  if (lbPages.ItemIndex < 0) and (lbPages.Items.Count > 0) then
    lbPages.ItemIndex := 0;
  if Assigned(FPageList) and Assigned(FPageList.ActivePage) and
    (TJvCustomPageAccess(FPageList.ActivePage).PageIndex <> lbPages.ItemIndex) then
    lbPages.ItemIndex := TJvCustomPageAccess(FPageList.ActivePage).PageIndex;
  SelectPage(lbPages.ItemIndex);
end;

procedure TfrmPageListEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TfrmPageListEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  if Item = FPageList then
  begin
    FPageList := nil;
    Close;
  end;
end;

procedure TfrmPageListEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then
    UpdateList(lbPages.ItemIndex);
end;

function TfrmPageListEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TfrmPageListEditor.SetPageList(const Value: TJvCustomPageList);
begin
  if FPageList <> Value then
  begin
    FPageList := Value;
    UpdateList(0);
  end;
end;

procedure TfrmPageListEditor.Add(Page: TJvCustomPage);
begin
  Page.Parent := PageList;
  Page.PageList := PageList;
  PageList.ActivePage := Page;
  Designer.SelectComponent(Page);
  //Designer.Modified; 
  lbPages.ItemIndex := lbPages.Items.Add(Page.Name);
end;

procedure TfrmPageListEditor.SelectPage(const Index: Integer);
var
  Page: TJvCustomPageAccess;
begin
  if Assigned(FPageList) and Active then
  begin
    Page := nil;
    if (Index >= 0) and (Index < FPageList.PageCount) then
      Page := TJvCustomPageAccess(FPageList.Pages[Index]);
    PageList.ActivePage := Page;
    Designer.SelectComponent(Page);
    Designer.Modified;
  end;
end;

procedure TfrmPageListEditor.UpdateList(ItemIndex: Integer);
var
  I: Integer;
begin
  if Assigned(FPageList) then
  begin
    lbPages.Items.BeginUpdate;
    try
      lbPages.Items.Clear;
      for I := 0 to FPageList.PageCount - 1 do
        lbPages.Items.Add(TJvCustomPageAccess(FPageList.Pages[I]).Name);
      if (ItemIndex >= 0) and (ItemIndex < lbPages.Items.Count) then
        lbPages.ItemIndex := ItemIndex
      else
        lbPages.ItemIndex := -1;
    finally
      lbPages.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmPageListEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmPageListEditor.acDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (lbPages.Items.Count > 0) and (lbPages.ItemIndex >= 0);
end;

procedure TfrmPageListEditor.lbPagesClick(Sender: TObject);
begin
  SelectPage(lbPages.ItemIndex);
end;

procedure TfrmPageListEditor.alEditorUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acMoveUp.Enabled := lbPages.ItemIndex > 0;
  acMoveDown.Enabled :=
    (lbPages.ItemIndex <> -1) and
    (lbPages.ItemIndex < lbPages.Items.Count - 1);
end;

procedure TfrmPageListEditor.lbPagesKeyPress(Sender: TObject;
  var Key: Char);
begin
  if lbPages.ItemIndex <> -1 then
  begin
    SelectPage(lbPages.ItemIndex);
    ActivateInspector(Key);
    Key := #0;
  end;
end;

procedure TfrmPageListEditor.FormCreate(Sender: TObject);
begin
  {$IFDEF COMPILER9_UP}
  FormStyle := fsStayOnTop;
  {$ENDIF COMPILER9_UP}
end;

end.