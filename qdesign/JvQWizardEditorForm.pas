{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizardEditorForm.PAS, released on 2002-01-29.

The Initial Developer of the Original Code is William Yu Wei.
Portions created by William Yu Wei are Copyright (C) 2002 William Yu Wei.
All Rights Reserved.

Contributor(s):
Peter Thörnqvist - converted to JVCL naming conventions on 2003-07-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{*****************************************************************************
  Purpose:   Jv Wizard Component Editor

  History:
  ---------------------------------------------------------------------------
  Date(mm/dd/yy)   Comments
  ---------------------------------------------------------------------------
  01/29/2002       Initial create
                   1) Move TJvWizardActivePageProperty, TJvWizardComponentEditor
                      class from JvWizardReg to here
                   2) TJvWizardPageListProperty added
                      TJvWizardPageList dialog form added
******************************************************************************}

{$I jvcl.inc}

unit JvQWizardEditorForm;

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QForms, QDialogs, Types, QTypes,
  QActnList, QImgList, QComCtrls, QStdCtrls, QToolWin, QMenus,  
  DesignIntf, DesignEditors,  
  ClxDesignWindows,  
  JvQWizard;

type
  TJvWizardActivePageProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // JvWizard Component Local Menu Editor
  TJvWizardComponentEditor = class(TComponentEditor)
  protected
    function GetWizard: TJvWizard; virtual;
    procedure AddPage(Page: TJvWizardCustomPage);
    procedure AddWelcomePage;
    procedure AddInteriorPage;
    procedure NextPage(Step: Integer);
    property Wizard: TJvWizard read GetWizard;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvWizardPageListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;
  
  TJvWizardPageListEditor = class(TClxDesignWindow) 
    tbrWizardPages: TToolBar;
    lbxWizardPages: TListBox;
    btnAddWelcomePage: TToolButton;
    btnDeletePages: TToolButton;
    ToolButton1: TToolButton;
    imgWizardPages: TImageList;
    actWizardPages: TActionList;
    actAddWelcomePage: TAction;
    actAddInteriorPage: TAction;
    actDeletePages: TAction;
    popWizard: TPopupMenu;
    AddWelcomePage1: TMenuItem;
    AddInteriorPage1: TMenuItem;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    acMoveUp: TAction;
    acMoveDown: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAddWelcomePageExecute(Sender: TObject);
    procedure actAddInteriorPageExecute(Sender: TObject);
    procedure actDeletePagesExecute(Sender: TObject);
    procedure actDeletePagesUpdate(Sender: TObject);
    procedure lbxWizardPagesClick(Sender: TObject);
    procedure lbxWizardPagesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbxWizardPagesDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbxWizardPagesDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure actWizardPagesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
  private
    FWizard: TJvWizard;
    procedure SetWizard(const Value: TJvWizard);
    procedure UpdatePageList(const CurrItemIndex: Integer);
    procedure SelectWizardPage(const Index: Integer);
  protected
    procedure AddPage(Page: TJvWizardCustomPage);
    procedure AddWelcomePage;
    procedure AddInteriorPage;
    property Wizard: TJvWizard
      read FWizard write SetWizard;
  public
    procedure Activated; override; 
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override; 
    function UniqueName(Component: TComponent): string; override;  
    function GetEditState: TEditState; override;
  end;

implementation


uses
  JvQDsgnConsts;




{$R *.xfm}




procedure ShowWizardPageListEditor(Designer: IDesigner; AWizard: TJvWizard);
var
  I: Integer;
  AWizardPageListEditor: TJvWizardPageListEditor;
begin
  // because the page list editor is not show modal, so
  // we need to find it rather than create a new instance.
  AWizardPageListEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TJvWizardPageListEditor then
      if TJvWizardPageListEditor(Screen.Forms[I]).Wizard = AWizard then
      begin
        AWizardPageListEditor := TJvWizardPageListEditor(Screen.Forms[I]);
        Break;
      end;
  // Show the wizard editor
  if Assigned(AWizardPageListEditor) then
  begin
    AWizardPageListEditor.Show;
    if AWizardPageListEditor.WindowState = wsMinimized then
      AWizardPageListEditor.WindowState := wsNormal;
  end
  else
  begin
    AWizardPageListEditor := TJvWizardPageListEditor.Create(Application);
    try 
      AWizardPageListEditor.Designer := Designer; 
      AWizardPageListEditor.Wizard := AWizard;
      AWizardPageListEditor.Show;
    except
      AWizardPageListEditor.Free;
      raise;
    end;
  end;
end;

//=== TJvWizardActivePageProperty ============================================

function TJvWizardActivePageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvWizardActivePageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.GetRoot.ComponentCount - 1 do
  begin
    Component := Designer.GetRoot.Components[I];
    if (Component.Name <> '') and (Component is TJvWizardCustomPage) and
      (TJvWizardCustomPage(Component).Wizard = GetComponent(0)) then
      Proc(Component.Name);
  end;
end;

//=== TJvWizardComponentEditor ===============================================

procedure TJvWizardComponentEditor.AddPage(Page: TJvWizardCustomPage);
begin
  Page.Parent := Wizard;
  Page.Wizard := Wizard;
  Designer.SelectComponent(Page);
  Wizard.ActivePage := Page;
  Designer.Modified;
end;

procedure TJvWizardComponentEditor.AddInteriorPage;
var
  Page: TJvWizardInteriorPage;
begin
  Page := TJvWizardInteriorPage.Create(Designer.GetRoot);
  try
    Page.Name := Designer.UniqueName(TJvWizardInteriorPage.ClassName);
    AddPage(Page);
  except
    Page.Free;
    raise;
  end;
end;

procedure TJvWizardComponentEditor.AddWelcomePage;
var
  Page: TJvWizardWelcomePage;
begin
  Page := TJvWizardWelcomePage.Create(Designer.GetRoot);
  try
    Page.Name := Designer.UniqueName(TJvWizardWelcomePage.ClassName);
    AddPage(Page);
  except
    Page.Free;
    raise;
  end;
end;

procedure TJvWizardComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowWizardPageListEditor(Designer, GetWizard);
    1:
      AddWelcomePage;
    2:
      AddInteriorPage;
    3:
      NextPage(1);
    4:
      NextPage(-1);
    5:
      if Assigned(Wizard.ActivePage) then
      begin
        Designer.SelectComponent(Wizard);
        Wizard.ActivePage.Free;
        Designer.Modified;
      end;
  end;
end;

function TJvWizardComponentEditor.GetWizard: TJvWizard;
begin
  if Component is TJvWizard then
    Result := TJvWizard(Component)
  else
    Result := TJvWizard(TJvWizardCustomPage(Component).Wizard);
end;

function TJvWizardComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsPageListEllipsis;
    1:
      Result := RsNewWelcomePage;
    2:
      Result := RsNewInteriorPage;
    3:
      Result := RsNextPage;
    4:
      Result := RsPreviousPage;
    5:
      Result := RsDeletePage;
  end;
end;

function TJvWizardComponentEditor.GetVerbCount: Integer;
begin
  Result := 6;
end;

procedure TJvWizardComponentEditor.NextPage(Step: Integer);
var
  Page: TJvWizardCustomPage;
begin
  Page := Wizard.FindNextPage(Wizard.ActivePageIndex, Step, False);
  if Assigned(Page) and (Page <> Wizard.ActivePage) then
  begin
    if Component is TJvWizardCustomPage then
      Designer.SelectComponent(Page);
    Wizard.ActivePage := Page;
    Designer.Modified;
  end;
end;

//=== TJvWizardPageListEditor ================================================

procedure TJvWizardPageListProperty.Edit;
begin
  ShowWizardPageListEditor(Designer, TJvWizard(GetComponent(0)));
end;

function TJvWizardPageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvWizardPageListProperty.GetValue: string;
var
  APageList: TList;
begin
  APageList := TList(Pointer(GetOrdValue));
  if not Assigned(APageList) or (APageList.Count <= 0) then
    Result := RsNone
  else
    Result := Format('(%s)', [GetPropType^.Name]);
end;

//=== TJvWizardPageList Dialog Form ==========================================

procedure TJvWizardPageListEditor.UpdatePageList(const CurrItemIndex: Integer);
var
  I: Integer;
begin
  if Assigned(FWizard) then
  begin
    lbxWizardPages.Items.BeginUpdate;
    try
      lbxWizardPages.Items.Clear;
      for I := 0 to FWizard.PageCount - 1 do
        lbxWizardPages.Items.Add(TJvWizardCustomPage(FWizard.Pages[I]).Name);
      if (CurrItemIndex >= 0) and (CurrItemIndex < lbxWizardPages.Items.Count) then
        lbxWizardPages.ItemIndex := CurrItemIndex
      else
        lbxWizardPages.ItemIndex := -1;
    finally
      lbxWizardPages.Items.EndUpdate;
    end;
  end;
end;

procedure TJvWizardPageListEditor.SelectWizardPage(const Index: Integer);
var
  Page: TJvWizardCustomPage;
begin
  if Assigned(FWizard) and Active then
  begin
    Page := nil;
    if (Index >= 0) and (Index < FWizard.PageCount) then
      Page := TJvWizardCustomPage(FWizard.Pages[Index]);
    Designer.SelectComponent(Page);
    Wizard.ActivePage := Page;
    Designer.Modified;
  end;
end;

procedure TJvWizardPageListEditor.Activated;
begin
  if (lbxWizardPages.ItemIndex < 0) and (lbxWizardPages.Items.Count > 0) then
    lbxWizardPages.ItemIndex := 0;
  if Assigned(FWizard) and Assigned(FWizard.ActivePage) and
    (FWizard.ActivePage.PageIndex <> lbxWizardPages.ItemIndex) then
    lbxWizardPages.ItemIndex := FWizard.ActivePage.PageIndex;
  SelectWizardPage(lbxWizardPages.ItemIndex);
end;



procedure TJvWizardPageListEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TJvWizardPageListEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  if Item = FWizard then
  begin
    FWizard := nil;
    Close;
  end;
end;

procedure TJvWizardPageListEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then
    UpdatePageList(lbxWizardPages.ItemIndex);
end;


function TJvWizardPageListEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;




function TJvWizardPageListEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TJvWizardPageListEditor.SetWizard(const Value: TJvWizard);
begin
  if FWizard <> Value then
  begin
    FWizard := Value;
    UpdatePageList(0);
  end;
end;

procedure TJvWizardPageListEditor.AddPage(Page: TJvWizardCustomPage);
begin
  Page.Parent := Wizard;
  Page.Wizard := Wizard;
  Wizard.ActivePage := Page;
  Designer.SelectComponent(Page);
  //Designer.Modified; !!! otherwise for some reason, the last item of the
  //list box will show twice when adding either welcome page or interior page.
  lbxWizardPages.ItemIndex := lbxWizardPages.Items.Add(Page.Name);
end;

procedure TJvWizardPageListEditor.AddInteriorPage;
var
  APage: TJvWizardCustomPage;
begin
  APage := TJvWizardInteriorPage.Create(Designer.GetRoot);
  try
    APage.Name := UniqueName(APage);
    AddPage(APage);
  except
    APage.Free;
    raise;
  end;
end;

procedure TJvWizardPageListEditor.AddWelcomePage;
var
  APage: TJvWizardCustomPage;
begin
  APage := TJvWizardWelcomePage.Create(Designer.GetRoot);
  try
    APage.Name := UniqueName(APage);
    AddPage(APage);
  except
    APage.Free;
    raise;
  end;
end;

procedure TJvWizardPageListEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJvWizardPageListEditor.actAddWelcomePageExecute(Sender: TObject);
begin
  AddWelcomePage;
end;

procedure TJvWizardPageListEditor.actAddInteriorPageExecute(Sender: TObject);
begin
  AddInteriorPage;
end;

procedure TJvWizardPageListEditor.actDeletePagesExecute(Sender: TObject);
begin
  if Assigned(Wizard.ActivePage) then
  begin
    if lbxWizardPages.ItemIndex >= 0 then
      lbxWizardPages.Items.Delete(Wizard.ActivePage.PageIndex);
    Designer.SelectComponent(Wizard);
    Wizard.ActivePage.Free;
    Designer.Modified;
  end;
end;

procedure TJvWizardPageListEditor.actDeletePagesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (lbxWizardPages.Items.Count > 0) and (lbxWizardPages.ItemIndex >= 0);
end;

procedure TJvWizardPageListEditor.lbxWizardPagesClick(Sender: TObject);
begin
  SelectWizardPage(lbxWizardPages.ItemIndex);
end;

procedure TJvWizardPageListEditor.lbxWizardPagesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lbxWizardPages.BeginDrag(False);
end;

procedure TJvWizardPageListEditor.lbxWizardPagesDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept :=
    (Source is TListBox) and
    (lbxWizardPages.ItemAtPos(Point(X, Y), True) <> -1) and
    (lbxWizardPages.ItemAtPos(Point(X, Y), True) <> lbxWizardPages.ItemIndex);
end;

procedure TJvWizardPageListEditor.lbxWizardPagesDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  OldIndex, NewIndex: Integer;
begin
  OldIndex := lbxWizardPages.ItemIndex;
  NewIndex := lbxWizardPages.ItemAtPos(Point(X,Y), True);
  lbxWizardPages.Items.Move(OldIndex, NewIndex);
  if Assigned(FWizard) then
  begin
    TJvWizardCustomPage(FWizard.Pages[OldIndex]).PageIndex := NewIndex;
    lbxWizardPages.ItemIndex := NewIndex;
  end;
end;

procedure TJvWizardPageListEditor.actWizardPagesUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acMoveUp.Enabled := lbxWizardPages.ItemIndex > 0;
  acMoveDown.Enabled :=
    (lbxWizardPages.ItemIndex <> -1) and
    (lbxWizardPages.ItemIndex < lbxWizardPages.Items.Count - 1);
end;

procedure TJvWizardPageListEditor.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lbxWizardPages.ItemIndex;
  lbxWizardPages.Items.Move(I, I-1);
  if Assigned(FWizard) then
  begin
    TJvWizardCustomPage(FWizard.Pages[I]).PageIndex := I - 1;
    lbxWizardPages.ItemIndex := I - 1;
  end;
end;

procedure TJvWizardPageListEditor.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lbxWizardPages.ItemIndex;
  lbxWizardPages.Items.Move(I, I+1);
  if Assigned(FWizard) then
  begin
    TJvWizardCustomPage(FWizard.Pages[I]).PageIndex := I + 1;
    lbxWizardPages.ItemIndex := I + 1;
  end;
end;

end.

