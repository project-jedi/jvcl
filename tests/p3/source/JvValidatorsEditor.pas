{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidatorsEditor.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvValidatorsEditor;

interface

uses                 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, JvValidators,
{$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignWindows,
{$ELSE}
  DsgnIntf, DsgnWnds,
{$ENDIF}
  StdCtrls, Menus, ActnList, ImgList;

type
  TfrmValidatorsEditor = class(TDesignWindow)
    ToolBar1: TToolBar;
    btnNew: TToolButton;
    btnDelete: TToolButton;
    StatusBar1: TStatusBar;
    lbValidators: TListBox;
    popNew: TPopupMenu;
    alEditor: TActionList;
    acDelete: TAction;
    il16: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    acMoveUp: TAction;
    acMoveDown: TAction;
    popForm: TPopupMenu;
    RequiredFieldValidator2: TMenuItem;
    RangeValidator2: TMenuItem;
    RegularExpressionValidator2: TMenuItem;
    CompareValidator2: TMenuItem;
    CustomValidator2: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    procedure alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acNewRequiredExecute(Sender: TObject);
    procedure acNewCompareExecute(Sender: TObject);
    procedure acNewRangeExecute(Sender: TObject);
    procedure acNewRegExpExecute(Sender: TObject);
    procedure acNewCustomExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure lbValidatorsClick(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FValidator: TJvValidators;
    function AddExisting(Validator: TJvBaseValidator): integer; overload;
    function AddNew(ValidatorClass: TJvBaseValidatorClass): integer; overload;
    procedure Delete(Index: integer);
    procedure ClearValidators;
    procedure SelectItem(AObject: TPersistent);
    procedure UpdateItem(Index: integer);
    procedure UpdateCaption;
    procedure SetValidator(const Value: TJvValidators);
    procedure DoAddNewValidator(Sender: TObject);
    procedure AddValidatorClasses;
  public
    procedure Activated; override;
{$IFDEF COMPILER6_UP}
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
{$ELSE}
    procedure ComponentDeleted(Component: IPersistent); override;
    function UniqueName(Component: TComponent): string; override;
    procedure FormClosed(AForm: TCustomForm); override;
    procedure FormModified; override;
{$ENDIF}
    function GetEditState: TEditState; override;
    property Validator: TJvValidators read FValidator write SetValidator;
  end;

  TJvValidatorComponent = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TJvPropertyValidateProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFNDEF COMPILER6_UP}
  // since D5 doesn't support interface style published properties,
  // these editors are supplied to make it easier to select a specific interface
  // implementor at design-time
  TJvValidationSummaryProperty = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvErrorProviderProperty = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  {$ENDIF}



implementation
uses
  JvErrProvider, TypInfo;

{$R *.dfm}

procedure ShowEditor(Designer: IDesigner; AValidator: TJvValidators);
var
  i: Integer;
  AEditor: TfrmValidatorsEditor;
begin
  // because the page list editor is not show modal, so
  // we need to find it rather than create a new instance.
  AEditor := nil;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] is TfrmValidatorsEditor then
    begin
      if TfrmValidatorsEditor(Screen.Forms[i]).Validator = AValidator then
      begin
        AEditor := TfrmValidatorsEditor(Screen.Forms[i]);
        Break;
      end;
    end;
  end;
  // Show the wizard editor
  if Assigned(AEditor) then
  begin
    AEditor.Show;
    if AEditor.WindowState = wsMinimized then
    begin
      AEditor.WindowState := wsNormal;
    end;
  end
  else
  begin
    AEditor := TfrmValidatorsEditor.Create(Application);
    try
{$IFDEF COMPILER6_UP}
      AEditor.Designer := Designer;
{$ELSE}
      AEditor.Designer := Designer as IFormDesigner;
{$ENDIF}
      AEditor.Validator := AValidator;
      AEditor.Show;
    except
      if Assigned(AEditor) then
      begin
        AEditor.Free;
      end;
      raise;
    end;
  end;
end;

{ TJvValidatorComponent }

procedure TJvValidatorComponent.ExecuteVerb(Index: Integer);
begin
  if (Index = 0) and (Component is TJvValidators) then
    ShowEditor(Designer, TJvValidators(Component))
  else
    inherited;
end;

function TJvValidatorComponent.GetVerb(Index: Integer): string;
begin
  Result := 'JvValidators Items Editor...';
end;

function TJvValidatorComponent.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TfrmValidatorsEditor }

procedure TfrmValidatorsEditor.FormCreate(Sender: TObject);
begin
  AddValidatorClasses;
end;

procedure TfrmValidatorsEditor.Activated;
var i: integer;
begin
  inherited;
  ClearValidators;
  if FValidator = nil then
    Exit;
  lbValidators.Items.BeginUpdate;
  try
    for i := 0 to FValidator.Count - 1 do
      AddExisting(FValidator.Items[i]);
  finally
    lbValidators.Items.EndUpdate;
    lbValidators.ItemIndex := 0;
  end;
end;

function TfrmValidatorsEditor.GetEditState: TEditState;
begin
  Result := [];
end;

{$IFDEF COMPILER6_UP}

procedure TfrmValidatorsEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TfrmValidatorsEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
var i, j: integer;
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if Item = Validator then
    begin
      Validator := nil;
      ClearValidators;
      Close;
    end
    else
      for i := 0 to lbValidators.Items.Count - 1 do
        if Item = lbValidators.Items.Objects[i] then
        begin
          j := lbValidators.ItemIndex;
          lbValidators.Items.Delete(i);
          if lbValidators.ItemIndex < 0 then
            lbValidators.ItemIndex := j;
          if lbValidators.ItemIndex < 0 then
            lbValidators.ItemIndex := j - 1;
          Exit;
        end;
    UpdateCaption;
  end;
end;

procedure TfrmValidatorsEditor.ItemsModified(const Designer: IDesigner);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    UpdateItem(lbValidators.ItemIndex);
    UpdateCaption;
  end;
end;
{$ELSE}

procedure TfrmValidatorsEditor.ComponentDeleted(Component: IPersistent);
var Item: TPersistent; i, j: integer;
begin
  inherited;
  Item := ExtractPersistent(Component);
  if not (csDestroying in ComponentState) then
  begin
    if Item = Validator then
    begin
      Validator := nil;
      ClearValidators;
      Close;
    end
    else
      for i := 0 to lbValidators.Items.Count - 1 do
        if Item = lbValidators.Items.Objects[i] then
        begin
          j := lbValidators.ItemIndex;
          lbValidators.Items.Delete(i);
          if lbValidators.ItemIndex < 0 then
            lbValidators.ItemIndex := j;
          if lbValidators.ItemIndex < 0 then
            lbValidators.ItemIndex := j - 1;
          Exit;
        end;
    UpdateCaption;
  end;
end;

procedure TfrmValidatorsEditor.FormClosed(AForm: TCustomForm);
begin
  inherited;
  if AForm = Designer.Form then
    Close;
end;

procedure TfrmValidatorsEditor.FormModified;
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    UpdateItem(lbValidators.ItemIndex);
    UpdateCaption;
  end;
end;

function TfrmValidatorsEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

{$ENDIF}

procedure TfrmValidatorsEditor.UpdateItem(Index: integer);
var i: integer;
begin
  with lbValidators do
    if (Index < 0) or (Index >= Items.Count) then
    begin
      for i := 0 to Items.Count - 1 do
        Items[i] := TComponent(Items.Objects[i]).Name;
    end
    else
      Items[Index] := TComponent(Items.Objects[Index]).Name;
end;

function TfrmValidatorsEditor.AddExisting(Validator: TJvBaseValidator): integer;
begin
  Result := lbValidators.Items.AddObject(Validator.Name, Validator);
  lbValidators.ItemIndex := Result;
  lbValidatorsClick(nil);
end;

function TfrmValidatorsEditor.AddNew(ValidatorClass: TJvBaseValidatorClass): integer;
var V: TJvBaseValidator;
begin
  V := ValidatorClass.Create(FValidator.Owner);
  try
    V.Name := Designer.UniqueName(V.ClassName);
    FValidator.Insert(V);
    Result := AddExisting(V);
  except
    V.Free;
    raise;
  end;
end;

procedure TfrmValidatorsEditor.ClearValidators;
begin
  lbValidators.Items.Clear;
end;

procedure TfrmValidatorsEditor.Delete(Index: integer);
var V:TJvBaseValidator;
begin
  with lbValidators do
    if (Index > -1) and (Index < Items.Count) then
    begin
      V := TJvBaseValidator(Items.Objects[Index]);
      FValidator.Remove(V);
      V.Free;
      Designer.Modified;
    end;
end;

procedure TfrmValidatorsEditor.SelectItem(AObject: TPersistent);
begin
  Designer.SelectComponent(AObject);
  Designer.Modified;
end;

procedure TfrmValidatorsEditor.SetValidator(const Value: TJvValidators);
begin
  FValidator := Value;
  Activated;
end;

procedure TfrmValidatorsEditor.UpdateCaption;
begin
  Caption := 'JvValidator Items Editor';
end;

procedure TfrmValidatorsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmValidatorsEditor.lbValidatorsClick(Sender: TObject);
begin
  if lbValidators.ItemIndex > -1 then
    with lbValidators do
      SelectItem(TJvBaseValidator(Items.Objects[ItemIndex]));
end;

procedure TfrmValidatorsEditor.alEditorUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acDelete.Enabled := lbValidators.ItemIndex > -1;
  acMoveUp.Enabled := lbValidators.ItemIndex > 0;
  acMoveDown.Enabled := (lbValidators.ItemIndex < lbValidators.Items.Count - 1) and acDelete.Enabled;
end;

procedure TfrmValidatorsEditor.acNewRequiredExecute(Sender: TObject);
begin
  AddNew(TJvRequiredFieldValidator);
end;

procedure TfrmValidatorsEditor.acNewCompareExecute(Sender: TObject);
begin
  AddNew(TJvCompareValidator);
end;

procedure TfrmValidatorsEditor.acNewRangeExecute(Sender: TObject);
begin
  AddNew(TJvRangeValidator);
end;

procedure TfrmValidatorsEditor.acNewRegExpExecute(Sender: TObject);
begin
  AddNew(TJvRegularExpressionValidator);
end;

procedure TfrmValidatorsEditor.acNewCustomExecute(Sender: TObject);
begin
  AddNew(TJvCustomValidator);
end;

procedure TfrmValidatorsEditor.acDeleteExecute(Sender: TObject);
begin
  Delete(lbValidators.ItemIndex);
end;

procedure TfrmValidatorsEditor.acMoveUpExecute(Sender: TObject);
var i: integer;
begin
  with lbValidators do
  begin
    i := ItemIndex;
    Items.Exchange(i, i - 1);
    FValidator.Exchange(i, i - 1);
  end;
end;

procedure TfrmValidatorsEditor.acMoveDownExecute(Sender: TObject);
var i: integer;
begin
  with lbValidators do
  begin
    i := ItemIndex;
    Items.Exchange(i, i + 1);
    FValidator.Exchange(i, i + 1);
  end;
end;

procedure TfrmValidatorsEditor.DoAddNewValidator(Sender:TObject);
begin
  with Sender as TAction do
    AddNew(TJvBaseValidatorClass(Tag));
end;

type
  TJvBaseValidatorAccess = class(TJvBaseValidator);

procedure TfrmValidatorsEditor.AddValidatorClasses;
var i,j,k:integer;
    A:TAction;
    M:TMenuItem;
    AName:string;
    AClass:TJvBaseValidatorClass;
begin
  j := TJvBaseValidatorAccess.BaseValidatorsCount;
  k := 0;
  for i := 0 to j - 1 do
  begin
    TJvBaseValidatorAccess.GetBaseValidatorInfo(i,AName,AClass);
    if AName = '' then
    begin
      Inc(k);
      Continue;
    end;
    A := TAction.Create(self);
    A.Caption := AName;
    A.Tag := integer(AClass);
    A.ImageIndex := 0;
    if i-k < 9 then
      A.ShortCut := ShortCut(Ord('0')+i+1-k,[ssCtrl]);
    A.OnExecute := DoAddNewValidator;
    M := TMenuItem.Create(self);
    M.Action := A;
    if i = 0 then
    begin
      M.Default := true;
      btnNew.Action := A;
    end;
    popNew.Items.Add(M);
  end;
  if j < 2 then
    btnNew.Style := tbsButton
  else
    btnNew.Style := tbsDropDown;
  ToolBar1.Width := 0;
end;

{ TJvPropertyValidateProperty }

function TJvPropertyValidateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TJvPropertyValidateProperty.GetValues(Proc: TGetStrProc);
const
  ValidKinds: TTypeKinds = [tkInteger, tkChar, tkEnumeration, tkFloat,
  tkString, tkSet, tkWChar, tkLString, tkWString, tkVariant, tkInt64];
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  i, j: integer;
  C: TControl;
begin
  if not (GetComponent(0) is TJvBaseValidator) then Exit;
  C := TJvBaseValidator(GetComponent(0)).ControlToValidate;
  if C = nil then
    Exit;
  j := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, nil);
  if j > 0 then
  begin
    GetMem(PropList, j * SizeOf(Pointer));
    j := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, PropList);
    if j > 0 then
    try
      for i := 0 to j - 1 do
      begin
        PropInfo := PropList^[i];
        if (PropInfo <> nil) and (PropInfo.PropType^.Kind in ValidKinds) then
          Proc(PropInfo.Name);
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

{ TJvValidationSummaryProperty }
{$IFNDEF COMPILER6_UP}
procedure TJvValidationSummaryProperty.GetValues(Proc: TGetStrProc);
var i:integer;obj:IJvValidationSummary;
begin
  for i := 0 to Designer.Form.ComponentCount - 1 do
    if Supports(Designer.Form.Components[i],IJvValidationSummary,obj) and
      (Designer.Form.Components[i] <> GetComponent(0)) then
      Proc(Designer.Form.Components[i].Name);
end;

{ TJvErrorProviderProperty }

procedure TJvErrorProviderProperty.GetValues(Proc: TGetStrProc);
var i:integer;obj:IJvErrorProvider;
begin
  for i := 0 to Designer.Form.ComponentCount - 1 do
    if Supports(Designer.Form.Components[i],IJvErrorProvider,obj) and
      (Designer.Form.Components[i] <> GetComponent(0)) then
      Proc(Designer.Form.Components[i].Name);
end;
{$ENDIF}

end.

