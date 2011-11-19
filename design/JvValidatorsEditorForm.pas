{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidatorsEditorForm.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvValidatorsEditorForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, StdCtrls, Menus, ActnList, ImgList,
  DesignEditors, DesignIntf, DesignWindows,
  JvValidators;

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
    N1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    procedure alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acDeleteExecute(Sender: TObject);
    procedure lbValidatorsClick(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FValidator: TJvValidators;
    FFilling: Boolean;
    function AddExisting(Validator: TJvBaseValidator): Integer; overload;
    function AddNew(ValidatorClass: TJvBaseValidatorClass): Integer; overload;
    procedure Delete(Index: Integer);
    procedure ClearValidators;
    procedure SelectItem(AObject: TPersistent);
    procedure UpdateItem(Index: Integer);
    procedure UpdateCaption;
    procedure SetValidator(const Value: TJvValidators);
    procedure DoAddNewValidator(Sender: TObject);
    procedure AddValidatorClasses;
  public
    procedure Activated; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    function GetEditState: TEditState; override;
    property Validator: TJvValidators read FValidator write SetValidator;
  end;

  TJvValidatorEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TJvPropertyValidateProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvPropertyToCompareProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  TypInfo,
  JvErrorIndicator, JvDsgnConsts;

{$R *.dfm}

const
  ValidKinds: TTypeKinds =
    [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
     tkWChar, tkLString, {$IFDEF UNICODE} tkUString, {$ENDIF} tkWString, tkVariant, tkInt64];

{$IFNDEF COMPILER12_UP}
type
  NativeInt = Integer;
{$ENDIF ~COMPILER12_UP}

procedure ShowEditor(Designer: IDesigner; AValidator: TJvValidators);
var
  I: Integer;
  AEditor: TfrmValidatorsEditor;
begin
  // because the page list editor is not show modal, so
  // we need to find it rather than create a new instance.
  AEditor := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TfrmValidatorsEditor then
      if TfrmValidatorsEditor(Screen.Forms[I]).Validator = AValidator then
      begin
        AEditor := TfrmValidatorsEditor(Screen.Forms[I]);
        Break;
      end;
  // Show the wizard editor
  if Assigned(AEditor) then
  begin
    AEditor.Show;
    if AEditor.WindowState = wsMinimized then
      AEditor.WindowState := wsNormal;
  end
  else
  begin
    AEditor := TfrmValidatorsEditor.Create(Application);
    try
      AEditor.Designer := Designer;
      AEditor.Validator := AValidator;
      AEditor.Show;
    except
      AEditor.Free;
      raise;
    end;
  end;
end;

//=== { TJvValidatorEditor } =================================================

procedure TJvValidatorEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = 0) and (Component is TJvValidators) then
    ShowEditor(Designer, TJvValidators(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TJvValidatorEditor.GetVerb(Index: Integer): string;
begin
  Result := RsJvValidatorsItemsEditorEllipsis;
end;

function TJvValidatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//== TfrmValidatorsEditor ====================================================

procedure TfrmValidatorsEditor.FormCreate(Sender: TObject);
begin
  {$IFDEF COMPILER9_UP}
  FormStyle := fsStayOnTop;
  {$ENDIF COMPILER9_UP}
  AddValidatorClasses;
end;

procedure TfrmValidatorsEditor.Activated;
var
  I: Integer;
  Index: Integer;
begin
  inherited Activated;
  if FFilling then
    Exit;
  FFilling := True;
  try
    Index := lbValidators.ItemIndex;
    lbValidators.Items.BeginUpdate;
    try
      ClearValidators;
      if FValidator <> nil then
        for I := 0 to FValidator.Count - 1 do
          AddExisting(FValidator.Items[I]);

      if lbValidators.Items.Count = 0 then
        Index := -1
      else
      if (Index >= lbValidators.Items.Count) then
        Index := 0;
      lbValidators.ItemIndex := Index;
    finally
      lbValidators.Items.EndUpdate;
    end;
  finally
    FFilling := False;
  end;
end;

function TfrmValidatorsEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TfrmValidatorsEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TfrmValidatorsEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
var
  I, J: Integer;
begin
  inherited ItemDeleted(ADesigner, Item);
  if not (csDestroying in ComponentState) then
  begin
    if Item = Validator then
    begin
      Validator := nil;
      ClearValidators;
      Close;
    end
    else
    begin
      lbValidators.Items.BeginUpdate;
      try
        for I := 0 to lbValidators.Items.Count - 1 do
          if Item = lbValidators.Items.Objects[I] then
          begin
            J := lbValidators.ItemIndex;
            lbValidators.Items.Delete(I);
            if lbValidators.ItemIndex < 0 then
              lbValidators.ItemIndex := J;
            if lbValidators.ItemIndex < 0 then
              lbValidators.ItemIndex := J - 1;
            Exit;
          end;
      finally
        lbValidators.Items.EndUpdate;
      end;
    end;
    UpdateCaption;
  end;
end;

procedure TfrmValidatorsEditor.ItemsModified(const Designer: IDesigner);
begin
  inherited ItemsModified(Designer);
  if not (csDestroying in ComponentState) then
  begin
    UpdateItem(lbValidators.ItemIndex);
    UpdateCaption;
  end;
end;

procedure TfrmValidatorsEditor.UpdateItem(Index: Integer);
var
  I: Integer;
begin
  with lbValidators do
    if (Index < 0) or (Index >= Items.Count) then
      for I := 0 to Items.Count - 1 do
        Items[I] := TComponent(Items.Objects[I]).Name
    else
      Items[Index] := TComponent(Items.Objects[Index]).Name;
end;

function TfrmValidatorsEditor.AddExisting(Validator: TJvBaseValidator): Integer;
begin
  Result := lbValidators.Items.AddObject(Validator.Name, Validator);
  if not FFilling then
  begin
    lbValidators.ItemIndex := Result;
    lbValidatorsClick(nil);
  end;
end;

function TfrmValidatorsEditor.AddNew(ValidatorClass: TJvBaseValidatorClass): Integer;
var
  V: TJvBaseValidator;
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

procedure TfrmValidatorsEditor.Delete(Index: Integer);
var
  V: TJvBaseValidator;
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
  Caption := RsJvValidatorItemsEditorEllipsis;
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
  acMoveDown.Enabled := (lbValidators.ItemIndex < lbValidators.Items.Count - 1) and
    acDelete.Enabled;
end;


procedure TfrmValidatorsEditor.acDeleteExecute(Sender: TObject);
begin
  Delete(lbValidators.ItemIndex);
end;

procedure TfrmValidatorsEditor.acMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  with lbValidators do
  begin
    I := ItemIndex;
    Items.Exchange(I, I - 1);
    FValidator.Exchange(I, I - 1);
  end;
end;

procedure TfrmValidatorsEditor.acMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  with lbValidators do
  begin
    I := ItemIndex;
    Items.Exchange(I, I + 1);
    FValidator.Exchange(I, I + 1);
  end;
end;

procedure TfrmValidatorsEditor.DoAddNewValidator(Sender: TObject);
begin
  with Sender as TAction do
    AddNew(TJvBaseValidatorClass(Tag));
end;

type
  TJvBaseValidatorAccess = class(TJvBaseValidator);

procedure TfrmValidatorsEditor.AddValidatorClasses;
var
  I, J, K: Integer;
  A: TAction;
  M: TMenuItem;
  AName: string;
  AClass: TJvBaseValidatorClass;
begin
  J := TJvBaseValidatorAccess.BaseValidatorsCount;
  K := 0;
  for I := 0 to J - 1 do
  begin
    TJvBaseValidatorAccess.GetBaseValidatorInfo(I, AName, AClass);
    if AName = '' then
    begin
      Inc(K);
      Continue;
    end;
    A := TAction.Create(Self);
    A.Caption := AName;
    A.Tag := NativeInt(AClass);
    A.ImageIndex := 0;
    if I - K < 9 then
      A.ShortCut := ShortCut(Ord('0') + I + 1 - K, [ssCtrl]);
    A.OnExecute := DoAddNewValidator;
    M := TMenuItem.Create(popNew);
    M.Action := A;
    if I = 0 then
    begin
      M.Default := True;
      btnNew.Action := A;
    end;
    popNew.Items.Add(M);
    M := TMenuItem.Create(popForm);
    M.Action := A;
    if I = 0 then
      M.Default := True;
    popForm.Items.Insert(I,M);
  end;
  if J < 2 then
    btnNew.Style := tbsButton
  else
    btnNew.Style := tbsDropDown;
  ToolBar1.Width := 0;
end;

//=== { TJvPropertyValidateProperty } ========================================

function TJvPropertyValidateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TJvPropertyValidateProperty.GetValues(Proc: TGetStrProc);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  I, J: Integer;
  C: TControl;
  V:TJvBaseValidator;
begin
  if not (GetComponent(0) is TJvBaseValidator) then
    Exit;
  V := TJvBaseValidator(GetComponent(0));
  C := V.ControlToValidate;
  if C = nil then
    Exit;
  J := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, nil);
  if J > 0 then
  begin
    GetMem(PropList, J * SizeOf(Pointer));
    J := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, PropList);
    if J > 0 then
    try
      if V.GetDataLink(C) <> nil then
        Proc(cValidatorsDBValue);
      for I := 0 to J - 1 do
      begin
        PropInfo := PropList^[I];
        if (PropInfo <> nil) and (PropInfo.PropType^.Kind in ValidKinds) then
          Proc({$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo.Name));
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

//=== { TJvPropertyToCompareProperty } =======================================

function TJvPropertyToCompareProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TJvPropertyToCompareProperty.GetValues(Proc: TGetStrProc);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  I, J: Integer;
  C: TControl;
  V:TJvControlsCompareValidator;
begin
  if not (GetComponent(0) is TJvControlsCompareValidator) then
    Exit;
  V := TJvControlsCompareValidator(GetComponent(0));
  C := V.CompareToControl;
  if C = nil then
    Exit;
  J := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, nil);
  if J > 0 then
  begin
    GetMem(PropList, J * SizeOf(Pointer));
    J := GetPropList(PTypeInfo(C.ClassInfo), ValidKinds, PropList);
    if J > 0 then
    try

      if V.GetDataLink(C) <> nil then
        Proc(cValidatorsDBValue);
        
      for I := 0 to J - 1 do
      begin
        PropInfo := PropList^[I];
        if (PropInfo <> nil) and (PropInfo.PropType^.Kind in ValidKinds) then
          Proc({$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo.Name));
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

end.
