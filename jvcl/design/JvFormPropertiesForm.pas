{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPresrDsn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit JvFormPropertiesForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Consts,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvJVCLUtils, JvFormPlacement, JvPropertyStorage, JvComponent;

{ TODO -oJVCL -cREIMPLEMENT :
  Add support for "Box" style procedures again but remove dependency on
  JvxCtrls and JvBoxProcs units }

type
  TJvFormPropsDlg = class(TJvForm)
    Bevel1: TBevel;
    Label30: TLabel;
    Label31: TLabel;
    Label2: TLabel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    FormBox: TGroupBox;
    ActiveCtrlBox: TCheckBox;
    SizeBox: TCheckBox;
    StateBox: TCheckBox;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearButton: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    ComponentsList: TListBox;
    PropertiesList: TListBox;
    StoredList: TListBox;
    LocationBox: TCheckBox;
    procedure AddButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure StoredListClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure StoredListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StoredListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PropertiesListDblClick(Sender: TObject);
  private
    FCompOwner: TComponent;
    FDesigner: IDesigner;
    procedure ListToIndex(List: TCustomListBox; Idx: Integer);
    procedure UpdateCurrent;
    procedure DeleteProp(I: Integer);
    function FindProp(const CompName, PropName: string;
      var IdxComp, IdxProp: Integer): Boolean;
    procedure ClearLists;
    procedure CheckAddItem(const CompName, PropName: string);
    procedure AddItem(IdxComp, IdxProp: Integer; AUpdate: Boolean);
    procedure BuildLists(StoredProps: TStrings);
    procedure CheckButtons;
    procedure SetStoredList(AList: TStrings);
  end;

  TJvFormStorageEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvStoredPropsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function ShowStorageDesigner(ACompOwner: TComponent; ADesigner: IDesigner;
  AStoredList: TStrings; var Options: TPlacementOptions): Boolean;

implementation

uses
  TypInfo,
  JvDsgnConsts;

{$R *.dfm}

//=== { TJvFormStorageEditor } ===============================================

procedure TJvFormStorageEditor.ExecuteVerb(Index: Integer);
var
  Storage: TJvFormStorage;
  Opt: TPlacementOptions;
begin
  Storage := Component as TJvFormStorage;
  if Index = 0 then
  begin
    Opt := Storage.Options;
    if ShowStorageDesigner(TComponent(Storage.Owner), Designer,
      Storage.StoredProps, Opt) then
    begin
      Storage.Options := Opt;
      Storage.SetNotification;
    end;
  end;
end;

function TJvFormStorageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsStorageDesigner;
  else
    Result := '';
  end;
end;

function TJvFormStorageEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== { TJvStoredPropsProperty } =============================================

function TJvStoredPropsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TJvStoredPropsProperty.GetValue: string;
begin
  if TStrings(GetOrdValue).Count > 0 then
    Result := inherited GetValue
  else
    Result := srNone;
end;

procedure TJvStoredPropsProperty.Edit;
var
  Storage: TJvFormStorage;
  Opt: TPlacementOptions;
begin
  Storage := GetComponent(0) as TJvFormStorage;
  Opt := Storage.Options;
  if ShowStorageDesigner(Storage.Owner as TComponent, Designer,
    Storage.StoredProps, Opt) then
  begin
    Storage.Options := Opt;
    Storage.SetNotification;
  end;
end;

//=== { TJvFormPropsDlg } ====================================================

function ShowStorageDesigner(ACompOwner: TComponent; ADesigner: IDesigner;
  AStoredList: TStrings; var Options: TPlacementOptions): Boolean;
begin
  with TJvFormPropsDlg.Create(Application) do
  try
    FCompOwner := ACompOwner;
    FDesigner := ADesigner;
    Screen.Cursor := crHourGlass;
    try
      UpdateStoredList(ACompOwner, AStoredList, False);
      SetStoredList(AStoredList);
      ActiveCtrlBox.Checked := fpActiveControl in Options;
      SizeBox.Checked := fpSize in Options;
      LocationBox.Checked := fpLocation in Options;
      StateBox.Checked := fpState in Options;
    finally
      Screen.Cursor := crDefault;
    end;
    Result := ShowModal = mrOk;
    if Result then
    begin
      AStoredList.Assign(StoredList.Items);
      Options := [];
      if ActiveCtrlBox.Checked then
        Include(Options, fpActiveControl);
      if SizeBox.Checked then
        Include(Options, fpSize);
      if LocationBox.Checked then
        Include(Options, fpLocation);
      if StateBox.Checked then
        Include(Options, fpState);
    end;
  finally
    Free;
  end;
end;

procedure TJvFormPropsDlg.ListToIndex(List: TCustomListBox; Idx: Integer);

  procedure SetItemIndex(Index: Integer);
  begin
    if TListBox(List).MultiSelect then
      TListBox(List).Selected[Index] := True;
    List.ItemIndex := Index;
  end;

begin
  if Idx < List.Items.Count then
    SetItemIndex(Idx)
  else
  if Idx - 1 < List.Items.Count then
    SetItemIndex(Idx - 1)
  else
  if List.Items.Count > 0 then
    SetItemIndex(0);
end;

procedure TJvFormPropsDlg.UpdateCurrent;
var
  IdxProp: Integer;
  List: TStrings;
begin
  IdxProp := PropertiesList.ItemIndex;
  if IdxProp < 0 then
    IdxProp := 0;
  if ComponentsList.Items.Count <= 0 then
  begin
    PropertiesList.Clear;
    Exit;
  end;
  if ComponentsList.ItemIndex < 0 then
    ComponentsList.ItemIndex := 0;
  List := TStrings(ComponentsList.Items.Objects[ComponentsList.ItemIndex]);
  if List.Count > 0 then
    PropertiesList.Items := List
  else
    PropertiesList.Clear;
  ListToIndex(PropertiesList, IdxProp);
  CheckButtons;
end;

procedure TJvFormPropsDlg.DeleteProp(I: Integer);
var
  CompName, PropName: string;
  IdxComp, IdxProp, Idx: Integer;
  StrList: TStringList;
begin
  Idx := StoredList.ItemIndex;
  if ParseStoredItem(StoredList.Items[I], CompName, PropName) then
  begin
    StoredList.Items.Delete(I);
    if FDesigner <> nil then
      FDesigner.Modified;
    ListToIndex(StoredList, Idx);
    {I := ComponentsList.ItemIndex;}
    if not FindProp(CompName, PropName, IdxComp, IdxProp) then
    begin
      if IdxComp < 0 then
      begin
        StrList := TStringList.Create;
        try
          StrList.Add(PropName);
          ComponentsList.Items.AddObject(CompName, StrList);
          ComponentsList.ItemIndex := ComponentsList.Items.IndexOf(CompName);
        except
          StrList.Free;
          raise;
        end;
      end
      else
        TStrings(ComponentsList.Items.Objects[IdxComp]).Add(PropName);
      UpdateCurrent;
    end;
  end;
end;

function TJvFormPropsDlg.FindProp(const CompName, PropName: string;
  var IdxComp, IdxProp: Integer): Boolean;
begin
  Result := False;
  IdxComp := ComponentsList.Items.IndexOf(CompName);
  if IdxComp >= 0 then
  begin
    IdxProp := TStrings(ComponentsList.Items.Objects[IdxComp]).IndexOf(PropName);
    if IdxProp >= 0 then
      Result := True;
  end;
end;

procedure TJvFormPropsDlg.ClearLists;
var
  I: Integer;
begin
  for I := 0 to ComponentsList.Items.Count - 1 do
    ComponentsList.Items.Objects[I].Free;
  ComponentsList.Items.Clear;
  ComponentsList.Clear;
  PropertiesList.Clear;
  StoredList.Clear;
end;

procedure TJvFormPropsDlg.AddItem(IdxComp, IdxProp: Integer; AUpdate: Boolean);
var
  Idx: Integer;
  StrList: TStringList;
  CompName, PropName: string;
  Component: TComponent;
begin
  CompName := ComponentsList.Items[IdxComp];
  Component := FCompOwner.FindComponent(CompName);
  if Component = nil then
    Exit;
  StrList := TStringList(ComponentsList.Items.Objects[IdxComp]);
  PropName := StrList[IdxProp];
  StrList.Delete(IdxProp);
  if StrList.Count = 0 then
  begin
    Idx := ComponentsList.ItemIndex;
    StrList.Free;
    ComponentsList.Items.Delete(IdxComp);
    ListToIndex(ComponentsList, Idx);
  end;
  StoredList.Items.AddObject(CreateStoredItem(CompName, PropName), Component);
  if FDesigner <> nil then
    FDesigner.Modified;
  StoredList.ItemIndex := StoredList.Items.Count - 1;
  if AUpdate then
    UpdateCurrent;
end;

procedure TJvFormPropsDlg.CheckAddItem(const CompName, PropName: string);
var
  IdxComp, IdxProp: Integer;
begin
  if FindProp(CompName, PropName, IdxComp, IdxProp) then
    AddItem(IdxComp, IdxProp, True);
end;

procedure TJvFormPropsDlg.BuildLists(StoredProps: TStrings);
var
  I, J: Integer;
  C: TComponent;
  List: TJvPropInfoList;
  StrList: TStringList;
  CompName, PropName: string;
begin
  ClearLists;
  if FCompOwner <> nil then
  begin
    for I := 0 to FCompOwner.ComponentCount - 1 do
    begin
      C := FCompOwner.Components[I];
      if (C is TJvFormPlacement) or (C.Name = '') then
        Continue;
      List := TJvPropInfoList.Create(C, tkProperties);
      try
        StrList := TStringList.Create;
        try
          StrList.Sorted := True;
          for J := 0 to List.Count - 1 do
            StrList.Add(List.Items[J]^.Name);
          ComponentsList.Items.AddObject(C.Name, StrList);
        except
          StrList.Free;
          raise;
        end;
      finally
        List.Free;
      end;
    end;
    if StoredProps <> nil then
    begin
      for I := 0 to StoredProps.Count - 1 do
        if ParseStoredItem(StoredProps[I], CompName, PropName) then
          CheckAddItem(CompName, PropName);
      ListToIndex(StoredList, 0);
    end;
  end
  else
    StoredList.Items.Clear;
  UpdateCurrent;
end;

procedure TJvFormPropsDlg.SetStoredList(AList: TStrings);
begin
  BuildLists(AList);
  if ComponentsList.Items.Count > 0 then
    ComponentsList.ItemIndex := 0;
  CheckButtons;
end;

procedure TJvFormPropsDlg.CheckButtons;
var
  Enable: Boolean;
begin
  AddButton.Enabled := (ComponentsList.ItemIndex >= 0) and
    (PropertiesList.ItemIndex >= 0);
  Enable := (StoredList.Items.Count > 0) and (StoredList.ItemIndex >= 0);
  DeleteButton.Enabled := Enable;
  ClearButton.Enabled := Enable;
  UpBtn.Enabled := Enable and (StoredList.ItemIndex > 0);
  DownBtn.Enabled := Enable and (StoredList.ItemIndex < StoredList.Items.Count - 1);
end;

procedure TJvFormPropsDlg.AddButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if PropertiesList.SelCount > 0 then
  begin
    for I := PropertiesList.Items.Count - 1 downto 0 do
      if PropertiesList.Selected[I] then
        AddItem(ComponentsList.ItemIndex, I, False);
    UpdateCurrent;
  end
  else
    AddItem(ComponentsList.ItemIndex, PropertiesList.ItemIndex, True);
  CheckButtons;
end;

procedure TJvFormPropsDlg.ClearButtonClick(Sender: TObject);
begin
  if StoredList.Items.Count > 0 then
  begin
    SetStoredList(nil);
    if FDesigner <> nil then
      FDesigner.Modified;
  end;
end;

procedure TJvFormPropsDlg.DeleteButtonClick(Sender: TObject);
begin
  DeleteProp(StoredList.ItemIndex);
end;

procedure TJvFormPropsDlg.ListClick(Sender: TObject);
begin
  if Sender = ComponentsList then
    UpdateCurrent
  else
    CheckButtons;
end;

procedure TJvFormPropsDlg.FormDestroy(Sender: TObject);
begin
  ClearLists;
end;

procedure TJvFormPropsDlg.StoredListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvFormPropsDlg.UpBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := StoredList.ItemIndex;
  if I > 0 then
  begin
    StoredList.Items.Exchange(I, I - 1);
    StoredList.ItemIndex := I - 1;
  end;
//  BoxMoveFocusedItem(StoredList, StoredList.ItemIndex - 1);
  if FDesigner <> nil then
    FDesigner.Modified;
  CheckButtons;
end;

procedure TJvFormPropsDlg.DownBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := StoredList.ItemIndex;
  if I < StoredList.Items.Count - 1 then
  begin
    StoredList.Items.Exchange(I, I + 1);
    StoredList.ItemIndex := I + 1;
  end;
//  BoxMoveFocusedItem(StoredList, StoredList.ItemIndex + 1);
  if FDesigner <> nil then
    FDesigner.Modified;
  CheckButtons;
end;

procedure TJvFormPropsDlg.StoredListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
//  BoxDragOver(StoredList, Source, X, Y, State, Accept, StoredList.Sorted);
  CheckButtons;
end;

procedure TJvFormPropsDlg.StoredListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
//  BoxMoveFocusedItem(StoredList, StoredList.ItemAtPos(Point(X, Y), True));
  if FDesigner <> nil then
    FDesigner.Modified;
  CheckButtons;
end;

procedure TJvFormPropsDlg.PropertiesListDblClick(Sender: TObject);
begin
  if AddButton.Enabled then
    AddButtonClick(nil);
end;

end.

