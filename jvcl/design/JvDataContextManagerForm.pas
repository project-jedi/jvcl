{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataContextManagerForm.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

Last Modified: 2003-07-16

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataContextManagerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDataProviderDesignerForm, Menus, ActnList, JvProviderTreeListFrame,
  JvBaseDsgnFrame, JvBaseDsgnToolbarFrame, JvStdToolbarDsgnFrame,
  JvProviderToolbarFrame, {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JvDataProvider;

type
  TfrmJvDataContextManager = class(TfrmDataProviderDesigner)
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    function GetProvider: IJvDataProvider; override;
    procedure SetProvider(Value: IJvDataProvider); override;
    procedure Loaded; override;
    function DesignerFormName: string; override;
  public
    { Public declarations }
  end;

procedure ManageProviderContexts(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);

implementation

uses
  JvContextProvider,
  JvBaseDsgnForm, JvDataProviderImpl, JvDsgnConsts;

{$R *.DFM}

function IsContextDesignForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvDataContextManager;
  if Result then
  begin
    with (Form as TfrmJvDataContextManager) do
      Result := (Pointer(Provider) = Args[0].VInterface) and
        (Pointer(Designer) = Args[1].VInterface);
  end;
end;

procedure ManageProviderContexts(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);
var
  Form: TfrmJvDataContextManager;
begin
  Form := TfrmJvDataContextManager(GetDesignerForm(IsContextDesignForm, [AProvider, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvDataContextManager.Create(nil);
    try
      if PropName <> '' then
        Form.PropName := '.' + PropName;
      Form.Provider := AProvider;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

type
  TJvContextRootItem = class(TJvBaseDataItem)
  protected
    procedure InitID; override;
  public
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

procedure TJvContextRootItem.InitID;
begin
  SetID(SDataItemRootID);
end;

function TJvContextRootItem.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
    Result := TExtensibleInterfacedPersistent(GetItems.GetImplementer).GetInterface(IID, Obj);
end;

//===TfrmJvDataContextManager=======================================================================

function TfrmJvDataContextManager.GetProvider: IJvDataProvider;
var
  ICR: IInterfaceComponentReference;
  CtxProv: TJvContextProvider;
begin
  if Supports(InternalProvider, IInterfaceComponentReference, ICR) then
  begin
    CtxProv := TJvContextProvider(ICR.GetComponent);
    {$IFNDEF COMPILER6_UP}
    Supports(CtxProv.Provider, IJvDataProvider, Result);
    {$ELSE}
    Result := CtxProv.Provider
    {$ENDIF}
  end;
end;

procedure TfrmJvDataContextManager.SetProvider(Value: IJvDataProvider);
var
  ICR: IInterfaceComponentReference;
  CtxProv: TJvContextProvider;
  ViewList: IJvDataConsumerViewList;
  ProviderImpl: TComponent;
begin
  FRootItem := nil;
  if csDestroying in ComponentState then Exit;
  if (InternalProvider <> nil) and (Value <> GetProvider) then
  begin
    if Supports(InternalProvider, IInterfaceComponentReference, ICR) then
    begin
      if Value <> nil then
        FRootItem := TJvContextRootItem.Create(InternalProvider as IJvDataItems);
      CtxProv := TJvContextProvider(ICR.GetComponent);
      {$IFNDEF COMPILER6_UP}
      if Value = nil then
        CtxProv.Provider := nil
      else if Supports(Value, IInterfaceComponentReference, ICR) then
        CtxProv.Provider := ICR.GetComponent;
      {$ELSE}
      CtxProv.Provider := Value;
      {$ENDIF}
    end;
    if Supports(fmeTreeList.Provider as IJvDataConsumer, IJvDataConsumerViewList, ViewList) then
      ViewList.RebuildView;
    ProviderImpl := (Provider as IInterfaceComponentReference).GetComponent;
    Caption := Format(SDataProviderContextManCaption, [ProviderImpl.Name, PropName]);
  end;
  if InternalProvider <> nil then
  begin
    InitContexts;
    UpdateSelectedItem(Self);
  end;
end;

procedure TfrmJvDataContextManager.Loaded;
begin
  inherited Loaded;
  if (fmeTreeList <> nil) and (InternalProvider = nil) then
    fmeTreeList.Provider.SetProviderIntf(TJvContextProvider.Create(Self));
end;

function TfrmJvDataContextManager.DesignerFormName: string;
begin
  Result := 'DataProvider Context Manager';
end;

procedure TfrmJvDataContextManager.FormDestroy(Sender: TObject);
begin
  inherited;
  FRootItem := nil;
end;

end.
