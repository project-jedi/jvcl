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

The Original Code is: JvDataContextManagerForm.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDataContextManagerForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  QWindows, QMessages, QGraphics, QControls, QForms, QDialogs, 
  DesignIntf, DesignEditors, 
  JvQDataProviderDesignerForm, QMenus, QActnList,
  JvQProviderTreeListFrame, JvQBaseDsgnFrame, JvQBaseDsgnToolbarFrame,
  JvQStdToolbarDsgnFrame, JvQProviderToolbarFrame, JvQDataProviderIntf,
  JvQDsgnTypes;

type
  TfrmJvDataContextManager = class(TfrmDataProviderDesigner)
    procedure FormDestroy(Sender: TObject);
  protected
    function GetProvider: IJvDataProvider; override;
    procedure SetProvider(Value: IJvDataProvider); override;
    procedure Loaded; override;
    function DesignerFormName: string; override;
  public
  end;

procedure ManageProviderContexts(AProvider: IJvDataProvider;
  ADesigner: IJvFormDesigner; PropName: string);

implementation

uses
  JvQContextProvider, JvQBaseDsgnForm, JvQDataProvider, JvQDsgnConsts;

{$R *.xfm}

function IsContextDesignForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvDataContextManager;
  if Result then
    with Form as TfrmJvDataContextManager do
      Result := (Pointer(Provider) = Args[0].VInterface) and
        (Pointer(Designer) = Args[1].VInterface);
end;

procedure ManageProviderContexts(AProvider: IJvDataProvider;
  ADesigner: IJvFormDesigner; PropName: string);
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
      raise;
    end;
  end;
  Form.Show; 
  // If you leave this code under BCB5, you will get this error when
  // you link the package:
  // Fatal: Type index 2147479547 is bad in module ..\..\design\JvDataContextManagerForm.pas
  Form.BringToFront; 
end;

//=== { TJvContextRootItem } =================================================

type
  TJvContextRootItem = class(TJvBaseDataItem)
  protected
    procedure InitID; override;
  public
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

procedure TJvContextRootItem.InitID;
begin
  SetID(RsDataItemRootID);
end;

function TJvContextRootItem.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
    Result := TExtensibleInterfacedPersistent(GetItems.GetImplementer).GetInterface(IID, Obj);
end;

//=== { TfrmJvDataContextManager } ===========================================

function TfrmJvDataContextManager.GetProvider: IJvDataProvider;
var
  ICR: IInterfaceComponentReference;
  CtxProv: TJvContextProvider;
begin
  if Supports(InternalProvider, IInterfaceComponentReference, ICR) then
  begin
    CtxProv := TJvContextProvider(ICR.GetComponent); 
    Result := CtxProv.Provider; 
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
  if csDestroying in ComponentState then
    Exit;
  if (InternalProvider <> nil) and (Value <> GetProvider) then
  begin
    if Supports(InternalProvider, IInterfaceComponentReference, ICR) then
    begin
      if Value <> nil then
        FRootItem := TJvContextRootItem.Create(InternalProvider as IJvDataItems);
      CtxProv := TJvContextProvider(ICR.GetComponent); 
      CtxProv.Provider := Value; 
    end;
    if Supports(fmeTreeList.Provider as IJvDataConsumer, IJvDataConsumerViewList, ViewList) then
      ViewList.RebuildView;
    ProviderImpl := (Provider as IInterfaceComponentReference).GetComponent;
    Caption := Format(RsDataProviderContextManCaption, [ProviderImpl.Name, PropName]);
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
  Result := RsDataProviderContextManager;
end;

procedure TfrmJvDataContextManager.FormDestroy(Sender: TObject);
begin
  FRootItem := nil;
end;

end.
