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

The Original Code is: JvColorProviderDesignerForm.pas, released on --.

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

unit JvQColorProviderDesignerForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QButtons, QActnList,
  Types,  
  DesignIntf, DesignEditors, 
  JvQBaseDsgnForm, JvQProviderTreeListFrame, JvQComponent,
  JvQDataProvider, JvQDataProviderIntf, JvQContextProvider,
  JvQProviderTreeListDsgnFrame, JvQColorProvider, JvQDsgnTypes,
  JvQColorProviderDsgnTreeFrame;

type
  TfrmJvColorProviderDesigner = class(TJvBaseDesign)
    lblColors: TLabel;
    lblMappings: TLabel;
    lblContext: TLabel;
    dpContexts: TJvContextProvider;
    btnOK: TButton;
    fmeColors: TfmeJvColorProviderDsgnTree;
    fmeMappings: TfmeJvProviderTreeListDsgn;
    fmeContexts: TfmeJvProviderTreeListDsgn;
    dpColorMapping: TJvColorMappingProvider;
    procedure btnOKClick(Sender: TObject);
  private
    FDesigner: IJvFormDesigner;
    FMappingConsumer: TJvDataConsumer;
    FCtxConsumer: TJvDataConsumer;
    FNewCtxResult: Integer;
    function GetProvider: IJvDataProvider;
    procedure SetProvider(Value: IJvDataProvider);
    procedure SetDesigner(Value: IJvFormDesigner);
  protected
    function DesignConsumer: TJvDataConsumer;
    function MappingConsumer: TJvDataConsumer;
    function CtxConsumer: TJvDataConsumer;
    procedure BeforeNewContext(Sender: TObject; Kind: Integer; var Allow: Boolean);
    procedure AfterNewContext(Sender: TObject; Item: IJvDataItem);
    procedure ColorProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property Provider: IJvDataProvider read GetProvider write SetProvider;
    property Designer: IJvFormDesigner read FDesigner write SetDesigner;
  end;

procedure DesignColorProvider(AProvider: IJvDataProvider; ADesigner: IJvFormDesigner);

implementation

uses
  JvQConsts, JvQDsgnConsts;



{$R *.xfm}


function IsColorProviderDesingForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvColorProviderDesigner;
  if Result then
    with Form as TfrmJvColorProviderDesigner do
      Result := (Pointer(Provider) = Args[0].VInterface) and
        (Pointer(Designer) = Args[1].VInterface);
end;

procedure DesignColorProvider(AProvider: IJvDataProvider; ADesigner: IJvFormDesigner);
var
  Form: TfrmJvColorProviderDesigner;
begin
  Form := TfrmJvColorProviderDesigner(GetDesignerForm(IsColorProviderDesingForm,
    [AProvider, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvColorProviderDesigner.Create(nil);
    try
      Form.Provider := AProvider;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//=== { TfrmJvColorProviderDesigner } ========================================

function TfrmJvColorProviderDesigner.GetProvider: IJvDataProvider;
begin
  if not (csDestroying in ComponentState) then
    Result := fmeColors.Provider.ProviderIntf
  else
    Result := nil;
end;

procedure TfrmJvColorProviderDesigner.SetProvider(Value: IJvDataProvider);
var
  ICR: IInterfaceComponentReference;
  ColorSettings: IJvColorProviderSettings;
  VL: IJvDataConsumerViewList;
begin
  (MappingConsumer as IJvDataConsumerServerNotify).RemoveClient(DesignConsumer);
  (CtxConsumer as IJvDataConsumerServerNotify).RemoveClient(DesignConsumer);
  DesignConsumer.SetProviderIntf(Value);
  if (Value <> nil) and Supports(DesignConsumer as IJvDataConsumer, IJvColorProviderSettings, ColorSettings) then
  begin
    ColorSettings.ColorBoxSettings.Active := False;
    with ColorSettings.TextSettings do
    begin
      Active := True;
      ShowHex := False;
      ShowRGB := False;
      ShowName := True;
    end;
    with ColorSettings.SystemColorSettings do
    begin
      Active := True;
      Caption := RsSystemColors;
      ShowHeader := True;
    end;
    with ColorSettings.StandardColorSettings do
    begin
      Active := True;
      Caption := RsStandardColors;
      ShowHeader := True;
    end;
    with ColorSettings.CustomColorSettings do
    begin
      Active := True;
      Caption := RsCustomColorsEllipsis;
      ShowHeader := True;
      AddColorSettings.Location := ailUseHeader;
    end;
    with ColorSettings.GroupingSettings do
    begin
      Active := True;
      FlatList := False;
      HeaderStyle := [ghsBoldFont, ghsDoubleCenterLine];
    end;
  end;
  dpColorMapping.ProviderIntf := Value as IJvColorProvider;
  dpContexts.ProviderIntf := Value;
  if Value <> nil then
  begin
    (MappingConsumer as IJvDataConsumerServerNotify).AddClient(DesignConsumer);
    (CtxConsumer as IJvDataConsumerServerNotify).AddClient(DesignConsumer);
    if (dpColorMapping as IJvDataProvider).GetItems.GetCount > 0 then
      fmeMappings.SelectItemID((dpColorMapping as IJvDataProvider).GetItems.GetItem(0).GetID);
    if (dpContexts as IJvDataProvider).GetItems.GetCount > 0 then
      fmeContexts.SelectItemID((dpContexts as IJvDataProvider).GetItems.GetItem(0).GetID);
  end;
  if Supports(DesignConsumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
    VL.ExpandOnNewItem := True;
  if Supports(Provider, IInterfaceComponentReference, ICR) then
    Caption := Format(RsDesigning, [ICR.GetComponent.Name])
  else
    Caption := Format(RsDesigning, [RsNone]);
end;

procedure TfrmJvColorProviderDesigner.SetDesigner(Value: IJvFormDesigner);
begin
  if Value <> FDesigner then
  begin
    FDesigner := Value;
    fmeColors.Designer := Value;
    fmeMappings.Designer := Value;
    fmeContexts.Designer := Value;
  end;
end;

function TfrmJvColorProviderDesigner.DesignConsumer: TJvDataConsumer;
begin
  if fmeColors <> nil then
    Result := fmeColors.Provider
  else
    Result := nil;
end;

function TfrmJvColorProviderDesigner.MappingConsumer: TJvDataConsumer;
begin
  if FMappingConsumer = nil then
  begin
    FMappingConsumer := TJvDataConsumer.Create(Self, [DPA_ConsumerDisplaysList]);
    FMappingConsumer.SetProviderIntf(dpColorMapping);
  end;
  Result := FMappingConsumer;
end;

function TfrmJvColorProviderDesigner.CtxConsumer: TJvDataConsumer;
begin
  if FCtxConsumer = nil then
  begin
    FCtxConsumer := TJvDataConsumer.Create(Self, [DPA_ConsumerDisplaysList]);
    FCtxConsumer.SetProviderIntf(dpContexts);
  end;
  Result := FCtxConsumer;
end;

procedure TfrmJvColorProviderDesigner.BeforeNewContext(Sender: TObject; Kind: Integer; var Allow: Boolean);
begin
  if Kind = -1 then
  begin
    FNewCtxResult := MessageDlg(RsColorMsg, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    Allow := FNewCtxResult in [mrYes, mrNo];
  end;
end;

type
  TOpenColorProvider = class(TJvColorProvider);

procedure TfrmJvColorProviderDesigner.AfterNewContext(Sender: TObject; Item: IJvDataItem);
var
  CtxItem: IJvDataContextItem;
  NewCtxIdx: Integer;
  CPImpl: TJvColorProvider;
begin
  if (FNewCtxResult = mrYes) and Supports(Item, IJvDataContextItem, CtxItem) then
  begin
    NewCtxIdx := CtxItem.GetContext.Contexts.IndexOf(CtxItem.GetContext);
    CPImpl := TJvColorProvider(CtxItem.GetContext.Contexts.Provider.GetImplementer);
    TOpenColorProvider(CPImpl).CopyFromDefCtx(NewCtxIdx);
  end;
end;

procedure TfrmJvColorProviderDesigner.ColorProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if AReason = pcrDestroy then
    Close;
end;

procedure TfrmJvColorProviderDesigner.Loaded;
begin
  inherited Loaded;
  if fmeColors <> nil then
    with fmeColors do
    begin
      UseVirtualRoot := False;
      Provider.OnProviderChanging := ColorProviderChanging;
    end;
  if fmeMappings <> nil then
    with fmeMappings do
    begin
      UseVirtualRoot := False;
      Provider.Slave := MappingConsumer;
    end;
  if fmeContexts <> nil then
    with fmeContexts do
    begin
      UseVirtualRoot := False;
      Provider.Slave := CtxConsumer;
      BeforeNewItem := BeforeNewContext;
      AfterNewItem := AfterNewContext;
    end;
end;

procedure TfrmJvColorProviderDesigner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

destructor TfrmJvColorProviderDesigner.Destroy;
begin
  FreeAndNil(FCtxConsumer);
  FreeAndNil(FMappingConsumer);
  inherited Destroy;
end;

procedure TfrmJvColorProviderDesigner.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Provider := nil;
end;

procedure TfrmJvColorProviderDesigner.btnOKClick(Sender: TObject);
begin
  Close;
end;

end.
