{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataConsumerItemSelectForm.pas, released on --.

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

unit JvDataConsumerItemSelectForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, Types,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvBaseDsgnForm, JvProviderTreeListFrame, JvDataProvider, JvDataProviderIntf,
  JvDsgnTypes;

type
  TfrmJvDataConsumerItemSelect = class(TJvBaseDesign)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    fmeTreeList: TfmeJvProviderTreeList;
    procedure fmeTreeListlvProviderDblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FConsumer: TJvDataConsumer;
    FDesigner: IJvFormDesigner;
  protected
    function GetConsumer: TJvDataConsumer;
    procedure SetConsumer(Value: TJvDataConsumer);
    procedure SetDesigner(Value: IJvFormDesigner);
    procedure SelectionChanged(Sender: TObject);
    procedure UpdateViewList;
    procedure UpdateConsumerSvc;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DesignerFormName: string; override;
    function AutoStoreSettings: Boolean; override;
  public
    property Consumer: TJvDataConsumer read GetConsumer write SetConsumer;
    property Designer: IJvFormDesigner read FDesigner write SetDesigner;
  end;

procedure DataConsumerSelectItem(AConsumer: TJvDataConsumer;
  ADesigner: IJvFormDesigner);

implementation

uses
  JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

function IsConsumerItemSelectForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvDataConsumerItemSelect;
  if Result then
    with Form as TfrmJvDataConsumerItemSelect do
      Result := (Consumer = Args[0].VObject) and (Pointer(Designer) = Args[1].VInterface);
end;

procedure DataConsumerSelectItem(AConsumer: TJvDataConsumer;
  ADesigner: IJvFormDesigner);
var
  Form: TfrmJvDataConsumerItemSelect;
begin
  Form := TfrmJvDataConsumerItemSelect(GetDesignerForm(IsConsumerItemSelectForm, [AConsumer, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvDataConsumerItemSelect.Create(nil);
    try
      Form.Consumer := AConsumer;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//=== { TfrmJvDataConsumerItemSelect } =======================================

function TfrmJvDataConsumerItemSelect.GetConsumer: TJvDataConsumer;
begin
  Result := FConsumer;
end;

type
  TOpenConsumer = class(TJvDataConsumer);

procedure TfrmJvDataConsumerItemSelect.SetConsumer(Value: TJvDataConsumer);
begin
  if Value <> Consumer then
  begin
    if Consumer <> nil then
      TOpenConsumer(Consumer).VCLComponent.RemoveFreeNotification(Self);
    FConsumer := Value;
    fmeTreeList.Provider.Slave := Value;
    if Value <> nil then
    begin
{      fmeTreeList.Provider.SetProviderIntf(Value.ProviderIntf);
      fmeTreeList.Provider.SetContextIntf(Value.ContextIntf);}
      UpdateViewList;
      if TOpenConsumer(Value).VCLComponent <> nil then
        TOpenConsumer(Value).VCLComponent.FreeNotification(Self);
    end
    else
      fmeTreeList.Provider.SetProviderIntf(nil);
  end;
end;

procedure TfrmJvDataConsumerItemSelect.SetDesigner(Value: IJvFormDesigner);
begin
  if Value <> FDesigner then
    FDesigner := Value;
end;

procedure TfrmJvDataConsumerItemSelect.SelectionChanged(Sender: TObject);
begin
end;

procedure TfrmJvDataConsumerItemSelect.UpdateViewList;
var
  ViewList: IJvDataConsumerViewList;
  ItemSelect: IJvDataConsumerItemSelect;
begin
  if Supports(fmeTreeList.Provider as IJvDataConsumer, IJvDataConsumerViewList, ViewList) then
  begin
    fmeTreeList.Provider.Enter;
    try
      if ViewList.Count = 0 then
        ViewList.RebuildView;
      {$IFDEF VCL}
      fmeTreeList.lvProvider.Items.Count := ViewList.Count;
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      {TODO : CLX does not support virtual ListViews}
      {$ENDIF VisualCLX}
      if Supports(Consumer as IJvDataConsumer, IJvDataConsumerItemSelect, ItemSelect) then
        if ItemSelect.GetItem <> nil then
        begin
          ViewList.ExpandTreeTo(ItemSelect.GetItem);
          fmeTreeList.SelectItemID(ItemSelect.GetItem.GetID);
        end;
    finally
      fmeTreeList.Provider.Leave;
    end;
  end;
end;

procedure TfrmJvDataConsumerItemSelect.UpdateConsumerSvc;
var
  ItemSelect: IJvDataConsumerItemSelect;
  NewItem: IJvDataItem;
begin
  if Supports(Consumer as IJvDataConsumer, IJvDataConsumerItemSelect, ItemSelect) then
  begin
    if fmeTreeList.GetSelectedIndex >= 0 then
      NewItem := fmeTreeList.GetDataItem(fmeTreeList.GetSelectedIndex)
    else
      NewItem := nil;
    if NewItem <> ItemSelect.GetItem then
    begin
      ItemSelect.SetItem(NewItem);
      if Designer <> nil then
        Designer.Modified;
    end;
  end;
end;

procedure TfrmJvDataConsumerItemSelect.Loaded;
begin
  inherited Loaded;
  if fmeTreeList <> nil then
    with fmeTreeList do
    begin
      OnItemSelect := SelectionChanged;
      UseVirtualRoot := False;
    end;
end;

procedure TfrmJvDataConsumerItemSelect.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (Consumer <> nil) and (TOpenConsumer(Consumer).VCLComponent = AComponent) then
      Consumer := nil;
end;

function TfrmJvDataConsumerItemSelect.DesignerFormName: string;
begin
  Result := RsDataProviderItemSelector;
end;

function TfrmJvDataConsumerItemSelect.AutoStoreSettings: Boolean;
begin
  Result := True;
end;

procedure TfrmJvDataConsumerItemSelect.fmeTreeListlvProviderDblClick(
  Sender: TObject);
begin
//  fmeTreeList.lvProviderDblClick(Sender);
  UpdateConsumerSvc;
  Close;
end;

procedure TfrmJvDataConsumerItemSelect.btnOKClick(Sender: TObject);
begin
  UpdateConsumerSvc;
  Close;
end;

procedure TfrmJvDataConsumerItemSelect.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmJvDataConsumerItemSelect.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action in [caHide, caFree] then
    Consumer := nil;
end;

end.
