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

Last Modified: 2003-07-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataConsumerItemSelectForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDsgnForm, JvProviderTreeListFrame, StdCtrls, ExtCtrls,
  {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JvDataProvider, JvDataProviderImpl;

type
  TfrmJvDataConsumerItemSelect = class(TJvBaseDesign)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    fmeTreeList: TfmeJvProviderTreeList;
    procedure fmeTreeListlvProviderDblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FConsumer: TJvDataConsumer;
    FDesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF};
  protected
    { Protected declarations }
    function GetConsumer: TJvDataConsumer;
    procedure SetConsumer(Value: TJvDataConsumer);
    procedure SetDesigner(Value: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
    procedure SelectionChanged(Sender: TObject);
    procedure UpdateViewList;
    procedure UpdateConsumerSvc;
    procedure Loaded; override;
    function DesignerFormName: string; override;
    function AutoStoreSettings: Boolean; override;
  public
    { Public declarations }
    property Consumer: TJvDataConsumer read GetConsumer write SetConsumer;
    property Designer: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF} read FDesigner write SetDesigner;
  end;

procedure DataConsumerSelectItem(AConsumer: TJvDataConsumer;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});

implementation

{$R *.DFM}

function IsConsumerItemSelectForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvDataConsumerItemSelect;
  if Result then
  begin
    with (Form as TfrmJvDataConsumerItemSelect) do
      Result := (Consumer = Args[0].VObject) and
        (Pointer(Designer) = Args[1].VInterface);
  end;
end;

procedure DataConsumerSelectItem(AConsumer: TJvDataConsumer;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
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

//===TfrmJvDataConsumerItemSelect===================================================================

function TfrmJvDataConsumerItemSelect.GetConsumer: TJvDataConsumer;
begin
  Result := FConsumer;
end;

procedure TfrmJvDataConsumerItemSelect.SetConsumer(Value: TJvDataConsumer);
begin
  if Value <> Consumer then
  begin
    FConsumer := Value;
    if Value <> nil then
    begin
      fmeTreeList.Provider.SetProviderIntf(Value.ProviderIntf);
      fmeTreeList.Provider.SetContextIntf(Value.ContextIntf);
      UpdateViewList;
    end
    else
      fmeTreeList.Provider.SetProviderIntf(nil);
  end;
end;

procedure TfrmJvDataConsumerItemSelect.SetDesigner(Value: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
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
    if ViewList.Count = 0 then
      ViewList.RebuildView;
    fmeTreeList.lvProvider.Items.Count := ViewList.Count;
    if Supports(Consumer as IJvDataConsumer, IJvDataConsumerItemSelect, ItemSelect) then
    begin
      if ItemSelect.GetItem <> nil then
      begin
        ViewList.ExpandTreeTo(ItemSelect.GetItem);
        fmeTreeList.SelectItemID(ItemSelect.GetItem.GetID);
      end;
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

function TfrmJvDataConsumerItemSelect.DesignerFormName: string;
begin
  Result := 'DataProvider Item Selector';
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
  inherited;
  Close;
end;

end.
