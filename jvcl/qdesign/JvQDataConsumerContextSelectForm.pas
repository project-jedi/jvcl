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

The Original Code is: JvDataConsumerContextSelectForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDataConsumerContextSelectForm;

{$I jvcl.inc}

interface

uses
  SysUtils,  
  QGraphics, QControls, QForms, QDialogs,
  QExtCtrls, QStdCtrls, Types, 
  JvQBaseDsgnForm, JvQProviderTreeListFrame, JvQDataProviderIntf, Classes;

type
  TfrmDataConsumerContextSelect = class(TJvBaseDesign)
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    fmeTreeList: TfmeJvProviderTreeList;
    procedure btnOkClick(Sender: TObject);
  private
    FConsumer: IJvDataConsumer;
  public
    property Consumer: IJvDataConsumer read FConsumer write FConsumer;
  end;

function ConsumerSelectContext(AConsumer: IJvDataConsumer): Boolean;

implementation

uses
  JvQContextProvider, JvQTypes, JvQDsgnConsts;



{$R *.xfm}


function ConsumerSelectContext(AConsumer: IJvDataConsumer): Boolean;
var
  SelectForm: TfrmDataConsumerContextSelect;
  CtxProvider: TJvContextProvider;
  ConsumerProvider: IJvDataConsumerProvider;
  ConsumerContext: IJvDataConsumerContext;
  CtxItem: IJvDataItem;
begin
  Result := False;
  SelectForm := TfrmDataConsumerContextSelect.Create(Application);
  try
    SelectForm.Consumer := AConsumer;
    CtxProvider := TJvContextProvider.Create(SelectForm);
    if Supports(AConsumer, IJvDataConsumerProvider, ConsumerProvider) then
    begin
      CtxProvider.ProviderIntf := ConsumerProvider.GetProvider;
      SelectForm.fmeTreeList.Provider.SetProviderIntf(CtxProvider);
      // Selecting current context (using IJvDataContextSearch)
      if Supports(AConsumer, IJvDataConsumerContext, ConsumerContext) then
      begin
        if ConsumerContext.GetContext <> nil then
        begin
          CtxItem := ((CtxProvider as IJvDataItems) as IJvDataContextSearch).Find(
            ConsumerContext.GetContext, True);
          if CtxItem <> nil then
            SelectForm.fmeTreeList.SelectItemID(CtxItem.GetID);
        end;
        Result := SelectForm.ShowModal = mrOk;
      end
      else
        raise EJVCLException.CreateRes(@RsEConsumerDoesNotSupportContextSelect);
    end
    else
      raise EJVCLException.CreateRes(@RsEIJvDataConsumerProviderIsNotSupported);
  finally
    SelectForm.Free;
  end;
end;

procedure TfrmDataConsumerContextSelect.btnOkClick(Sender: TObject);
var
  ConsumerContext: IJvDataConsumerContext;
  CtxItem: IJvDataItem;
  ContextItemInfo: IJvDataContextItem;
begin
  if Supports(Consumer, IJvDataConsumerContext, ConsumerContext) then
  begin
    CtxItem := fmeTreeList.GetDataItem(fmeTreeList.GetSelectedIndex);
    if CtxItem <> nil then
    begin
      if Supports(CtxItem, IJvDataContextItem, ContextItemInfo) then
        ConsumerContext.SetContext(ContextItemInfo.GetContext);
    end
    else
      ConsumerContext.SetContext(nil);
  end;
end;

end.
