unit JvDataConsumerContextSelectForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDsgnForm, ExtCtrls, StdCtrls, JvProviderTreeListFrame,
  JvDataProvider;

type
  TfrmDataConsumerContextSelect = class(TJvBaseDesign)
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    fmeTreeList: TfmeJvProviderTreeList;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FConsumer: IJvDataConsumer;
  public
    { Public declarations }
    property Consumer: IJvDataConsumer read FConsumer write FConsumer;
  end;

function ConsumerSelectContext(AConsumer: IJvDataConsumer): Boolean;


resourcestring
  sConsumerDoesNotSupportContextSelect = 'Consumer does not support context selection.';
  sIJvDataConsumerProviderIsNotSupport = 'IJvDataConsumerProvider is not supported by the specified consumer.';

implementation

uses
  JvContextProvider, JvTypes;

{$R *.DFM}

function ConsumerSelectContext(AConsumer: IJvDataConsumer): Boolean;
var
  SelectForm: TfrmDataConsumerContextSelect;
  CtxProvider: TJvContextProvider;
  ConsumerProvider: IJvDataConsumerProvider;
  ConsumerContext: IJvDataConsumerContext;
  CtxItem: IJvDataItem;
begin
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
        raise EJVCLException.Create(sConsumerDoesNotSupportContextSelect);
    end
    else
      raise EJVCLException.Create(sIJvDataConsumerProviderIsNotSupport);
  except
    SelectForm.Free;
    raise;
  end;
end;

procedure TfrmDataConsumerContextSelect.btnOkClick(Sender: TObject);
var
  ConsumerContext: IJvDataConsumerContext;
  CtxItem: IJvDataItem;
  ContextItemInfo: IContextItem;
begin
  if Supports(Consumer, IJvDataConsumerContext, ConsumerContext) then
  begin
    CtxItem := fmeTreeList.GetDataItem(fmeTreeList.GetSelectedIndex);
    if (CtxItem <> nil) then
    begin
      if Supports(CtxItem, IContextItem, ContextItemInfo) then
        ConsumerContext.SetContext(ContextItemInfo.GetContext);
    end
    else
      ConsumerContext.SetContext(nil);
  end;
end;

end.
