unit AskDtxCheckU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvAppStorage, JvAppRegistryStorage, StdCtrls,
  ActnList,
  DelphiParser;

type
  TfrmAskDtxCheck = class(TForm)
    lblVisibility: TLabel;
    chbDefaultText: TCheckBox;
    chbLinks: TCheckBox;
    chbParameters: TCheckBox;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    Button1: TButton;
    Button2: TButton;
    JvAppRegistryStore1: TJvAppRegistryStorage;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  private
    FOptions: TDtxParseOptions;
  protected
    procedure Init;
    procedure Load;
    procedure Save;
    procedure Uitvoeren;
  public
    class function Execute(var AOptions: TDtxParseOptions): Boolean;
  end;

implementation

uses
  TypInfo;

{$R *.dfm}

procedure TfrmAskDtxCheck.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmAskDtxCheck.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TfrmAskDtxCheck.Execute(
  var AOptions: TDtxParseOptions): Boolean;
begin
  with TfrmAskDtxCheck.Create(Application) do
  try
    FOptions := AOptions;
    Init;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Uitvoeren;
      AOptions := FOptions;
    end;
  finally
    Free;
  end;
end;

procedure TfrmAskDtxCheck.Load;
begin
  JvAppRegistryStore1.ReadSet('DtxCheckOptions\Options', TypeInfo(TDtxParseOptions), FOptions, FOptions);
end;

procedure TfrmAskDtxCheck.Save;
begin
  JvAppRegistryStore1.WriteSet('DtxCheckOptions\Options', TypeInfo(TDtxParseOptions), FOptions);
end;

procedure TfrmAskDtxCheck.Init;
begin
  Load;

  chbLinks.Checked := dpoLinks in FOptions;
  chbParameters.Checked := dpoParameters in FOptions;
  chbDefaultText.Checked := dpoDefaultText in FOptions;
end;

procedure TfrmAskDtxCheck.Uitvoeren;
begin
  FOptions := [];
  if chbLinks.Checked then
    Include(FOptions, dpoLinks);
  if chbParameters.Checked then
    Include(FOptions, dpoParameters);
  if chbDefaultText.Checked then
    Include(FOptions, dpoDefaultText);

  Save;
end;

end.

