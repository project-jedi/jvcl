unit PrintFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DepWalkConsts, PersistForm, PersistSettings;

type
  TfrmPrint = class(TfrmPersistable)
    Label1: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  protected
    { IPersistSettings}
    procedure Load(Storage:TPersistStorage);override;
    procedure Save(Storage:TPersistStorage);override;
  public
    { Public declarations }
    class function Execute:boolean;
  end;


implementation

{$R *.dfm}

{ TfrmPrint }

class function TfrmPrint.Execute: boolean;
var Storage:TPersistStorage;
begin
  with self.Create(Application) do
  try
    Storage := GetStorage;
    try
      Load(Storage);
      Result := ShowModal = mrOK;
      if Result then
      begin
        Save(Storage);
        Storage.UpdateFile;
      end;
    finally
      Storage.Free;
    end;
  finally
    Free;
  end;
end;

procedure TfrmPrint.Load(Storage: TPersistStorage);
begin
  inherited;
  cbFormat.ItemIndex := Storage.ReadInteger('Printing','Print Format',0);
end;

procedure TfrmPrint.Save(Storage: TPersistStorage);
begin
  inherited;
  Storage.WriteInteger('Printing','Print Format',cbFormat.ItemIndex);
end;

end.
