unit PrintFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DepWalkConsts, PersistSettings;

type
  TfrmPrint = class(TForm, IPersistSettings)
    Label1: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
    { IPersistSettings}
    procedure Load(Storage:TPersistSettings);
    procedure Save(Storage:TPersistSettings);
  public
    { Public declarations }
    class function Execute(Options:TPersistSettings):boolean;
  end;


implementation

{$R *.dfm}

{ TfrmPrint }

class function TfrmPrint.Execute(Options:TPersistSettings): boolean;
begin
  with self.Create(Application) do
  try
    Load(Options);
    Result := ShowModal = mrOK;
    if Result then
      Save(Options);
  finally
    Free;
  end;
end;

procedure TfrmPrint.Load(Storage: TPersistSettings);
begin
  cbFormat.ItemIndex := Storage.ReadInteger('Printing','Print Format',0);
  Top := Storage.ReadInteger(ClassName, 'Top', (Screen.Height - ClientHeight) div 2);
  Left := Storage.ReadInteger(ClassName, 'Left', (Screen.Width - ClientWidth) div 2);
end;

procedure TfrmPrint.Save(Storage: TPersistSettings);
begin
  Storage.WriteInteger('Printing','Print Format',cbFormat.ItemIndex);
  if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
  begin
    Storage.WriteInteger(ClassName, 'Top', Top);
    Storage.WriteInteger(ClassName, 'Left', Left);
  end;
end;

end.
