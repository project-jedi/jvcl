unit StatsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, PersistSettings;

type
  TfrmUnitStats = class(TForm, IPersistSettings)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    reUsed: TRichEdit;
    Label3: TLabel;
    reUses: TRichEdit;
    btnOK: TButton;
  private
    { Private declarations }
    procedure Load(Storage:TPersistSettings);
    procedure Save(Storage:TPersistSettings);
  public
    { Public declarations }
    class procedure Execute(const UnitName:string;UsedByStrings,UsesStrings:TStrings);
  end;


implementation

{$R *.dfm}

{ TfrmUnitStats }

class procedure TfrmUnitStats.Execute(const UnitName: string; UsedByStrings,
  UsesStrings: TStrings);
begin
  with self.Create(Application) do
  try
    Caption := Format(Caption,[ExtractFilename(UnitName)]);
    edName.Text := UnitName;
    reUsed.Lines := UsedByStrings;
    reUses.Lines := UsesStrings;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmUnitStats.Load(Storage: TPersistSettings);
begin
  Top := Storage.ReadInteger(ClassName, 'Top', (Screen.Height - ClientHeight) div 2);
  Left := Storage.ReadInteger(ClassName, 'Left', (Screen.Width - ClientWidth) div 2);
end;

procedure TfrmUnitStats.Save(Storage: TPersistSettings);
begin
  if not IsZoomed(Handle) and not IsIconic(Application.Handle) then
  begin
    Storage.WriteInteger(ClassName, 'Top', Top);
    Storage.WriteInteger(ClassName, 'Left', Left);
  end;
end;

end.
