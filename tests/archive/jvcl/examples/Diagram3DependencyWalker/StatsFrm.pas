unit StatsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, PersistForm, PersistSettings;

type
  TfrmUnitStats = class(TfrmPersistable)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    reUsed: TRichEdit;
    Label3: TLabel;
    reUses: TRichEdit;
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute(const UnitName: string; UsedByStrings, UsesStrings: TStrings);
  end;


implementation

{$R *.dfm}

{ TfrmUnitStats }

class procedure TfrmUnitStats.Execute(const UnitName: string; UsedByStrings,
  UsesStrings: TStrings);
var Storage: TPersistStorage;
begin
  with self.Create(Application) do
  try
    Storage := GetStorage;
    try
      Load(Storage);
      Caption := Format(Caption, [ExtractFilename(UnitName)]);
      edName.Text := UnitName;
      reUsed.Lines := UsedByStrings;
      reUses.Lines := UsesStrings;
      ShowModal;
      Save(Storage);
      Storage.UpdateFile;
    finally
      Storage.Free;
    end;
  finally
    Free;
  end;
end;


end.

