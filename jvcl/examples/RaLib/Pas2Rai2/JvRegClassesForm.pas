unit JvRegClassesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TJvRegClasses  = class(TForm)
    memClasses: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
  public
  end;

var
  RegClasses: TJvRegClasses;

implementation

{$R *.DFM}

procedure TJvRegClasses.RegAuto1AfterLoad(Sender: TObject);
begin
  try
    memClasses.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
  except
    memClasses.Lines.Add('TObject');
    memClasses.Lines.Add('TStream');
  end;
end;

procedure TJvRegClasses.RegAuto1AfterSave(Sender: TObject);
begin
  memClasses.Lines.SaveToFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
end;

end.
