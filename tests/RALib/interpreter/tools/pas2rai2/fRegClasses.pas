unit fRegClasses;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvRegAuto;

type
  TRegClasses = class(TForm)
    memClasses: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RegAuto1: TJvRegAuto;
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegClasses: TRegClasses;

implementation

{$R *.DFM}

procedure TRegClasses.RegAuto1AfterLoad(Sender: TObject);
begin
  try
    memClasses.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
  except
    memClasses.Lines.Add('TObject');
    memClasses.Lines.Add('TStream');
  end;
end;

procedure TRegClasses.RegAuto1AfterSave(Sender: TObject);
begin
  memClasses.Lines.SaveToFile(ExtractFilePath(Application.ExeName) + 'classes.ini');
end;

end.
