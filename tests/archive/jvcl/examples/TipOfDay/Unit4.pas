{$I JVCL.INC}
unit Unit4;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvTipWin, JvTransBtn;

type
  TForm4 = class(TForm)
    JvTip: TJvTipWindow;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FileDirectory:string;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button2Click(Sender: TObject);
begin
JvTip.Execute;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
OpenDialog1.InitialDir:=FileDirectory;
if OpenDialog1.Execute then
JvTip.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
SaveDialog1.InitialDir:= FileDirectory;
if SaveDialog1.Execute then
JvTip.SaveToFile(SaveDialog1.FileName);

end;

procedure TForm4.FormCreate(Sender: TObject);

begin
FileDirectory:=ExtractFileDir(Application.ExeName);
end;

end.
