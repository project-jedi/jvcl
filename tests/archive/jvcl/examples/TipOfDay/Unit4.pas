{$I JVCL.INC}
unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvBaseDlg, JvTipOfDay;

type
  TForm4 = class(TForm)
    JvTip: TJvTipOfDay;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    cbStyle: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FileDirectory: string;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button2Click(Sender: TObject);
begin
  JvTip.Style := TJvTipOfDayStyle(cbStyle.ItemIndex);
  JvTip.Execute;
  if toShowOnStartUp in JvTip.Options then
    ShowMessage('You want to see tips the next time the application is started')
  else
    ShowMessage('You don''t want to see tips the next time the application is started')
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := FileDirectory;
  if OpenDialog1.Execute then
    JvTip.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := FileDirectory;
  if SaveDialog1.Execute then
    JvTip.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FileDirectory := ExtractFileDir(Application.ExeName);
  cbStyle.ItemIndex := 0;
end;

end.

