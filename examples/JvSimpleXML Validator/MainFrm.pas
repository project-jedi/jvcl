unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvComponent, JvSimpleXml, ExtCtrls;

type
  TForm1 = class(TForm)
    JvSimpleXml1: TJvSimpleXml;
    reXML: TRichEdit;
    btnLoad: TButton;
    btnValidate: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    sbResults: TStatusBar;
    procedure btnLoadClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure reXMLChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    reXML.Lines.LoadFromFile(OpenDialog1.Filename);
end;

procedure TForm1.btnValidateClick(Sender: TObject);
begin
  try
    JvSimpleXML1.LoadFromString(reXML.Lines.Text);
    sbResults.Panels[0].Text := 'No errors encountered in XML';
  except
    on E:Exception do
      sbResults.Panels[0].Text := E.Message;
  end;
end;

procedure TForm1.reXMLChange(Sender: TObject);
begin
  btnValidate.Enabled := reXML.GetTextLen > 0;
end;

end.
