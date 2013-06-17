unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvComponentBase, JvAppStorage, JvAppXMLStorage, Vcl.ComCtrls, JclSimpleXml,
  ExtCtrls, JvParameterList, JvExExtCtrls, JvExtComponent, JvPanel, JvXMLBrowser, Grids, Vcl.StdCtrls, JvExStdCtrls, JvMemo,
  JvSimpleXml, Vcl.ExtDlgs;

type
  TXMLBrowserDemo = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    JvSimpleXML1: TJvSimpleXML;
    Panel2: TPanel;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Button3: TButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FXML: TJvSimpleXML;
    FXMLBrowser: TJvXMLBrowserControl;
    { Private declarations }
  public
    property XML: TJvSimpleXML read FXML;
    property XMLBrowser: TJvXMLBrowserControl read FXMLBrowser;
    { Public declarations }
  end;

var
  XMLBrowserDemo: TXMLBrowserDemo;

{$I jvcl.inc}

implementation

uses
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  JvDynControlEngineDevExpCx,
  {$ELSE}
  JvDynControlEngineJvcl,
  {$ENDIF}
  JvParameterListParameter;

{$R *.dfm}

procedure TXMLBrowserDemo.Button1Click(Sender: TObject);
begin
  if OpenTextFileDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenTextFileDialog1.FileName);
    XMLBrowser.XMLData := Memo1.Lines.Text;
  end;
end;

procedure TXMLBrowserDemo.Button2Click(Sender: TObject);
begin
  XML.LoadFromString(Memo1.Lines.Text);
  ShowXMLBrowser(XML, 'XML Details');
end;

procedure TXMLBrowserDemo.Button3Click(Sender: TObject);
begin
  XMLBrowser.XMLData := Memo1.Lines.Text;
end;

procedure TXMLBrowserDemo.FormCreate(Sender: TObject);
begin
  FXML := TJvSimpleXML.Create(self);

  FXMLBrowser := TJvXMLBrowserControl.Create(self);
  FXMLBrowser.Parent := Panel2;
  FXMLBrowser.Align := alClient;
end;

procedure TXMLBrowserDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FXMLBrowser);
  FreeAndNil(FXML);

end;

procedure TXMLBrowserDemo.FormShow(Sender: TObject);
begin
//  Memo1.Lines.LoadFromFile('e:\Delphi\Projects\OraTool\Exe\Definition\StatementRepository2\l.xml');
//  Memo1.Lines.LoadFromFile('e:\Delphi\Components\jvcl\jvcl\packages\D14 Packages.groupproj');
  XMLBrowser.XMLData := Memo1.Lines.Text;
end;

end.
