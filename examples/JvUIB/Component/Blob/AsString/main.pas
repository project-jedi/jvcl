unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    Button1: TButton;
    Memo: TMemo;
    Button2: TButton;
    UpdateQuery: TJvUIBQuery;
    Description: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var str: String;
begin
  Memo.Clear;
  Query.Open;
  while not Query.EOF do
  begin
    Memo.Lines.Add('Project Name: ' + Query.Fields.ByNameAsString['proj_name']);
    Memo.Lines.Add('Product: ' + Query.Fields.ByNameAsString['product']);
    Query.ReadBlob('proj_desc', str);

    Memo.Lines.Add('Description: ' + Str);
    Memo.Lines.Add('');
    Query.Next;
  end;
  Query.Close(etmCommit);
end;

procedure TForm1.Button2Click(Sender: TObject);
var Str: String;
begin
  Str := Description.Text;
  UpdateQuery.ParamsSetBlob(0, Str);
  UpdateQuery.ExecSQL;
  UpdateQuery.Close(etmCommit);

end;

end.
