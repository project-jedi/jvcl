unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
   SysUtils, Classes, JvUIB;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Button1: TButton;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Query.Params.AsInteger[0] := 623;
  Query.Open;
  memo.Clear;
  while not Query.EOF do
    with Query, Fields do
    begin
      memo.Lines.Add(format('%s %s, Salary: %f',
        [ByNameAsString['FIRST_NAME'],
         ByNameAsString['LAST_NAME'],
         ByNameAsCurrency['SALARY']]));
      Next;
    end;
  Query.Close(etmCommit);

end;

end.
