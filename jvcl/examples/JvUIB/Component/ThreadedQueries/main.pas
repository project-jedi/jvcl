unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, SyncObjs;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Button1: TButton;
    Transaction: TJvUIBTransaction;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TMyThread = class(TThread)
  protected
    procedure Execute; override;
    destructor destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 49 do
    TMyThread.Create(False);
end;

var
  x: integer = 0;

{ TMyThread }

destructor TMyThread.destroy;
begin
  beep;
  inherited;
end;

procedure TMyThread.Execute;
var
  Query: TJvUIBQuery;
begin
  FreeOnTerminate := true;
  //Form1.DataBase.Lock; //simulate single thread
  try
    Query := TJvUIBQuery.Create(nil);
    try
      Query.Transaction := Form1.Transaction;
      Query.FetchBlobs := True;
      Query.SQL.Text := 'select * from project';
      Query.Open;
      while not Query.EOF do
      begin
        Query.Next;
        Sleep(10); // simulate activity
      end;
    finally
      Query.Free;
    end;
  finally
    //Form1.DataBase.UnLock; //simulate single thread
  end;
end;

end.
