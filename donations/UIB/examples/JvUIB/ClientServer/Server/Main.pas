unit Main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvUIBSrv;

type
  TMainForm = class(TForm)
    DataBase: TJvUIBDataBase;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JvUIBServer.DefaultPort := 9545;
  JvUIBServer.Start;
  DataBase.Connected := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JvUIBServer.Stop;
end;

end.
