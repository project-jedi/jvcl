unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvUrlListGrabber, JvUrlGrabbers, StdCtrls;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    grabber : TJvUrlListGrabber;
  public
    { Public declarations }
    procedure grabberConnectionClosed(Sender : TJvUrlListGrabber; Grabber : TJvUrlGrabber);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
uses JvTypes;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  grabber := TJvUrlListGrabber.Create(Self);
  grabber.URLs.Add('http://server');
  with grabber.Grabbers[0] do
  begin
    OutputMode := omFile;
    FileName := 'c:\test.txt';
    Start;
  end;
  grabber.OnConnectionClosed := grabberConnectionClosed;
end;

procedure TfrmMain.grabberConnectionClosed(Sender: TJvUrlListGrabber; Grabber : TJvUrlGrabber);
begin
  Application.messagebox('Finished', '', 0);
  grabber.Free;
end;

end.
