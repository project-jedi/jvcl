unit ScriptForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MyLabel;

type
  TScript = class(TForm)
    MyLabel1: TMyLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Script: TScript;

implementation

{$R *.dfm}

procedure TScript.Button1Click(Sender: TObject);
begin
  MyLabel1.DoSomething;
end;

procedure TScript.Button2Click(Sender: TObject);
begin
  MyLabel1.SomeProperty := 'SomeProperty';
end;

end.
