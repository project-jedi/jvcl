unit SplashUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, jpeg;

type
  TSplashForm = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure Image1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SplashClosed: Boolean;
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.DFM}

procedure TSplashForm.Image1Click(Sender: TObject);
begin
  SplashClosed := True;
end;

procedure TSplashForm.Timer1Timer(Sender: TObject);
begin
  SplashClosed := True;
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  SplashClosed := False;
end;

end.
