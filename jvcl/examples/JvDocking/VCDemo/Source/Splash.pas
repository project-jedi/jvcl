unit Splash;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TSplashs = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SplashClosed: Boolean;
  end;

var
  Splashs: TSplashs;

implementation

{$R *.dfm}

procedure TSplashs.Timer1Timer(Sender: TObject);
begin
  SplashClosed := True;
end;

procedure TSplashs.FormCreate(Sender: TObject);
begin
  SplashClosed := False;
end;

procedure TSplashs.Image1Click(Sender: TObject);
begin
  SplashClosed := True;
end;

end.
