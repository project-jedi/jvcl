unit fJvHLEdPropDlgTestParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvComponent;

type
  TMyParams  = class(TForm)
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
  published
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Load;
    procedure Save;
    function GetAppCaption: String;
  end;

implementation

{$R *.DFM}

procedure TMyParams .Load;
begin
end;

procedure TMyParams .Save;
begin
end;

function TMyParams .GetAppCaption: String;
begin
  Result := Edit1.Text;
end;

end.
