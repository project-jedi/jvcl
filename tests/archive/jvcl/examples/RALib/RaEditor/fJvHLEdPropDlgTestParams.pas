unit fJvHLEdPropDlgTestParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvRegAuto, JvComponent;

type
  TMyParams  = class(TForm)
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Edit1: TEdit;
    RegAuto1: TJvRegAuto;
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
  RegAuto1.Load;
end;

procedure TMyParams .Save;
begin
  RegAuto1.Save;
end;

function TMyParams .GetAppCaption: String;
begin
  Result := Edit1.Text;
end;

end.
