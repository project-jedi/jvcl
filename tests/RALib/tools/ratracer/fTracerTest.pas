unit fTracerTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbTimer1: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbTimer1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses iMTracer;

{$R *.DFM}

procedure TForm1.FormResize(Sender: TObject);
begin
  Tracer.Writeln(' Width= '+IntToStr(Width)
  +' Height= '+IntToStr(Height));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Tracer.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  ras : integer = 0;
begin
  Tracer.Writeln(IntToStr(Ras)+' Width= '+IntToStr(Width)
  +' Height= '+IntToStr(Height));
  inc(ras);
end;

procedure TForm1.cbTimer1Click(Sender: TObject);
begin
  if cbTimer1.Checked then
    Tracer.TimerStart(1)
  else Tracer.TimerStop(1)
end;

end.
