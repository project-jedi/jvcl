unit fJvLineNumbersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvEditor, JvHLEditor, StdCtrls;

type
  TJvLineNumbersMain = class(TForm)
    Panel1: TPanel;
    GutterFont: TLabel;
    JvHLEditor1: TJvEditor;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure JvHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LineNumbersMain: TJvLineNumbersMain;

implementation

{$R *.DFM}

procedure TJvLineNumbersMain.JvHLEditor1PaintGutter(Sender: TObject;
  Canvas: TCanvas);
var
  i: integer;
  Rect: TRect;
  oldFont: TFont;
begin
  oldFont := TFont.Create;
  try
    oldFont.Assign(Canvas.Font);
    Canvas.Font := GutterFont.Font;
    with JvHLEditor1 do
      for i := TopRow to TopRow + VisibleRowCount do
      begin
        Rect := Bounds(2, (i - TopRow) * CellRect.Height, GutterWidth - 2 - 5, CellRect.Height);
        DrawText(Canvas.Handle, PChar(IntToStr(i + 1)), -1, Rect, DT_RIGHT or DT_VCENTER or DT_SINGLELINE);
      end;
  finally
    Canvas.Font := oldFont;
    oldFont.Free;
  end;
end;

procedure TJvLineNumbersMain.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    JVHLEditor1.Lines.LoadFromFile(OpenDialog1.Filename);
end;

end.

