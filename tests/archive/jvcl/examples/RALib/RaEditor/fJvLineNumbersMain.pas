unit fJvLineNumbersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvEditor, JvHLEditor, StdCtrls;

type
  TJvLineNumbersMain  = class(TForm)
    RAHLEditor1: TJvEditor;
    Panel1: TPanel;
    GutterFont: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RAHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LineNumbersMain: TJvLineNumbersMain ;

implementation

{$R *.DFM}

procedure TJvLineNumbersMain .FormCreate(Sender: TObject);
var
  FN: String;
begin
  FN := ExtractFilePath(Application.ExeName) + '..\..\README.TXT';
  if FileExists(FN) then
    RAHLEditor1.Lines.LoadFromFile(FN)
  else
    RAHLEditor1.Lines.Add('          File "' + ExpandFileName(FN) + '" not found !');
end;

procedure TJvLineNumbersMain .RAHLEditor1PaintGutter(Sender: TObject;
  Canvas: TCanvas);
var
  i: Integer;
  Rect: TRect;
  oldFont: TFont;  
begin
  oldFont := TFont.Create;
  try                                            
    oldFont.Assign(Canvas.Font);
    Canvas.Font := GutterFont.Font;
    with RAHLEditor1 do
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

end.
