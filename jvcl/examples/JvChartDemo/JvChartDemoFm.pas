{******************************************************************}
{  Chart Demo program for JvChart (formerly AABsoftGraph)          }
{                                                                  }
{  BAR/LINE Charting Component for JEDI                            }
{                                                                  }
{  Original                                                        }
{  (C) 1996 AABsoft and Mårten Henrichson                          }
{  Modified 2003 Warren Postma                                     }
{******************************************************************}

unit JvChartDemoFm;

interface

uses
  SysUtils,  WinTypes,  WinProcs,
  Messages,  Classes,   Graphics,
  Controls,  Forms,     Dialogs,
  ExtCtrls,  StdCtrls,  Buttons,
  Spin,      JvChart, JvComponent;

type
  TJvChartDemoForm = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    ColorDialog1: TColorDialog;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    FontDialog1: TFontDialog;
    SpeedButton12: TSpeedButton;
    SpeedButton14: TSpeedButton;
    ButtonNewValues: TButton;
    CheckBox1: TCheckBox;
    Chart: TJvChart;
    procedure ButtonNewValuesClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  JvChartDemoForm: TJvChartDemoForm;

implementation

{$R *.DFM}

procedure TJvChartDemoForm.ButtonNewValuesClick(Sender: TObject);
var
   I           : Integer;
   nValueCount : Integer;
   hgt,hg0: Double;
begin
   Randomize;
   nValueCount := 20;
   Chart.ResetGraphModule;
   
   for I := 0 to nValueCount-1 do
   begin
     if i >0 then begin
      hgt := (Random( (I mod 5)*250)*5 +7500);
      hg0 := Random( (I mod 3)*650 ) +1003;
     end else begin
      hgt := 7000; // first element must be fixed for debug 0/1 offset purposes
      hg0 := 1000;

     end;
     // Set Data.Value[Pen, Series] := dataValue ...
      Chart.Data.Value[0,I] := hgt;
      Chart.Data.Value[1,I]  := hg0;
      Chart.Data.Value[2,I] := hgt-hg0;
      Chart.Options.XLegends.Add( FormatDateTime( 'yyyy-mm-dd', (now-3.0)+(i/16) ) );
   end;
   with Chart.Options do begin
       Title := 'Chart Title';
       XAxisHeader := 'Date/Time';
       YAxisHeader := 'Readings (ug/m3)';
       PenCount := 3;

       PenLegends.Clear;
       PenLegends.Add('HgT');
       PenLegends.Add('Hg0');
       PenLegends.Add('Hg2+');

       PenUnit.Clear;
       PenUnit.Add('ug/m3');
       PenUnit.Add('ug/m3');
       PenUnit.Add('ug/m3');

       ShowLegend := TRUE;
       ChartKind := ckChartLine;
   end;
   Chart.AutoFormatGraph;
   //Chart.ResizeChartCanvas;
end;

procedure TJvChartDemoForm.SpeedButton1Click(Sender: TObject);
begin
   Chart.GraphToClipboard;
end;

procedure TJvChartDemoForm.FormResize(Sender: TObject);
begin
 if not Assigned(Chart) then exit;
   Chart.ResizeChartCanvas;
end;

procedure TJvChartDemoForm.SpeedButton2Click(Sender: TObject);
begin
   Chart.PrintGraph;
end;

procedure TJvChartDemoForm.SpeedButton3Click(Sender: TObject);
begin
   Chart.Options.ChartKind := ckChartBar;
   // ShowAsBar;
end;

procedure TJvChartDemoForm.SpeedButton4Click(Sender: TObject);
begin
{   Chart.ShowAsLine; this show it without marks}
   Chart.Options.ChartKind := ckChartLineWithMarkers;
end;

procedure TJvChartDemoForm.SpeedButton5Click(Sender: TObject);
begin
   Chart.Options.ChartKind := ckChartStackedBarAverage;
end;

procedure TJvChartDemoForm.SpeedButton6Click(Sender: TObject);
begin
   Chart.Options.ChartKind := ckChartStackedBar;
end;

procedure TJvChartDemoForm.SpeedButton7Click(Sender: TObject);
begin
   Chart.PivotData; // TODO: CRASH.
end;

procedure TJvChartDemoForm.SpeedButton8Click(Sender: TObject);
begin
   //Chart.ShowAsBarWithAve;
   Chart.Options.ChartKind := ckChartBarAverage;
end;

procedure TJvChartDemoForm.SpeedButton10Click(Sender: TObject);
begin
   // Chart.ShowAsPie;
   Chart.Options.ChartKind := ckChartPieChart;
end;

procedure TJvChartDemoForm.SpeedButton12Click(Sender: TObject);
begin
   //Chart.ShowAsMark;
   Chart.Options.ChartKind := ckChartMarkers;
end;

procedure TJvChartDemoForm.SpinEdit1Change(Sender: TObject);
begin
//   Chart.Options.ColorScheme := SpinEdit1.Value;
//   Chart.PlotGraph;
end;

procedure TJvChartDemoForm.Button2Click(Sender: TObject);
begin
//   Chart.SetYGap(15);
//   Chart.SetYGapCount(15);
   Chart.PlotGraph;
end;

procedure TJvChartDemoForm.SpeedButton11Click(Sender: TObject);
begin
   {Get the current font for the Header text...}
   FontDialog1.Font.Assign( Chart.Options.AxisFont );

   if FontDialog1.Execute then
   begin
      {Set the font for the Header text...}
      Chart.Options.AxisFont.Assign( FontDialog1.Font );

   end;
   Chart.PlotGraph;
   Chart.Invalidate;
end;

procedure TJvChartDemoForm.Button4Click(Sender: TObject);
begin
//   Chart.SetYMaxMin(10000,0);
 Chart.Options.YMax := 10000;
 Chart.Options.YMin := 10000;
 Chart.AutoFormatGraph;
//   Chart.PlotGraph;
end;

procedure TJvChartDemoForm.FormActivate(Sender: TObject);
begin
if not Assigned(Chart) then exit;
   {This is needed for Win 3.X, as it does not send Resize message at startup}
   Chart.ResizeChartCanvas;
end;

procedure TJvChartDemoForm.SpeedButton14Click(Sender: TObject);
begin

  Chart.Options.ChartKind := ckChartDeltaAverage;
end;

procedure TJvChartDemoForm.FormCreate(Sender: TObject);
begin
if not Assigned(Chart) then exit;
//   Chart.ShowAsLineWithMark;
  ButtonNewValuesClick(Sender);
end;

procedure TJvChartDemoForm.SpeedButton13Click(Sender: TObject);
begin

  Chart.Align := alClient;
end;

end.
