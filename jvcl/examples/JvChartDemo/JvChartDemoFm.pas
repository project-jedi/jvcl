{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChartDemoFm.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is AABsoft and Mårten Henrichson.
(C) 1996 AABsoft and Mårten Henrichson.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-07
  Modified 2003 Warren Postma

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  BAR/LINE Charting Component for JEDI

Known Issues:
-----------------------------------------------------------------------------}

unit JvChartDemoFm;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, Spin,
  JvChart, JvComponent, JvExControls, StatsClasses, Menus;

type
  TJvChartDemoForm = class(TForm)
    Panel1: TPanel;
    ButtonBarChart: TSpeedButton;
    ButtonLine: TSpeedButton;
    ButtonStackedBarAve: TSpeedButton;
    ButtonStackedBar: TSpeedButton;
    ButtonBarAve: TSpeedButton;
    ColorDialog1: TColorDialog;
    ButtonPie: TSpeedButton;
    FontDialog1: TFontDialog;
    ButtonMarkers: TSpeedButton;
    ButtonDeltaAverage: TSpeedButton;
    Chart: TJvChart;
    ButtonLineMarker: TSpeedButton;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Demo1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    AboutJVCL301: TMenuItem;
    ShowgapinLineChart1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    Generatenewrandomvalues1: TMenuItem;
    CopyToClipboard1: TMenuItem;
    N2: TMenuItem;
    SetHeaderFont1: TMenuItem;
    Scrolling1: TMenuItem;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Timer1: TTimer;
    ShowDataInListbox1: TMenuItem;
    N4: TMenuItem;
    LargeDataset576samples1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure ButtonLineClick(Sender: TObject);
    procedure ButtonBarChartClick(Sender: TObject);
    procedure ButtonStackedBarAveClick(Sender: TObject);
    procedure ButtonStackedBarClick(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure ButtonBarAveClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonPieClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonMarkersClick(Sender: TObject);
    procedure ButtonDeltaAverageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonLineMarkerClick(Sender: TObject);
    procedure Panel2DblClick(Sender: TObject);
    procedure ShowgapinLineChart1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Generatenewrandomvalues1Click(Sender: TObject);
    procedure CopyToClipboard1Click(Sender: TObject);
    procedure SetHeaderFont1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure AboutJVCL301Click(Sender: TObject);
    procedure Scrolling1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ShowDataInListbox1Click(Sender: TObject);
    procedure LargeDataset576samples1Click(Sender: TObject);
  private
  public
      procedure NewValues;
  end;

var
  JvChartDemoForm: TJvChartDemoForm;

implementation

uses JvJVCLAboutForm, JVCLVer, JvDsgnConsts, // JVCL About box stuff
     Math, ShellAPi; // NaN handling




{$R *.dfm}

procedure TJvChartDemoForm.NewValues;
var
  I: Integer;
  nValueCount: Integer;
  hgt, hg0, hg2p: Double;
  StatHgt,StatHg0:TStatArray;
  dt,ds:Double;
  foo,foo1,foo2:Integer;
begin
  ListBox1.Clear;
  Chart.Data.Clear;

  StatHgt := TStatArray.Create(10); // Initialize for rolling average of last 10 samples.
  StatHg0 := TStatArray.Create(10);
  try

  Randomize;

  if LargeDataset576samples1.Checked  then begin

     // A larger bogus data set for demonstration purposes.
     nValueCount := 576;  // 2.5 minute sample period, 576 samples = 1 day.
     foo := 20; // Used in generating our bogus data below, not really important.
     Chart.Options.XAxisValuesPerDivision := 24; // 24 samples * 150 seconds = 1 hour time divisions (     Chart.Options.XAxisValuesPerDivision := 4;
     Chart.Options.XAxisDateTimeMode := true;  // Use datetime timestamp labels, just Fer Instance.

  end else begin

     // A smaller bogus data set for demonstration purposes.
     nValueCount := 20; // Other chart types don't deal well with 576 samples.
     foo := 1;  // Used in generating our bogus data below, not really important.
     Chart.Options.XAxisValuesPerDivision := 4; // five divisions, 4 values per division
     Chart.Options.XAxisDateTimeMode := false; // Use text labels, just Fer Instance.

     // Print every fifth sample's X Axis Legend value, below
     // the chart value (Set to XAxisLegendSkipBy 1 to print em all,
     // which would get crowded awful quick)
     Chart.Options.XAxisLegendSkipBy := 5;
  end;

  //Chart.ResetGraphModule;    // Clears YMax.
  dt := Trunc(now- 1.0); // yesterday, midnight.
  foo1 := Random(5)+2;   // more randomness
  foo2 := Random(3)+5;   // more randomness
  for I := 0 to nValueCount - 1 do
  begin
    if i > 0 then
    begin  // generate random data that appears to show a sawtooth-frequency-pattern plus a lot of random noise:
      hgt := Abs(Random(80)+(Random(((I div foo) mod foo1) * 250) * 5 + 9500));
      hg0 := Abs(Random(280)+Random(((I div foo) mod foo2) * 650)*5 + 1003);
    end
    else
    begin
      hgt := 7000; // First sample always known value, helps me troubleshoot.
      hg0 := 1000;
    end;
    StatHgt.AddValue(hgt);
    StatHg0.AddValue(hg0);


    // PAY ATTENTION HERE, this is where we set the Chart.Data.Value[ index,pen] := <values>
    // stuff which is the MOST IMPORTANT THING, unless you like blank charts:

    // Set Data.Value[Pen, Series] := dataValue ...
    Chart.Data.Value[0, I] := StatHgt.Average /1000;

    // Test blanks on big chart, show missing data:
     Chart.Data.Value[1, I] := StatHg0.Average/1000;

    hg2p := (StatHgt.Average - StatHg0.Average)/1000;
    if hg2p < 0.0 then
        hg2p := 0.0;
    Chart.Data.Value[2, I] := hg2p;
    ds := dt + (i / 576);

    // There are TWO ways to get an X Axis label plotted:
    if Chart.Options.XAxisDateTimeMode then
         Chart.Data.Timestamp[I] := ds // X legends generated by timestamps
    else
        // X Legends generated by user are used by default.
        // This would be redundant, and would be a waste of memory
        // if Chart.Options.XAxisDateTimeMode was also set.
        Chart.Options.XLegends.Add(FormatDateTime('hh:nn:ss', ds) );



    { How to make a gap in the data! }
    if (nValueCount>100 ) and ShowgapinLineChart1.Checked then begin
      if (I > 100) and (I < 130) then begin
         Chart.Data.Value[0, I] := NaN; // Use special Math.NaN const
         Chart.Data.Value[1, I] := NaN; // Use special Math.NaN const
         Chart.Data.Value[2, I] := NaN; // Use special Math.NaN const
      end;
    end;

    // Just so that the last two values are visibly different, to make sure
    // the chart handles the final vaulues correctly, we set all the final
    // values to known amounts also:
    if (I=nValueCount-2) then begin
         Chart.Data.Value[0, I] := 6.0; // Use special Math.NaN const
         Chart.Data.Value[1, I] := 4.0; // Use special Math.NaN const
         Chart.Data.Value[2, I] := 2.0; // Use special Math.NaN const
    end else if (I=nValueCount-1) then begin
         Chart.Data.Value[0, I] := 3.0; // Use special Math.NaN const
         Chart.Data.Value[1, I] := 2.0; // Use special Math.NaN const
         Chart.Data.Value[2, I] := 1.0; // Use special Math.NaN const

    end;

    ListBox1.Items.Append( Chart.Data.DebugStr(I) );
  end;



  with Chart.Options do
  begin
    Title := 'Chart Title';
    XAxisHeader := 'Date/Time';
    YAxisHeader := 'Readings (ng/m3)';
    PenCount := 3;

    PenLegends.Clear;
    PenLegends.Add('HgT');
    PenLegends.Add('Hg0');
    PenLegends.Add('Hg2+');

    PenUnit.Clear;
    PenUnit.Add('ug/m3');
    PenUnit.Add('ug/m3');
    PenUnit.Add('ug/m3');

    //ShowLegend := TRUE;
    Legend := clChartLegendBelow;
    
    //ChartKind := ckChartLine;
  end;
    Chart.AutoFormatGraph;
    //Chart.PlotGraph;
   //Chart.ResizeChartCanvas;
  finally
      FreeAndNil(StatHgt);
      FreeAndNil(StatHg0);
  end;
end;

procedure TJvChartDemoForm.FormResize(Sender: TObject);
begin
  if Assigned(Chart) then
    Chart.ResizeChartCanvas;
end;

procedure TJvChartDemoForm.ButtonBarChartClick(Sender: TObject);
begin
  Chart.Options.ChartKind := ckChartBar;
  NewValues;
  //Chart.PlotGraph;
end;

procedure TJvChartDemoForm.ButtonLineClick(Sender: TObject);
var
 I:Integer;
begin
  Chart.Options.ChartKind := ckChartLine;
  for I := 0 to Chart.Options.PenCount-1 do begin
     Chart.Options.PenMarkerKind[I] := pmkNone;
  end;
  
  NewValues;
end;


procedure TJvChartDemoForm.ButtonLineMarkerClick(Sender: TObject);
var
  I:Integer;
begin
  Chart.Options.ChartKind := ckChartLine;
  Chart.Options.PenMarkerKind[0] := pmkDiamond; // demonstrate both Diamond and Circle Marks.
  Chart.Options.PenMarkerKind[1] := pmkDiamond;
  Chart.Options.PenMarkerKind[2] := pmkCircle;

  NewValues;

end;


procedure TJvChartDemoForm.ButtonStackedBarAveClick(Sender: TObject);
begin
  Chart.Options.ChartKind := ckChartStackedBarAverage;
  NewValues;
end;

procedure TJvChartDemoForm.ButtonStackedBarClick(Sender: TObject);
begin
  Chart.Options.ChartKind := ckChartStackedBar;
  NewValues;
end;

procedure TJvChartDemoForm.SpeedButton7Click(Sender: TObject);
begin
//  Chart.PivotData; // TODO: This causes exceptions. not sure why we want this.
end;

procedure TJvChartDemoForm.ButtonBarAveClick(Sender: TObject);
begin
  //Chart.ShowAsBarWithAve;
  Chart.Options.ChartKind := ckChartBarAverage;
end;

procedure TJvChartDemoForm.ButtonPieClick(Sender: TObject);
begin
  // Chart.ShowAsPie;
  Chart.Options.ChartKind := ckChartPieChart;
end;

procedure TJvChartDemoForm.ButtonMarkersClick(Sender: TObject);
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

procedure TJvChartDemoForm.Button4Click(Sender: TObject);
begin
//   Chart.SetYMaxMin(10000,0);
//  Chart.Options.YMax := 10000;
//  Chart.Options.YMin := 10000;
  Chart.AutoFormatGraph;
//   Chart.PlotGraph;
end;

procedure TJvChartDemoForm.ButtonDeltaAverageClick(Sender: TObject);
begin
  Chart.Options.ChartKind := ckChartDeltaAverage;
end;

procedure TJvChartDemoForm.FormCreate(Sender: TObject);
begin
  if Assigned(Chart) then
    //   Chart.ShowAsLineWithMark;
    NewValues;
end;

procedure TJvChartDemoForm.Panel2DblClick(Sender: TObject);
begin
 ShellExecute( HWND(nil), 'show', 'http://homepages.borland.com/jedi/jvcl/', nil,nil,SW_SHOW);
end;

procedure TJvChartDemoForm.ShowgapinLineChart1Click(Sender: TObject);
begin
  ShowgapinLineChart1.Checked := not ShowgapinLineChart1.Checked;
  ButtonLine.Down := true;
  NewValues;

end;

procedure TJvChartDemoForm.Print1Click(Sender: TObject);
begin
  Chart.PrintGraph;
end;

procedure TJvChartDemoForm.Generatenewrandomvalues1Click(Sender: TObject);
begin
 NewValues;
end;

procedure TJvChartDemoForm.CopyToClipboard1Click(Sender: TObject);
begin
  Chart.GraphToClipboard;
end;

procedure TJvChartDemoForm.SetHeaderFont1Click(Sender: TObject);
begin
   {Get the current font for the Header text...}
  FontDialog1.Font.Assign(Chart.Options.AxisFont);

  if FontDialog1.Execute then
    {Set the font for the Header text...}
    Chart.Options.AxisFont := FontDialog1.Font;
  Chart.PlotGraph;
  Chart.Invalidate;

end;

procedure TJvChartDemoForm.About1Click(Sender: TObject);
begin
  Application.MessageBox( PChar(
  'JvChart comes from AABSoft Graph written by  Mårten Henrichson, JVCL 3.0 '+
  'version by Warren Postma.  '),'About JvChart', MB_OK);
end;

procedure TJvChartDemoForm.AboutJVCL301Click(Sender: TObject);
begin
  TJvJVCLAboutForm.Execute(False);
end;

procedure TJvChartDemoForm.Scrolling1Click(Sender: TObject);
begin
  Scrolling1.Checked := not Scrolling1.Checked;
  Timer1.Enabled := Scrolling1.Checked;
end;

procedure TJvChartDemoForm.Timer1Timer(Sender: TObject);
begin
  Chart.Data.Scroll;
  Chart.PlotGraph;

end;

procedure TJvChartDemoForm.ShowDataInListbox1Click(Sender: TObject);
begin
  ShowDataInListbox1.Checked := not ShowDataInListbox1.Checked;
  ListBox1.Visible := ShowDataInListbox1.Checked;
end;

procedure TJvChartDemoForm.LargeDataset576samples1Click(Sender: TObject);
begin
   LargeDataset576samples1.Checked := not LargeDataset576samples1.Checked;
   NewValues;
end;

end.

