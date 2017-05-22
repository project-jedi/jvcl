{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChartDemoFm.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is AABsoft and M�rten Henrichson.
(C) 1996 AABsoft and M�rten Henrichson.
All Rights Reserved.

Contributor(s): -

Last Modified: 2005-01-14

  Modified March 2007 by Warren Postma - Show new JvChart 2007 Features.


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  BAR/LINE Charting Component for JEDI [Updated March 2007]

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
    PanelTop: TPanel;
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
    DateTimeAxisMode: TMenuItem;
    PrintOptions1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    MenuSecondaryAxisMode: TMenuItem;
    MenuNegValueTest: TMenuItem;
    SpeedButtonTestMouseOver: TSpeedButton;
    NewFeaturesfor20071: TMenuItem;
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
    procedure DateTimeAxisModeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrintOptions1Click(Sender: TObject);
    procedure MenuSecondaryAxisModeClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MenuNegValueTestClick(Sender: TObject);
    procedure SpeedButtonTestMouseOverClick(Sender: TObject);
    procedure ChartEndFloatingMarkerDrag(Sender: TJvChart;
      FloatingMarker: TJvChartFloatingMarker);
    procedure ChartBeginFloatingMarkerDrag(Sender: TJvChart;
      FloatingMarker: TJvChartFloatingMarker);
    procedure ChartChartPaint(Sender: TJvChart; aCanvas: TCanvas);
    procedure NewFeaturesfor20071Click(Sender: TObject);
  private

      // Our waveform generator uses the following as state-variables:
    FGenerationIndex: Integer;
    Foo, Foo1, Foo2: Integer;
    Fhgt, Fhg0, Fhg2p: Double;
    FStatHgt, FStatHg0: TStatArray;
    Fdt, Fds: Double;
    FNegValueFlag: Boolean;

  protected
    procedure _Generate;
    procedure _StoreValue(I: integer);
    function _QAProblemScatter: Integer;

  public
    procedure NewValues;
  end;

var
  JvChartDemoForm: TJvChartDemoForm;

implementation

uses
  JvJVCLAboutForm, // JVCL About box stuff.
  Math, // Math:NaN handling, function isNan in D6 and higher.
  ShellApi; // ShellApi:ShellExecute function

{$R *.dfm}

{ Bogus vageuly sinusoidal signal generator }

procedure TJvChartDemoForm._Generate;
begin
  FHgt := Abs(Random(80) + (Random(((FGenerationIndex div foo) mod foo1) * 250) * 5 + 9500));
  FHg0 := Abs(Random(280) + Random(((FGenerationIndex div foo) mod foo2) * 650) * 5 + 1003);
  if FNegValueFlag then
  begin
    if (Random(80) > 75) or (FGenerationIndex = 2) then
    begin
      FHg0 := -1 * FHg0; // Generate a negative value! (just to show what it looks like)
    end;
  end;
  Inc(FGenerationIndex);
end;

{ Bogus random spiky-looking function to simulate a QA value,
  which hovers within +/- 10% of perfect (100%), but
  with relatively infrequent spiky errors }

function TJvChartDemoForm._QAProblemScatter: Integer;
var
  n, m: Double;
begin
  n := Log10(Random(10000) + 1); // Random is my favourite function. How about you? -WP
  n := n * Log10(Random(10000) + 1);
  m := Log10(Random(10000) + 1);
  m := m * Log10(Random(10000) + 1);
  n := Abs(100 + n - m);
  if (n < 0) then
    n := 0;
  if (n > 150) then
    n := 150;

  Result := Round(n);
end;

procedure TJvChartDemoForm._StoreValue(I: integer);
begin
  FStatHgt.AddValue(Fhgt);
  FStatHg0.AddValue(Fhg0);

    // PAY ATTENTION HERE, this is where we set the Chart.Data.Value[ index,pen] := <values>
    // stuff which is the MOST IMPORTANT THING, unless you like blank charts:

    // Set Data.Value[Pen, Series] := dataValue ...
  Chart.Data.Value[0, I] := FStatHgt.Average / 1000;

  if FHg0 < 0 then
  begin
    Chart.Data.Value[1, I] := FHg0 / 1000;
    // Don't show average with negative samples or my negative demo ability goes away (averaged out!)
  end
  else
  begin
    // Test blanks on big chart, show missing data:
    Chart.Data.Value[1, I] := FStatHg0.Average / 1000;
  end;

  Fhg2p := (FStatHgt.Average - FStatHg0.Average) / 1000;
  if Fhg2p < 0.0 then
    Fhg2p := 0.0;
  Chart.Data.Value[2, I] := Fhg2p;

  Fds := Fdt + (FGenerationIndex / 576);

    // There are TWO ways to get an X Axis label plotted:
  if DateTimeAxisMode.Checked then
    Chart.Data.Timestamp[I] := Fds // X legends generated by timestamps
  else
    // X Legends generated by user are used by default.
    // This would be redundant, and would be a waste of memory
    // if Chart.Options.XAxisDateTimeMode was also set.
    Chart.Options.XLegends.Add(FormatDateTime('hh:nn:ss', Fds));

  if MenuSecondaryAxisMode.Checked then
  begin
    if I = 1 then
      Chart.Data.Value[3, I] := 100
    else
    if (I mod 4) = 3 then
    begin
        //Chart.Data.Value[3,I] :=  1+ ((I mod 12) * 10) // stairstep
        //random:
      Chart.Data.Value[3, I] := _QAProblemScatter;
    end
    else
      Chart.Data.Value[3, I] := NaN; // leave some blanks.
  end;
end;

procedure TJvChartDemoForm.NewFeaturesfor20071Click(Sender: TObject);
var
 vbar:TJvChartVerticalBar;
 hbar:TJvChartHorizontalBar;
begin
   // Try out new features for 2007:

   // Gradients.
   Chart.Options.GradientColor := $00FDEEDB; // powder blue (baby blue) mostly white.
   Chart.Options.GradientDirection :=grDown;



   // Vertical Bar.
   vbar := Chart.AddVerticalBar;
   vbar.Color := $00FDDDC7;
   vbar.Visible := true;
   vbar.XLeft := 6;
   vbar.XRight := 7;

   // Horizontal Bar.
   hbar := Chart.AddHorizontalBar;
   hbar.Color := $007CCAC7;
   hbar.Visible := true;
   hbar.YTop := 3;
   hbar.YBottom := 2;





   // Lots of things not shown here are new in 2007:
   //  Floating Marker Count, Delete a floating marker, etc,
   // search for 'NEW 2007' in JvChart.pas for a complete
   // look at new things.

   // last thing, always update graph:
   Chart.PlotGraph;
   //Chart.Refresh;


end;

procedure TJvChartDemoForm.NewValues;
var
  I: Integer;
  nValueCount: Integer;
begin
//Chart.Options.Title := 'Click on the Chart Title to change it!';
  Chart.Options.PenCount := 3;

  Chart.ClearFloatingMarkers; // remove any previous markers.


  ListBox1.Clear;
  Chart.Data.Clear;

  Chart.Options.PrimaryYAxis.YMax := 20;

  Randomize;

  Chart.Options.XAxisDateTimeMode := DateTimeAxisMode.Checked; // Use datetime timestamp labels, just Fer Instance.

  if not Chart.Options.XAxisDateTimeMode then
    Chart.Options.XAxisLegendSkipBy := 5;

  if LargeDataset576samples1.Checked then
  begin
     // A larger bogus data set for demonstration purposes.
    nValueCount := 576; // 2.5 minute sample period, 576 samples = 1 day.
    foo := 5; // Used in generating our bogus data below, not really important.
    Chart.Options.XAxisValuesPerDivision := 24;
      // 24 samples * 150 seconds = 1 hour time divisions (     Chart.Options.XAxisValuesPerDivision := 4;
  end
  else
  begin
     // A smaller bogus data set for demonstration purposes.
    nValueCount := 24; // 2.5 minute sample period, 24 samples =1 hour.
    foo := 1; // Used in generating our bogus data below, not really important.
    Chart.Options.XAxisValuesPerDivision := 4; // five divisions, 4 values per division
  end;

  //Chart.ResetGraphModule;    // Clears YMax.
  Fdt := Trunc(now - 1.0); // yesterday, midnight.
  Foo1 := Random(5) + 2; // more randomness
  Foo2 := Random(3) + 5; // more randomness
  FGenerationIndex := 1;
  for I := 0 to nValueCount - 1 do
  begin
    if i > 0 then
    begin // generate random data that appears to show a sawtooth-frequency-pattern plus a lot of random noise:
      _Generate;
    end
    else
    begin
      Fhgt := 7000; // First sample always known value, helps me troubleshoot.
      Fhg0 := 1000;
    end;

    _StoreValue(I);

    // Override stored value in special cases:

    { How to make a gap in the data! }
    if (nValueCount > 100) and ShowgapinLineChart1.Checked then
    begin
      if (I > 100) and (I < 130) then
      begin
        Chart.Data.Value[0, I] := NaN; // Use special Math.NaN const
        Chart.Data.Value[1, I] := NaN; // Use special Math.NaN const
        Chart.Data.Value[2, I] := NaN; // Use special Math.NaN const
      end;
    end;

    // Just so that the last two values are visibly different, to make sure
    // the chart handles the final vaulues correctly, we set all the final
    // values to known amounts also:
    if I = nValueCount - 2 then
    begin
      Chart.Data.Value[0, I] := 6.0; // Use special Math.NaN const
      Chart.Data.Value[1, I] := 4.0; // Use special Math.NaN const
      Chart.Data.Value[2, I] := 2.0; // Use special Math.NaN const
    end
    else
    if I = nValueCount - 1 then
    begin
      Chart.Data.Value[0, I] := 3.0; // Use special Math.NaN const
      Chart.Data.Value[1, I] := 2.0; // Use special Math.NaN const
      Chart.Data.Value[2, I] := 1.0; // Use special Math.NaN const

    end;

    ListBox1.Items.Append(Chart.Data.DebugStr(I));
  end;

  with Chart.Options do
  begin
    Title := 'Chart Title';
    XAxisHeader := 'Date/Time';
    YAxisHeader := 'Readings (ng/m3)';

    if FNegValueFlag then
    begin
      PrimaryYAxis.YMin := -20;
      PrimaryYAxis.YMax := 20;
    end
    else
      PrimaryYAxis.YMin := 0;
    // Try out the pen styles:
    if ChartKind = ckChartStackedBar then
      PenStyle[0] := psClear // THIS IS HOW YOU TEMPORARILY HIDE ONE PEN!
    else
      PenStyle[0] := psSolid;
    PenStyle[1] := psDash;
    PenStyle[2] := psDot;

    if MenuSecondaryAxisMode.Checked then
    begin
      PenCount := 4; // Add a pen for right side demo.
      SecondaryYAxis.YMax := 140; // Example shows Q/A percentage. Experimental results
                                    // results are compared to expected results, and the
                                    // response percentage, is plotted from 0% to 140%
                                    // of expected value.

      SecondaryYAxis.YLegendDecimalPlaces := 2;

      PenSecondaryAxisFlag[3] := True; // Move pen index 3 (Fourth pen) to secondary axis.
      PenMarkerKind[3] := pmkDiamond;
      PenValueLabels[3] := true; // Label with text.
      PenStyle[3] := psClear; // Markers only, no lines.
      PenColor[3] := clGray;
      MarkerSize := 5; // Make 'em bigger.
    end
    else
    begin
      PenCount := 3;
      MarkerSize := 3; // Make 'em little
    end;

    PenLegends.Clear;
    PenLegends.Add('HgT');
    PenLegends.Add('Hg0');
    PenLegends.Add('Hg2+');
    if MenuSecondaryAxisMode.Checked then
      PenLegends.Add('Quality%');

    PenUnit.Clear;
    PenUnit.Add('ug/m3');
    PenUnit.Add('ug/m3');
    PenUnit.Add('ug/m3');
    if MenuSecondaryAxisMode.Checked then
      PenUnit.Add('%'); // Optional Pen in percentage scale.

    //ShowLegend := TRUE;
    Legend := clChartLegendBelow;

    //ChartKind := ckChartLine;
  end;
//    Chart.AutoFormatGraph;
  Chart.PlotGraph;

   //Chart.ResizeChartCanvas;
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
  I: Integer;
begin
  Chart.Options.ChartKind := ckChartLine;
  for I := 0 to Chart.Options.PenCount - 1 do
  begin
    Chart.Options.PenMarkerKind[I] := pmkNone;
  end;

  NewValues;
end;

procedure TJvChartDemoForm.ButtonLineMarkerClick(Sender: TObject);
//var
//  I:Integer;
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

  NewValues;
  //Chart.Plo
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
  Chart.PlotGraph;
end;

procedure TJvChartDemoForm.Button4Click(Sender: TObject);
begin
//  Chart.AutoFormatGraph;  WAP Removed. BAD CODE.
  Chart.PlotGraph;
end;

procedure TJvChartDemoForm.ButtonDeltaAverageClick(Sender: TObject);
begin
  Chart.Options.ChartKind := ckChartDeltaAverage;
end;

procedure TJvChartDemoForm.FormCreate(Sender: TObject);
begin
  FStatHgt := TStatArray.Create(10); // Initialize for rolling average of last 10 samples.
  FStatHg0 := TStatArray.Create(10);

  if Assigned(Chart) then
    //   Chart.ShowAsLineWithMark;
    NewValues;
end;

procedure TJvChartDemoForm.Panel2DblClick(Sender: TObject);
begin
  ShellExecute(HWND(nil), 'show', 'http://jvcl.delphi-jedi.org/', nil, nil, SW_SHOW);
end;

procedure TJvChartDemoForm.ShowgapinLineChart1Click(Sender: TObject);
begin
  ShowgapinLineChart1.Checked := not ShowgapinLineChart1.Checked;
  ButtonLine.Down := true;
  NewValues;
end;

procedure TJvChartDemoForm.Print1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
    Chart.PrintGraph;
end;

procedure TJvChartDemoForm.Generatenewrandomvalues1Click(Sender: TObject);
begin
  if SpeedButtonTestMouseOver.Down then exit;

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
  Application.MessageBox(PChar(
    'JvChart comes from AABSoft Graph written by  M�rten Henrichson, JVCL 3.0 ' +
    'version by Warren Postma.  '), 'About JvChart', MB_OK);
end;

procedure TJvChartDemoForm.AboutJVCL301Click(Sender: TObject);
begin
  TJvJVCLAboutForm.Execute(False);
end;

procedure TJvChartDemoForm.Scrolling1Click(Sender: TObject);
begin
  if SpeedButtonTestMouseOver.Down then exit;

  Scrolling1.Checked := not Scrolling1.Checked;
  Timer1.Enabled := Scrolling1.Checked;
end;

procedure TJvChartDemoForm.Timer1Timer(Sender: TObject);
begin
  if SpeedButtonTestMouseOver.Down then exit;

  Chart.Data.Scroll;
  _Generate;
  _StoreValue(Chart.Data.ValueCount - 1);
  Chart.PlotGraph;
end;

procedure TJvChartDemoForm.ShowDataInListbox1Click(Sender: TObject);
begin
  ShowDataInListbox1.Checked := not ShowDataInListbox1.Checked;
  ListBox1.Visible := ShowDataInListbox1.Checked;

  if not ShowDataInListbox1.Checked then
  begin
    Chart.CursorPosition := -1; // Invisible.
  end;
end;

procedure TJvChartDemoForm.LargeDataset576samples1Click(Sender: TObject);
begin
  if SpeedButtonTestMouseOver.Down then exit;

  LargeDataset576samples1.Checked := not LargeDataset576samples1.Checked;
  NewValues;
end;

procedure TJvChartDemoForm.DateTimeAxisModeClick(Sender: TObject);
begin
  DateTimeAxisMode.Checked := not DateTimeAxisMode.Checked;
  if SpeedButtonTestMouseOver.Down then begin
      Chart.Options.XLegends.Clear;
  end else
    NewValues;
end;

procedure TJvChartDemoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStatHgt);
  FreeAndNil(FStatHg0);
end;

procedure TJvChartDemoForm.PrintOptions1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TJvChartDemoForm.MenuSecondaryAxisModeClick(Sender: TObject);
begin
  MenuSecondaryAxisMode.Checked := not MenuSecondaryAxisMode.Checked;

  if MenuSecondaryAxisMode.Checked then
  begin
    ButtonLine.Down := true;
    ButtonLineClick(Sender);
  end
  else
    NewValues;
end;

procedure TJvChartDemoForm.ListBox1DblClick(Sender: TObject);
begin
  Chart.CursorPosition := ListBox1.ItemIndex; // Highlight one sample.
end;

procedure TJvChartDemoForm.ListBox1Click(Sender: TObject);
begin
  Chart.CursorPosition := ListBox1.ItemIndex; // Highlight one sample.
end;

procedure TJvChartDemoForm.MenuNegValueTestClick(Sender: TObject);
begin
  FNegValueFlag := MenuNegValueTest.Checked;
  NewValues;
end;

{ Simple Chart Tests for finding bug }
procedure TJvChartDemoForm.SpeedButtonTestMouseOverClick(Sender: TObject);
var
  i:Integer;
  n:Double;
  marker1,marker2,draggableCursor:TJvChartFloatingMarker;
begin
  ListBox1.Clear;
  Scrolling1.Checked := false;

  Chart.Options.Title := 'Click on the Red Diamonds or the vertical gray line and Drag them!';
  Chart.Options.ChartKind := ckChartLine;
  Chart.Options.XLegends.Clear;
  Chart.Options.XAxisHeader := '';

  Chart.Options.MouseDragObjects := true; // NEW: MOUSE DRAG FLOATING OBJECTS!

  for i := 0 to Chart.Options.PenCount - 1 do
  begin
    Chart.Options.PenMarkerKind[I] := pmkNone;
  end;
  Chart.Data.Clear;
  Chart.Options.XValueCount := 80;
  Chart.Data.ValueCount := 6;
  Chart.Options.PenCount := 1;
  for i := 0 to Chart.Options.XValueCount-1  do begin
    n := 30-(25*((1-((i-25)/15))*(1-((i-25)/15))));
    if n<5 then
        n := 5 -(Sqrt((Abs(n))/15));
    ListBox1.Items.Add( FloatToStrF(n,ffFixed,6,4));
    Chart.Data.Value[0,i] := n;
  end;
  Chart.Options.PrimaryYAxis.YMax := 50;

  Chart.Options.PenStyle[0] := psSolid;

  // NEW: Add a floating marker:
  marker1 := Chart.AddFloatingMarker;
  marker1.XPosition := 13;
  marker1.YPosition := Chart.Data.Value[0,marker1.XPosition]; // Snap to Pen 1
  marker1.XDraggable := true; // make it mouse-moveable.
  marker1.YDraggable := true;
  marker1.Caption :='Start';
  marker1.Visible := true;


  marker2 := Chart.AddFloatingMarker;
  marker2.XPosition := 66;
  marker2.YPosition := Chart.Data.Value[0,marker2.XPosition]; // Snap to Pen 1
  marker2.LineToMarker := marker1.index; // Connect with a line to marker1
  marker2.XDraggable := true; // make it mouse-moveable.
  marker2.YDraggable := true;
  marker2.Caption := 'End';
  marker2.Visible := true;

  // NOTE: Do not Free marker1 or marker2.
  // Marker objects are freed automatically
  // by the Chart.

  // a draggable cursor object:
  draggableCursor := Chart.AddFloatingMarker;
  draggableCursor.LineVertical := true; // Make a vertical Line
  draggableCursor.Marker := pmkNone; // No marker. So it is a line only.
  draggableCursor.XDraggable := true; // make it draggable.
  draggableCursor.XPosition := 40;
  draggableCursor.LineColor := clDkGray;
  draggableCursor.LineStyle := psSolid;
  draggableCursor.LineWidth := 2;
  draggableCursor.Caption := FloatToStrF( Chart.Data.Value[0,draggableCursor.XPosition], ffFixed, 6,4 );
  draggableCursor.Visible := true;






  Chart.PlotGraph;


end;

procedure TJvChartDemoForm.ChartEndFloatingMarkerDrag(Sender: TJvChart;
  FloatingMarker: TJvChartFloatingMarker);
begin
  // Snap to line:
  if (FloatingMarker.Index<2) then // One of the first two markers?
    FloatingMarker.YPosition :=  Chart.Data.Value[0, FloatingMarker.XPosition]
  else // update caption
    FloatingMarker.Caption := FloatToStrF( Chart.Data.Value[0,FloatingMarker.XPosition],ffFixed,6,4 );


end;

procedure TJvChartDemoForm.ChartBeginFloatingMarkerDrag(Sender: TJvChart;
  FloatingMarker: TJvChartFloatingMarker);
begin
    if FloatingMarker.Index=2 then
        FloatingMarker.Caption := '?';

end;

procedure TJvChartDemoForm.ChartChartPaint(Sender: TJvChart;
  aCanvas: TCanvas);
begin
  aCanvas.Pen.Color := clRed;
  aCanvas.Pen.Style := psSolid;
  aCanvas.MoveTo(0,0);
  aCanvas.LineTo(100,100);
end;

end.
