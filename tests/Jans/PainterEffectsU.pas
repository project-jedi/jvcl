unit PainterEffectsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, JvDrawImage;

type
  TPainterEffectsF = class(TForm)
    effectspanel: TPanel;
    EBar: TScrollBar;
    extrabar: TScrollBar;
    ETree: TTreeView;
    cxbar: TScrollBar;
    cybar: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure EListClick(Sender: TObject);
    procedure EBarChange(Sender: TObject);
  private
    PainterF: TJvDrawImage;
    procedure bar(max, min, pos: integer);
    { Private declarations }
  public
    { Public declarations }
    procedure setDrawImage(aDrawImage: TJvDrawImage);
  end;

var
  PainterEffectsF: TPainterEffectsF;

implementation

{$R *.DFM}

procedure TPainterEffectsF.bar(max, min, pos: integer);
begin
  Ebar.Max := max;
  Ebar.Min := min;
  Ebar.Position := pos;
end;

procedure TPainterEffectsF.EListClick(Sender: TObject);
var
  index: integer;
begin
  if ETree.Selected = nil then exit;
  index := ETree.Selected.ImageIndex;
  if index < 0 then exit;
  case index of
    0: bar(100, 0, 0); //contrast
    1: bar(255, 0, 0); //saturation
    2: bar(100, 0, 0); //brightness
    3: bar(20, 0, 0); //gaussian blur
    4: bar(30, 0, 0); //split blur
    5: bar(200, 0, 0); //color noise
    6: bar(200, 0, 0); //mono noise
    7: bar(20, 0, 0); //smooth
    8: bar(15, 3, 5); //seamless
    9: bar(30, 2, 15); //mosaic
    10: bar(50, 1, 10); //twist
    11: bar(100, 55, 60); //fisheye
    12: bar(200, 1, 15); //wave
    13: bar(200, 1, 15); //wave extra
    14: bar(10, 1, 1); //wave inference
    15: bar(360, 0, 0); //smooth rotate
    16: bar(10, 1, 1); //split light
    17: bar(400, 1, 1); //wings
    18: bar(400, 1, 1); //wings down
    19: bar(400, 1, 1); //wings up
    20: bar(400, 1, 1); //wings fold
    21: bar(400, 1, 1); //wings touche
    22: bar(400, 1, 1); //wings flyer
    23: bar(400, 1, 1); //wings flipover
    24: bar(400, 1, 1); //wings wavy
    25: bar(100, 0, 0); //emboss
    26: bar(255, 0, 128); //filter red
    27: bar(255, 0, 128); //filter green
    28: bar(255, 0, 128); //filter blue
    29: bar(255, 0, 128); //filter Xred
    30: bar(255, 0, 128); //filter Xgreen
    31: bar(255, 0, 128); //filter xblue
    32: bar(255, 0, 0); //squeezehor
    33: bar(255, 0, 0); //squeezetop
    34: bar(255, 0, 0); //squeezebottom
    35: bar(255, 0, 0); //squeezediamond
    36: bar(255, 0, 0); //squeezewaste
    37: bar(255, 0, 0); //squeezeround
    38: bar(255, 0, 0); //squeezeround2
    39: bar(255, 0, 0); //splitround
    40: bar(255, 0, 0); //splitwaste
    41: bar(100, 0, 0); //shear
    42: bar(100, 1, 1); //plasma
    43: bar(100, 0, 0); //mandelbrot
    44: bar(100, 0, 0); //julia
    45: bar(127, 5, 19); //triangles
    46: bar(100, 3, 3); //ripple tooth
    47: bar(100, 3, 3); //ripple triangle
    48: bar(100, 3, 3); //ripple random
    49: bar(100, 3, 3); //texturize tile
    50: bar(100, 3, 3); //texturize overlap
    51: bar(100, 1, 1); //map
    52: bar(100, 0, 0); //blend;
    53: bar(255, 0, 1); //solarize
    54: bar(255, 1, 1); //posterize
  end;
end;

procedure TPainterEffectsF.EBarChange(Sender: TObject);
var
  index: integer;
begin
  if ETree.selected = nil then exit;
  index := ETree.selected.ImageIndex;
  if index < 0 then exit;
  case index of
    0: PainterF.contrastbarChange(sender);
    1: PainterF.saturationbarChange(sender);
    2: PainterF.lightnessbarChange(sender);
    3: PainterF.blurbarChange(sender);
    4: PainterF.splitblurbarChange(sender);
    5: PainterF.colornoisebarChange(sender);
    6: PainterF.mononoisebarChange(sender);
    7: PainterF.smoothbarChange(sender);
    8: PainterF.seambarChange;
    9: PainterF.MosaicbarChange;
    10: PainterF.TwistbarChange;
    11: PainterF.FisheyebarChange;
    12: PainterF.wavebarChange;
    13: PainterF.waveextraChange;
    14: PainterF.waveinfChange;
    15: PainterF.rotatebar;
    16: PainterF.xformAbarchange;
    17: PainterF.marblebarchange;
    18: PainterF.marble2barchange;
    19: PainterF.marble3barchange;
    20: PainterF.marble4barchange;
    21: PainterF.marble5barchange;
    22: PainterF.marble6barchange;
    23: PainterF.marble7barchange;
    24: PainterF.marble8barchange;
    25: PainterF.embossbarchange;
    26: PainterF.filterredbarchange;
    27: PainterF.filtergreenbarchange;
    28: PainterF.filterbluebarchange;
    29: PainterF.filterxredbarchange;
    30: PainterF.filterxgreenbarchange;
    31: PainterF.filterxbluebarchange;
    32: PainterF.squeezehorbarchange;
    33: PainterF.squeezetopbarchange;
    34: PainterF.squeezebotbarchange;
    35: PainterF.squeezediamondbarchange;
    36: PainterF.squeezewastebarchange;
    37: PainterF.squeezeroundbarchange;
    38: PainterF.squeezeround2barchange;
    39: PainterF.splitroundbarchange;
    40: PainterF.splitwastebarchange;
    41: PainterF.shearbarchange;
    42: PainterF.plasmabarchange;
    43: PainterF.DrawMandelJulia(true);
    44: PainterF.DrawMandelJulia(false);
    45: PainterF.drawTriangles;
    46: PainterF.rippleTooth;
    47: painterF.rippleTriangle;
    48: painterF.rippleRandom;
    49: PainterF.texturizeTile;
    50: painterF.texturizeOverlap;
    51: PainterF.drawmap;
    52: PainterF.drawblend;
    53: PainterF.drawsolarize;
    54: painterF.posterize;
  end;
end;

procedure TPainterEffectsF.setDrawImage(aDrawImage: TJvDrawImage);
begin
  PainterF := aDrawImage;
end;

end.
