{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPainterEffectsU.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPainterEffectsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  JvDrawImage, JvComponent;

type
  TPainterEffectsForm = class(TJvForm)
    EffectsPanel: TPanel;
    EBar: TScrollBar;
    ExtraBar: TScrollBar;
    ETree: TTreeView;
    CXBar: TScrollBar;
    CYBar: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ETreeClick(Sender: TObject);
    procedure EBarChange(Sender: TObject);
  private
    FPainterForm: TJvDrawImage;
    procedure Bar(AMax, AMin, APos: Integer);
  public
    procedure SetDrawImage(ADrawImage: TJvDrawImage);
  end;

implementation

{$R *.dfm}

procedure TPainterEffectsForm.Bar(AMax, AMin, APos: Integer);
begin
  EBar.Max := AMax;
  EBar.Min := AMin;
  EBar.Position := APos;
end;

procedure TPainterEffectsForm.ETreeClick(Sender: TObject);
begin
  // (rom) better change to a const array to index
  if ETree.Selected <> nil then
    case ETree.Selected.ImageIndex of
      0:
        Bar(100, 0, 0); // contrast
      1:
        Bar(255, 0, 0); // saturation
      2:
        Bar(100, 0, 0); // brightness
      3:
        Bar(20, 0, 0); // gaussian blur
      4:
        Bar(30, 0, 0); // split blur
      5:
        Bar(200, 0, 0); // color noise
      6:
        Bar(200, 0, 0); // mono noise
      7:
        Bar(20, 0, 0); // smooth
      8:
        Bar(15, 3, 5); // seamless
      9:
        Bar(30, 2, 15); // mosaic
      10:
        Bar(50, 1, 10); // twist
      11:
        Bar(100, 55, 60); //f isheye
      12:
        Bar(200, 1, 15); // wave
      13:
        Bar(200, 1, 15); // wave extra
      14:
        Bar(10, 1, 1); // wave inference
      15:
        Bar(360, 0, 0); // smooth rotate
      16:
        Bar(10, 1, 1); // split light
      17:
        Bar(400, 1, 1); // wings
      18:
        Bar(400, 1, 1); // wings down
      19:
        Bar(400, 1, 1); // wings up
      20:
        Bar(400, 1, 1); // wings fold
      21:
        Bar(400, 1, 1); // wings touche
      22:
        Bar(400, 1, 1); // wings flyer
      23:
        Bar(400, 1, 1); // wings flipover
      24:
        Bar(400, 1, 1); // wings wavy
      25:
        Bar(100, 0, 0); // emboss
      26:
        Bar(255, 0, 128); // filter red
      27:
        Bar(255, 0, 128); // filter green
      28:
        Bar(255, 0, 128); // filter blue
      29:
        Bar(255, 0, 128); // filter Xred
      30:
        Bar(255, 0, 128); // filter Xgreen
      31:
        Bar(255, 0, 128); // filter xblue
      32:
        Bar(255, 0, 0); // squeezehor
      33:
        Bar(255, 0, 0); // squeezetop
      34:
        Bar(255, 0, 0); // squeezebottom
      35:
        Bar(255, 0, 0); // squeezediamond
      36:
        Bar(255, 0, 0); // squeezewaste
      37:
        Bar(255, 0, 0); // squeezeround
      38:
        Bar(255, 0, 0); // squeezeround2
      39:
        Bar(255, 0, 0); // splitround
      40:
        Bar(255, 0, 0); // splitwaste
      41:
        Bar(100, 0, 0); // shear
      42:
        Bar(100, 1, 1); // plasma
      43:
        Bar(100, 0, 0); // mandelbrot
      44:
        Bar(100, 0, 0); // julia
      45:
        Bar(127, 5, 19); // triangles
      46:
        Bar(100, 3, 3); // ripple tooth
      47:
        Bar(100, 3, 3); // ripple triangle
      48:
        Bar(100, 3, 3); // ripple random
      49:
        Bar(100, 3, 3); // texturize tile
      50:
        Bar(100, 3, 3); // texturize overlap
      51:
        Bar(100, 1, 1); // map
      52:
        Bar(100, 0, 0); // blend;
      53:
        Bar(255, 0, 1); // solarize
      54:
        Bar(255, 1, 1); // posterize
    end;
end;

procedure TPainterEffectsForm.EBarChange(Sender: TObject);
begin
  if ETree.Selected <> nil then
    with FPainterForm do
      case ETree.Selected.ImageIndex of
        0:
          ContrastBarChange(Sender);
        1:
          SaturationBarChange(Sender);
        2:
          lightnessBarChange(Sender);
        3:
          BlurBarChange(Sender);
        4:
          SplitBlurBarChange(Sender);
        5:
          ColorNoiseBarChange(Sender);
        6:
          MonoNoiseBarChange(Sender);
        7:
          SmoothBarChange(Sender);
        8:
          SeamBarChange;
        9:
          MosaicBarChange;
        10:
          TwistBarChange;
        11:
          FisheyeBarChange;
        12:
          WaveBarChange;
        13:
          WaveExtraChange;
        14:
          WaveInfChange;
        15:
          RotateBar;
        16:
          XFormABarChange;
        17:
          MarbleBarChange;
        18:
          Marble2BarChange;
        19:
          Marble3BarChange;
        20:
          Marble4BarChange;
        21:
          Marble5BarChange;
        22:
          Marble6BarChange;
        23:
          Marble7BarChange;
        24:
          Marble8BarChange;
        25:
          EmbossBarChange;
        26:
          FilterRedBarChange;
        27:
          FilterGreenBarChange;
        28:
          FilterBlueBarChange;
        29:
          FilterXRedBarChange;
        30:
          FilterXGreenBarChange;
        31:
          FilterXBlueBarChange;
        32:
          SqueezeHorBarChange;
        33:
          SqueezeTopBarChange;
        34:
          SqueezeBotBarChange;
        35:
          SqueezeDiamondBarChange;
        36:
          SqueezeWasteBarChange;
        37:
          SqueezeRoundBarChange;
        38:
          SqueezeRound2BarChange;
        39:
          SplitRoundBarChange;
        40:
          SplitWasteBarChange;
        41:
          ShearBarChange;
        42:
          PlasmaBarChange;
        43:
          DrawMandelJulia(True);
        44:
          DrawMandelJulia(False);
        45:
          DrawTriangles;
        46:
          RippleTooth;
        47:
          RippleTriangle;
        48:
          RippleRandom;
        49:
          TexturizeTile;
        50:
          TexturizeOverlap;
        51:
          DrawMap;
        52:
          DrawBlend;
        53:
          DrawSolarize;
        54:
          Posterize;
      end;
end;

procedure TPainterEffectsForm.SetDrawImage(ADrawImage: TJvDrawImage);
begin
  FPainterForm := ADrawImage;
end;

end.
