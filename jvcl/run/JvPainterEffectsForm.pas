{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPainterEffectsU.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPainterEffectsForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QComCtrls,
  {$ENDIF VisualCLX}
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

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure TPainterEffectsForm.Bar(AMax, AMin, APos: Integer);
begin
  EBar.Max := AMax;
  EBar.Min := AMin;
  EBar.Position := APos;
end;

procedure TPainterEffectsForm.ETreeClick(Sender: TObject);
type
  TBarParams = record
    Max: Integer;
    Min: Integer;
    Pos: Integer;
  end;
const
  cEffects: array [0..54] of TBarParams =
   (
    (Max: 100; Min: 0;  Pos: 0),    // contrast
    (Max: 255; Min: 0;  Pos: 0),    // saturation
    (Max: 100; Min: 0;  Pos: 0),    // brightness
    (Max: 20;  Min: 0;  Pos: 0),    // gaussian blur
    (Max: 30;  Min: 0;  Pos: 0),    // split blur
    (Max: 200; Min: 0;  Pos: 0),    // color noise
    (Max: 200; Min: 0;  Pos: 0),    // mono noise
    (Max: 20;  Min: 0;  Pos: 0),    // smooth
    (Max: 15;  Min: 3;  Pos: 5),    // seamless
    (Max: 30;  Min: 2;  Pos: 15),   // mosaic
    (Max: 50;  Min: 1;  Pos: 10),   // twist
    (Max: 100; Min: 55; Pos: 60),   // fisheye
    (Max: 200; Min: 1;  Pos: 15),   // wave
    (Max: 200; Min: 1;  Pos: 15),   // wave extra
    (Max: 10;  Min: 1;  Pos: 1),    // wave inference
    (Max: 360; Min: 0;  Pos: 0),    // smooth rotate
    (Max: 10;  Min: 1;  Pos: 1),    // split light
    (Max: 400; Min: 1;  Pos: 1),    // wings
    (Max: 400; Min: 1;  Pos: 1),    // wings down
    (Max: 400; Min: 1;  Pos: 1),    // wings up
    (Max: 400; Min: 1;  Pos: 1),    // wings fold
    (Max: 400; Min: 1;  Pos: 1),    // wings touche
    (Max: 400; Min: 1;  Pos: 1),    // wings flyer
    (Max: 400; Min: 1;  Pos: 1),    // wings flipover
    (Max: 400; Min: 1;  Pos: 1),    // wings wavy
    (Max: 100; Min: 0;  Pos: 0),    // emboss
    (Max: 255; Min: 0;  Pos: 128),  // filter red
    (Max: 255; Min: 0;  Pos: 128),  // filter green
    (Max: 255; Min: 0;  Pos: 128),  // filter blue
    (Max: 255; Min: 0;  Pos: 128),  // filter Xred
    (Max: 255; Min: 0;  Pos: 128),  // filter Xgreen
    (Max: 255; Min: 0;  Pos: 128),  // filter xblue
    (Max: 255; Min: 0;  Pos: 0),    // squeezehor
    (Max: 255; Min: 0;  Pos: 0),    // squeezetop
    (Max: 255; Min: 0;  Pos: 0),    // squeezebottom
    (Max: 255; Min: 0;  Pos: 0),    // squeezediamond
    (Max: 255; Min: 0;  Pos: 0),    // squeezewaste
    (Max: 255; Min: 0;  Pos: 0),    // squeezeround
    (Max: 255; Min: 0;  Pos: 0),    // squeezeround2
    (Max: 255; Min: 0;  Pos: 0),    // splitround
    (Max: 255; Min: 0;  Pos: 0),    // splitwaste
    (Max: 100; Min: 0;  Pos: 0),    // shear
    (Max: 100; Min: 1;  Pos: 1),    // plasma
    (Max: 100; Min: 0;  Pos: 0),    // mandelbrot
    (Max: 100; Min: 0;  Pos: 0),    // julia
    (Max: 127; Min: 5;  Pos: 19),   // triangles
    (Max: 100; Min: 3;  Pos: 3),    // ripple tooth
    (Max: 100; Min: 3;  Pos: 3),    // ripple triangle
    (Max: 100; Min: 3;  Pos: 3),    // ripple random
    (Max: 100; Min: 3;  Pos: 3),    // texturize tile
    (Max: 100; Min: 3;  Pos: 3),    // texturize overlap
    (Max: 100; Min: 1;  Pos: 1),    // map
    (Max: 100; Min: 0;  Pos: 0),    // blend;
    (Max: 255; Min: 0;  Pos: 1),    // solarize
    (Max: 255; Min: 1;  Pos: 1)     // posterize
   );
var
  N: Integer;
begin
  if ETree.Selected <> nil then
  begin
    N := ETree.Selected.ImageIndex;
    if (N >= Low(cEffects)) and (N >= Low(cEffects)) then
      Bar(cEffects[N].Max, cEffects[N].Min, cEffects[N].Pos);
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
