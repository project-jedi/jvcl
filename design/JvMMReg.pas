{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMMReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvMMReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

procedure Register;

implementation

uses
  Classes, Graphics, ExtCtrls,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF USEWINDOWS}
  JvId3v1, JvId3v2, JvID3v2EditorForm, JvID3v2Base, JvWavePlayer,
  {$ENDIF USEWINDOWS}
  {$IFDEF VCL}
  JvAVICaptureEditors, JvAVICapture,
  {$ENDIF VCL}
  JvDsgnConsts,
  JvAni, JvAnimate, JvBmpAnimator, JvPicClip, JvIconList,
  JvEasterEgg, JvGradient, JvGradientHeaderPanel,
  JvImageRotate, JvImageTransform, JvImageSquare, JvPcx, JvStarfield,
  JvWaitingGradient, JvWaitingProgress, JvSpecialProgress,
  JvColorTrackBar,
  {$IFDEF USE_JV_GIF}
  JvGIF, JvGIFCtrl,
  {$ENDIF USE_JV_GIF}
  JvSlider, JvAnimatedImage, JvSpecialImage,
  JvPictureEditors, JvAnimatedEditor, JvPictureEditForm,
  JvIconListForm;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvMMReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvMMReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  RegisterComponents(RsPaletteImageAnimator, [TJvAnimate, TJvBmpAnimator,
    TJvPicClip, TJvImageRotate, TJvImageTransform,
    TJvImageSquare, TJvStarfield, {$IFDEF USE_JV_GIF} TJvGIFAnimator, {$ENDIF}
    TJvAnimatedImage, TJvSpecialImage {$IFDEF VCL}, TJvAVICapture{$ENDIF}]);
  RegisterComponents(RsPaletteBarPanel, [TJvGradientHeaderPanel, TJvGradient,
    TJvWaitingGradient, TJvSpecialProgress, TJvWaitingProgress, TJvColorTrackBar]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvSlider]);
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [TJvID3v1, TJvID3v2, TJvWavePlayer]);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  RegisterPropertyEditor(TypeInfo(TJvID3FileInfo), nil, '', TJvID3FileInfoEditor);
  {$ENDIF USEWINDOWS}
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TJvDriverIndex), nil, '', TJvDriverIndexEditor);
  RegisterPropertyEditor(TypeInfo(TJvVirtualKey), nil, '', TJvVirtualKeyEditor);
  {$ENDIF VCL}

  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(TPicture),TObject,'',TJvPictProperty);
  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TJvGraphicsEditor);
  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

  {$IFDEF USE_JV_GIF}
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF USE_JV_GIF}

end;

end.
