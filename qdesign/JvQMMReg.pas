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

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQMMReg;

interface

procedure Register;

implementation

uses
  Classes,

  DesignEditors, DesignIntf, QGraphics,

  JvQDsgnConsts, JvQQtKeyEditorForm,
  JvQAni, JvQBmpAnimator, JvQPicClip, JvQIconList,
  JvQGradient, JvQGradientHeaderPanel,
  {$IFDEF MSWINDOWS}
  JvQId3v1, JvQId3v2, JvQID3v2Base, JvQWavePlayer,
  {$ENDIF MSWINDOWS}
  JvQImageRotate, JvQImageTransform, JvQImageSquare, JvQStarfield,
  JvQWaitingGradient, JvQWaitingProgress, JvQSpecialProgress,
  JvQSlider, JvQAnimatedImage, JvQSpecialImage, JvQAnimatedEditor,
  JvQIconListForm, JvQColorTrackBar
  {$IFDEF _LINUX}, JvQPictureEditors,  JvQPictureEditForm {$ENDIF}
  ;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvMMReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvMMReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  RegisterComponents(RsPaletteImageAnimator, [TJvBmpAnimator,
    TJvPicClip, TJvImageRotate, TJvImageTransform,
    TJvImageSquare, TJvStarfield,
    TJvAnimatedImage, TJvSpecialImage]);
  RegisterComponents(RsPaletteBarPanel, [TJvGradientHeaderPanel, TJvGradient,
    TJvWaitingGradient, TJvSpecialProgress, TJvWaitingProgress, TJvColorTrackBar]);
  {$IFDEF MSWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [TJvID3v1, TJvID3v2, TJvWavePlayer]);
  {$ENDIF MSWINDOWS}
  RegisterComponents(RsPaletteSliderSplitter, [TJvSlider]);

  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
//  RegisterPropertyEditor(TypeInfo(TJvDriverIndex), nil, '', TJvDriverIndexEditor);
//  RegisterPropertyEditor(TypeInfo(TJvQtKey), nil, '', TJvQtKeyEditor);
//  RegisterPropertyEditor(TypeInfo(TJvID3FileInfo), nil, '', TJvID3FileInfoEditor);

  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  {$IFDEF _LINUX}
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  {$ENDIF LINUX}
//  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
//  RegisterPropertyEditor(TypeInfo(TPicture),TObject,'',TJvPictProperty);
//  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
//  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);
//  RegisterComponentEditor(TImage, TJvGraphicsEditor);
  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}

  {$IFDEF USE_JV_GIF}
//  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF USE_JV_GIF}

end;

end.
