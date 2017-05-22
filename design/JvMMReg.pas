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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMMReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Graphics, ExtCtrls,
  DesignEditors, DesignIntf,
  JvDsgnConfig,
  JvId3v1, JvId3v2, JvID3v2EditorForm, JvID3v2Base, JvWavePlayer,
  JvAVICaptureEditors, JvAVICapture,
  JvDsgnConsts,
  JvAni, JvAnimate, JvBmpAnimator, JvPicClip, JvIconList,
  JvEasterEgg, JvGradient, JvGradientHeaderPanel,
  JvImageRotate, JvImageTransform, JvImageSquare, JvPcx, JvStarfield,
  JvWaitingGradient, JvWaitingProgress, JvSpecialProgress,
  JvColorTrackBar, JvCursor,
  JvGIF, JvGIFCtrl,
  JvSlider, JvAnimatedImage, JvSpecialImage, JvPictureEditors,
  JvAnimatedEditor, JvPictureEditForm, JvIconListForm,
  JvFullColorDialogs, JvFullColorCtrls, JvFullColorEditors,
  JvFullColorSpacesEditors, JvFullColorSpaces;

{$R JvMMReg.dcr}

procedure Register;
begin
  RegisterComponents(RsPaletteImageAnimator, [TJvAnimate, TJvBmpAnimator,
    TJvPicClip, TJvImageRotate, TJvImageTransform,
    TJvImageSquare, TJvStarfield, TJvGIFAnimator,
    TJvAnimatedImage, TJvSpecialImage, TJvAVICapture]);
  RegisterComponents(RsPaletteBarPanel, [TJvGradientHeaderPanel, TJvGradient,
    TJvWaitingGradient, TJvSpecialProgress, TJvWaitingProgress, TJvColorTrackBar]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvSlider]);
  RegisterComponents(RsPaletteNonVisual, [TJvID3v1, TJvID3v2, TJvWavePlayer]);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  RegisterPropertyEditor(TypeInfo(TJvID3FileInfo), nil, '', TJvID3FileInfoEditor);
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(TJvDriverIndex), nil, '', TJvDriverIndexEditor);
  RegisterPropertyEditor(TypeInfo(TJvVirtualKey), nil, '', TJvVirtualKeyEditor);

  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  if JvOptionRegisterGlobalDesignEditors then
  begin
    RegisterPropertyEditor(TypeInfo(TPicture),TObject,'',TJvPictProperty);
    RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
    RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);
    RegisterComponentEditor(TImage, TJvGraphicsEditor);
    RegisterPropertyEditor(TypeInfo(TColor), nil, '', TJvFullColorProperty);
  end;

  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$IFDEF COMPILER12_UP}
  // prevent "Invalid graphic format" exception in the IDE's Picture-Edit.
  RegisterPropertyEditor(TypeInfo(TJvGIFImage), nil, '', TJvGraphicPropertyEditor);
  {$ENDIF COMPILER12_UP}

  // JvFullColor components and editors
  RegisterComponents(RsPaletteBarPanel, [TJvFullColorPanel, TJvFullColorTrackBar, TJvFullColorGroup]);
  RegisterComponents(RsPaletteLabel, [TJvFullColorLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvFullColorSpaceCombo, TJvFullColorAxisCombo]);
  RegisterComponents(RsPaletteDialog, [TJvFullColorDialog, TJvFullColorCircleDialog]);
  RegisterComponents(RsPaletteVisual, [TJvFullColorCircle]);

  RegisterPropertyEditor(TypeInfo(TJvFullColor), nil, '', TJvFullColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvFullColorList), nil, '', TJvFullColorListEditor);
  RegisterSelectionEditor(TJvFullColorPanel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorTrackBar, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorCircle, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorLabel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorSpaceCombo, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorAxisCombo, TJvFullColorSelection);
  RegisterPropertyEditor(TypeInfo(TJvFullColorSpaceID), nil, '', TJvColorIDEditor);
end;

end.