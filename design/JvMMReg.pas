{$I JVCL.INC}

unit JvMMReg;

interface

procedure Register;

implementation
{.$DEFINE USE_JV_GIF}
uses
  Classes, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvAni, JvAnimate, JvBmpAnimator, JvPicClip, JvIconList,
  JvEasterEgg, JvGradient, JvGradientHeaderPanel, JvId3v1, JvId3v2,
  JvImageRotate, JvImageTransform, JvImageWindow, JvPcx,
  JvStarfield, JvWaitingGradient, JvWaitingProgress, JvWavePlayer,
  JvSpecialProgress, JvSlider, {$IFDEF USE_JV_GIF} JvGIF, JvGIFCtrl, {$ENDIF} JvID3v2Base, JvAnimatedImage,
  JvSpecialImage, JvAVICapture, JvPictureEditors,

  JvAnimatedEditor, JvID3v2EditorForm, JvPictureEditForm, JvIconListForm, JvAVICaptureEditors;

{$R ..\resources\JvMMReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteImageAnimator,[
    TJvAnimate, TJvBmpAnimator, TJvPicClip,
    TJvImageRotate, TJvImageTransform, TJvImageWindow, TJvImageSquare,
    TJvStarfield, {$IFDEF USE_JV_GIF}TJvGIFAnimator, {$ENDIF} TJvAnimatedImage,
    TJvSpecialImage, TJvAVICapture
    ]);
  RegisterComponents(SPaletteBarPanel,[
    TJvGradientHeaderPanel, TJvGradient, TJvWaitingGradient, TJvSpecialProgress, TJvWaitingProgress
    ]);
  RegisterComponents(SPaletteNonVisual,[
    TJvId3v1, TJvId3v2, TJvWavePlayer
    ]);
  RegisterComponents(SPaletteSliderSplitter,[
    TJvSlider
    ]);

  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(TJvDriverIndex), nil, '', TJvDriverIndexEditor);
  RegisterPropertyEditor(TypeInfo(TJvVirtualKey), nil, '', TJvVirtualKeyEditor);

  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(typeinfo(TPicture),TObject,'',TJvPictProperty);
  {$ENDIF}

  {$IFDEF USE_JV_GIF}
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF}
end;

end.
