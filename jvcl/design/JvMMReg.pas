{$I JVCL.INC}

unit JvMMReg;

interface

procedure Register;

implementation
{.$DEFINE USE_JV_GIF}
uses
  Classes, DesignIntf,
  JvAni, JvAnimate, JvBmpAnimator, JvPicClip, JvIconList, 
  JvVisualId3v1, JvVisualId3v2, JvEasterEgg, JvGradient, JvGradientCaption, JvId3v1, JvId3v2,
  JvImageRotate, JvImageTransform, JvImageWindow, JvPcx,
  JvStarfield, JvWaitingGradient, JvWaitingProgress, JvWavePlayer,
  JvSpecialProgress, JvSlider, {$IFDEF USE_JV_GIF} JvGIF, JvGIFCtrl, {$ENDIF} JvID3v2Base, JvAnimatedImage,
  JvSpecialImage,

  JvAnimatedEditor, JvID3v2EditorForm, JvPictureEditForm, JvIconListForm, JvGradientCaptionForm;

{$R ..\resources\JvMMReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Multimedia',[
    TJvAnimate, TJvBmpAnimator, TJvPicClip,
    TJvGradient, TJvGradientCaption,
    TJvId3v1, TJvId3v2, TJvVisualId3v1, TJvVisualId3v2, TJvID3Controller,
    TJvImageRotate, TJvImageTransform, TJvImageWindow,
    TJvStarfield, TJvWaitingGradient, TJvWaitingProgress, TJvWavePlayer,
    TJvSpecialProgress, TJvSlider, {$IFDEF USE_JV_GIF}TJvGIFAnimator, {$ENDIF} TJvAnimatedImage,
    TJvSpecialImage
    ]);
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);

  RegisterComponentEditor(TJvGradientCaption,TGradientCaptionEditor);

  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);

  {$IFDEF USE_JV_GIF}
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF}
end;

end.
