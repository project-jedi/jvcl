{$I JVCL.INC}

unit JvMMReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf,
  JvAni, JvAnimate, JvBmpAnimator, JvPicClip,
  JvVisualId3v1, JvVisualId3v2, JvEasterEgg, JvGradient, JvGradientCaption, JvId3v1, JvId3v2,
  JvImageRotate, JvImageTransform, JvImageWindow, JvPcx,
  JvStarfield, JvWaitingGradient, JvWaitingProgress, JvWavePlayer,
  JvSpecialProgress, JvSlider, JvGIF, JvGIFCtrl, JvID3v2Base, JvAnimatedImage,
  JvSpecialImage,

  JvAnimatedEditor, JvPictureEditForm;

{.$R ..\resources\JvMMReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Multimedia',[
    TJvAnimate, TJvBmpAnimator, TJvPicClip,
    TJvGradient, TJvGradientCaption,
    TJvId3v1, TJvId3v2, TJvVisualId3v1, TJvVisualId3v2, TJvID3Controller,
    TJvImageRotate, TJvImageTransform, TJvImageWindow,
    TJvStarfield, TJvWaitingGradient, TJvWaitingProgress, TJvWavePlayer,
    TJvSpecialProgress, TJvSlider, TJvGIFAnimator, TJvAnimatedImage,
    TJvSpecialImage
    ]);
  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
end;

end.
