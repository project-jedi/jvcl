{$I JVCL.INC}

unit JvMMReg;

interface

procedure Register;

implementation
{.$DEFINE USE_JV_GIF}
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvAni, JvAnimate, JvBmpAnimator, JvPicClip, JvIconList,
  JvEasterEgg, JvGradient, JvGradientHeaderPanel, JvId3v1, JvId3v2,
  JvImageRotate, JvImageTransform, JvImageWindow, JvPcx,
  JvStarfield, JvWaitingGradient, JvWaitingProgress, JvWavePlayer,
  JvSpecialProgress, JvSlider, {$IFDEF USE_JV_GIF} JvGIF, JvGIFCtrl, {$ENDIF} JvID3v2Base, JvAnimatedImage,
  JvSpecialImage,

  JvAnimatedEditor, JvID3v2EditorForm, JvPictureEditForm, JvIconListForm;

{$R ..\resources\JvMMReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Multimedia',[
    TJvAnimate, TJvBmpAnimator, TJvPicClip,
    TJvGradient, TJvGradientHeaderPanel,
    TJvId3v1, TJvId3v2, 
    TJvImageRotate, TJvImageTransform, TJvImageWindow,
    TJvStarfield, TJvWaitingGradient, TJvWaitingProgress, TJvWavePlayer,
    TJvSpecialProgress, TJvSlider, {$IFDEF USE_JV_GIF}TJvGIFAnimator, {$ENDIF} TJvAnimatedImage,
    TJvSpecialImage
    ]);
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);



  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);

  {$IFDEF USE_JV_GIF}
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF}
end;

end.
