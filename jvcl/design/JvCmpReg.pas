{$I JVCL.INC}

unit JvCmpReg;

interface

procedure Register;

implementation
uses
  Classes,
  JvAlarms, JvAni, JvAnimate, JvBmpAnimator, JvConverter, JvDataEmbedded,
  JvEnterTab, JvMergeManager, JvPageManager, JvPatchFile, JvPicClip,
  JvStringHolder, JvTimeLimit, JvWinHelp, JvTranslator, JvPrint;

{.$R ..\resources\JvCmpReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Components',[
    TJvAlarms, TJvAnimate, TJvBmpAnimator, TJvConverter, TJvDataEmbedded,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager, TJvPatchFile, TJvPicClip,
    TJvStrHolder, TJvTimeLimit, TJvWinHelp, TJvTranslator, TJvPrint

    ]);
end;

end.
