unit JVCLArchiveReg;
interface
uses
  JvAnalogClock, JvArrow, JvBlinkingLED, JvBreatheSkin, JvClock,
  JvCommandEdit, JvCoupler, JvCPUUsage, JvFileInfo,
  JvHighlighter, JvImageWindow, JvMemoryInfos,
  JvMousePositionner, JvNagScreen, JvPerforated, JvPopupMemo,
  JvRadioControl, JvRegAuto, JvTransLED, JvTransparentForm,
  JvWinampApi,
  JvxCtrls,
  JvPlacemnt;

procedure Register;

implementation

uses
  Classes,
  JvBaseEdits;

const
  JVCLArchivePalette = 'Jv Obsolete';

procedure Register;
begin
  RegisterComponents(JVCLArchivePalette,
  [
    TJvAnalogClock, TJvArrow, TJvBlinkingLED, TJvBreatheSkin, TJvClock,
    TJvCommandEdit, TJvCoupler, TJvCpuUsage, TJvFileInfo,
    TJvHighlighter, TJvImageWindow, TJvImageSquare, TJvMemoryInfos,
    TJvMousePositionner, TJvNagScreen, TJvPerforated, TJvPopupMemo,
    TJvRadioControl, TJvRegAuto, TJvTransLED, TJvTransparentForm,
    TJvWinampApi,
    TJvSecretPanel, TJvSpeedButton, TJvTextListBox, TJvxCheckListBox, TJvxLabel
    {TJvFormPlacement}
  ]);

  RegisterComponents(JVCLArchivePalette,
  [
    TJvCalcEdit, TJvxCurrencyEdit
  ]);
end;

end.