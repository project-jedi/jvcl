{$I JVCL.INC}

unit JvCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf,
  JvZoom, JvAnalogClock, JvBehaviorLabel, JvArrowButton, JvaScrollText, JvCaptionButton,
  JvClock, JvContentScroller, JvColorBox, JvColorButton, JvDice,
  JvDriveCtrls, JvFooter, JvHtControls, JvInstallLabel, JvItemsPanel,
  JvListComb, JvPageScroller, JvRegistryTreeView, JvRollOut, JvScrollPanel,
  JvScrollText, JvSpacer, JvSpeedBar, JvSplit, JvSplitter, JvSwitch, JvSyncSplitter,
  JvTransparentButton, JvTransLED, JvxClock, JvSpeedbarSetupForm,
  JvColorForm, JvDsgnIntf, JvImageDrawThread, JvRegAuto, JvWinampLabel,
  JvButtons, JvCaptionPanel,
  JvBehaviorLabelEditor;

{.$R ..\resources\JvCtrlsReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Controls',[
    TJvZoom, TJvAnalogClock, TJvBehaviorLabel, TJvArrowButton, TJvaScrollText,
    TJvCaptionButton, TJvClock, TJvContentScroller, TJvColorButton, TJvDice,
    TJvDriveCombo, TJvDriveList, TJvFileListBox, TJvDirectoryListBox,
    TJvFooter, TJvInstallLabel, TJvItemsPanel,
    TJvHtListBox, TJvHTComboBox, TJvHTLabel,
    TJvImageComboBox, TJvImageListBox, TJvPageScroller, TJvRegistryTreeView,
    TJvRollOut, TJvScrollingWindow, TJvScrollText, TJvSpacer, TJvSpeedBar,
    TJvSplitter, TJvxSplitter, TJvSwitch, TJvSyncSplitter,  TJvTransparentButton,
    TJvTransparentButton2, TJvTransLED, TJvxClock, TJvRegAuto, TJvWinampLabel,
    TJvHTButton, TJvCaptionPanel, TJvBehaviorLabel
    ]);
  RegisterPropertyEditor(typeinfo(TJvLabelBehaviorName),TJvBehaviorLabel,'Behavior',TJvLabelBehaviorProperty);
end;

end.
