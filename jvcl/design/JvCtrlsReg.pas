{$I JVCL.INC}

unit JvCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, Controls, DesignIntf, DesignEditors, 
  JvZoom, JvAnalogClock, JvBehaviorLabel, JvArrowButton, JvaScrollText, JvCaptionButton,
  JvClock, JvContentScroller, JvColorBox, JvColorButton, JvDice,
  JvDriveCtrls, JvFooter, JvGroupHeader, JvHint, JvHtControls, JvInstallLabel, JvItemsPanel,
  JvListComb, JvPageScroller, JvRegistryTreeView, JvRollOut, JvScrollPanel,
  JvScrollText, JvSpacer, JvSpeedBar, JvSplit, JvSplitter, JvSwitch, JvSyncSplitter,
  JvTransparentButton, JvTransLED, JvxClock, JvSpeedbarSetupForm,
  JvColorForm, JvDsgnIntf, JvImageDrawThread, JvRegAuto, JvWinampLabel, JvPlaylist, JvComponentPanel,
  JvButtons, JvCaptionPanel, JvScrollMax, JvUninstallControls, JvMovableBevel,
  JvRegAutoEditor, JvScrollMaxEditor, JvBehaviorLabelEditor, JvGroupHeaderEditor, JvFooterEditor,
  JvSpeedbarForm,
  JvDsgnEditors;

{$R ..\resources\JvCtrlsReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Controls',[
    TJvZoom, TJvAnalogClock, TJvBehaviorLabel, TJvArrowButton, TJvaScrollText,
    TJvCaptionButton, TJvClock, TJvContentScroller, TJvColorButton, TJvDice,
    TJvDriveCombo, TJvDriveList, TJvFileListBox, TJvDirectoryListBox,
    TJvFooter, TJvGroupHeader, TJvInstallLabel, TJvItemsPanel,
    TJvHint, TJvHtListBox, TJvHTComboBox, TJvHTLabel, TJvHTButton, TJvComponentPanel,
    TJvImageComboBox, TJvImageListBox, TJvPageScroller, TJvRegistryTreeView,
    TJvRollOut, TJvScrollingWindow, TJvScrollText, TJvSpacer, TJvSpeedBar,
    TJvSplitter, TJvxSplitter, TJvSwitch, TJvSyncSplitter,  TJvTransparentButton,
    TJvTransparentButton2, TJvTransLED, TJvxClock, TJvRegAuto, TJvWinampLabel, TJvPlaylist,
    TJvCaptionPanel, TJvScrollMax, TJvBehaviorLabel,
    TJvUninstallComboBox,TJvUninstallListBox, TJvMovableBevel
    ]);

  RegisterPropertyEditor(typeinfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(typeinfo(TJvLabelBehaviorName),TJvBehaviorLabel,'Behavior',TJvLabelBehaviorProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
  RegisterPropertyEditor(TypeInfo(TDateTime),TJvAlarmInfo,'Date',TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime),TJvAlarmInfo,'Date',TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime),TJvAnalogClock,'Time',TJvTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', TStringProperty);

  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  RegisterComponentEditor(TJvGroupHeader, TJvGroupHeaderEditor);
  RegisterComponentEditor(TJvFooter, TJvFooterEditor);
  RegisterComponentEditor(TJvImageListBox, TJvStringsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvStringsEditor);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);
  RegisterComponentEditor(TJvRegAuto, TJvRegAutoEditor);

  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterClass(TJvScrollMaxBand);

end;

end.
