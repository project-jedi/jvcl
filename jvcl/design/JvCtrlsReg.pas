{$I JVCL.INC}

unit JvCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, Controls,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvZoom, JvAnalogClock, JvBehaviorLabel, JvArrowButton, JvaScrollText, JvCaptionButton,
  JvClock, JvContentScroller, JvColorBox, JvColorButton, JvDice,
  JvDriveCtrls, JvFooter, JvGroupHeader, JvHint, JvHtControls, JvInstallLabel, JvItemsPanel,
  JvListComb, JvPageScroller, JvRegistryTreeView, JvRollOut, JvScrollPanel,
  JvScrollText, JvSpacer, JvSpeedBar, JvSplit, JvSplitter, JvSwitch, JvSyncSplitter,
  JvTransparentButton, JvTransLED, JvxClock, JvSpeedbarSetupForm,
  JvColorForm, JvDsgnIntf, JvImageDrawThread, JvRegAuto, JvWinampLabel, JvPlaylist, JvComponentPanel,
  JvButtons, JvCaptionPanel, JvScrollMax, JvUninstallControls, JvMovableBevel,
  JvRegAutoEditorForm, JvScrollMaxEditor, JvBehaviorLabelEditor, JvGroupHeaderEditor, JvFooterEditor,
  JvSpeedbarForm,
  JvDsgnEditors;

{$R ..\resources\JvCtrlsReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteButton,[
    TJvTransparentButton, TJvTransparentButton2,
    TJvArrowButton, TJvCaptionButton, TJvColorButton, TJvHTButton,
    TJvTransLED, TJvSpacer, TJvSwitch

    ]);
  RegisterComponents(SPaletteBarPanel,[
    TJvSpeedBar,
    TJvCaptionPanel, TJvItemsPanel, TJvMovableBevel, TJvRollOut,
    TJvFooter, TJvGroupHeader, TJvComponentPanel
    ]);
  RegisterComponents(SPaletteLabel,[
    TJvBehaviorLabel, TJvInstallLabel, TJvHTLabel, TJvWinampLabel
    ]);
  RegisterComponents(SPaletteListComboTree,[
    TJvImageComboBox, TJvImageListBox,
    TJvHTListBox, TJvHTComboBox,
    TJvUninstallComboBox,TJvUninstallListBox,
    TJvDriveCombo, TJvDriveList, TJvFileListBox, TJvDirectoryListBox,
    TJvRegistryTreeView, TJvPlaylist
    ]);
  RegisterComponents(SPaletteScrollerTracker,[
    TJvScrollMax, TJvaScrollText, TJvContentScroller, TJvPageScroller,
    TJvScrollingWindow, TJvScrollText
    ]);
  RegisterComponents(SPaletteSliderSplitter,[
    TJvSplitter, TJvxSplitter, TJvSyncSplitter
    ]);
  RegisterComponents(SPaletteVisual,[
    TJvAnalogClock, TJvClock, TJvxClock,
    TJvZoom, TJvDice
    ]);
  RegisterComponents(SPaletteNonVisual,[TJvHint, TJvRegAuto]);

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
