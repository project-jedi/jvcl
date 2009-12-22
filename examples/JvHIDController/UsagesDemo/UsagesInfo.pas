{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit UsagesInfo;

interface

uses
  Hid, HidUsage;

procedure UsageAndUsagePageText(UsagePage, Usage: TUsage; var UsagePageText, UsageText: string);

implementation

uses
  SysUtils;

resourcestring
  Rs0 = '0';
  Rs1 = '1';
  Rs14SegmentDirectMap = '14 Segment Direct Map';
  Rs2 = '2';
  Rs2DControlReport = '2D Control Report';
  Rs2DModeAdjust = '2D Mode Adjust';
  Rs2DModeSelect = '2D Mode Select';
  Rs3 = '3';
  Rs3DDigitizer = '3D Digitizer';
  Rs3DGameController = '3D Game Controller';
  Rs4 = '4';
  Rs5 = '5';
  Rs6 = '6';
  Rs7 = '7';
  Rs7SegmentDirectMap = '7 Segment Direct Map';
  Rs8 = '8';
  Rs9 = '9';
  RsA = 'A';
  RsabsoluteStateofcharge = 'absolute State of charge';
  RsAccelerator = 'Accelerator';
  RsACPresent = 'AC Present';
  RsActivePower = 'Active Power';
  RsActiveTime = 'Active Time';
  RsActuatorOverrideSwitch = 'Actuator Override Switch';
  RsActuatorPower = 'Actuator Power';
  RsActuatorsEnabled = 'Actuators Enabled';
  RsAddEAN2_3LabelDefinition = 'Add EAN 2/3 Label Definition';
  RsAgain = 'Again';
  RsAileron = 'Aileron';
  RsAileronTrim = 'Aileron Trim';
  RsAimDuration = 'Aim Duration';
  RsAiming_PointerMode = 'Aiming/Pointer Mode';
  RsAimingLaserPattern = 'Aiming Laser Pattern';
  RsAirplane = 'Airplane';
  RsAlarmInhibited = 'Alarm Inhibited';
  RsAlphanumeric = 'Alphanumeric';
  RsAlphanumericDisplay = 'Alphanumeric Display';
  RsAlternateAudioDecrement = 'Alternate Audio Decrement';
  RsAlternateAudioIncrement = 'Alternate Audio Increment';
  RsAlternateErase = 'Alternate Erase';
  RsAlternateFunction = 'Alternate Function';
  RsAltitude = 'Altitude';
  RsAM_PM = 'AM/PM';
  RsAmber = 'Amber';
  RsAnimatronicDevice = 'Animatronic Device';
  RsAnsweringMachine = 'Answering Machine';
  RsAnswerOn_Off = 'Answer On/Off';
  RsAnti_TorqueControl = 'Anti-Torque Control';
  RsApostroph = '''';
  RsApparentPower = 'Apparent Power';
  RsApplication = 'Application';
  RsApplicationControlAddToCart = 'Application Control Add To Cart';
  RsApplicationControlAllCaps = 'Application Control All Caps';
  RsApplicationControlAttachComment = 'Application Control Attach Comment';
  RsApplicationControlAttachFile = 'Application Control Attach File';
  RsApplicationControlBack = 'Application Control Back';
  RsApplicationControlBold = 'Application Control Bold';
  RsApplicationControlBookmarks = 'Application Control Bookmarks';
  RsApplicationControlBulletedList = 'Application Control Bulleted List';
  RsApplicationControlBuyCheckout = 'Application Control Buy Checkout';
  RsApplicationControlCancel = 'Application Control Cancel';
  RsApplicationControlCatalog = 'Application Control Catalog';
  RsApplicationControlClearAlarm = 'Application Control Clear Alarm';
  RsApplicationControlCollapse = 'Application Control Collapse';
  RsApplicationControlCollapseAll = 'Application Control Collapse All';
  RsApplicationControlCopy = 'Application Control Copy';
  RsApplicationControlCose = 'Application Control Cose';
  RsApplicationControlCut = 'Application Control Cut';
  RsApplicationControlDelete = 'Application Control Delete';
  RsApplicationControlDeleteComment = 'Application Control Delete Comment';
  RsApplicationControlDemote = 'Application Control Demote';
  RsApplicationControlDistributeHorizontally = 'Application Control Distribute Horizontally';
  RsApplicationControlDistributeVertically = 'Application Control Distribute Vertically';
  RsApplicationControlDownload = 'Application Control Download';
  RsApplicationControlEdit = 'Application Control Edit';
  RsApplicationControlEditTimeZones = 'Application Control Edit Time Zones';
  RsApplicationControlExit = 'Application Control Exit';
  RsApplicationControlExpand = 'Application Control Expand';
  RsApplicationControlExpandAll = 'Application Control Expand All';
  RsApplicationControlFilter = 'Application Control Filter';
  RsApplicationControlFind = 'Application Control Find';
  RsApplicationControlFindandReplace = 'Application Control Find and Replace';
  RsApplicationControlFlipHorizontal = 'Application Control Flip Horizontal';
  RsApplicationControlFlipVertical = 'Application Control Flip Vertical';
  RsApplicationControlFontColor = 'Application Control Font Color';
  RsApplicationControlFontSelect = 'Application Control Font Select';
  RsApplicationControlFontSize = 'Application Control Font Size';
  RsApplicationControlFormat = 'Application Control Format';
  RsApplicationControlForward = 'Application Control Forward';
  RsApplicationControlForwardMessage = 'Application Control Forward Message';
  RsApplicationControlFullScreenView = 'Application Control Full Screen View';
  RsApplicationControlGoTo = 'Application Control Go To';
  RsApplicationControlHistory = 'Application Control History';
  RsApplicationControlHome = 'Application Control Home';
  RsApplicationControlIndentDecrease = 'Application Control Indent Decrease';
  RsApplicationControlIndentIncrease = 'Application Control Indent Increase';
  RsApplicationControlInsertColumn = 'Application Control Insert Column';
  RsApplicationControlInsertFile = 'Application Control Insert File';
  RsApplicationControlInsertMode = 'Application Control Insert Mode';
  RsApplicationControlInsertObject = 'Application Control Insert Object';
  RsApplicationControlInsertPicture = 'Application Control Insert Picture';
  RsApplicationControlInsertRow = 'Application Control Insert Row';
  RsApplicationControlInsertSymbol = 'Application Control Insert Symbol';
  RsApplicationControlItalics = 'Application Control Italics';
  RsApplicationControlJustifyBlockHorizontally = 'Application Control Justify Block Horizontally';
  RsApplicationControlJustifyBlockVertically = 'Application Control Justify Block Vertically';
  RsApplicationControlJustifyBottom = 'Application Control Justify Bottom';
  RsApplicationControlJustifyCenterHorizontally = 'Application Control Justify Center Horizontally';
  RsApplicationControlJustifyCenterVertically = 'Application Control Justify Center Vertically';
  RsApplicationControlJustifyLeft = 'Application Control Justify Left';
  RsApplicationControlJustifyRight = 'Application Control Justify Right';
  RsApplicationControlJustifyTop = 'Application Control Justify Top';
  RsApplicationControlLock = 'Application Control Lock';
  RsApplicationControlMaximize = 'Application Control Maximize';
  RsApplicationControlMerge = 'Application Control Merge';
  RsApplicationControlMinimize = 'Application Control Minimize';
  RsApplicationControlMirrorHorizontal = 'Application Control Mirror Horizontal';
  RsApplicationControlMirrorVertical = 'Application Control Mirror Vertical';
  RsApplicationControlNew = 'Application Control New';
  RsApplicationControlNewWindow = 'Application Control New Window';
  RsApplicationControlNextLink = 'Application Control Next Link';
  RsApplicationControlNo = 'Application Control No';
  RsApplicationControlNormalView = 'Application Control Normal View';
  RsApplicationControlNumberedList = 'Application Control Numbered List';
  RsApplicationControlOpen = 'Application Control Open';
  RsApplicationControlPan = 'Application Control Pan';
  RsApplicationControlPanLeft = 'Application Control Pan Left';
  RsApplicationControlPanRight = 'Application Control Pan Right';
  RsApplicationControlPaste = 'Application Control Paste';
  RsApplicationControlPasteSpecial = 'Application Control Paste Special';
  RsApplicationControlPreviousLink = 'Application Control Previous Link';
  RsApplicationControlPrint = 'Application Control Print';
  RsApplicationControlPrintPreview = 'Application Control Print Preview';
  RsApplicationControlPromote = 'Application Control Promote';
  RsApplicationControlProperties = 'Application Control Properties';
  RsApplicationControlProtect = 'Application Control Protect';
  RsApplicationControlRedo_Repeat = 'Application Control Redo/Repeat';
  RsApplicationControlRefresh = 'Application Control Refresh';
  RsApplicationControlRename = 'Application Control Rename';
  RsApplicationControlReply = 'Application Control Reply';
  RsApplicationControlReplyAll = 'Application Control Reply All';
  RsApplicationControlResetAlarm = 'Application Control Reset Alarm';
  RsApplicationControlResize = 'Application Control Resize';
  RsApplicationControlRestartNumbering = 'Application Control Restart Numbering';
  RsApplicationControlRotate = 'Application Control Rotate';
  RsApplicationControlSave = 'Application Control Save';
  RsApplicationControlSaveandClose = 'Application Control Save and Close';
  RsApplicationControlScroll = 'Application Control Scroll';
  RsApplicationControlScrollDown = 'Application Control Scroll Down';
  RsApplicationControlScrollUp = 'Application Control Scroll Up';
  RsApplicationControlSearch = 'Application Control Search';
  RsApplicationControlSelectAll = 'Application Control Select All';
  RsApplicationControlSelectColumn = 'Application Control Select Column';
  RsApplicationControlSelectObject = 'Application Control Select Object';
  RsApplicationControlSelectParagraph = 'Application Control Select Paragraph';
  RsApplicationControlSelectRow = 'Application Control Select Row';
  RsApplicationControlSelectSentence = 'Application Control Select Sentence';
  RsApplicationControlSelectTable = 'Application Control Select Table';
  RsApplicationControlSelectTimeZone = 'Application Control Select Time Zone';
  RsApplicationControlSelectWord = 'Application Control Select Word';
  RsApplicationControlSend = 'Application Control Send';
  RsApplicationControlSend_Receive = 'Application Control Send/Receive';
  RsApplicationControlSendTo = 'Application Control Send To';
  RsApplicationControlSetAlarm = 'Application Control Set Alarm';
  RsApplicationControlSetBorders = 'Application Control Set Borders';
  RsApplicationControlSetClock = 'Application Control Set Clock';
  RsApplicationControlSnoozeAlarm = 'Application Control Snooze Alarm';
  RsApplicationControlSort = 'Application Control Sort';
  RsApplicationControlSortAscending = 'Application Control Sort Ascending';
  RsApplicationControlSortDescending = 'Application Control Sort Descending';
  RsApplicationControlSplit = 'Application Control Split';
  RsApplicationControlStop = 'Application Control Stop';
  RsApplicationControlStrikethrough = 'Application Control Strikethrough';
  RsApplicationControlSubscript = 'Application Control Subscript';
  RsApplicationControlSubscriptions = 'Application Control Subscriptions';
  RsApplicationControlSuperscript = 'Application Control Superscript';
  RsApplicationControlSynchronize = 'Application Control Synchronize';
  RsApplicationControlTileHorizontally = 'Application Control Tile Horizontally';
  RsApplicationControlTileVertically = 'Application Control Tile Vertically';
  RsApplicationControlUnderline = 'Application Control Underline';
  RsApplicationControlUndo = 'Application Control Undo';
  RsApplicationControlUnlock = 'Application Control Unlock';
  RsApplicationControlUnprotect = 'Application Control Unprotect';
  RsApplicationControlUpload = 'Application Control Upload';
  RsApplicationControlViewClock = 'Application Control View Clock';
  RsApplicationControlViewComment = 'Application Control View Comment';
  RsApplicationControlViewToggle = 'Application Control View Toggle';
  RsApplicationControlYes = 'Application Control Yes';
  RsApplicationControlZoom = 'Application Control Zoom';
  RsApplicationControlZoomIn = 'Application Control Zoom In';
  RsApplicationControlZoomOut = 'Application Control Zoom Out';
  RsApplicationLaunchA_VCapture_Playback = 'Application Launch A/V Capture/Playback';
  RsApplicationLaunchAlarms = 'Application Launch Alarms';
  RsApplicationLaunchButtonConfigurationTool = 'Application Launch Button Configuration Tool';
  RsApplicationLaunchButtons = 'Application Launch Buttons';
  RsApplicationLaunchCalculator = 'Application Launch Calculator';
  RsApplicationLaunchCalendar_Schedule = 'Application Launch Calendar/Schedule';
  RsApplicationLaunchCheckbook_Finance = 'Application Launch Checkbook/Finance';
  RsApplicationLaunchClock = 'Application Launch Clock';
  RsApplicationLaunchCommandLineProcessor_Run = 'Application Launch Command Line Processor/Run';
  RsApplicationLaunchConsumerControlConfiguration = 'Application Launch Consumer Control Configuration';
  RsApplicationLaunchContacts_Addressbook = 'Application Launch Contacts/Addressbook';
  RsApplicationLaunchControlPanel = 'Application Launch Control Panel';
  RsApplicationLaunchDatabaseApplication = 'Application Launch Database Application';
  RsApplicationLaunchDesktop = 'Application Launch Desktop';
  RsApplicationLaunchDictionary = 'Application Launch Dictionary';
  RsApplicationLaunchDocuments = 'Application Launch Documents';
  RsApplicationLaunchEmailReader = 'Application Launch Email Reader';
  RsApplicationLaunchEncryption = 'Application Launch Encryption';
  RsApplicationLaunchFileBrowser = 'Application Launch File Browser';
  RsApplicationLaunchGrammarCheck = 'Application Launch Grammar Check';
  RsApplicationLaunchGraphicsEditor = 'Application Launch Graphics Editor';
  RsApplicationLaunchIntegratedHelpCenter = 'Application Launch Integrated Help Center';
  RsApplicationLaunchInternetBrowser = 'Application Launch Internet Browser';
  RsApplicationLaunchKeyboardLayout = 'Application Launch Keyboard Layout';
  RsApplicationLaunchLAN_WANBrowser = 'Application Launch LAN/WAN Browser';
  RsApplicationLaunchLocalMachineBrowser = 'Application Launch Local Machine Browser';
  RsApplicationLaunchLog_Journal_Timecard = 'Application Launch Log/Journal/Timecard';
  RsApplicationLaunchLogoff = 'Application Launch Logoff';
  RsApplicationLaunchLogon = 'Application Launch Logon';
  RsApplicationLaunchLogon_Logoff = 'Application Launch Logon/Logoff';
  RsApplicationLaunchNetworkChat = 'Application Launch Network Chat';
  RsApplicationLaunchNetworkConference = 'Application Launch Network Conference';
  RsApplicationLaunchNewsreader = 'Application Launch Newsreader';
  RsApplicationLaunchNextTask_Application = 'Application Launch Next Task/Application';
  RsApplicationLaunchPowerStatus = 'Application Launch Power Status';
  RsApplicationLaunchPreemptiveHaltTask_Application = 'Application Launch Preemptive Halt Task/Application';
  RsApplicationLaunchPresentationApplication = 'Application Launch Presentation Application';
  RsApplicationLaunchPreviousTask_Application = 'Application Launch Previous Task/Application';
  RsApplicationLaunchProcess_TaskManager = 'Application Launch Process/Task Manager';
  RsApplicationLaunchProgrammableButtonConfiguration = 'Application Launch Programmable Button Configuration';
  RsApplicationLaunchRemoteNetworking_ISPConnect = 'Application Launch Remote Networking/ISP Connect';
  RsApplicationLaunchScreensaver = 'Application Launch Screensaver';
  RsApplicationLaunchSelectTask_Application = 'Application Launch Select Task/Application';
  RsApplicationLaunchSpellCheck = 'Application Launch Spell Check';
  RsApplicationLaunchSpreadsheet = 'Application Launch Spreadsheet';
  RsApplicationLaunchTask_ProjectManager = 'Application Launch Task/Project Manager';
  RsApplicationLaunchTelephony_Dialer = 'Application Launch Telephony/Dialer';
  RsApplicationLaunchTerminalLock_Screensaver = 'Application Launch Terminal Lock/Screensaver';
  RsApplicationLaunchTextEditor = 'Application Launch Text Editor';
  RsApplicationLaunchThesaurus = 'Application Launch Thesaurus';
  RsApplicationLaunchVirusProtection = 'Application Launch Virus Protection';
  RsApplicationLaunchVoicemail = 'Application Launch Voicemail';
  RsApplicationLaunchWirelessStatus = 'Application Launch Wireless Status';
  RsApplicationLaunchWordProcessor = 'Application Launch Word Processor';
  RsArmature = 'Armature';
  RsArmWeapons = 'Arm Weapons';
  RsArticulatedArm = 'Articulated Arm';
  RsASCIICharacterSet = 'ASCII Character Set';
  RsAssignSelection = 'Assign Selection';
  RsAtRate = 'At Rate';
  RsAtRateOK = 'At Rate OK';
  RsAtRateTimetoEmpty = 'At Rate Time to Empty';
  RsAtRateTimetoFull = 'At Rate Time to Full';
  RsAttackLevel = 'Attack Level';
  RsAttackTime = 'Attack Time';
  RsAttributeData = 'Attribute Data';
  RsAttributeReadBack = 'Attribute Read Back';
  RsAttributeReport = 'Attribute Report';
  RsAudibleAlarmControl = 'Audible Alarm Control';
  RsAutomobile = 'Automobile';
  RsAverageCurrent = 'Average Current';
  RsAverageTimetoEmpty = 'Average Time to Empty';
  RsAverageTimetoFull = 'Average Time to Full';
  RsAwaitingPower = 'Awaiting Power';
  RsAxesEnable = 'Axes Enable';
  RsAzimuth = 'Azimuth';
  RsAztecCode = 'Aztec Code';
  RsB = 'B';
  RsBackslash = '\';
  RsBackslash2 = '\ 2';
  RsBackspace = 'Backspace';
  RsBadCount = 'Bad Count';
  RsBalance = 'Balance';
  RsBalanceLeft = 'Balance Left';
  RsBalanceRight = 'Balance Right';
  RsBallast = 'Ballast';
  RsBarCodeBadgeReader = 'Bar Code Badge Reader';
  RsBarCodePresent = 'Bar Code Present';
  RsBarCodePresentSensor = 'Bar Code Present Sensor';
  RsBarCodeScanner = 'Bar Code Scanner';
  RsBarCodeScannerCradle = 'Bar Code Scanner Cradle';
  RsBarrelElevation = 'Barrel Elevation';
  RsBarrelPressure = 'Barrel Pressure';
  RsBarrelSwitch = 'Barrel Switch';
  RsBarSpaceData = 'Bar Space Data';
  RsBaseballBat = 'Baseball Bat';
  RsBass = 'Bass';
  RsBassBoost = 'Bass Boost';
  RsBassDecrement = 'Bass Decrement';
  RsBassIncrement = 'Bass Increment';
  RsBattery = 'Battery';
  RsBatteryID = 'Battery ID';
  RsBatteryInsertion = 'Battery Insertion';
  RsBatteryLow = 'Battery Low';
  RsBatteryOk = 'Battery Ok';
  RsBatteryOperation = 'Battery Operation';
  RsBatteryPackModelLevel = 'Battery Pack Model Level';
  RsBatteryPresent = 'Battery Present';
  RsBatteryStrength = 'Battery Strength';
  RsBatterySupported = 'Battery Supported';
  RsBatterySystem = 'Battery System';
  RsBatterySystemID = 'Battery System ID';
  RsBC412 = 'BC412';
  RsBeeperState = 'Beeper State';
  RsBelowRemainingCapacity = 'Below Remaining Capacity';
  RsBelt = 'Belt';
  RsBicycle = 'Bicycle';
  RsBicycleCrank = 'Bicycle Crank';
  RsBlockHandle = 'Block Handle';
  RsBlockLoadStatus = 'Block Load Status';
  RsBlockType = 'Block Type';
  RsBodySuit = 'Body Suit';
  RsBooklandEAN = 'Bookland EAN';
  RsBoost = 'Boost';
  RsBrake = 'Brake';
  RsBroadcastMode = 'Broadcast Mode';
  RsBroadcasttoCharger = 'Broadcast to Charger';
  RsBuck = 'Buck';
  RsBump = 'Bump';
  RsBusy = 'Busy';
  RsButton = 'Button';
  RsByteCount = 'Byte Count';
  RsC = 'C';
  RsCalibrationCount = 'Calibration Count';
  RsCallerID = 'Caller ID';
  RsCallWaitingTone = 'Call Waiting Tone';
  RsCancel = 'Cancel';
  RsCapacityGranularity1 = 'Capacity Granularity 1';
  RsCapacityGranularity2 = 'Capacity Granularity 2';
  RsCapacityMode = 'Capacity Mode';
  RsCapsLock = 'Caps Lock';
  RsChaffRelease = 'Chaff Release';
  RsChangedStatus = 'Changed Status';
  RsChannel = 'Channel';
  RsChannelCenter = 'Channel Center';
  RsChannelCenterFront = 'Channel Center Front';
  RsChannelCode = 'Channel Code';
  RsChannelDecrement = 'Channel Decrement';
  RsChannelFront = 'Channel Front';
  RsChannelIncrement = 'Channel Increment';
  RsChannelLeft = 'Channel Left';
  RsChannelLowFrequencyEnhancement = 'Channel Low Frequency Enhancement';
  RsChannelRight = 'Channel Right';
  RsChannelSide = 'Channel Side';
  RsChannelSurround = 'Channel Surround';
  RsChannelTop = 'Channel Top';
  RsChannelUnknown = 'Channel Unknown';
  RsCharacterAttribute = 'Character Attribute';
  RsCharacterAttributeBlink = 'Character Attribute Blink';
  RsCharacterAttributeEnhance = 'Character Attribute Enhance';
  RsCharacterAttributeUnderline = 'Character Attribute Underline';
  RsCharacterHeight = 'Character Height';
  RsCharacterReport = 'Character Report';
  RsCharacterSpacingHorizontal = 'Character Spacing Horizontal';
  RsCharacterSpacingVertical = 'Character Spacing Vertical';
  RsCharacterWidth = 'Character Width';
  RsChargeController = 'Charge Controller';
  RsCharger = 'Charger';
  RsChargerConnection = 'Charger Connection';
  RsChargerID = 'Charger ID';
  RsChargerSelectorSupport = 'Charger Selector Support';
  RsChargerSpec = 'Charger Spec';
  RsCharging = 'Charging';
  RsChargingIndicator = 'Charging Indicator';
  RsCheck = 'Check';
  RsCheckDigit = 'Check Digit';
  RsCheckDigitCodabarEnable = 'Check Digit Codabar Enable';
  RsCheckDigitCode39Enable = 'Check Digit Code 39 Enable';
  RsCheckDigitDisable = 'Check Digit Disable';
  RsCheckDigitEnableInterleaved2of5OPCC = 'Check Digit Enable Interleaved 2 of 5 OPCC';
  RsCheckDigitEnableInterleaved2of5USS = 'Check Digit Enable Interleaved 2 of 5 USS';
  RsCheckDigitEnableOneMSI_Plessey = 'Check Digit Enable One MSI/Plessey';
  RsCheckDigitEnableStandard2of5OPCC = 'Check Digit Enable Standard 2 of 5 OPCC';
  RsCheckDigitEnableStandard2of5USS = 'Check Digit Enable Standard 2 of 5 USS';
  RsCheckDigitEnableTwoMSI_Plessey = 'Check Digit Enable Two MSI/Plessey';
  RsCheckDisablePrice = 'Check Disable Price';
  RsCheckEnable4DigitPrice = 'Check Enable 4 Digit Price';
  RsCheckEnable5DigitPrice = 'Check Enable 5 Digit Price';
  RsCheckEnableEuropean4DigitPrice = 'Check Enable European 4 Digit Price';
  RsCheckEnableEuropean5DigitPrice = 'Check Enable European 5 Digit Price';
  RsCine = 'Cine';
  RsClass1ALaser = 'Class 1A Laser';
  RsClass2Laser = 'Class 2 Laser';
  RsClear = 'Clear';
  RsClear_Again = 'Clear/Again';
  RsClearAllEAN2_3LabelDefinitions = 'Clear All EAN 2/3 Label Definitions';
  RsClearDisplay = 'Clear Display';
  RsClearMark = 'Clear Mark';
  RsClimateControlEnable = 'Climate Control Enable';
  RsClipStore = 'Clip Store';
  RsClosedCaption = 'Closed Caption';
  RsClosedCaptionSelect = 'Closed Caption Select';
  RsClutch = 'Clutch';
  RsCodabar = 'Codabar';
  RsCodabarControlReport = 'Codabar Control Report';
  RsCode128 = 'Code 128';
  RsCode128ControlReport = 'Code 128 Control Report';
  RsCode16 = 'Code 16';
  RsCode32 = 'Code 32';
  RsCode39 = 'Code 39';
  RsCode39ControlReport = 'Code 39 Control Report';
  RsCode49 = 'Code 49';
  RsCode93 = 'Code 93';
  RsCodeOne = 'Code One';
  RsCollectiveControl = 'Collective Control';
  RsColorcode = 'Colorcode';
  RsColorDopplerAdjust = 'Color Doppler Adjust';
  RsColorDopplerModeSelect = 'Color Doppler Mode Select';
  RsColumn = 'Column';
  RsColumns = 'Columns';
  RsComma = ',';
  RsCommitParameterstoNVM = 'Commit Parameters to NVM';
  RsCommunicationLost = 'Communication Lost';
  RsConditioningFlag = 'Conditioning Flag';
  RsConference = 'Conference';
  RsConfigActivePower = 'Config Active Power';
  RsConfigApparentPower = 'Config Apparent Power';
  RsConfigCurrent = 'Config Current';
  RsConfigFrequency = 'Config Frequency';
  RsConfigHumidity = 'Config Humidity';
  RsConfigPercentLoad = 'Config Percent Load';
  RsConfigTemperature = 'Config Temperature';
  RsConfigVoltage = 'Config Voltage';
  RsConfirmationTone1 = 'Confirmation Tone 1';
  RsConfirmationTone2 = 'Confirmation Tone 2';
  RsConnectiontoSMBus = 'Connection to SMBus';
  RsConstantArticleSurveillanceNotification = 'Constant Article Surveillance Notification';
  RsConsumer = 'Consumer';
  RsConsumerControl = 'Consumer Control';
  RsConsumerIndicatorCameraOff = 'Consumer Indicator Camera Off';
  RsConsumerIndicatorCameraOn = 'Consumer Indicator Camera On';
  RsConsumerIndicatorCAV = 'Consumer Indicator CAV';
  RsConsumerIndicatorCLV = 'Consumer Indicator CLV';
  RsConsumerIndicatorEqualizerEnable = 'Consumer Indicator Equalizer Enable';
  RsConsumerIndicatorHighCutFilter = 'Consumer Indicator High Cut Filter';
  RsConsumerIndicatorLowCutFilter = 'Consumer Indicator Low Cut Filter';
  RsConsumerIndicatorMute = 'Consumer Indicator Mute';
  RsConsumerIndicatorRepeat = 'Consumer Indicator Repeat';
  RsConsumerIndicatorSamplingRateDetect = 'Consumer Indicator Sampling Rate Detect';
  RsConsumerIndicatorSoundFieldOn = 'Consumer Indicator Sound Field On';
  RsConsumerIndicatorStereo = 'Consumer Indicator Stereo';
  RsConsumerIndicatorSurroundFieldOn = 'Consumer Indicator Surround Field On';
  RsConsumerIndicatorToneEnable = 'Consumer Indicator Tone Enable';
  RsContactScanner = 'Contact Scanner';
  RsConvertEAN8to13Type = 'Convert EAN 8 to 13 Type';
  RsConvertUPC_EtoA = 'Convert UPC-E to A';
  RsConvertUPCAtoEAN_13 = 'Convert UPC A to EAN-13';
  RsCoordinateMeasuringMachine = 'Coordinate Measuring Machine';
  RsCopy = 'Copy';
  RsCordlessScannerBase = 'Cordless Scanner Base';
  RsCountedBuffer = 'Counted Buffer';
  RsCounterReset = 'Counter Reset';
  RsCPOffset = 'CP Offset';
  RsCreateNewEffectReport = 'Create New Effect Report';
  RsCrSel_Props = 'CrSel/Props';
  RsCurrent = 'Current';
  RsCurrentNotRegulated = 'Current Not Regulated';
  RsCurrentOutofRange = 'Current Out of Range';
  RsCursorBlink = 'Cursor Blink';
  RsCursorEnable = 'Cursor Enable';
  RsCursorMode = 'Cursor Mode';
  RsCursorPixelPositioning = 'Cursor Pixel Positioning';
  RsCursorPositionReport = 'Cursor Position Report';
  RsCustomForceData = 'Custom Force Data';
  RsCustomForceDataReport = 'Custom Force Data Report';
  RsCustomForceVendorDefinedData = 'Custom Force Vendor Defined Data';
  RsCut = 'Cut';
  RsCycleCount = 'Cycle Count';
  RsCyclicControl = 'Cyclic Control';
  RsCyclicTrim = 'Cyclic Trim';
  RsD = 'D';
  RsDaily = 'Daily';
  RsDataLengthMethod = 'Data Length Method';
  RsDataLengthMethodCheckforDiscrete = 'Data Length Method Check for Discrete';
  RsDataLengthMethodCheckinRange = 'Data Length Method Check in Range';
  RsDataLengthMethodReadAny = 'Data Length Method Read Any';
  RsDataMatrix = 'Data Matrix';
  RsDataOnScreen = 'Data On Screen';
  RsDataPrefix = 'Data Prefix';
  RsDataReadBack = 'Data Read Back';
  RsDataScaling = 'Data Scaling';
  RsDataValid = 'Data Valid';
  RsDataWeight = 'Data Weight';
  RsDeadBand = 'Dead Band';
  RsDecodeData = 'Decode Data';
  RsDecodedDataContinued = 'Decoded Data Continued';
  RsDel = 'Del';
  RsDelayBeforeReboot = 'Delay Before Reboot';
  RsDelayBeforeShutdown = 'Delay Before Shutdown';
  RsDelayBeforeStartup = 'Delay Before Startup';
  RsDepth = 'Depth';
  RsDepthGainCompensation = 'Depth Gain Compensation';
  RsDesigncapacity = 'Design capacity';
  RsDeviceControlDeviceContinue = 'Device Control Device Continue';
  RsDeviceControlDevicePause = 'Device Control Device Pause';
  RsDeviceControlDeviceReset = 'Device Control Device Reset';
  RsDeviceControlDisableActuators = 'Device Control Disable Actuators';
  RsDeviceControlEnableActuators = 'Device Control Enable Actuators';
  RsDeviceControlStopAllEffects = 'Device Control Stop All Effects';
  RsDeviceGain = 'Device Gain';
  RsDeviceGainReport = 'Device Gain Report';
  RsDeviceManagedPool = 'Device Managed Pool';
  RsDevicePaused = 'Device Paused';
  RsDial = 'Dial';
  RsDigitizer = 'Digitizer';
  RsDirection = 'Direction';
  RsDirectionEnable = 'Direction Enable';
  RsDisableCheckDigitTransmit = 'Disable Check Digit Transmit';
  RsDischarging = 'Discharging';
  RsDisplayAttributesReport = 'Display Attributes Report';
  RsDisplayBrightness = 'Display Brightness';
  RsDisplayContrast = 'Display Contrast';
  RsDisplayControlReport = 'Display Control Report';
  RsDisplayData = 'Display Data';
  RsDisplayEnable = 'Display Enable';
  RsDisplayStatus = 'Display Status';
  RsDiveBreak = 'Dive Break';
  RsDivePlane = 'Dive Plane';
  RsDoNotDisturb = 'Do Not Disturb';
  RsDot = '.';
  RsDownArrow = 'Down Arrow';
  RsDownloadForceSample = 'Download Force Sample';
  RsDrop = 'Drop';
  RsDumbBarCodeScanner = 'Dumb Bar Code Scanner';
  RsDuration = 'Duration';
  RsDuressAlarm = 'Duress Alarm';
  RsE = 'E';
  RsEAN_13 = 'EAN-13';
  RsEAN_8 = 'EAN-8';
  RsEAN_99128Mandatory = 'EAN-99 128 Mandatory';
  RsEAN_99P5_128Optional = 'EAN-99 P5/128 Optional';
  RsEAN13FlagDigit1 = 'EAN 13 Flag Digit 1';
  RsEAN13FlagDigit2 = 'EAN 13 Flag Digit 2';
  RsEAN13FlagDigit3 = 'EAN 13 Flag Digit 3';
  RsEAN2_3LabelControlReport = 'EAN 2/3 Label Control Report';
  RsEAN8FlagDigit1 = 'EAN 8 Flag Digit 1';
  RsEAN8FlagDigit2 = 'EAN 8 Flag Digit 2';
  RsEAN8FlagDigit3 = 'EAN 8 Flag Digit 3';
  RsEANThreeLabel = 'EAN Three Label';
  RsEANTwoLabel = 'EAN Two Label';
  RsEDIDInformation = 'EDID Information';
  RsEffectBlockCount = 'Effect Block Count';
  RsEffectBlockIndex = 'Effect Block Index';
  RsEffectOperation = 'Effect Operation';
  RsEffectOperationReport = 'Effect Operation Report';
  RsEffectPlaying = 'Effect Playing';
  RsEffectType = 'Effect Type';
  RsEffectTypeConstantForce = 'Effect Type Constant Force';
  RsEffectTypeCustomForceData = 'Effect Type Custom Force Data';
  RsEffectTypeDamper = 'Effect Type Damper';
  RsEffectTypeFriction = 'Effect Type Friction';
  RsEffectTypeInertia = 'Effect Type Inertia';
  RsEffectTypeRamp = 'Effect Type Ramp';
  RsEffectTypeSawtoothDown = 'Effect Type Sawtooth Down';
  RsEffectTypeSawtoothUp = 'Effect Type Sawtooth Up';
  RsEffectTypeSine = 'Effect Type Sine';
  RsEffectTypeSpring = 'Effect Type Spring';
  RsEffectTypeSquare = 'Effect Type Square';
  RsEffectTypeTriangle = 'Effect Type Triangle';
  RsEject = 'Eject';
  RsElectronicArticleSurveillanceNotification = 'Electronic Article Surveillance Notification';
  RsElectronicCountermeasures = 'Electronic Countermeasures';
  RsElevator = 'Elevator';
  RsElevatorTrim = 'Elevator Trim';
  RsEmpty = 'Empty';
  RsEnableAutopilot = 'Enable Autopilot';
  RsEnableCheckDigitTransmit = 'Enable Check Digit Transmit';
  RsEnablePolling = 'Enable Polling';
  RsEnd = 'End';
  RsEnterChannel = 'Enter Channel';
  RsEnterDisc = 'Enter Disc';
  RsEnterNotReturn = 'Enter (not Return)';
  RsEqual = '=';
  RsEraser = 'Eraser';
  RsError = 'Error';
  RsErrorFontDataCannotBeRead = 'Error Font Data Cannot Be Read';
  RsErrorIndication = 'Error Indication';
  RsErrorNotaLoadableChar = 'Error Not a Loadable Char';
  RsEsc = 'Esc';
  RsExecute = 'Execute';
  RsExSel = 'ExSel';
  RsExtendedPlay = 'Extended Play';
  RsExternalPowerConnected = 'External Power Connected';
  RsF = 'F';
  RsF1 = 'F1';
  RsF10 = 'F10';
  RsF11 = 'F11';
  RsF12 = 'F12';
  RsF13 = 'F13';
  RsF14 = 'F14';
  RsF15 = 'F15';
  RsF16 = 'F16';
  RsF17 = 'F17';
  RsF18 = 'F18';
  RsF19 = 'F19';
  RsF2 = 'F2';
  RsF20 = 'F20';
  RsF21 = 'F21';
  RsF22 = 'F22';
  RsF23 = 'F23';
  RsF24 = 'F24';
  RsF3 = 'F3';
  RsF4 = 'F4';
  RsF5 = 'F5';
  RsF6 = 'F6';
  RsF7 = 'F7';
  RsF8 = 'F8';
  RsF9 = 'F9';
  RsFadeLevel = 'Fade Level';
  RsFadeTime = 'Fade Time';
  RsFanEnable = 'Fan Enable';
  RsFanSpeed = 'Fan Speed';
  RsFastBlinkOff_Time = 'Fast Blink Off-Time';
  RsFastBlinkOn_Time = 'Fast Blink On-Time';
  RsFastForward = 'Fast Forward';
  RsFeature = 'Feature';
  RsFeatureNotification = 'Feature Notification';
  RsFind = 'Find';
  RsFinger = 'Finger';
  RsFireAlarm = 'Fire Alarm';
  RsFirstDiscreteLengthtoDecode = 'First Discrete Length to Decode';
  RsFixedBeeper = 'Fixed Beeper';
  RsFlareRelease = 'Flare Release';
  RsFlash = 'Flash';
  RsFlashOn_Time = 'Flash On-Time';
  RsFlexor = 'Flexor';
  RsFlight = 'Flight';
  RsFlightCommunications = 'Flight Communications';
  RsFlightControlStick = 'Flight Control Stick';
  RsFlightStick = 'Flight Stick';
  RsFlightYoke = 'Flight Yoke';
  RsFlipper = 'Flipper';
  RsFlow = 'Flow';
  RsFlowID = 'Flow ID';
  RsFocus = 'Focus';
  RsFont14Segment = 'Font 14 Segment';
  RsFont7Segment = 'Font 7 Segment';
  RsFontData = 'Font Data';
  RsFontReadBack = 'Font Read Back';
  RsFontReport = 'Font Report';
  RsForwardCalls = 'Forward Calls';
  RsFragmentDecoding = 'Fragment Decoding';
  RsFrameBack = 'Frame Back';
  RsFrameForward = 'Frame Forward';
  RsFreeSpaceWand = 'Free Space Wand';
  RsFreeze_Thaw = 'Freeze/Thaw';
  RsFrequency = 'Frequency';
  RsFrequencyOutofRange = 'Frequency Out of Range';
  RsFrontBrake = 'Front Brake';
  RsFullASCIIConversion = 'Full ASCII Conversion';
  RsFullChargeCapacity = 'Full Charge Capacity';
  RsFullyCharged = 'Fully Charged';
  RsFullyDischarged = 'Fully Discharged';
  RsFunctionButtons = 'Function Buttons';
  RsG = 'G';
  RsGain = 'Gain';
  RsGame = 'Game';
  RsGamepad = 'Gamepad';
  RsGamepadFire_Jump = 'Gamepad Fire/Jump';
  RsGamepadTrigger = 'Gamepad Trigger';
  RsGang = 'Gang';
  RsGangID = 'Gang ID';
  RsGeneric = 'Generic';
  RsGenericDesktop = 'Generic Desktop';
  RsGenericGUIApplicationControls = 'Generic GUI Application Controls';
  RsGenericIndicator = 'Generic Indicator';
  RsGlove = 'Glove';
  RsGolfClub = 'Golf Club';
  RsGood = 'Good';
  RsGoodDecodeIndication = 'Good Decode Indication';
  RsGoodReadLampDuration = 'Good Read Lamp Duration';
  RsGoodReadLampIntensity = 'Good Read Lamp Intensity';
  RsGoodReadLED = 'Good Read LED';
  RsGoodReadToneFrequency = 'Good Read Tone Frequency';
  RsGoodReadToneLength = 'Good Read Tone Length';
  RsGoodReadToneVolume = 'Good Read Tone Volume';
  RsGoodReadWhentoWrite = 'Good Read When to Write';
  RsGraphicEqualizer = 'Graphic Equalizer';
  RsGraveAccent = '`';
  RsGreen = 'Green';
  RsGRWTIAfterDecode = 'GRWTI After Decode';
  RsGRWTIBeep_LampAfterTransmit = 'GRWTI Beep/Lamp After Transmit';
  RsGRWTINoBeep_LampUseatAll = 'GRWTI No Beep/Lamp Use at All';
  RsGunAutomatic = 'Gun Automatic';
  RsGunBolt = 'Gun Bolt';
  RsGunBurst = 'Gun Burst';
  RsGunClip = 'Gun Clip';
  RsGunDevice = 'Gun Device';
  RsGunSafety = 'Gun Safety';
  RsGunSelector = 'Gun Selector';
  RsGunSingleShot = 'Gun Single Shot';
  RsH = 'H';
  RsHandleBars = 'Handle Bars';
  RsHandset = 'Handset';
  RsHandsFreeScanning = 'Hands Free Scanning';
  RsHandTracker = 'Hand Tracker';
  RsHashMark2 = '# 2';
  RsHatSwitch = 'Hat Switch';
  RsHeadMountedDisplay = 'Head Mounted Display';
  RsHeadphone = 'Headphone';
  RsHeadset = 'Headset';
  RsHeadTracker = 'Head Tracker';
  RsHeaterPresent = 'Heater Present';
  RsHeightofPOV = 'Height of POV';
  RsHelicopter = 'Helicopter';
  RsHelp = 'Help';
  RsHighVoltageTransfer = 'High Voltage Transfer';
  RsHold = 'Hold';
  RsHoldupAlarm = 'Holdup Alarm';
  RsHome = 'Home';
  RsHookSwitch = 'Hook Switch';
  RsHorizontalScroll = 'Horizontal Scroll';
  RsHumidity = 'Humidity';
  RsI = 'I';
  RsiDeviceChemistery = 'iDeviceChemistery';
  RsiDeviceName = 'iDeviceName';
  RsIllumination = 'Illumination';
  RsiManufacturer = 'iManufacturer';
  RsiManufacturerName = 'iManufacturerName';
  RsiName = 'iName';
  RsIndicatorColor = 'Indicator Color';
  RsIndicatorFastBlink = 'Indicator Fast Blink';
  RsIndicatorFlash = 'Indicator Flash';
  RsIndicatorOff = 'Indicator Off';
  RsIndicatorOn = 'Indicator On';
  RsIndicatorSlowBlink = 'Indicator Slow Blink';
  RsInhibitCharge = 'Inhibit Charge';
  RsInitialized = 'Initialized';
  RsInitiateBarcodeRead = 'Initiate Barcode Read';
  RsInput = 'Input';
  RsInputID = 'Input ID';
  RsInRange = 'In Range';
  RsIns = 'Ins';
  RsInsideDialTone = 'Inside Dial Tone';
  RsInsideRingback = 'Inside Ringback';
  RsInsideRingTone = 'Inside Ring Tone';
  RsInterleaved2of5 = 'Interleaved 2 of 5';
  RsInterleaved2of5ControlReport = 'Interleaved 2 of 5 Control Report';
  RsInternalChargeController = 'Internal Charge Controller';
  RsInternalFailure = 'Internal Failure';
  RsInternational1 = 'International 1';
  RsInternational2 = 'International 2';
  RsInternational3 = 'International 3';
  RsInternational4 = 'International 4';
  RsInternational5 = 'International 5';
  RsInternational6 = 'International 6';
  RsInternational7 = 'International 7';
  RsInternational8 = 'International 8';
  RsInternational9 = 'International 9';
  RsIntrinsicallySafe = 'Intrinsically Safe';
  RsInUseIndicator = 'In Use Indicator';
  RsInvert = 'Invert';
  RsiOEMInformation = 'iOEMInformation';
  RsiProduct = 'iProduct';
  RsIron1 = 'Iron 1';
  RsIron10 = 'Iron 10';
  RsIron11 = 'Iron 11';
  RsIron2 = 'Iron 2';
  RsIron3 = 'Iron 3';
  RsIron4 = 'Iron 4';
  RsIron5 = 'Iron 5';
  RsIron6 = 'Iron 6';
  RsIron7 = 'Iron 7';
  RsIron8 = 'Iron 8';
  RsIron9 = 'Iron 9';
  RsiSerialNumber = 'iSerialNumber';
  RsIsochCustomForceEnable = 'Isoch Custom Force Enable';
  RsItalianPharmacyCode = 'Italian Pharmacy Code';
  RsJ = 'J';
  RsJoystick = 'Joystick';
  RsK = 'K';
  RsKey0 = 'Key 0';
  RsKey1 = 'Key 1';
  RsKey2 = 'Key 2';
  RsKey3 = 'Key 3';
  RsKey4 = 'Key 4';
  RsKey5 = 'Key 5';
  RsKey6 = 'Key 6';
  RsKey7 = 'Key 7';
  RsKey8 = 'Key 8';
  RsKey9 = 'Key 9';
  RsKeyA = 'Key A';
  RsKeyB = 'Key B';
  RsKeyboard = 'Keyboard';
  RsKeyboardCapsLock = 'Keyboard Caps Lock';
  RsKeyboardCompose = 'Keyboard Compose';
  RsKeyboardKana = 'Keyboard Kana';
  RsKeyboardNumLock = 'Keyboard Num Lock';
  RsKeyboardPower = 'Keyboard Power';
  RsKeyboardScrollLock = 'Keyboard Scroll Lock';
  RsKeyboardShift = 'Keyboard Shift';
  RsKeyC = 'Key C';
  RsKeyD = 'Key D';
  RsKeypad = 'Keypad';
  RsKeypad0 = 'Keypad 0';
  RsKeypad00 = 'Keypad 00';
  RsKeypad000 = 'Keypad 000';
  RsKeypad1 = 'Keypad 1';
  RsKeypad2 = 'Keypad 2';
  RsKeypad3 = 'Keypad 3';
  RsKeypad4 = 'Keypad 4';
  RsKeypad5 = 'Keypad 5';
  RsKeypad6 = 'Keypad 6';
  RsKeypad7 = 'Keypad 7';
  RsKeypad8 = 'Keypad 8';
  RsKeypad9 = 'Keypad 9';
  RsKeypadA = 'Keypad A';
  RsKeypadAt = 'Keypad @';
  RsKeypadB = 'Keypad B';
  RsKeypadBackspace = 'Keypad Backspace';
  RsKeypadBiggerThan = 'Keypad <';
  RsKeypadBinary = 'Keypad Binary';
  RsKeypadBinaryAnd = 'Keypad &';
  RsKeypadBinaryOr = 'Keypad |';
  RsKeypadC = 'Keypad C';
  RsKeypadCircumflex = 'Keypad ^';
  RsKeypadClear = 'Keypad Clear';
  RsKeypadClearEntry = 'Keypad Clear Entry';
  RsKeypadColon = 'Keypad :';
  RsKeypadComma = 'Keypad ,';
  RsKeypadCurrencySubunit = 'Keypad Currency Subunit';
  RsKeypadCurrencyUnit = 'Keypad Currency Unit';
  RsKeypadD = 'Keypad D';
  RsKeypadDecimal = 'Keypad Decimal';
  RsKeypadDecimalSeparator = 'Keypad Decimal Separator';
  RsKeypadDot = 'Keypad .';
  RsKeypadE = 'Keypad E';
  RsKeypadEnter = 'Keypad Enter';
  RsKeypadEqual = 'Keypad =';
  RsKeypadEqual2 = 'Keypad = 2';
  RsKeypadExclamation = 'Keypad !';
  RsKeypadF = 'Keypad F';
  RsKeypadHashmark = 'Keypad #';
  RsKeypadHexadecimal = 'Keypad Hexadecimal';
  RsKeypadLCurlyBracket = 'Keypad {';
  RsKeypadLessThan = 'Keypad >';
  RsKeypadLogicalAnd = 'Keypad &&';
  RsKeypadLogicalOr = 'Keypad ||';
  RsKeypadLRoundBracket = 'Keypad (';
  RsKeypadMemoryAdd = 'Keypad Memory Add';
  RsKeypadMemoryClear = 'Keypad Memory Clear';
  RsKeypadMemoryDivide = 'Keypad Memory Divide';
  RsKeypadMemoryMinus = 'Keypad Memory Minus';
  RsKeypadMemoryMultiply = 'Keypad Memory Multiply';
  RsKeypadMemoryRecall = 'Keypad Memory Recall';
  RsKeypadMemoryStore = 'Keypad Memory Store';
  RsKeypadMemorySubtract = 'Keypad Memory Subtract';
  RsKeypadMinus = 'Keypad -';
  RsKeypadNumLock = 'Keypad Num Lock';
  RsKeypadOctal = 'Keypad Octal';
  RsKeypadPercent = 'Keypad %';
  RsKeypadPlus = 'Keypad +';
  RsKeypadRCurlyBracket = 'Keypad }';
  RsKeypadReserved1 = 'Keypad Reserved1';
  RsKeypadReserved2 = 'Keypad Reserved2';
  RsKeypadRRoundBracket = 'Keypad )';
  RsKeypadSlash = 'Keypad /';
  RsKeypadSpace = 'Keypad Space';
  RsKeypadStar = 'Keypad *';
  RsKeypadTabulator = 'Keypad Tabulator';
  RsKeypadThousandsSeparator = 'Keypad Thousands Separator';
  RsKeypadXOR = 'Keypad XOR';
  RsKeyPound = 'Key #';
  RsKeyStar = 'Key *';
  RsKlasseEinsLaserClass1Laser = 'Klasse Eins Laser (Class 1 Laser)';
  RsL = 'L';
  RsLandingGear = 'Landing Gear';
  RsLanguage1 = 'Language 1';
  RsLanguage2 = 'Language 2';
  RsLanguage3 = 'Language 3';
  RsLanguage4 = 'Language 4';
  RsLanguage5 = 'Language 5';
  RsLanguage6 = 'Language 6';
  RsLanguage7 = 'Language 7';
  RsLanguage8 = 'Language 8';
  RsLanguage9 = 'Language 9';
  RsLaserOnTime = 'Laser On Time';
  RsLaserState = 'Laser State';
  RsLeanForward_Backward = 'Lean Forward/Backward';
  RsLeanRight_Left = 'Lean Right/Left';
  RsLED = 'LED';
  RsLeftAlt = 'Left Alt';
  RsLeftArrow = 'Left Arrow';
  RsLeftCtrl = 'Left Ctrl';
  RsLeftGUI = 'Left GUI';
  RsLeftShift = 'Left Shift';
  RsLevel2 = 'Level 2';
  RsLevel3 = 'Level 3';
  RsLightEnable = 'Light Enable';
  RsLightIlluminationLevel = 'Light Illumination Level';
  RsLightPen = 'Light Pen';
  RsLine = 'Line';
  RsLineBusyTone = 'Line Busy Tone';
  RsLoadError = 'Load Error';
  RsLoadFull = 'Load Full';
  RsLoadSuccess = 'Load Success';
  RsLockoutTime = 'Lockout Time';
  RsLoftWedge = 'Loft Wedge';
  RsLongPlay = 'Long Play';
  RsLongRangeScanner = 'Long Range Scanner';
  RsLoopCount = 'Loop Count';
  RsLoudness = 'Loudness';
  RsLowVoltageTransfer = 'Low Voltage Transfer';
  RsLSqBracket = '[';
  RsM = 'M';
  RsMagicCarpet = 'Magic Carpet';
  RsMagneticStripeReader = 'Magnetic Stripe Reader';
  RsMagnitude = 'Magnitude';
  RsManufacturerAccess = 'Manufacturer Access';
  RsManufacturerData = 'Manufacturer Data';
  RsManufacturerDate = 'Manufacturer Date';
  RsMark = 'Mark';
  RsMasterMode = 'Master Mode';
  RsMaxError = 'Max Error';
  RsMaxiCode = 'MaxiCode';
  RsMaximumLengthtoDecode = 'Maximum Length to Decode';
  RsMediaSelectCable = 'Media Select Cable';
  RsMediaSelectCall = 'Media Select Call';
  RsMediaSelectCD = 'Media Select CD';
  RsMediaSelectComputer = 'Media Select Computer';
  RsMediaSelectDVD = 'Media Select DVD';
  RsMediaSelectGames = 'Media Select Games';
  RsMediaSelectHome = 'Media Select Home';
  RsMediaSelection = 'Media Selection';
  RsMediaSelectMessages = 'Media Select Messages';
  RsMediaSelectProgramGuide = 'Media Select Program Guide';
  RsMediaSelectSAP = 'Media Select SAP';
  RsMediaSelectSatellite = 'Media Select Satellite';
  RsMediaSelectSecurity = 'Media Select Security';
  RsMediaSelectTape = 'Media Select Tape';
  RsMediaSelectTelephone = 'Media Select Telephone';
  RsMediaSelectTuner = 'Media Select Tuner';
  RsMediaSelectTV = 'Media Select TV';
  RsMediaSelectVCR = 'Media Select VCR';
  RsMediaSelectVideoPhone = 'Media Select Video Phone';
  RsMediaSelectWWW = 'Media Select WWW';
  RsMediaTransportFastForward = 'Media Transport Fast Forward';
  RsMediaTransportForward = 'Media Transport Forward';
  RsMediaTransportIndicatorRecordingFormatDetect = 'Media Transport Indicator Recording Format Detect';
  RsMediaTransportIndicatorSpinning = 'Media Transport Indicator Spinning';
  RsMediaTransportPause = 'Media Transport Pause';
  RsMediaTransportPlay = 'Media Transport Play';
  RsMediaTransportRecord = 'Media Transport Record';
  RsMediaTransportReverse = 'Media Transport Reverse';
  RsMediaTransportRewind = 'Media Transport Rewind';
  RsMediaTransportStop = 'Media Transport Stop';
  RsMedicalAlarm = 'Medical Alarm';
  RsMedicalInstrument = 'Medical Instrument';
  RsMedicalUltrasound = 'Medical Ultrasound';
  RsMenu = 'Menu';
  RsMenuDown = 'Menu Down';
  RsMenuEscape = 'Menu Escape';
  RsMenuLeft = 'Menu Left';
  RsMenuPick = 'Menu Pick';
  RsMenuRight = 'Menu Right';
  RsMenuUp = 'Menu Up';
  RsMenuValueDecrease = 'Menu Value Decrease';
  RsMenuValueIncrease = 'Menu Value Increase';
  RsMessage = 'Message';
  RsMessageControls = 'Message Controls';
  RsMicroPDF = 'MicroPDF';
  RsMicrophone = 'Microphone';
  RsMicrophoneEnable = 'Microphone Enable';
  RsMinimumLengthtoDecode = 'Minimum Length to Decode';
  RsMinus = '-';
  RsMirrorSpeedControl = 'Mirror Speed Control';
  RsMisc1DControlReport = 'Misc 1D Control Report';
  RsModeStep = 'Mode Step';
  RsModuleReset = 'Module Reset';
  RsMonitorControl = 'Monitor Control';
  RsMonthly = 'Monthly';
  RsMotion = 'Motion';
  RsMotionModeAdjust = 'Motion Mode Adjust';
  RsMotionModeSelect = 'Motion Mode Select';
  RsMotionWakeup = 'Motion Wakeup';
  RsMotorcycle = 'Motorcycle';
  RsMotorState = 'Motor State';
  RsMotorTimeout = 'Motor Timeout';
  RsMouse = 'Mouse';
  RsMoveDestination = 'Move Destination';
  RsMoveForward_Backward = 'Move Forward/Backward';
  RsMoveLength = 'Move Length';
  RsMoveRight_Left = 'Move Right/Left';
  RsMoveSource = 'Move Source';
  RsMoveUp_Down = 'Move Up/Down';
  RsMPX = 'MPX';
  RsMSI_Plessey = 'MSI/Plessey';
  RsMSIPlesseyControlReport = 'MSI Plessey Control Report';
  RsMSRDeviceReadOnly = 'MSR Device Read Only';
  RsMulti_Axis = 'Multi-Axis';
  RsMulti_RangeScanner = 'Multi-Range Scanner';
  RsMultiModeIndicator = 'Multi Mode Indicator';
  RsMultiplePointDigitizer = 'Multiple Point Digitizer';
  RsMute = 'Mute';
  RsN = 'N';
  RsNeedReplacement = 'Need Replacement';
  RsNegativeCoefficient = 'Negative Coefficient';
  RsNegativeSaturation = 'Negative Saturation';
  RsNewGame = 'New Game';
  RsNext = 'Next';
  RsNoButtonPressed = 'No Button Pressed';
  RsNoEnumeratedValueSelected = 'No Enumerated Value Selected';
  RsNoReadMessage = 'No Read Message';
  RsNormal = 'Normal';
  RsNotOnFileIndication = 'Not On File Indication';
  RsNotOnFileVolume = 'Not On File Volume';
  RsNumericKeypad = 'Numeric Keypad';
  RsNumLock = 'Num Lock';
  RsO = 'O';
  RsOar = 'Oar';
  RsOculometer = 'Oculometer';
  RsOffLine = 'Off Line';
  RsOffset = 'Offset';
  RsOKtoUse = 'OK to Use';
  RsOnce = 'Once';
  RsOnLine = 'On Line';
  RsOPEffectStart = 'OP Effect Start';
  RsOPEffectStartSolo = 'OP Effect Start Solo';
  RsOPEffectStop = 'OP Effect Stop';
  RsOper = 'Oper';
  RsOptionalManufacturerFunction1 = 'Optional Manufacturer Function 1';
  RsOptionalManufacturerFunction2 = 'Optional Manufacturer Function 2';
  RsOptionalManufacturerFunction3 = 'Optional Manufacturer Function 3';
  RsOptionalManufacturerFunction4 = 'Optional Manufacturer Function 4';
  RsOptionalManufacturerFunction5 = 'Optional Manufacturer Function 5';
  RsOrderMovie = 'Order Movie';
  RsOrdinal = 'Ordinal';
  RsOut = 'Out';
  RsOutlet = 'Outlet';
  RsOutletID = 'Outlet ID';
  RsOutletSystem = 'Outlet System';
  RsOutletSystemID = 'Outlet System ID';
  RsOutput = 'Output';
  RsOutputConnection = 'Output Connection';
  RsOutputID = 'Output ID';
  RsOutsideDialTone = 'Outside Dial Tone';
  RsOutsideRingback = 'Outside Ringback';
  RsOutsideRingTone = 'Outside Ring Tone';
  RsOvercharged = 'Overcharged';
  RsOverload = 'Overload';
  RsOvertemperature = 'Overtemperature';
  RsP = 'P';
  RsParameterBlockOffset = 'Parameter Block Offset';
  RsParameterBlockSize = 'Parameter Block Size';
  RsParameterScanning = 'Parameter Scanning';
  RsParametersChanged = 'Parameters Changed';
  RsPark = 'Park';
  RsPaste = 'Paste';
  RsPause = 'Pause';
  RsPDF_417 = 'PDF-417';
  RsPen = 'Pen';
  RsPercentLoad = 'Percent Load';
  RsPeriod = 'Period';
  RsPeriodical = 'Periodical';
  RsPeriodicalAuto_DiscriminatePlus2 = 'Periodical Auto-Discriminate +2';
  RsPeriodicalAuto_DiscriminatePlus5 = 'Periodical Auto-Discriminate +5';
  RsPeriodicalIgnorePlus2 = 'Periodical Ignore +2';
  RsPeriodicalIgnorePlus5 = 'Periodical Ignore +5';
  RsPeriodicalOnlyDecodewithPlus2 = 'Periodical Only Decode with +2';
  RsPeriodicalOnlyDecodewithPlus5 = 'Periodical Only Decode with +5';
  RsPgDn = 'PgDn';
  RsPgUp = 'PgUp';
  RsPhase = 'Phase';
  RsPhone = 'Phone';
  RsPhoneDirectory = 'Phone Directory';
  RsPhoneMute = 'Phone Mute';
  RsPhysicalInputDeviceForceFeedback = 'Physical Input Device (Force Feedback)';
  RsPhysicalInterfaceDevice = 'Physical Interface Device';
  RsPIDBlockFreeReport = 'PID Block Free Report';
  RsPIDBlockLoadReport = 'PID Block Load Report';
  RsPIDDeviceControl = 'PID Device Control';
  RsPIDDeviceControlReport = 'PID Device Control Report';
  RsPIDStateReport = 'PID State Report';
  RsPinballDevice = 'Pinball Device';
  RsPitchForward_Backward = 'Pitch Forward/Backward';
  RsPlay = 'Play';
  RsPlay_Pause = 'Play/Pause';
  RsPlay_Skip = 'Play/Skip';
  RsPlaybackSpeed = 'Playback Speed';
  RsPlayer = 'Player';
  RsPlus10 = '+10';
  RsPlus100 = '+100';
  RsPointingDevice = 'Pointing Device';
  RsPointofView = 'Point of View';
  RsPolarityInvertedBarCode = 'Polarity Inverted Bar Code';
  RsPolarityNormalBarCode = 'Polarity Normal Bar Code';
  RsPoliceAlarm = 'Police Alarm';
  RsPoolAlignment = 'Pool Alignment';
  RsPoolMoveReport = 'Pool Move Report';
  RsPoolReport = 'Pool Report';
  RsPosiCode = 'PosiCode';
  RsPositiveCoefficient = 'Positive Coefficient';
  RsPositiveSaturation = 'Positive Saturation';
  RsPostfail = 'Postfail';
  RsPower = 'Power';
  RsPowerConverter = 'Power Converter';
  RsPowerConverterID = 'Power Converter ID';
  RsPowerDevice = 'Power Device';
  RsPowerFail = 'Power Fail';
  RsPowerOnResetScanner = 'Power On Reset Scanner';
  RsPowerSummary = 'Power Summary';
  RsPowerSummaryID = 'Power Summary ID';
  RsPowerSupply = 'Power Supply';
  RsPowerupBeep = 'Powerup Beep';
  RsPowerWedge = 'Power Wedge';
  RsPrefixAIMI = 'Prefix AIMI';
  RsPrefixNode = 'Prefix Node';
  RsPrefixProprietary = 'Prefix Proprietary';
  RsPresent = 'Present';
  RsPresentStatus = 'Present Status';
  RsPreventReadofBarcodes = 'Prevent Read of Barcodes';
  RsPrimaryBattery = 'Primary Battery';
  RsPrimaryBatterySupport = 'Primary Battery Support';
  RsPrint = 'Print';
  RsPrinterIndicatorPaperJam = 'Printer Indicator Paper Jam';
  RsPrinterIndicatorPaperOut = 'Printer Indicator Paper Out';
  RsPrintScreen = 'Print Screen';
  RsPrior = 'Prior';
  RsPriorityRingback = 'Priority Ringback';
  RsPriorityRingTone = 'Priority Ring Tone';
  RsProgramChangeKeys = 'Program Change Keys';
  RsProgrammableBeeper = 'Programmable Beeper';
  RsProgrammableButton = 'Programmable Button';
  RsProgrammableButtons = 'Programmable Buttons';
  RsProximity = 'Proximity';
  RsProximitySensor = 'Proximity Sensor';
  RsPuck = 'Puck';
  RsPutter = 'Putter';
  RsQ = 'Q';
  RsQRCode = 'QR Code';
  RsQuality = 'Quality';
  RsQuit = 'Quit';
  RsR = 'R';
  RsRAMPoolAvailable = 'RAM Pool Available';
  RsRAMPoolSize = 'RAM Pool Size';
  RsRampStart = 'Ramp Start';
  RsRampStop = 'Ramp Stop';
  RsRandomPlay = 'Random Play';
  RsRate = 'Rate';
  RsRawDataPolarity = 'Raw Data Polarity';
  RsRawScannedDataReport = 'Raw Scanned Data Report';
  RsRe_ZeroCount = 'Re-Zero Count';
  RsReady = 'Ready';
  RsRearBrake = 'Rear Brake';
  RsRecallLast = 'Recall Last';
  RsRecallNumber = 'Recall Number';
  RsRechargable = 'Rechargable';
  RsRecord = 'Record';
  RsRed = 'Red';
  RsRedial = 'Redial';
  RsRelativeStateofCharge = 'Relative State of Charge';
  RsRemainingCapacity = 'Remaining Capacity';
  RsRemainingCapacityLimit = 'Remaining Capacity Limit';
  RsRemainingTimeLimit = 'Remaining Time Limit';
  RsRemainingTimeLimitExpired = 'Remaining Time Limit Expired';
  RsRemote = 'Remote';
  RsReorderTone = 'Reorder Tone';
  RsRepeat = 'Repeat';
  RsRepeatfromMark = 'Repeat from Mark';
  RsReserved = 'Reserved';
  RsReset = 'Reset';
  RsResettozero = 'Reset to zero';
  RsReturnNotEnter = 'Return (not Enter)';
  RsReturntoMark = 'Return to Mark';
  RsRewind = 'Rewind';
  RsRightAlt = 'Right Alt';
  RsRightArrow = 'Right Arrow';
  RsRightCtrl = 'Right Ctrl';
  RsRightGUI = 'Right GUI';
  RsRightShift = 'Right Shift';
  RsRingEnable = 'Ring Enable';
  RsRinger = 'Ringer';
  RsRingSelect = 'Ring Select';
  RsRollover = 'Rollover';
  RsRollRight_Left = 'Roll Right/Left';
  RsROMFlag = 'ROM Flag';
  RsROMPoolSize = 'ROM Pool Size';
  RsRoomTemperature = 'Room Temperature';
  RsRotationalXAxis = 'Rotational X Axis';
  RsRotationalYAxis = 'Rotational Y Axis';
  RsRotationalZAxis = 'Rotational Z Axis';
  RsRow = ' Row';
  RsRowingMachine = 'Rowing Machine';
  RsRows = 'Rows';
  RsRSqBracket = ']';
  RsRudder = 'Rudder';
  RsRunTimetoEmpty = 'Run Time to Empty';
  RsS = 'S';
  RsSafetySwitch = 'Safety Switch';
  RsSailing = 'Sailing';
  RsSampleCount = 'Sample Count';
  RsSamplePeriod = 'Sample Period';
  RsSandWedge = 'Sand Wedge';
  RsSave = 'Save';
  RsScaleAttributeReport = 'Scale Attribute Report';
  RsScaleClassGeneric = 'Scale Class Generic';
  RsScaleClassIIIEnglish = 'Scale Class III English';
  RsScaleClassIIILEnglish = 'Scale Class IIIL English';
  RsScaleClassIIILMetric = 'Scale Class IIIL Metric';
  RsScaleClassIIIMetric = 'Scale Class III Metric';
  RsScaleClassIIMetric = 'Scale Class II Metric';
  RsScaleClassIMetric = 'Scale Class I Metric';
  RsScaleClassIMetricClass = 'Scale Class I Metric Class';
  RsScaleClassIVEnglish = 'Scale Class IV English';
  RsScaleClassIVMetric = 'Scale Class IV Metric';
  RsScaleControlReport = 'Scale Control Report';
  RsScaleDataReport = 'Scale Data Report';
  RsScaleDeviceClass = 'Scale Device Class';
  RsScaleStatisticsReport = 'Scale Statistics Report';
  RsScaleStatusClass = 'Scale Status Class';
  RsScaleStatusEnforcedZeroReturn = 'Scale Status Enforced Zero Return';
  RsScaleStatusFault = 'Scale Status Fault';
  RsScaleStatusInMotion = 'Scale Status In Motion';
  RsScaleStatusOverWeightLimit = 'Scale Status Over Weight Limit';
  RsScaleStatusReport = 'Scale Status Report';
  RsScaleStatusRequiresCalibration = 'Scale Status Requires Calibration';
  RsScaleStatusRequiresRezeroing = 'Scale Status Requires Rezeroing';
  RsScaleStatusStableatCenterofZero = 'Scale Status Stable at Center of Zero';
  RsScaleStatusUnderZero = 'Scale Status Under Zero';
  RsScaleStatusWeightStable = 'Scale Status Weight Stable';
  RsScaleStatusZeroScale = 'Scale Status Zero Scale';
  RsScaleWeightLimitReport = 'Scale Weight Limit Report';
  RsScannedDataReport = 'Scanned Data Report';
  RsScannerDataAccuracy = 'Scanner Data Accuracy';
  RsScannerinCradle = 'Scanner in Cradle';
  RsScannerinRange = 'Scanner in Range';
  RsScannerReadConfidence = 'Scanner Read Confidence';
  RsScanNextTrack = 'Scan Next Track';
  RsScanPreviousTrack = 'Scan Previous Track';
  RsScreenCalls = 'Screen Calls';
  RsScreenSaverDelay = 'Screen Saver Delay';
  RsScreenSaverEnable = 'Screen Saver Enable';
  RsScrollLock = 'Scroll Lock';
  RsSearchMarkBackwards = 'Search Mark Backwards';
  RsSearchMarkForward = 'Search Mark Forward';
  RsSecondaryFlipper = 'Secondary Flipper';
  RsSecondaryTipSwitch = 'Secondary Tip Switch';
  RsSecondDiscreteLengthtoDecode = 'Second Discrete Length to Decode';
  RsSecurityEnable = 'Security Enable';
  RsSelect = 'Select';
  RsSelectDisc = 'Select Disc';
  RsSelectedIndicator = 'Selected Indicator';
  RsSelection = 'Selection';
  RsSelectorRevision = 'Selector Revision';
  RsSelectWeapons = 'Select Weapons';
  RsSemicolon = ';';
  RsSend = 'Send';
  RsSeparator = 'Separator';
  RsSerialNumber = 'Serial Number';
  RsSetConditionReport = 'Set Condition Report';
  RsSetConstantForceReport = 'Set Constant Force Report';
  RsSetCustomForceReport = 'Set Custom Force Report';
  RsSetEffectReport = 'Set Effect Report';
  RsSetEnvelopeReport = 'Set Envelope Report';
  RsSetParameterDefaultValues = 'Set Parameter Default Values';
  RsSetPeriodicReport = 'Set Periodic Report';
  RsSetRampForceReport = 'Set Ramp Force Report';
  RsSettingsReport = 'Settings Report';
  RsSharedParameterBlocks = 'Shared Parameter Blocks';
  RsShifter = 'Shifter';
  RsShootBall = 'Shoot Ball';
  RsShowCounter = 'Show Counter';
  RsShutdownImminent = 'Shutdown Imminent';
  RsShutdownRequested = 'Shutdown Requested';
  RsSimulation = 'Simulation';
  RsSimultaneousEffectsMax = 'Simultaneous Effects Max';
  RsSlash = '/';
  RsSleep = 'Sleep';
  RsSleepAfter = 'Sleep After';
  RsSleepMode = 'Sleep Mode';
  RsSlider = 'Slider';
  RsSlope = 'Slope';
  RsSlow = 'Slow';
  RsSlowBlinkOff_Time = 'Slow Blink Off-Time';
  RsSlowBlinkOn_Time = 'Slow Blink On-Time';
  RsSlowTracking = 'Slow Tracking';
  RsSMBAlarmWarning = 'SMB Alarm Warning';
  RsSMBBatteryMode = 'SMB Battery Mode';
  RsSMBBatteryStatus = 'SMB Battery Status';
  RsSMBChargerMode = 'SMB Charger Mode';
  RsSMBChargerSpecInfo = 'SMB Charger Spec Info';
  RsSMBChargerStatus = 'SMB Charger Status';
  RsSMBErrorCode = 'SMB Error Code';
  RsSMBSelectorInfo = 'SMB Selector Info';
  RsSMBSelectorPresets = 'SMB Selector Presets';
  RsSMBSelectorstate = 'SMB Selector state';
  RsSnapshot = 'Snapshot';
  RsSoftControlAdjust = 'Soft Control Adjust';
  RsSoftControlSelect = 'Soft Control Select';
  RsSoftStepPrimary = 'Soft Step Primary';
  RsSoftStepSecondary = 'Soft Step Secondary';
  RsSoundErrorBeep = 'Sound Error Beep';
  RsSoundGoodReadBeep = 'Sound Good Read Beep';
  RsSoundNotOnFileBeep = 'Sound Not On File Beep';
  RsSpace = 'Space';
  RsSpaceship = 'Spaceship';
  RsSpeakerPhone = 'Speaker Phone';
  RsSpeakerSystem = 'Speaker System';
  RsSpecificationInfo = 'Specification Info';
  RsSpectralDopplerAdjust = 'Spectral Doppler Adjust';
  RsSpectralDopplerModeSelect = 'Spectral Doppler Mode Select';
  RsSpeedDial = 'Speed Dial';
  RsSpeedSelect = 'Speed Select';
  RsSport = 'Sport';
  RsSports = 'Sports';
  RsStand_by = 'Stand-by';
  RsStandard2of5 = 'Standard 2 of 5';
  RsStandard2of5ControlReport = 'Standard 2 of 5 Control Report';
  RsStandard2of5IATA = 'Standard 2 of 5 IATA';
  RsStandardPlay = 'Standard Play';
  RsStart = 'Start';
  RsStartDelay = 'Start Delay';
  RsStatNotReady = 'Stat Not Ready';
  RsStatReady = 'Stat Ready';
  RsStatusReport = 'Status Report';
  RsSteering = 'Steering';
  RsStereoEnable = 'Stereo Enable';
  RsStereoPlotter = 'Stereo Plotter';
  RsStickFaceAngle = 'Stick Face Angle';
  RsStickFollowThrough = 'Stick Follow Through';
  RsStickHeel_Toe = 'Stick Heel/Toe';
  RsStickHeight = 'Stick Height';
  RsStickSpeed = 'Stick Speed';
  RsStickTempo = 'Stick Tempo';
  RsStickType = 'Stick Type';
  RsStill = 'Still';
  RsStop = 'Stop';
  RsStop_Eject = 'Stop/Eject';
  RsStoreNumber = 'Store Number';
  RsStylus = 'Stylus';
  RsSub_channel = 'Sub-channel';
  RsSub_channelDecrement = 'Sub-channel Decrement';
  RsSub_channelIncrement = 'Sub-channel Increment';
  RsSubmarine = 'Submarine';
  RsSuperCode = 'SuperCode';
  RsSurroundMode = 'Surround Mode';
  RsSwitchable = 'Switchable';
  RsSwitchOffControl = 'Switch Off Control';
  RsSwitchOn_Off = 'Switch On/Off';
  RsSwitchOnControl = 'Switch On Control';
  RsSymbologyIdentifier1 = 'Symbology Identifier 1';
  RsSymbologyIdentifier2 = 'Symbology Identifier 2';
  RsSymbologyIdentifier3 = 'Symbology Identifier 3';
  RsSysRequest = 'Sys Request';
  RsSystemControl = 'System Control';
  RsSystemControlApplicationBreak = 'System Control Application Break';
  RsSystemControlApplicationDebuggerBreak = 'System Control Application Debugger Break';
  RsSystemControlAppMenu = 'System Control App Menu';
  RsSystemControlBothDisplays = 'System Control Both Displays';
  RsSystemControlBreak = 'System Control Break';
  RsSystemControlColdRestart = 'System Control Cold Restart';
  RsSystemControlContextMenu = 'System Control Context Menu';
  RsSystemControlDebuggerBreak = 'System Control Debugger Break';
  RsSystemControlDock = 'System Control Dock';
  RsSystemControlDPadDown = 'System Control DPad Down';
  RsSystemControlDPadLeft = 'System Control DPad Left';
  RsSystemControlDPadRight = 'System Control DPad Right';
  RsSystemControlDPadUp = 'System Control DPad Up';
  RsSystemControlDualDisplays = 'System Control Dual Displays';
  RsSystemControlExternalDisplay = 'System Control External Display';
  RsSystemControlHelpMenu = 'System Control Help Menu';
  RsSystemControlHibernate = 'System Control Hibernate';
  RsSystemControlInternalDisplay = 'System Control Internal Display';
  RsSystemControlInvertDisplay = 'System Control Invert Display';
  RsSystemControlLCDAutoscaleDisplay = 'System Control LCD Autoscale Display';
  RsSystemControlMainMenu = 'System Control Main Menu';
  RsSystemControlMenuDown = 'System Control Menu Down';
  RsSystemControlMenuExit = 'System Control Menu Exit';
  RsSystemControlMenuLeft = 'System Control Menu Left';
  RsSystemControlMenuRight = 'System Control Menu Right';
  RsSystemControlMenuSelect = 'System Control Menu Select';
  RsSystemControlMenuUp = 'System Control Menu Up';
  RsSystemControlPowerDown = 'System Control Power Down';
  RsSystemControlSetup = 'System Control Setup';
  RsSystemControlSleep = 'System Control Sleep';
  RsSystemControlSpeakerMute = 'System Control Speaker Mute';
  RsSystemControlSwapPrimary_SecondaryDisplays = 'System Control Swap Primary/Secondary Displays';
  RsSystemControlToggleInternal_ExternalDisplay = 'System Control Toggle Internal/External Display';
  RsSystemControlUndock = 'System Control Undock';
  RsSystemControlWakeUp = 'System Control Wake Up';
  RsSystemControlWarmRestart = 'System Control Warm Restart';
  RsSystemSuspend = 'System Suspend';
  RsT = 'T';
  RsTabletFunctionKeys = 'Tablet Function Keys';
  RsTabletPick = 'Tablet Pick';
  RsTabulator = 'Tabulator';
  RsTank = 'Tank';
  RsTap = 'Tap';
  RsTelephony = 'Telephony';
  RsTelephonyCallPickup = 'Telephony Call Pickup';
  RsTelephonyConference = 'Telephony Conference';
  RsTelephonyCoverage = 'Telephony Coverage';
  RsTelephonyDataMode = 'Telephony Data Mode';
  RsTelephonyDoNotDisturb = 'Telephony Do Not Disturb';
  RsTelephonyHeadSet = 'Telephony Head Set';
  RsTelephonyHold = 'Telephony Hold';
  RsTelephonyMessageWaiting = 'Telephony Message Waiting';
  RsTelephonyMicrophone = 'Telephony Microphone';
  RsTelephonyNightMode = 'Telephony Night Mode';
  RsTelephonyOffHook = 'Telephony Off Hook';
  RsTelephonyRing = 'Telephony Ring';
  RsTelephonySendCalls = 'Telephony Send Calls';
  RsTelephonySpeaker = 'Telephony Speaker';
  RsTemperature = 'Temperature';
  RsTerminateCharge = 'Terminate Charge';
  RsTerminateDischarge = 'Terminate Discharge';
  RsTest = 'Test';
  RsTested = 'Tested';
  RsThermistorCold = 'Thermistor Cold';
  RsThermistorHot = 'Thermistor Hot';
  RsThermistorOverRange = 'Thermistor Over Range';
  RsThermistorUnderrange = 'Thermistor Under range';
  RsThrottle = 'Throttle';
  RsTipPressure = 'Tip Pressure';
  RsTipSwitch = 'Tip Switch';
  RsToeBrake = 'Toe Brake';
  RsToggleControl = 'Toggle Control';
  RsTonesOff = 'Tones Off';
  RsTouch = 'Touch';
  RsTouchPad = 'Touch Pad';
  RsTouchScreen = 'Touch Screen';
  RsTrack1Data = 'Track 1 Data';
  RsTrack1Length = 'Track 1 Length';
  RsTrack2Data = 'Track 2 Data';
  RsTrack2Length = 'Track 2 Length';
  RsTrack3Data = 'Track 3 Data';
  RsTrack3Length = 'Track 3 Length';
  RsTrackControl = 'Track Control';
  RsTrackData = 'Track Data';
  RsTracking = 'Tracking';
  RsTrackingDecrement = 'Tracking Decrement';
  RsTrackingIncrement = 'Tracking Increment';
  RsTrackJISData = 'Track JIS Data';
  RsTrackJISLength = 'Track JIS Length';
  RsTrackNormal = 'Track Normal';
  RsTransducerIndex = 'Transducer Index';
  RsTransfer = 'Transfer';
  RsTransmitCheckDigit = 'Transmit Check Digit';
  RsTransmitPower = 'Transmit Power';
  RsTransmitStart_Stop = 'Transmit Start/Stop';
  RsTreadmill = 'Treadmill';
  RsTreble = 'Treble';
  RsTrebleDecrement = 'Treble Decrement';
  RsTrebleIncrement = 'Treble Increment';
  RsTri_Optic = 'Tri-Optic';
  RsTrigger = 'Trigger';
  RsTriggerButton = 'Trigger Button';
  RsTriggerless = 'Triggerless';
  RsTriggerMode = 'Trigger Mode';
  RsTriggerModeBlinkingLaserOn = 'Trigger Mode Blinking Laser On';
  RsTriggerModeContinuousLaserOn = 'Trigger Mode Continuous Laser On';
  RsTriggerModeLaserOnWhilePulled = 'Trigger Mode Laser On While Pulled';
  RsTriggerModeLaserStaysOnAfterTriggerRelease = 'Trigger Mode Laser Stays On After Trigger Release';
  RsTriggerRepeatInterval = 'Trigger Repeat Interval';
  RsTriggerReport = 'Trigger Report';
  RsTriggerState = 'Trigger State';
  RsTurnRight_Left = 'Turn Right/Left';
  RsTurretDirection = 'Turret Direction';
  RsTwist = 'Twist';
  RsTypeSpecificBlockHandle = 'Type Specific Block Handle';
  RsTypeSpecificBlockOffset = 'Type Specific Block Offset';
  RsU = 'U';
  RsUCC_EAN_128 = 'UCC/EAN-128';
  RsUltraCode = 'UltraCode';
  RsUndefined = 'Undefined';
  RsUndefinedError = 'Undefined Error';
  RsUndo = 'Undo';
  RsUnicode = 'Unicode';
  RsUnicodeCharacterSet = 'Unicode Character Set';
  RsUntouch = 'Untouch';
  RsUpArrow = 'Up Arrow';
  RsUPC_A = 'UPC-A';
  RsUPC_Awith128Mandatory = 'UPC-A with 128 Mandatory';
  RsUPC_Awith128Optional = 'UPC-A with 128 Optional';
  RsUPC_AwithP5Optional = 'UPC-A with P5 Optional';
  RsUPC_E = 'UPC-E';
  RsUPC_E1 = 'UPC-E1';
  RsUPC_EAN = 'UPC/EAN';
  RsUPC_EANControlReport = 'UPC/EAN Control Report';
  RsUPC_EANCouponCode = 'UPC/EAN Coupon Code';
  RsUPC_EANPeriodicals = 'UPC/EAN Periodicals';
  RsUpdate = 'Update';
  RsUPS = 'UPS';
  RsUSBEnumeratedValues = 'USB Enumerated Values';
  RsUSBMonitor = 'USB Monitor';
  RsUSD_5SlugCode = 'USD-5 (Slug Code)';
  RsUsed = 'Used';
  RsUseNext = 'Use Next';
  RsV = 'V';
  RsVCR_TV = 'VCR/TV';
  RsVCRAquisition = 'VCR Aquisition';
  RsVCRPlus = 'VCR Plus';
  RsVDIFInformation = 'VDIF Information';
  RsVelocityBrakeX = 'Velocity Brake X';
  RsVelocityBrakeY = 'Velocity Brake Y';
  RsVelocityBrakeZ = 'Velocity Brake Z';
  RsVelocityX = 'Velocity X';
  RsVelocityY = 'Velocity Y';
  RsVelocityZ = 'Velocity Z';
  RsVeriCode = 'VeriCode';
  RsVerticalScroll = 'Vertical Scroll';
  RsVESAVersion = 'VESA Version';
  RsVESAVirtualControls = 'VESA Virtual Controls';
  RsVest = 'Vest';
  RsVirtualReality = 'Virtual Reality';
  RsVno = 'Vno';
  RsVoiceMail = 'Voice Mail';
  RsVoltage = 'Voltage';
  RsVoltageNotRegulated = 'Voltage Not Regulated';
  RsVoltageoutofRange = 'Voltage out of Range';
  RsVolume = 'Volume';
  RsVolumeDecrement = 'Volume Decrement';
  RsVolumeDown = 'Volume Down';
  RsVolumeIncrement = 'Volume Increment';
  RsVolumeUp = 'Volume Up';
  RsW = 'W';
  RsWand = 'Wand';
  RsWarningCapacityLimit = 'Warning Capacity Limit';
  RsWaterResistant = 'Water Resistant';
  RsWeekly = 'Weekly';
  RsWeighingDevice = 'Weighing Device';
  RsWeightUnitAvoirTon = 'Weight Unit Avoir Ton';
  RsWeightUnitCarats = 'Weight Unit Carats';
  RsWeightUnitClass = 'Weight Unit Class';
  RsWeightUnitGrains = 'Weight Unit Grains';
  RsWeightUnitGram = 'Weight Unit Gram';
  RsWeightUnitKilogram = 'Weight Unit Kilogram';
  RsWeightUnitMetricTon = 'Weight Unit Metric Ton';
  RsWeightUnitMilligram = 'Weight Unit Milligram';
  RsWeightUnitOunce = 'Weight Unit Ounce';
  RsWeightUnitPennyweights = 'Weight Unit Pennyweights';
  RsWeightUnitPound = 'Weight Unit Pound';
  RsWeightUnitTaels = 'Weight Unit Taels';
  RsWeightUnitTroyOunce = 'Weight Unit Troy Ounce';
  RsWheel = 'Wheel';
  RsWhiteBoard = 'White Board';
  RsWingFlaps = 'Wing Flaps';
  RsWirelessChannel = 'Wireless Channel';
  RsWirelessID = 'Wireless ID';
  RsWood1 = 'Wood 1';
  RsWood3 = 'Wood 3';
  RsWood5 = 'Wood 5';
  RsWood7 = 'Wood 7';
  RsWood9 = 'Wood 9';
  RsX = 'X';
  RsXAxis = 'X Axis';
  RsXTilt = 'X Tilt';
  RsY = 'Y';
  RsYAxis = 'Y Axis';
  RsYTilt = 'Y Tilt';
  RsZ = 'Z';
  RsZAxis = 'Z Axis';
  RsZoomAdjust = 'Zoom Adjust';
  RsZoomSelect = 'Zoom Select';

procedure UsageAndUsagePageText(UsagePage, Usage: TUsage; var UsagePageText, UsageText: string);
begin
  UsagePageText := '';
  UsageText := '';
  case UsagePage of
    HID_USAGE_PAGE_UNDEFINED:
      UsagePageText := RsUndefined;
    HID_USAGE_PAGE_GENERIC:
      begin
        UsagePageText := RsGenericDesktop;
        case Usage of
          HID_USAGE_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_GENERIC_POINTER:
            UsageText := RsPointingDevice;
          HID_USAGE_GENERIC_MOUSE:
            UsageText := RsMouse;
          HID_USAGE_GENERIC_RESERVED1:
            UsageText := RsReserved;
          HID_USAGE_GENERIC_JOYSTICK:
            UsageText := RsJoystick;
          HID_USAGE_GENERIC_GAMEPAD:
            UsageText := RsGamepad;
          HID_USAGE_GENERIC_KEYBOARD:
            UsageText := RsKeyboard;
          HID_USAGE_GENERIC_KEYPAD:
            UsageText := RsKeypad;
          HID_USAGE_GENERIC_MULTIAXIS:
            UsageText := RsMulti_Axis;
          HID_USAGE_GENERIC_X:
            UsageText := RsXAxis;
          HID_USAGE_GENERIC_Y:
            UsageText := RsYAxis;
          HID_USAGE_GENERIC_Z:
            UsageText := RsZAxis;
          HID_USAGE_GENERIC_RX:
            UsageText := RsRotationalXAxis;
          HID_USAGE_GENERIC_RY:
            UsageText := RsRotationalYAxis;
          HID_USAGE_GENERIC_RZ:
            UsageText := RsRotationalZAxis;
          HID_USAGE_GENERIC_SLIDER:
            UsageText := RsSlider;
          HID_USAGE_GENERIC_DIAL:
            UsageText := RsDial;
          HID_USAGE_GENERIC_WHEEL:
            UsageText := RsWheel;
          HID_USAGE_GENERIC_HATSWITCH:
            UsageText := RsHatSwitch;
          HID_USAGE_GENERIC_COUNTED_BUFFER:
            UsageText := RsCountedBuffer;
          HID_USAGE_GENERIC_BYTE_COUNT:
            UsageText := RsByteCount;
          HID_USAGE_GENERIC_MOTION_WAKEUP:
            UsageText := RsMotionWakeup;
          HID_USAGE_GENERIC_START:
            UsageText := RsStart;
          HID_USAGE_GENERIC_SELECT:
            UsageText := RsSelect;
          HID_USAGE_GENERIC_RESERVED2:
            UsageText := RsReserved;
          HID_USAGE_GENERIC_VX:
            UsageText := RsVelocityX;
          HID_USAGE_GENERIC_VY:
            UsageText := RsVelocityY;
          HID_USAGE_GENERIC_VZ:
            UsageText := RsVelocityZ;
          HID_USAGE_GENERIC_VBRX:
            UsageText := RsVelocityBrakeX;
          HID_USAGE_GENERIC_VBRY:
            UsageText := RsVelocityBrakeY;
          HID_USAGE_GENERIC_VBRZ:
            UsageText := RsVelocityBrakeZ;
          HID_USAGE_GENERIC_VNO:
            UsageText := RsVno;
          HID_USAGE_FEATURE_NOTIFICATION:
            UsageText := RsFeatureNotification;
          HID_USAGE_GENERIC_SYSTEM_CTL:
            UsageText := RsSystemControl;
          HID_USAGE_GENERIC_SYSCTL_POWER:
            UsageText := RsSystemControlPowerDown;
          HID_USAGE_GENERIC_SYSCTL_SLEEP:
            UsageText := RsSystemControlSleep;
          HID_USAGE_GENERIC_SYSCTL_WAKE:
            UsageText := RsSystemControlWakeUp;
          HID_USAGE_GENERIC_SYSCTL_CONTEXT_MENU:
            UsageText := RsSystemControlContextMenu;
          HID_USAGE_GENERIC_SYSCTL_MAIN_MENU:
            UsageText := RsSystemControlMainMenu;
          HID_USAGE_GENERIC_SYSCTL_APP_MENU:
            UsageText := RsSystemControlAppMenu;
          HID_USAGE_GENERIC_SYSCTL_HELP_MENU:
            UsageText := RsSystemControlHelpMenu;
          HID_USAGE_GENERIC_SYSCTL_MENU_EXIT:
            UsageText := RsSystemControlMenuExit;
          HID_USAGE_GENERIC_SYSCTL_MENU_SELECT:
            UsageText := RsSystemControlMenuSelect;
          HID_USAGE_GENERIC_SYSCTL_MENU_RIGHT:
            UsageText := RsSystemControlMenuRight;
          HID_USAGE_GENERIC_SYSCTL_MENU_LEFT:
            UsageText := RsSystemControlMenuLeft;
          HID_USAGE_GENERIC_SYSCTL_MENU_UP:
            UsageText := RsSystemControlMenuUp;
          HID_USAGE_GENERIC_SYSCTL_MENU_DOWN:
            UsageText := RsSystemControlMenuDown;
          HID_USAGE_GENERIC_SYSCTL_COLD_RESTART:
            UsageText := RsSystemControlColdRestart;
          HID_USAGE_GENERIC_SYSCTL_WARM_RESTART:
            UsageText := RsSystemControlWarmRestart;
          HID_USAGE_GENERIC_SYSCTL_DPAD_UP:
            UsageText := RsSystemControlDPadUp;
          HID_USAGE_GENERIC_SYSCTL_DPAD_DOWN:
            UsageText := RsSystemControlDPadDown;
          HID_USAGE_GENERIC_SYSCTL_DPAD_RIGHT:
            UsageText := RsSystemControlDPadRight;
          HID_USAGE_GENERIC_SYSCTL_DPAD_LEFT:
            UsageText := RsSystemControlDPadLeft;
          HID_USAGE_GENERIC_SYSCTL_DOCK:
            UsageText := RsSystemControlDock;
          HID_USAGE_GENERIC_SYSCTL_UNDOCK:
            UsageText := RsSystemControlUndock;
          HID_USAGE_GENERIC_SYSCTL_SETUP:
            UsageText := RsSystemControlSetup;
          HID_USAGE_GENERIC_SYSCTL_BREAK:
            UsageText := RsSystemControlBreak;
          HID_USAGE_GENERIC_SYSCTL_DEBUGGER_BREAK:
            UsageText := RsSystemControlDebuggerBreak;
          HID_USAGE_GENERIC_SYSCTL_APP_BREAK:
            UsageText := RsSystemControlApplicationBreak;
          HID_USAGE_GENERIC_SYSCTL_APP_DEBUGGER_BREAK:
            UsageText := RsSystemControlApplicationDebuggerBreak;
          HID_USAGE_GENERIC_SYSCTL_SYSTEM_SPEAKER_MUTE:
            UsageText := RsSystemControlSpeakerMute;
          HID_USAGE_GENERIC_SYSCTL_SYSTEM_HIBERNATE:
            UsageText := RsSystemControlHibernate;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_INVERT:
            UsageText := RsSystemControlInvertDisplay;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_INTERNAL:
            UsageText := RsSystemControlInternalDisplay;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_EXTERNAL:
            UsageText := RsSystemControlExternalDisplay;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_BOTH:
            UsageText := RsSystemControlBothDisplays;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_DUAL:
            UsageText := RsSystemControlDualDisplays;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_TOGGLE_INT_EXT:
            UsageText := RsSystemControlToggleInternal_ExternalDisplay;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_SWAP:
            UsageText := RsSystemControlSwapPrimary_SecondaryDisplays;
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_LCD_AUTOSCALE:
            UsageText := RsSystemControlLCDAutoscaleDisplay;
        end;
      end;
    HID_USAGE_PAGE_SIMULATION:
      begin
        UsagePageText := RsSimulation;
        case Usage of
          HID_USAGE_SIMULATION_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_SIMULATION_FLIGHT:
            UsageText := RsFlight;
          HID_USAGE_SIMULATION_AUTOMOBILE:
            UsageText := RsAutomobile;
          HID_USAGE_SIMULATION_TANK:
            UsageText := RsTank;
          HID_USAGE_SIMULATION_SPACESHIP:
            UsageText := RsSpaceship;
          HID_USAGE_SIMULATION_SUBMARINE:
            UsageText := RsSubmarine;
          HID_USAGE_SIMULATION_SAILING:
            UsageText := RsSailing;
          HID_USAGE_SIMULATION_MOTORCYCLE:
            UsageText := RsMotorcycle;
          HID_USAGE_SIMULATION_SPORTS:
            UsageText := RsSports;
          HID_USAGE_SIMULATION_AIRPLANE:
            UsageText := RsAirplane;
          HID_USAGE_SIMULATION_HELICOPTER:
            UsageText := RsHelicopter;
          HID_USAGE_SIMULATION_MAGIC_CARPET:
            UsageText := RsMagicCarpet;
          HID_USAGE_SIMULATION_BICYCLE:
            UsageText := RsBicycle;
          HID_USAGE_SIMULATION_FLIGHT_CONTROL_STICK:
            UsageText := RsFlightControlStick;
          HID_USAGE_SIMULATION_FLIGHT_STICK:
            UsageText := RsFlightStick;
          HID_USAGE_SIMULATION_CYCLIC_CONTROL:
            UsageText := RsCyclicControl;
          HID_USAGE_SIMULATION_CYCLIC_TRIM:
            UsageText := RsCyclicTrim;
          HID_USAGE_SIMULATION_FLIGHT_YOKE:
            UsageText := RsFlightYoke;
          HID_USAGE_SIMULATION_TRACK_CONTROL:
            UsageText := RsTrackControl;
          HID_USAGE_SIMULATION_AILERON:
            UsageText := RsAileron;
          HID_USAGE_SIMULATION_AILERON_TRIM:
            UsageText := RsAileronTrim;
          HID_USAGE_SIMULATION_ANTITORQUE_CONTROL:
            UsageText := RsAnti_TorqueControl;
          HID_USAGE_SIMULATION_AUTOPILOT_ENABLE:
            UsageText := RsEnableAutopilot;
          HID_USAGE_SIMULATION_CHAFF_RELEASE:
            UsageText := RsChaffRelease;
          HID_USAGE_SIMULATION_COLLECTIVE_CONTROL:
            UsageText := RsCollectiveControl;
          HID_USAGE_SIMULATION_DIVE_BREAK:
            UsageText := RsDiveBreak;
          HID_USAGE_SIMULATION_ELECTRONIC_COUNTERMEASURES:
            UsageText := RsElectronicCountermeasures;
          HID_USAGE_SIMULATION_ELEVATOR:
            UsageText := RsElevator;
          HID_USAGE_SIMULATION_ELEVATOR_TRIM:
            UsageText := RsElevatorTrim;
          HID_USAGE_SIMULATION_RUDDER:
            UsageText := RsRudder;
          HID_USAGE_SIMULATION_THROTTLE:
            UsageText := RsThrottle;
          HID_USAGE_SIMULATION_FLIGHT_COMMUNICATIONS:
            UsageText := RsFlightCommunications;
          HID_USAGE_SIMULATION_FLARE_RELEASE:
            UsageText := RsFlareRelease;
          HID_USAGE_SIMULATION_LANDING_GEAR:
            UsageText := RsLandingGear;
          HID_USAGE_SIMULATION_TOE_BRAKE:
            UsageText := RsToeBrake;
          HID_USAGE_SIMULATION_TRIGGER:
            UsageText := RsTrigger;
          HID_USAGE_SIMULATION_WEAPONS_ARM:
            UsageText := RsArmWeapons;
          HID_USAGE_SIMULATION_WEAPONS_SELECT:
            UsageText := RsSelectWeapons;
          HID_USAGE_SIMULATION_WING_FLAPS:
            UsageText := RsWingFlaps;
          HID_USAGE_SIMULATION_ACCELERATOR:
            UsageText := RsAccelerator;
          HID_USAGE_SIMULATION_BRAKE:
            UsageText := RsBrake;
          HID_USAGE_SIMULATION_CLUTCH:
            UsageText := RsClutch;
          HID_USAGE_SIMULATION_SHIFTER:
            UsageText := RsShifter;
          HID_USAGE_SIMULATION_STEERING:
            UsageText := RsSteering;
          HID_USAGE_SIMULATION_TURRET_DIRECTION:
            UsageText := RsTurretDirection;
          HID_USAGE_SIMULATION_BARREL_ELEVATION:
            UsageText := RsBarrelElevation;
          HID_USAGE_SIMULATION_DIVE_PLANE:
            UsageText := RsDivePlane;
          HID_USAGE_SIMULATION_BALLAST:
            UsageText := RsBallast;
          HID_USAGE_SIMULATION_BICYCLE_CRANK:
            UsageText := RsBicycleCrank;
          HID_USAGE_SIMULATION_HANDLE_BARS:
            UsageText := RsHandleBaRs;
          HID_USAGE_SIMULATION_FRONT_BRAKE:
            UsageText := RsFrontBrake;
          HID_USAGE_SIMULATION_REAR_BRAKE:
            UsageText := RsRearBrake;
        end;
      end;
    HID_USAGE_PAGE_VR:
      begin
        UsagePageText := RsVirtualReality;
        case Usage of
          HID_USAGE_VR_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_VR_BELT:
            UsageText := RsBelt;
          HID_USAGE_VR_BODY_SUIT:
            UsageText := RsBodySuit;
          HID_USAGE_VR_FLEXOR:
            UsageText := RsFlexor;
          HID_USAGE_VR_GLOVE:
            UsageText := RsGlove;
          HID_USAGE_VR_HEAD_TRACKER:
            UsageText := RsHeadTracker;
          HID_USAGE_VR_HEAD_MOUNTED_DISPLAY:
            UsageText := RsHeadMountedDisplay;
          HID_USAGE_VR_HAND_TRACKER:
            UsageText := RsHandTracker;
          HID_USAGE_VR_OCULOMETER:
            UsageText := RsOculometer;
          HID_USAGE_VR_VEST:
            UsageText := RsVest;
          HID_USAGE_VR_ANIMATRONIC_DEVICE:
            UsageText := RsAnimatronicDevice;
          HID_USAGE_VR_STEREO_ENABLE:
            UsageText := RsStereoEnable;
          HID_USAGE_VR_DISPLAY_ENABLE:
            UsageText := RsDisplayEnable;
        end;
      end;
    HID_USAGE_PAGE_SPORT:
      begin
        UsagePageText := RsSport;
        case Usage of
          HID_USAGE_SPORT_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_SPORT_BASEBALL_BAT:
            UsageText := RsBaseballBat;
          HID_USAGE_SPORT_GOLF_CLUB:
            UsageText := RsGolfClub;
          HID_USAGE_SPORT_ROWING_MACHINE:
            UsageText := RsRowingMachine;
          HID_USAGE_SPORT_TREADMILL:
            UsageText := RsTreadmill;
          HID_USAGE_SPORT_OAR:
            UsageText := RsOar;
          HID_USAGE_SPORT_SLOPE:
            UsageText := RsSlope;
          HID_USAGE_SPORT_RATE:
            UsageText := RsRate;
          HID_USAGE_SPORT_STICK_SPEED:
            UsageText := RsStickSpeed;
          HID_USAGE_SPORT_STICK_FACE_ANGLE:
            UsageText := RsStickFaceAngle;
          HID_USAGE_SPORT_STICK_HEEL_TOE:
            UsageText := RsStickHeel_Toe;
          HID_USAGE_SPORT_STICK_FOLLOW_THROUGH:
            UsageText := RsStickFollowThrough;
          HID_USAGE_SPORT_STICK_TEMPO:
            UsageText := RsStickTempo;
          HID_USAGE_SPORT_STICK_TYPE:
            UsageText := RsStickType;
          HID_USAGE_SPORT_STICK_HEIGHT:
            UsageText := RsStickHeight;
          HID_USAGE_SPORT_PUTTER:
            UsageText := RsPutter;
          HID_USAGE_SPORT_IRON_1:
            UsageText := RsIron1;
          HID_USAGE_SPORT_IRON_2:
            UsageText := RsIron2;
          HID_USAGE_SPORT_IRON_3:
            UsageText := RsIron3;
          HID_USAGE_SPORT_IRON_4:
            UsageText := RsIron4;
          HID_USAGE_SPORT_IRON_5:
            UsageText := RsIron5;
          HID_USAGE_SPORT_IRON_6:
            UsageText := RsIron6;
          HID_USAGE_SPORT_IRON_7:
            UsageText := RsIron7;
          HID_USAGE_SPORT_IRON_8:
            UsageText := RsIron8;
          HID_USAGE_SPORT_IRON_9:
            UsageText := RsIron9;
          HID_USAGE_SPORT_IRON_10:
            UsageText := RsIron10;
          HID_USAGE_SPORT_IRON_11:
            UsageText := RsIron11;
          HID_USAGE_SPORT_SAND_WEDGE:
            UsageText := RsSandWedge;
          HID_USAGE_SPORT_LOFT_WEDGE:
            UsageText := RsLoftWedge;
          HID_USAGE_SPORT_POWER_WEDGE:
            UsageText := RsPowerWedge;
          HID_USAGE_SPORT_WOOD_1:
            UsageText := RsWood1;
          HID_USAGE_SPORT_WOOD_3:
            UsageText := RsWood3;
          HID_USAGE_SPORT_WOOD_5:
            UsageText := RsWood5;
          HID_USAGE_SPORT_WOOD_7:
            UsageText := RsWood7;
          HID_USAGE_SPORT_WOOD_9:
            UsageText := RsWood9;
        end;
      end;
    HID_USAGE_PAGE_GAME:
      begin
        UsagePageText := RsGame;
        case Usage of
          HID_USAGE_GAME_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_GAME_3D_GAME_CONTROLLER:
            UsageText := Rs3DGameController;
          HID_USAGE_GAME_PINBALL_DEVICE:
            UsageText := RsPinballDevice;
          HID_USAGE_GAME_GUN_DEVICE:
            UsageText := RsGunDevice;
          HID_USAGE_GAME_POINT_OF_VIEW:
            UsageText := RsPointofView;
          HID_USAGE_GAME_TURN_RIGHT_LEFT:
            UsageText := RsTurnRight_Left;
          HID_USAGE_GAME_PITCH_FORWARD_BACKWARD:
            UsageText := RsPitchForward_Backward;
          HID_USAGE_GAME_ROLL_RIGHT_LEFT:
            UsageText := RsRollRight_Left;
          HID_USAGE_GAME_MOVE_RIGHT_LEFT:
            UsageText := RsMoveRight_Left;
          HID_USAGE_GAME_MOVE_FORWARD_BACKWARD:
            UsageText := RsMoveForward_Backward;
          HID_USAGE_GAME_MOVE_UP_DOWN:
            UsageText := RsMoveUp_Down;
          HID_USAGE_GAME_LEAN_RIGHT_LEFT:
            UsageText := RsLeanRight_Left;
          HID_USAGE_GAME_LEAN_FORWARD_BACKWARD:
            UsageText := RsLeanForward_Backward;
          HID_USAGE_GAME_HEIGHT_OF_POV:
            UsageText := RsHeightofPOV;
          HID_USAGE_GAME_FLIPPER:
            UsageText := RsFlipper;
          HID_USAGE_GAME_SECONDARY_FLIPPER:
            UsageText := RsSecondaryFlipper;
          HID_USAGE_GAME_BUMP:
            UsageText := RsBump;
          HID_USAGE_GAME_NEW_GAME:
            UsageText := RsNewGame;
          HID_USAGE_GAME_SHOOT_BALL:
            UsageText := RsShootBall;
          HID_USAGE_GAME_PLAYER:
            UsageText := RsPlayer;
          HID_USAGE_GAME_GUN_BOLT:
            UsageText := RsGunBolt;
          HID_USAGE_GAME_GUN_CLIP:
            UsageText := RsGunClip;
          HID_USAGE_GAME_GUN_SELECTOR:
            UsageText := RsGunSelector;
          HID_USAGE_GAME_GUN_SINGLE_SHOT:
            UsageText := RsGunSingleShot;
          HID_USAGE_GAME_GUN_BURST:
            UsageText := RsGunBurst;
          HID_USAGE_GAME_GUN_AUTOMATIC:
            UsageText := RsGunAutomatic;
          HID_USAGE_GAME_GUN_SAFETY:
            UsageText := RsGunSafety;
          HID_USAGE_GAME_GAMEPAD_FIRE_JUMP:
            UsageText := RsGamepadFire_Jump;
          HID_USAGE_GAME_GAMEPAD_TRIGGER:
            UsageText := RsGamepadTrigger;
        end;
      end;
    HID_USAGE_PAGE_GENERIC_GAME_CONTROLS:
      begin
        UsagePageText := RsGeneric;
        case Usage of
          HID_USAGE_GENERIC_GAME_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_GENERIC_GAME_BATTERY_STRENGTH:
            UsageText := RsBatteryStrength;
          HID_USAGE_GENERIC_GAME_WIRELESS_CHANNEL:
            UsageText := RsWirelessChannel;
          HID_USAGE_GENERIC_GAME_WIRELESS_ID:
            UsageText := RsWirelessID;
        end;
      end;
    HID_USAGE_PAGE_KEYBOARD:
      begin
        UsagePageText := RsKeyboard;
        case Usage of
          HID_USAGE_KEYBOARD_NOEVENT:
            UsageText := RsEmpty;
          HID_USAGE_KEYBOARD_ROLLOVER:
            UsageText := RsRollover;
          HID_USAGE_KEYBOARD_POSTFAIL:
            UsageText := RsPostfail;
          HID_USAGE_KEYBOARD_UNDEFINED:
            UsageText := RsUndefinedError;
          HID_USAGE_KEYBOARD_aA:
            UsageText := RsA;
          HID_USAGE_KEYBOARD_bB:
            UsageText := RsB;
          HID_USAGE_KEYBOARD_cC:
            UsageText := RsC;
          HID_USAGE_KEYBOARD_dD:
            UsageText := RsD;
          HID_USAGE_KEYBOARD_eE:
            UsageText := RsE;
          HID_USAGE_KEYBOARD_fF:
            UsageText := RsF;
          HID_USAGE_KEYBOARD_gG:
            UsageText := RsG;
          HID_USAGE_KEYBOARD_hH:
            UsageText := RsH;
          HID_USAGE_KEYBOARD_iI:
            UsageText := RsI;
          HID_USAGE_KEYBOARD_jJ:
            UsageText := RsJ;
          HID_USAGE_KEYBOARD_kK:
            UsageText := RsK;
          HID_USAGE_KEYBOARD_lL:
            UsageText := RsL;
          HID_USAGE_KEYBOARD_mM:
            UsageText := RsM;
          HID_USAGE_KEYBOARD_nN:
            UsageText := RsN;
          HID_USAGE_KEYBOARD_oO:
            UsageText := RsO;
          HID_USAGE_KEYBOARD_pP:
            UsageText := RsP;
          HID_USAGE_KEYBOARD_qQ:
            UsageText := RsQ;
          HID_USAGE_KEYBOARD_rR:
            UsageText := RsR;
          HID_USAGE_KEYBOARD_sS:
            UsageText := RsS;
          HID_USAGE_KEYBOARD_tT:
            UsageText := RsT;
          HID_USAGE_KEYBOARD_uU:
            UsageText := RsU;
          HID_USAGE_KEYBOARD_vV:
            UsageText := RsV;
          HID_USAGE_KEYBOARD_wW:
            UsageText := RsW;
          HID_USAGE_KEYBOARD_xX:
            UsageText := RsX;
          HID_USAGE_KEYBOARD_yY:
            UsageText := RsY;
          HID_USAGE_KEYBOARD_zZ:
            UsageText := RsZ;
          HID_USAGE_KEYBOARD_ONE:
            UsageText := Rs1;
          HID_USAGE_KEYBOARD_TWO:
            UsageText := Rs2;
          HID_USAGE_KEYBOARD_THREE:
            UsageText := Rs3;
          HID_USAGE_KEYBOARD_FOUR:
            UsageText := Rs4;
          HID_USAGE_KEYBOARD_FIVE:
            UsageText := Rs5;
          HID_USAGE_KEYBOARD_SIX:
            UsageText := Rs6;
          HID_USAGE_KEYBOARD_SEVEN:
            UsageText := Rs7;
          HID_USAGE_KEYBOARD_EIGHT:
            UsageText := Rs8;
          HID_USAGE_KEYBOARD_NINE:
            UsageText := Rs9;
          HID_USAGE_KEYBOARD_ZERO:
            UsageText := Rs0;
          HID_USAGE_KEYBOARD_ENTER:
            UsageText := RsEnterNotReturn;
          HID_USAGE_KEYBOARD_ESCAPE:
            UsageText := RsEsc;
          HID_USAGE_KEYBOARD_BACKSPACE:
            UsageText := RsBackspace;
          HID_USAGE_KEYBOARD_TAB:
            UsageText := RsTabulator;
          HID_USAGE_KEYBOARD_SPACE:
            UsageText := RsSpace;
          HID_USAGE_KEYBOARD_MINUS:
            UsageText := RsMinus;
          HID_USAGE_KEYBOARD_EQUAL:
            UsageText := RsEqual;
          HID_USAGE_KEYBOARD_LSQBRACKET:
            UsageText := RsLSqBracket;
          HID_USAGE_KEYBOARD_RSQBRACKET:
            UsageText := RsRSqBracket;
          HID_USAGE_KEYBOARD_BACKSLASH:
            UsageText := RsBackslash;
          HID_USAGE_KEYBOARD_HASHMARK2:
            UsageText := RsHashMark2;
          HID_USAGE_KEYBOARD_SEMICOLON:
            UsageText := RsSemicolon;
          HID_USAGE_KEYBOARD_APOSTROPH:
            UsageText := RsApostroph;
          HID_USAGE_KEYBOARD_GRAVEACCENT:
            UsageText := RsGraveAccent;
          HID_USAGE_KEYBOARD_COMMA:
            UsageText := RsComma;
          HID_USAGE_KEYBOARD_DOT:
            UsageText := RsDot;
          HID_USAGE_KEYBOARD_SLASH:
            UsageText := RsSlash;
          HID_USAGE_KEYBOARD_CAPS_LOCK:
            UsageText := RsCapsLock;
          HID_USAGE_KEYBOARD_F1:
            UsageText := RsF1;
          HID_USAGE_KEYBOARD_F2:
            UsageText := RsF2;
          HID_USAGE_KEYBOARD_F3:
            UsageText := RsF3;
          HID_USAGE_KEYBOARD_F4:
            UsageText := RsF4;
          HID_USAGE_KEYBOARD_F5:
            UsageText := RsF5;
          HID_USAGE_KEYBOARD_F6:
            UsageText := RsF6;
          HID_USAGE_KEYBOARD_F7:
            UsageText := RsF7;
          HID_USAGE_KEYBOARD_F8:
            UsageText := RsF8;
          HID_USAGE_KEYBOARD_F9:
            UsageText := RsF9;
          HID_USAGE_KEYBOARD_F10:
            UsageText := RsF10;
          HID_USAGE_KEYBOARD_F11:
            UsageText := RsF11;
          HID_USAGE_KEYBOARD_F12:
            UsageText := RsF12;
          HID_USAGE_KEYBOARD_PRINT_SCREEN:
            UsageText := RsPrintScreen;
          HID_USAGE_KEYBOARD_SCROLL_LOCK:
            UsageText := RsScrollLock;
          HID_USAGE_KEYBOARD_PAUSE:
            UsageText := RsPause;
          HID_USAGE_KEYBOARD_INSERT:
            UsageText := RsIns;
          HID_USAGE_KEYBOARD_HOME:
            UsageText := RsHome;
          HID_USAGE_KEYBOARD_PAGEUP:
            UsageText := RsPgUp;
          HID_USAGE_KEYBOARD_DELETE:
            UsageText := RsDel;
          HID_USAGE_KEYBOARD_END:
            UsageText := RsEnd;
          HID_USAGE_KEYBOARD_PAGEDOWN:
            UsageText := RsPgDn;
          HID_USAGE_KEYBOARD_RIGHT:
            UsageText := RsRightArrow;
          HID_USAGE_KEYBOARD_LEFT:
            UsageText := RsLeftArrow;
          HID_USAGE_KEYBOARD_DOWN:
            UsageText := RsDownArrow;
          HID_USAGE_KEYBOARD_UP:
            UsageText := RsUpArrow;
          HID_USAGE_KEYPAD_NUM_LOCK:
            UsageText := RsKeypadNumLock;
          HID_USAGE_KEYPAD_SLASH:
            UsageText := RsKeypadSlash;
          HID_USAGE_KEYPAD_STAR:
            UsageText := RsKeypadStar;
          HID_USAGE_KEYPAD_MINUS:
            UsageText := RsKeypadMinus;
          HID_USAGE_KEYPAD_PLUS:
            UsageText := RsKeypadPlus;
          HID_USAGE_KEYPAD_ENTER:
            UsageText := RsKeypadEnter;
          HID_USAGE_KEYPAD_ONE:
            UsageText := RsKeypad1;
          HID_USAGE_KEYPAD_TWO:
            UsageText := RsKeypad2;
          HID_USAGE_KEYPAD_THREE:
            UsageText := RsKeypad3;
          HID_USAGE_KEYPAD_FOUR:
            UsageText := RsKeypad4;
          HID_USAGE_KEYPAD_FIVE:
            UsageText := RsKeypad5;
          HID_USAGE_KEYPAD_SIX:
            UsageText := RsKeypad6;
          HID_USAGE_KEYPAD_SEVEN:
            UsageText := RsKeypad7;
          HID_USAGE_KEYPAD_EIGHT:
            UsageText := RsKeypad8;
          HID_USAGE_KEYPAD_NINE:
            UsageText := RsKeypad9;
          HID_USAGE_KEYPAD_ZERO:
            UsageText := RsKeypad0;
          HID_USAGE_KEYPAD_DOT:
            UsageText := RsKeypadDot;
          HID_USAGE_KEYBOARD_BACKSLASH2:
            UsageText := RsBackslash2;
          HID_USAGE_KEYBOARD_APPLICATION:
            UsageText := RsApplication;
          HID_USAGE_KEYBOARD_POWER:
            UsageText := RsPower;
          HID_USAGE_KEYPAD_EQUAL2:
            UsageText := RsKeypadEqual2;
          HID_USAGE_KEYBOARD_F13:
            UsageText := RsF13;
          HID_USAGE_KEYBOARD_F14:
            UsageText := RsF14;
          HID_USAGE_KEYBOARD_F15:
            UsageText := RsF15;
          HID_USAGE_KEYBOARD_F16:
            UsageText := RsF16;
          HID_USAGE_KEYBOARD_F17:
            UsageText := RsF17;
          HID_USAGE_KEYBOARD_F18:
            UsageText := RsF18;
          HID_USAGE_KEYBOARD_F19:
            UsageText := RsF19;
          HID_USAGE_KEYBOARD_F20:
            UsageText := RsF20;
          HID_USAGE_KEYBOARD_F21:
            UsageText := RsF21;
          HID_USAGE_KEYBOARD_F22:
            UsageText := RsF22;
          HID_USAGE_KEYBOARD_F23:
            UsageText := RsF23;
          HID_USAGE_KEYBOARD_F24:
            UsageText := RsF24;
          HID_USAGE_KEYBOARD_EXECUTE:
            UsageText := RsExecute;
          HID_USAGE_KEYBOARD_HELP:
            UsageText := RsHelp;
          HID_USAGE_KEYBOARD_MENU:
            UsageText := RsMenu;
          HID_USAGE_KEYBOARD_SELECT:
            UsageText := RsSelect;
          HID_USAGE_KEYBOARD_STOP:
            UsageText := RsStop;
          HID_USAGE_KEYBOARD_AGAIN:
            UsageText := RsAgain;
          HID_USAGE_KEYBOARD_UNDO:
            UsageText := RsUndo;
          HID_USAGE_KEYBOARD_CUT:
            UsageText := RsCut;
          HID_USAGE_KEYBOARD_COPY:
            UsageText := RsCopy;
          HID_USAGE_KEYBOARD_PASTE:
            UsageText := RsPaste;
          HID_USAGE_KEYBOARD_FIND:
            UsageText := RsFind;
          HID_USAGE_KEYBOARD_MUTE:
            UsageText := RsMute;
          HID_USAGE_KEYBOARD_VOLUME_UP:
            UsageText := RsVolumeUp;
          HID_USAGE_KEYBOARD_VOLUME_DOWN:
            UsageText := RsVolumeDown;
          HID_USAGE_KEYBOARD_LOCKCAPS:
            UsageText := RsCapsLock;
          HID_USAGE_KEYBOARD_LOCKNUM:
            UsageText := RsNumLock;
          HID_USAGE_KEYBOARD_LOCKSCROLL:
            UsageText := RsScrollLock;
          HID_USAGE_KEYPAD_COMMA:
            UsageText := RsKeypadComma;
          HID_USAGE_KEYPAD_EQUALSIGN:
            UsageText := RsKeypadEqual;
          HID_USAGE_KEYBOARD_INATL1:
            UsageText := RsInternational1;
          HID_USAGE_KEYBOARD_INATL2:
            UsageText := RsInternational2;
          HID_USAGE_KEYBOARD_INATL3:
            UsageText := RsInternational3;
          HID_USAGE_KEYBOARD_INATL4:
            UsageText := RsInternational4;
          HID_USAGE_KEYBOARD_INATL5:
            UsageText := RsInternational5;
          HID_USAGE_KEYBOARD_INATL6:
            UsageText := RsInternational6;
          HID_USAGE_KEYBOARD_INATL7:
            UsageText := RsInternational7;
          HID_USAGE_KEYBOARD_INATL8:
            UsageText := RsInternational8;
          HID_USAGE_KEYBOARD_INATL9:
            UsageText := RsInternational9;
          HID_USAGE_KEYBOARD_LANG1:
            UsageText := RsLanguage1;
          HID_USAGE_KEYBOARD_LANG2:
            UsageText := RsLanguage2;
          HID_USAGE_KEYBOARD_LANG3:
            UsageText := RsLanguage3;
          HID_USAGE_KEYBOARD_LANG4:
            UsageText := RsLanguage4;
          HID_USAGE_KEYBOARD_LANG5:
            UsageText := RsLanguage5;
          HID_USAGE_KEYBOARD_LANG6:
            UsageText := RsLanguage6;
          HID_USAGE_KEYBOARD_LANG7:
            UsageText := RsLanguage7;
          HID_USAGE_KEYBOARD_LANG8:
            UsageText := RsLanguage8;
          HID_USAGE_KEYBOARD_LANG9:
            UsageText := RsLanguage9;
          HID_USAGE_KEYBOARD_ALTERASE:
            UsageText := RsAlternateErase;
          HID_USAGE_KEYBOARD_SYSREQ:
            UsageText := RsSysRequest;
          HID_USAGE_KEYBOARD_CANCEL:
            UsageText := RsCancel;
          HID_USAGE_KEYBOARD_CLEAR:
            UsageText := RsClear;
          HID_USAGE_KEYBOARD_PRIOR:
            UsageText := RsPrior;
          HID_USAGE_KEYBOARD_RETURN:
            UsageText := RsReturnNotEnter;
          HID_USAGE_KEYBOARD_SEPARATOR:
            UsageText := RsSeparator;
          HID_USAGE_KEYBOARD_OUT:
            UsageText := RsOut;
          HID_USAGE_KEYBOARD_OPER:
            UsageText := RsOper;
          HID_USAGE_KEYBOARD_CLEAR_AGAIN:
            UsageText := RsClear_Again;
          HID_USAGE_KEYBOARD_CRSEL:
            UsageText := RsCrSel_Props;
          HID_USAGE_KEYBOARD_EXSEL:
            UsageText := RsExSel;
          HID_USAGE_KEYPAD_HUNDREDS:
            UsageText := RsKeypad00;
          HID_USAGE_KEYPAD_THOUSANDS:
            UsageText := RsKeypad000;
          HID_USAGE_KEYPAD_THOUSANDS_SEP:
            UsageText := RsKeypadThousandsSeparator;
          HID_USAGE_KEYPAD_DECIMAL_SEP:
            UsageText := RsKeypadDecimalSeparator;
          HID_USAGE_KEYPAD_CURR_UNIT:
            UsageText := RsKeypadCurrencyUnit;
          HID_USAGE_KEYPAD_CURR_SUBUNIT:
            UsageText := RsKeypadCurrencySubunit;
          HID_USAGE_KEYPAD_LROUNDBRACKET:
            UsageText := RsKeypadLRoundBracket;
          HID_USAGE_KEYPAD_RROUNDBRACKET:
            UsageText := RsKeypadRRoundBracket;
          HID_USAGE_KEYPAD_LCURLYBRACKET:
            UsageText := RsKeypadLCurlyBracket;
          HID_USAGE_KEYPAD_RCURLYBRACKET:
            UsageText := RsKeypadRCurlyBracket;
          HID_USAGE_KEYPAD_TABULATOR:
            UsageText := RsKeypadTabulator;
          HID_USAGE_KEYPAD_BACKSPACE:
            UsageText := RsKeypadBackspace;
          HID_USAGE_KEYPAD_A:
            UsageText := RsKeypadA;
          HID_USAGE_KEYPAD_B:
            UsageText := RsKeypadB;
          HID_USAGE_KEYPAD_C:
            UsageText := RsKeypadC;
          HID_USAGE_KEYPAD_D:
            UsageText := RsKeypadD;
          HID_USAGE_KEYPAD_E:
            UsageText := RsKeypadE;
          HID_USAGE_KEYPAD_F:
            UsageText := RsKeypadF;
          HID_USAGE_KEYPAD_XOR:
            UsageText := RsKeypadXOR;
          HID_USAGE_KEYPAD_CIRCUMFLEX:
            UsageText := RsKeypadCircumflex;
          HID_USAGE_KEYPAD_PERCENT:
            UsageText := RsKeypadPercent;
          HID_USAGE_KEYPAD_BIGGER_THAN:
            UsageText := RsKeypadBiggerThan;
          HID_USAGE_KEYPAD_LESS_THAN:
            UsageText := RsKeypadLessThan;
          HID_USAGE_KEYPAD_BINARY_AND:
            UsageText := RsKeypadBinaryAnd;
          HID_USAGE_KEYPAD_LOGICAL_AND:
            UsageText := RsKeypadLogicalAnd;
          HID_USAGE_KEYPAD_BINARY_OR:
            UsageText := RsKeypadBinaryOr;
          HID_USAGE_KEYPAD_LOGICAL_OR:
            UsageText := RsKeypadLogicalOr;
          HID_USAGE_KEYPAD_COLON:
            UsageText := RsKeypadColon;
          HID_USAGE_KEYPAD_HASHMARK:
            UsageText := RsKeypadHashmark;
          HID_USAGE_KEYPAD_SPACE:
            UsageText := RsKeypadSpace;
          HID_USAGE_KEYPAD_AT:
            UsageText := RsKeypadAt;
          HID_USAGE_KEYPAD_EXCLAMATION:
            UsageText := RsKeypadExclamation;
          HID_USAGE_KEYPAD_MEM_STORE:
            UsageText := RsKeypadMemoryStore;
          HID_USAGE_KEYPAD_MEM_RECALL:
            UsageText := RsKeypadMemoryRecall;
          HID_USAGE_KEYPAD_MEM_CLEAR:
            UsageText := RsKeypadMemoryClear;
          HID_USAGE_KEYPAD_MEM_ADD:
            UsageText := RsKeypadMemoryAdd;
          HID_USAGE_KEYPAD_MEM_SUBTRACT:
            UsageText := RsKeypadMemorySubtract;
          HID_USAGE_KEYPAD_MEM_MULTIPLY:
            UsageText := RsKeypadMemoryMultiply;
          HID_USAGE_KEYPAD_MEM_DIVIDE:
            UsageText := RsKeypadMemoryDivide;
          HID_USAGE_KEYPAD_PLUS_MINUS:
            UsageText := RsKeypadMemoryMinus;
          HID_USAGE_KEYPAD_CLEAR:
            UsageText := RsKeypadClear;
          HID_USAGE_KEYPAD_CLEAR_ENTRY:
            UsageText := RsKeypadClearEntry;
          HID_USAGE_KEYPAD_BINARY:
            UsageText := RsKeypadBinary;
          HID_USAGE_KEYPAD_OCTAL:
            UsageText := RsKeypadOctal;
          HID_USAGE_KEYPAD_DECIMAL:
            UsageText := RsKeypadDecimal;
          HID_USAGE_KEYPAD_HEXADECIMAL:
            UsageText := RsKeypadHexadecimal;
          HID_USAGE_KEYPAD_RESERVED1:
            UsageText := RsKeypadReserved1;
          HID_USAGE_KEYPAD_RESERVED2:
            UsageText := RsKeypadReserved2;
          HID_USAGE_KEYBOARD_LCTRL:
            UsageText := RsLeftCtrl;
          HID_USAGE_KEYBOARD_LSHFT:
            UsageText := RsLeftShift;
          HID_USAGE_KEYBOARD_LALT:
            UsageText := RsLeftAlt;
          HID_USAGE_KEYBOARD_LGUI:
            UsageText := RsLeftGUI;
          HID_USAGE_KEYBOARD_RCTRL:
            UsageText := RsRightCtrl;
          HID_USAGE_KEYBOARD_RSHFT:
            UsageText := RsRightShift;
          HID_USAGE_KEYBOARD_RALT:
            UsageText := RsRightAlt;
          HID_USAGE_KEYBOARD_RGUI:
            UsageText := RsRightGUI;
        end;
      end;
    HID_USAGE_PAGE_LED:
      begin
        UsagePageText := RsLED;
        case Usage of
          HID_USAGE_LED_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_LED_NUM_LOCK:
            UsageText := RsKeyboardNumLock;
          HID_USAGE_LED_CAPS_LOCK:
            UsageText := RsKeyboardCapsLock;
          HID_USAGE_LED_SCROLL_LOCK:
            UsageText := RsKeyboardScrollLock;
          HID_USAGE_LED_COMPOSE:
            UsageText := RsKeyboardCompose;
          HID_USAGE_LED_KANA:
            UsageText := RsKeyboardKana;
          HID_USAGE_LED_POWER:
            UsageText := RsKeyboardPower;
          HID_USAGE_LED_SHIFT:
            UsageText := RsKeyboardShift;
          HID_USAGE_LED_DO_NOT_DISTURB:
            UsageText := RsTelephonyDoNotDisturb;
          HID_USAGE_LED_MUTE:
            UsageText := RsConsumerIndicatorMute;
          HID_USAGE_LED_TONE_ENABLE:
            UsageText := RsConsumerIndicatorToneEnable;
          HID_USAGE_LED_HIGH_CUT_FILTER:
            UsageText := RsConsumerIndicatorHighCutFilter;
          HID_USAGE_LED_LOW_CUT_FILTER:
            UsageText := RsConsumerIndicatorLowCutFilter;
          HID_USAGE_LED_EQUALIZER_ENABLE:
            UsageText := RsConsumerIndicatorEqualizerEnable;
          HID_USAGE_LED_SOUND_FIELD_ON:
            UsageText := RsConsumerIndicatorSoundFieldOn;
          HID_USAGE_LED_SURROUND_FIELD_ON:
            UsageText := RsConsumerIndicatorSurroundFieldOn;
          HID_USAGE_LED_REPEAT:
            UsageText := RsConsumerIndicatorRepeat;
          HID_USAGE_LED_STEREO:
            UsageText := RsConsumerIndicatorStereo;
          HID_USAGE_LED_SAMPLING_RATE_DETECT:
            UsageText := RsConsumerIndicatorSamplingRateDetect;
          HID_USAGE_LED_SPINNING:
            UsageText := RsMediaTransportIndicatorSpinning;
          HID_USAGE_LED_CAV:
            UsageText := RsConsumerIndicatorCAV;
          HID_USAGE_LED_CLV:
            UsageText := RsConsumerIndicatorCLV;
          HID_USAGE_LED_RECORDING_FORMAT_DET:
            UsageText := RsMediaTransportIndicatorRecordingFormatDetect;
          HID_USAGE_LED_OFF_HOOK:
            UsageText := RsTelephonyOffHook;
          HID_USAGE_LED_RING:
            UsageText := RsTelephonyRing;
          HID_USAGE_LED_MESSAGE_WAITING:
            UsageText := RsTelephonyMessageWaiting;
          HID_USAGE_LED_DATA_MODE:
            UsageText := RsTelephonyDataMode;
          HID_USAGE_LED_BATTERY_OPERATION:
            UsageText := RsBatteryOperation;
          HID_USAGE_LED_BATTERY_OK:
            UsageText := RsBatteryOk;
          HID_USAGE_LED_BATTERY_LOW:
            UsageText := RsBatteryLow;
          HID_USAGE_LED_SPEAKER:
            UsageText := RsTelephonySpeaker;
          HID_USAGE_LED_HEAD_SET:
            UsageText := RsTelephonyHeadSet;
          HID_USAGE_LED_HOLD:
            UsageText := RsTelephonyHold;
          HID_USAGE_LED_MICROPHONE:
            UsageText := RsTelephonyMicrophone;
          HID_USAGE_LED_COVERAGE:
            UsageText := RsTelephonyCoverage;
          HID_USAGE_LED_NIGHT_MODE:
            UsageText := RsTelephonyNightMode;
          HID_USAGE_LED_SEND_CALLS:
            UsageText := RsTelephonySendCalls;
          HID_USAGE_LED_CALL_PICKUP:
            UsageText := RsTelephonyCallPickup;
          HID_USAGE_LED_CONFERENCE:
            UsageText := RsTelephonyConference;
          HID_USAGE_LED_STAND_BY:
            UsageText := RsStand_by;
          HID_USAGE_LED_CAMERA_ON:
            UsageText := RsConsumerIndicatorCameraOn;
          HID_USAGE_LED_CAMERA_OFF:
            UsageText := RsConsumerIndicatorCameraOff;
          HID_USAGE_LED_ON_LINE:
            UsageText := RsOnLine;
          HID_USAGE_LED_OFF_LINE:
            UsageText := RsOffLine;
          HID_USAGE_LED_BUSY:
            UsageText := RsBusy;
          HID_USAGE_LED_READY:
            UsageText := RsReady;
          HID_USAGE_LED_PAPER_OUT:
            UsageText := RsPrinterIndicatorPaperOut;
          HID_USAGE_LED_PAPER_JAM:
            UsageText := RsPrinterIndicatorPaperJam;
          HID_USAGE_LED_REMOTE:
            UsageText := RsRemote;
          HID_USAGE_LED_FORWARD:
            UsageText := RsMediaTransportForward;
          HID_USAGE_LED_REVERSE:
            UsageText := RsMediaTransportReverse;
          HID_USAGE_LED_STOP:
            UsageText := RsMediaTransportStop;
          HID_USAGE_LED_REWIND:
            UsageText := RsMediaTransportRewind;
          HID_USAGE_LED_FAST_FORWARD:
            UsageText := RsMediaTransportFastForward;
          HID_USAGE_LED_PLAY:
            UsageText := RsMediaTransportPlay;
          HID_USAGE_LED_PAUSE:
            UsageText := RsMediaTransportPause;
          HID_USAGE_LED_RECORD:
            UsageText := RsMediaTransportRecord;
          HID_USAGE_LED_ERROR:
            UsageText := RsError;
          HID_USAGE_LED_SELECTED_INDICATOR:
            UsageText := RsSelectedIndicator;
          HID_USAGE_LED_IN_USE_INDICATOR:
            UsageText := RsInUseIndicator;
          HID_USAGE_LED_MULTI_MODE_INDICATOR:
            UsageText := RsMultiModeIndicator;
          HID_USAGE_LED_INDICATOR_ON:
            UsageText := RsIndicatorOn;
          HID_USAGE_LED_INDICATOR_FLASH:
            UsageText := RsIndicatorFlash;
          HID_USAGE_LED_INDICATOR_SLOW_BLINK:
            UsageText := RsIndicatorSlowBlink;
          HID_USAGE_LED_INDICATOR_FAST_BLINK:
            UsageText := RsIndicatorFastBlink;
          HID_USAGE_LED_INDICATOR_OFF:
            UsageText := RsIndicatorOff;
          HID_USAGE_LED_FLASH_ON_TIME:
            UsageText := RsFlashOn_Time;
          HID_USAGE_LED_SLOW_BLINK_ON_TIME:
            UsageText := RsSlowBlinkOn_Time;
          HID_USAGE_LED_SLOW_BLINK_OFF_TIME:
            UsageText := RsSlowBlinkOff_Time;
          HID_USAGE_LED_FAST_BLINK_ON_TIME:
            UsageText := RsFastBlinkOn_Time;
          HID_USAGE_LED_FAST_BLINK_OFF_TIME:
            UsageText := RsFastBlinkOff_Time;
          HID_USAGE_LED_INDICATOR_COLOR:
            UsageText := RsIndicatorColor;
          HID_USAGE_LED_RED:
            UsageText := RsRed;
          HID_USAGE_LED_GREEN:
            UsageText := RsGreen;
          HID_USAGE_LED_AMBER:
            UsageText := RsAmber;
          HID_USAGE_LED_GENERIC_INDICATOR:
            UsageText := RsGenericIndicator;
          HID_USAGE_LED_SYSTEM_SUSPEND:
            UsageText := RsSystemSuspend;
          HID_USAGE_LED_EXTERNAL_POWER:
            UsageText := RsExternalPowerConnected;
        end;
      end;
    HID_USAGE_PAGE_BUTTON:
      begin
        UsagePageText := RsButton;
        case Usage of
          HID_USAGE_BUTTON_NO_BUTTON:
            UsageText := RsNoButtonPressed;
            // Usage 1..65535 is the button number
        end;
      end;
    HID_USAGE_PAGE_ORDINAL:
      begin
        UsagePageText := RsOrdinal;
        case Usage of
          HID_USAGE_ORDINAL_RESERVED:
            UsageText := RsReserved;
            // Usage 1..65535 is the ordinal number
        end;
      end;
    HID_USAGE_PAGE_TELEPHONY:
      begin
        UsagePageText := RsTelephony;
        case Usage of
          HID_USAGE_TELEPHONY_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_TELEPHONY_PHONE:
            UsageText := RsPhone;
          HID_USAGE_TELEPHONY_ANSWERING_MACHINE:
            UsageText := RsAnsweringMachine;
          HID_USAGE_TELEPHONY_MESSAGE_CONTROLS:
            UsageText := RsMessageControls;
          HID_USAGE_TELEPHONY_HANDSET:
            UsageText := RsHandset;
          HID_USAGE_TELEPHONY_HEADSET:
            UsageText := RsHeadset;
          HID_USAGE_TELEPHONY_KEYPAD:
            UsageText := RsKeypad;
          HID_USAGE_TELEPHONY_PROGRAMMABLE_BUTTON:
            UsageText := RsProgrammableButton;
          HID_USAGE_TELEPHONY_HOOK_SWITCH:
            UsageText := RsHookSwitch;
          HID_USAGE_TELEPHONY_FLASH:
            UsageText := RsFlash;
          HID_USAGE_TELEPHONY_FEATURE:
            UsageText := RsFeature;
          HID_USAGE_TELEPHONY_HOLD:
            UsageText := RsHold;
          HID_USAGE_TELEPHONY_REDIAL:
            UsageText := RsRedial;
          HID_USAGE_TELEPHONY_TRANSFER:
            UsageText := RsTransfer;
          HID_USAGE_TELEPHONY_DROP:
            UsageText := RsDrop;
          HID_USAGE_TELEPHONY_PARK:
            UsageText := RsPark;
          HID_USAGE_TELEPHONY_FORWARD_CALLS:
            UsageText := RsForwardCalls;
          HID_USAGE_TELEPHONY_ALTERNATE_FUNCTION:
            UsageText := RsAlternateFunction;
          HID_USAGE_TELEPHONY_LINE:
            UsageText := RsLine;
          HID_USAGE_TELEPHONY_SPEAKER_PHONE:
            UsageText := RsSpeakerPhone;
          HID_USAGE_TELEPHONY_CONFERENCE:
            UsageText := RsConference;
          HID_USAGE_TELEPHONY_RING_ENABLE:
            UsageText := RsRingEnable;
          HID_USAGE_TELEPHONY_RING_SELECT:
            UsageText := RsRingSelect;
          HID_USAGE_TELEPHONY_PHONE_MUTE:
            UsageText := RsPhoneMute;
          HID_USAGE_TELEPHONY_CALLER_ID:
            UsageText := RsCallerID;
          HID_USAGE_TELEPHONY_SEND:
            UsageText := RsSend;
          HID_USAGE_TELEPHONY_SPEED_DIAL:
            UsageText := RsSpeedDial;
          HID_USAGE_TELEPHONY_STORE_NUMBER:
            UsageText := RsStoreNumber;
          HID_USAGE_TELEPHONY_RECALL_NUMBER:
            UsageText := RsRecallNumber;
          HID_USAGE_TELEPHONY_PHONE_DIRECTORY:
            UsageText := RsPhoneDirectory;
          HID_USAGE_TELEPHONY_VOICE_MAIL:
            UsageText := RsVoiceMail;
          HID_USAGE_TELEPHONY_SCREEN_CALLS:
            UsageText := RsScreenCalls;
          HID_USAGE_TELEPHONY_DO_NOT_DISTURB:
            UsageText := RsDoNotDisturb;
          HID_USAGE_TELEPHONY_MESSAGE:
            UsageText := RsMessage;
          HID_USAGE_TELEPHONY_ANSWER_ON_OFF:
            UsageText := RsAnswerOn_Off;
          HID_USAGE_TELEPHONY_INSIDE_DIAL_TONE:
            UsageText := RsInsideDialTone;
          HID_USAGE_TELEPHONY_OUTSIDE_DIAL_TONE:
            UsageText := RsOutsideDialTone;
          HID_USAGE_TELEPHONY_INSIDE_RING_TONE:
            UsageText := RsInsideRingTone;
          HID_USAGE_TELEPHONY_OUTSIDE_RING_TONE:
            UsageText := RsOutsideRingTone;
          HID_USAGE_TELEPHONY_PRIORITY_RING_TONE:
            UsageText := RsPriorityRingTone;
          HID_USAGE_TELEPHONY_INSIDE_RINGBACK:
            UsageText := RsInsideRingback;
          HID_USAGE_TELEPHONY_PRIORITY_RINGBACK:
            UsageText := RsPriorityRingback;
          HID_USAGE_TELEPHONY_LINE_BUSY_TONE:
            UsageText := RsLineBusyTone;
          HID_USAGE_TELEPHONY_REORDER_TONE:
            UsageText := RsReorderTone;
          HID_USAGE_TELEPHONY_CALL_WAITING_TONE:
            UsageText := RsCallWaitingTone;
          HID_USAGE_TELEPHONY_CONFIRMATION_TONE_1:
            UsageText := RsConfirmationTone1;
          HID_USAGE_TELEPHONY_CONFIRMATION_TONE_2:
            UsageText := RsConfirmationTone2;
          HID_USAGE_TELEPHONY_TONES_OFF:
            UsageText := RsTonesOff;
          HID_USAGE_TELEPHONY_OUTSIDE_RINGBACK:
            UsageText := RsOutsideRingback;
          HID_USAGE_TELEPHONY_RINGER:
            UsageText := RsRinger;
          HID_USAGE_TELEPHONY_KEY_0:
            UsageText := RsKey0;
          HID_USAGE_TELEPHONY_KEY_1:
            UsageText := RsKey1;
          HID_USAGE_TELEPHONY_KEY_2:
            UsageText := RsKey2;
          HID_USAGE_TELEPHONY_KEY_3:
            UsageText := RsKey3;
          HID_USAGE_TELEPHONY_KEY_4:
            UsageText := RsKey4;
          HID_USAGE_TELEPHONY_KEY_5:
            UsageText := RsKey5;
          HID_USAGE_TELEPHONY_KEY_6:
            UsageText := RsKey6;
          HID_USAGE_TELEPHONY_KEY_7:
            UsageText := RsKey7;
          HID_USAGE_TELEPHONY_KEY_8:
            UsageText := RsKey8;
          HID_USAGE_TELEPHONY_KEY_9:
            UsageText := RsKey9;
          HID_USAGE_TELEPHONY_KEY_STAR:
            UsageText := RsKeyStar;
          HID_USAGE_TELEPHONY_KEY_POUND:
            UsageText := RsKeyPound;
          HID_USAGE_TELEPHONY_KEY_A:
            UsageText := RsKeyA;
          HID_USAGE_TELEPHONY_KEY_B:
            UsageText := RsKeyB;
          HID_USAGE_TELEPHONY_KEY_C:
            UsageText := RsKeyC;
          HID_USAGE_TELEPHONY_KEY_D:
            UsageText := RsKeyD;
        end;
      end;
    HID_USAGE_PAGE_CONSUMER:
      begin
        UsagePageText := RsConsumer;
        case Usage of
          HID_USAGE_CONSUMER_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_CONSUMER_CONSUMER_CONTROL:
            UsageText := RsConsumerControl;
          HID_USAGE_CONSUMER_NUMERIC_KEY_PAD:
            UsageText := RsNumericKeypad;
          HID_USAGE_CONSUMER_PROGRAMMABLE_BUTTONS:
            UsageText := RsProgrammableButtons;
          HID_USAGE_CONSUMER_MICROPHONE:
            UsageText := RsMicrophone;
          HID_USAGE_CONSUMER_HEADPHONE:
            UsageText := RsHeadphone;
          HID_USAGE_CONSUMER_GRAPHIC_EQUALIZER:
            UsageText := RsGraphicEqualizer;
          HID_USAGE_CONSUMER_PLUS_10:
            UsageText := RsPlus10;
          HID_USAGE_CONSUMER_PLUS_100:
            UsageText := RsPlus100;
          HID_USAGE_CONSUMER_AM_PM:
            UsageText := RsAM_PM;
          HID_USAGE_CONSUMER_POWER:
            UsageText := RsPower;
          HID_USAGE_CONSUMER_RESET:
            UsageText := RsReset;
          HID_USAGE_CONSUMER_SLEEP:
            UsageText := RsSleep;
          HID_USAGE_CONSUMER_SLEEP_AFTER:
            UsageText := RsSleepAfter;
          HID_USAGE_CONSUMER_SLEEP_MODE:
            UsageText := RsSleepMode;
          HID_USAGE_CONSUMER_ILLUMINATION:
            UsageText := RsIllumination;
          HID_USAGE_CONSUMER_FUNCTION_BUTTONS:
            UsageText := RsFunctionButtons;
          HID_USAGE_CONSUMER_MENU:
            UsageText := RsMenu;
          HID_USAGE_CONSUMER_MENU_PICK:
            UsageText := RsMenuPick;
          HID_USAGE_CONSUMER_MENU_UP:
            UsageText := RsMenuUp;
          HID_USAGE_CONSUMER_MENU_DOWN:
            UsageText := RsMenuDown;
          HID_USAGE_CONSUMER_MENU_LEFT:
            UsageText := RsMenuLeft;
          HID_USAGE_CONSUMER_MENU_RIGHT:
            UsageText := RsMenuRight;
          HID_USAGE_CONSUMER_MENU_ESCAPE:
            UsageText := RsMenuEscape;
          HID_USAGE_CONSUMER_MENU_VALUE_INCREASE:
            UsageText := RsMenuValueIncrease;
          HID_USAGE_CONSUMER_MENU_VALUE_DECREASE:
            UsageText := RsMenuValueDecrease;
          HID_USAGE_CONSUMER_DATA_ON_SCREEN:
            UsageText := RsDataOnScreen;
          HID_USAGE_CONSUMER_CLOSED_CAPTION:
            UsageText := RsClosedCaption;
          HID_USAGE_CONSUMER_CLOSED_CAPTION_SELECT:
            UsageText := RsClosedCaptionSelect;
          HID_USAGE_CONSUMER_VCR_TV:
            UsageText := RsVCR_TV;
          HID_USAGE_CONSUMER_BROADCAST_MODE:
            UsageText := RsBroadcastMode;
          HID_USAGE_CONSUMER_SNAPSHOT:
            UsageText := RsSnapshot;
          HID_USAGE_CONSUMER_STILL:
            UsageText := RsStill;
          HID_USAGE_CONSUMER_SELECTION:
            UsageText := RsSelection;
          HID_USAGE_CONSUMER_ASSIGN_SELECTION:
            UsageText := RsAssignSelection;
          HID_USAGE_CONSUMER_MODE_STEP:
            UsageText := RsModeStep;
          HID_USAGE_CONSUMER_RECALL_LAST:
            UsageText := RsRecallLast;
          HID_USAGE_CONSUMER_ENTER_CHANNEL:
            UsageText := RsEnterChannel;
          HID_USAGE_CONSUMER_ORDER_MOVIE:
            UsageText := RsOrderMovie;
          HID_USAGE_CONSUMER_CHANNEL:
            UsageText := RsChannel;
          HID_USAGE_CONSUMER_MEDIA_SELECTION:
            UsageText := RsMediaSelection;
          HID_USAGE_CONSUMER_MEDIA_SELECT_COMPUTER:
            UsageText := RsMediaSelectComputer;
          HID_USAGE_CONSUMER_MEDIA_SELECT_TV:
            UsageText := RsMediaSelectTV;
          HID_USAGE_CONSUMER_MEDIA_SELECT_WWW:
            UsageText := RsMediaSelectWWW;
          HID_USAGE_CONSUMER_MEDIA_SELECT_DVD:
            UsageText := RsMediaSelectDVD;
          HID_USAGE_CONSUMER_MEDIA_SELECT_TELEPHONE:
            UsageText := RsMediaSelectTelephone;
          HID_USAGE_CONSUMER_MEDIA_SELECT_PROGRAM_GUIDE:
            UsageText := RsMediaSelectProgramGuide;
          HID_USAGE_CONSUMER_MEDIA_SELECT_VIDEO_PHONE:
            UsageText := RsMediaSelectVideoPhone;
          HID_USAGE_CONSUMER_MEDIA_SELECT_GAMES:
            UsageText := RsMediaSelectGames;
          HID_USAGE_CONSUMER_MEDIA_SELECT_MESSAGES:
            UsageText := RsMediaSelectMessages;
          HID_USAGE_CONSUMER_MEDIA_SELECT_CD:
            UsageText := RsMediaSelectCD;
          HID_USAGE_CONSUMER_MEDIA_SELECT_VCR:
            UsageText := RsMediaSelectVCR;
          HID_USAGE_CONSUMER_MEDIA_SELECT_TUNER:
            UsageText := RsMediaSelectTuner;
          HID_USAGE_CONSUMER_QUIT:
            UsageText := RsQuit;
          HID_USAGE_CONSUMER_HELP:
            UsageText := RsHelp;
          HID_USAGE_CONSUMER_MEDIA_SELECT_TAPE:
            UsageText := RsMediaSelectTape;
          HID_USAGE_CONSUMER_MEDIA_SELECT_CABLE:
            UsageText := RsMediaSelectCable;
          HID_USAGE_CONSUMER_MEDIA_SELECT_SATELLITE:
            UsageText := RsMediaSelectSatellite;
          HID_USAGE_CONSUMER_MEDIA_SELECT_SECURITY:
            UsageText := RsMediaSelectSecurity;
          HID_USAGE_CONSUMER_MEDIA_SELECT_HOME:
            UsageText := RsMediaSelectHome;
          HID_USAGE_CONSUMER_MEDIA_SELECT_CALL:
            UsageText := RsMediaSelectCall;
          HID_USAGE_CONSUMER_CHANNEL_INCREMENT:
            UsageText := RsChannelIncrement;
          HID_USAGE_CONSUMER_CHANNEL_DECREMENT:
            UsageText := RsChannelDecrement;
          HID_USAGE_CONSUMER_MEDIA_SELECT_SAP:
            UsageText := RsMediaSelectSAP;
          HID_USAGE_CONSUMER_RESERVED:
            UsageText := RsReserved;
          HID_USAGE_CONSUMER_VCR_PLUS:
            UsageText := RsVCRPlus;
          HID_USAGE_CONSUMER_ONCE:
            UsageText := RsOnce;
          HID_USAGE_CONSUMER_DAILY:
            UsageText := RsDaily;
          HID_USAGE_CONSUMER_WEEKLY:
            UsageText := RsWeekly;
          HID_USAGE_CONSUMER_MONTHLY:
            UsageText := RsMonthly;
          HID_USAGE_CONSUMER_PLAY:
            UsageText := RsPlay;
          HID_USAGE_CONSUMER_PAUSE:
            UsageText := RsPause;
          HID_USAGE_CONSUMER_RECORD:
            UsageText := RsRecord;
          HID_USAGE_CONSUMER_FAST_FORWARD:
            UsageText := RsFastForward;
          HID_USAGE_CONSUMER_REWIND:
            UsageText := RsRewind;
          HID_USAGE_CONSUMER_SCAN_NEXT_TRACK:
            UsageText := RsScanNextTrack;
          HID_USAGE_CONSUMER_SCAN_PREV_TRACK:
            UsageText := RsScanPreviousTrack;
          HID_USAGE_CONSUMER_STOP:
            UsageText := RsStop;
          HID_USAGE_CONSUMER_EJECT:
            UsageText := RsEject;
          HID_USAGE_CONSUMER_RANDOM_PLAY:
            UsageText := RsRandomPlay;
          HID_USAGE_CONSUMER_SELECT_DISC:
            UsageText := RsSelectDisc;
          HID_USAGE_CONSUMER_ENTER_DISC:
            UsageText := RsEnterDisc;
          HID_USAGE_CONSUMER_REPEAT:
            UsageText := RsRepeat;
          HID_USAGE_CONSUMER_TRACKING:
            UsageText := RsTracking;
          HID_USAGE_CONSUMER_TRACK_NORMAL:
            UsageText := RsTrackNormal;
          HID_USAGE_CONSUMER_SLOW_TRACKING:
            UsageText := RsSlowTracking;
          HID_USAGE_CONSUMER_FRAME_FORWARD:
            UsageText := RsFrameForward;
          HID_USAGE_CONSUMER_FRAME_BACK:
            UsageText := RsFrameBack;
          HID_USAGE_CONSUMER_MARK:
            UsageText := RsMark;
          HID_USAGE_CONSUMER_CLEAR_MARK:
            UsageText := RsClearMark;
          HID_USAGE_CONSUMER_REPEAT_FROM_MARK:
            UsageText := RsRepeatfromMark;
          HID_USAGE_CONSUMER_RETURN_TO_MARK:
            UsageText := RsReturntoMark;
          HID_USAGE_CONSUMER_SEARCH_MARK_FORWARD:
            UsageText := RsSearchMarkForward;
          HID_USAGE_CONSUMER_SEARCK_MARK_BACKWARDS:
            UsageText := RsSearchMarkBackwards;
          HID_USAGE_CONSUMER_COUNTER_RESET:
            UsageText := RsCounterReset;
          HID_USAGE_CONSUMER_SHOW_COUNTER:
            UsageText := RsShowCounter;
          HID_USAGE_CONSUMER_TRACKING_INCREMENT:
            UsageText := RsTrackingIncrement;
          HID_USAGE_CONSUMER_TRACKING_DECREMENT:
            UsageText := RsTrackingDecrement;
          HID_USAGE_CONSUMER_STOP_EJECT:
            UsageText := RsStop_Eject;
          HID_USAGE_CONSUMER_PLAY_PAUSE:
            UsageText := RsPlay_Pause;
          HID_USAGE_CONSUMER_PLAY_SKIP:
            UsageText := RsPlay_Skip;
          HID_USAGE_CONSUMER_VOLUME:
            UsageText := RsVolume;
          HID_USAGE_CONSUMER_BALANCE:
            UsageText := RsBalance;
          HID_USAGE_CONSUMER_MUTE:
            UsageText := RsMute;
          HID_USAGE_CONSUMER_BASS:
            UsageText := RsBass;
          HID_USAGE_CONSUMER_TREBLE:
            UsageText := RsTreble;
          HID_USAGE_CONSUMER_BASS_BOOST:
            UsageText := RsBassBoost;
          HID_USAGE_CONSUMER_SURROUND_MODE:
            UsageText := RsSurroundMode;
          HID_USAGE_CONSUMER_LOUDNESS:
            UsageText := RsLoudness;
          HID_USAGE_CONSUMER_MPX:
            UsageText := RsMPX;
          HID_USAGE_CONSUMER_VOLUME_INCREMENT:
            UsageText := RsVolumeIncrement;
          HID_USAGE_CONSUMER_VOLUME_DECREMENT:
            UsageText := RsVolumeDecrement;
          HID_USAGE_CONSUMER_SPEED_SELECT:
            UsageText := RsSpeedSelect;
          HID_USAGE_CONSUMER_PLAYBACK_SPEED:
            UsageText := RsPlaybackSpeed;
          HID_USAGE_CONSUMER_STANDARD_PLAY:
            UsageText := RsStandardPlay;
          HID_USAGE_CONSUMER_LONG_PLAY:
            UsageText := RsLongPlay;
          HID_USAGE_CONSUMER_EXTENDED_PLAY:
            UsageText := RsExtendedPlay;
          HID_USAGE_CONSUMER_SLOW:
            UsageText := RsSlow;
          HID_USAGE_CONSUMER_FAN_ENABLE:
            UsageText := RsFanEnable;
          HID_USAGE_CONSUMER_FAN_SPEED:
            UsageText := RsFanSpeed;
          HID_USAGE_CONSUMER_LIGHT_ENABLE:
            UsageText := RsLightEnable;
          HID_USAGE_CONSUMER_LIGHT_ILLUMINATION_LEVEL:
            UsageText := RsLightIlluminationLevel;
          HID_USAGE_CONSUMER_CLIMATE_CONTROL_ENABLE:
            UsageText := RsClimateControlEnable;
          HID_USAGE_CONSUMER_ROOM_TEMPERATURE:
            UsageText := RsRoomTemperature;
          HID_USAGE_CONSUMER_SECURITY_ENABLE:
            UsageText := RsSecurityEnable;
          HID_USAGE_CONSUMER_FIRE_ALARM:
            UsageText := RsFireAlarm;
          HID_USAGE_CONSUMER_POLICE_ALARM:
            UsageText := RsPoliceAlarm;
          HID_USAGE_CONSUMER_PROXIMITY:
            UsageText := RsProximity;
          HID_USAGE_CONSUMER_MOTION:
            UsageText := RsMotion;
          HID_USAGE_CONSUMER_DURESS_ALARM:
            UsageText := RsDuressAlarm;
          HID_USAGE_CONSUMER_HOLDUP_ALARM:
            UsageText := RsHoldupAlarm;
          HID_USAGE_CONSUMER_MEDICAL_ALARM:
            UsageText := RsMedicalAlarm;
          HID_USAGE_CONSUMER_BALANCE_RIGHT:
            UsageText := RsBalanceRight;
          HID_USAGE_CONSUMER_BALANCE_LEFT:
            UsageText := RsBalanceLeft;
          HID_USAGE_CONSUMER_BASS_INCREMENT:
            UsageText := RsBassIncrement;
          HID_USAGE_CONSUMER_BASS_DECREMENT:
            UsageText := RsBassDecrement;
          HID_USAGE_CONSUMER_TREBLE_INCREMENT:
            UsageText := RsTrebleIncrement;
          HID_USAGE_CONSUMER_TREBLE_DECREMENT:
            UsageText := RsTrebleDecrement;
          HID_USAGE_CONSUMER_SPEAKER_SYSTEM:
            UsageText := RsSpeakerSystem;
          HID_USAGE_CONSUMER_CHANNEL_LEFT:
            UsageText := RsChannelLeft;
          HID_USAGE_CONSUMER_CHANNEL_RIGHT:
            UsageText := RsChannelRight;
          HID_USAGE_CONSUMER_CHANNEL_CENTER:
            UsageText := RsChannelCenter;
          HID_USAGE_CONSUMER_CHANNEL_FRONT:
            UsageText := RsChannelFront;
          HID_USAGE_CONSUMER_CHANNEL_CENTER_FRONT:
            UsageText := RsChannelCenterFront;
          HID_USAGE_CONSUMER_CHANNEL_SIDE:
            UsageText := RsChannelSide;
          HID_USAGE_CONSUMER_CHANNEL_SURROUND:
            UsageText := RsChannelSurround;
          HID_USAGE_CONSUMER_CHANNEL_LOW_FREQ_ENH:
            UsageText := RsChannelLowFrequencyEnhancement;
          HID_USAGE_CONSUMER_CHANNEL_TOP:
            UsageText := RsChannelTop;
          HID_USAGE_CONSUMER_CHANNEL_UNKNOWN:
            UsageText := RsChannelUnknown;
          HID_USAGE_CONSUMER_SUB_CHANNEL:
            UsageText := RsSub_channel;
          HID_USAGE_CONSUMER_SUB_CHANNEL_INCREMENT:
            UsageText := RsSub_channelIncrement;
          HID_USAGE_CONSUMER_SUB_CHANNEL_DECREMENT:
            UsageText := RsSub_channelDecrement;
          HID_USAGE_CONSUMER_ALTERNATE_AUDIO_INCREMENT:
            UsageText := RsAlternateAudioIncrement;
          HID_USAGE_CONSUMER_ALTERNATE_AUDIO_DECREMENT:
            UsageText := RsAlternateAudioDecrement;
          HID_USAGE_CONSUMER_APP_LAUNCH_BUTTONS:
            UsageText := RsApplicationLaunchButtons;
          HID_USAGE_CONSUMER_AL_LAUNCH_BUTTON_CONFIG_TOOL:
            UsageText := RsApplicationLaunchButtonConfigurationTool;
          HID_USAGE_CONSUMER_AL_PROG_BUTTON_CONFIG:
            UsageText := RsApplicationLaunchProgrammableButtonConfiguration;
          HID_USAGE_CONSUMER_AL_CONSUMER_CONTROL_CONFIG:
            UsageText := RsApplicationLaunchConsumerControlConfiguration;
          HID_USAGE_CONSUMER_AL_WORD_PROCESSOR:
            UsageText := RsApplicationLaunchWordProcessor;
          HID_USAGE_CONSUMER_AL_TEXT_EDITOR:
            UsageText := RsApplicationLaunchTextEditor;
          HID_USAGE_CONSUMER_AL_SPREADSHEET:
            UsageText := RsApplicationLaunchSpreadsheet;
          HID_USAGE_CONSUMER_AL_GRAPHICS_EDITOR:
            UsageText := RsApplicationLaunchGraphicsEditor;
          HID_USAGE_CONSUMER_AL_PRESENTATION_APP:
            UsageText := RsApplicationLaunchPresentationApplication;
          HID_USAGE_CONSUMER_AL_DATABASE_APP:
            UsageText := RsApplicationLaunchDatabaseApplication;
          HID_USAGE_CONSUMER_AL_EMAIL_READER:
            UsageText := RsApplicationLaunchEmailReader;
          HID_USAGE_CONSUMER_AL_NEWSREADER:
            UsageText := RsApplicationLaunchNewsreader;
          HID_USAGE_CONSUMER_AL_VOICEMAIL:
            UsageText := RsApplicationLaunchVoicemail;
          HID_USAGE_CONSUMER_AL_CONTACTS_ADDESSBOOK:
            UsageText := RsApplicationLaunchContacts_Addressbook;
          HID_USAGE_CONSUMER_AL_CALENDAR_SCHEDULE:
            UsageText := RsApplicationLaunchCalendar_Schedule;
          HID_USAGE_CONSUMER_AL_TASK_PROJECT_MANAGER:
            UsageText := RsApplicationLaunchTask_ProjectManager;
          HID_USAGE_CONSUMER_AL_LOG_JOURNAL_TIMECARD:
            UsageText := RsApplicationLaunchLog_Journal_Timecard;
          HID_USAGE_CONSUMER_AL_CHECKBOOK_FINANCE:
            UsageText := RsApplicationLaunchCheckbook_Finance;
          HID_USAGE_CONSUMER_AL_CALCULATOR:
            UsageText := RsApplicationLaunchCalculator;
          HID_USAGE_CONSUMER_AL_AV_CAPTURE_PLAYBACK:
            UsageText := RsApplicationLaunchA_VCapture_Playback;
          HID_USAGE_CONSUMER_AL_LOCAL_MACHINE_BROWSER:
            UsageText := RsApplicationLaunchLocalMachineBrowser;
          HID_USAGE_CONSUMER_AL_LAN_WAN_BROWSER:
            UsageText := RsApplicationLaunchLAN_WANBrowser;
          HID_USAGE_CONSUMER_AL_INTERNET_BROWSER:
            UsageText := RsApplicationLaunchInternetBrowser;
          HID_USAGE_CONSUMER_AL_REMOTE_NETWORKING_ISP_CONNECT:
            UsageText := RsApplicationLaunchRemoteNetworking_ISPConnect;
          HID_USAGE_CONSUMER_AL_NETWORK_CONFERENCE:
            UsageText := RsApplicationLaunchNetworkConference;
          HID_USAGE_CONSUMER_AL_NETWORK_CHAT:
            UsageText := RsApplicationLaunchNetworkChat;
          HID_USAGE_CONSUMER_AL_TELEPHONY_DIALER:
            UsageText := RsApplicationLaunchTelephony_Dialer;
          HID_USAGE_CONSUMER_AL_LOGON:
            UsageText := RsApplicationLaunchLogon;
          HID_USAGE_CONSUMER_AL_LOGOFF:
            UsageText := RsApplicationLaunchLogoff;
          HID_USAGE_CONSUMER_AL_LOGON_LOGOFF:
            UsageText := RsApplicationLaunchLogon_Logoff;
          HID_USAGE_CONSUMER_AL_TERMINAL_LOCK_SCREENSAVER:
            UsageText := RsApplicationLaunchTerminalLock_Screensaver;
          HID_USAGE_CONSUMER_AL_CONTROL_PANEL:
            UsageText := RsApplicationLaunchControlPanel;
          HID_USAGE_CONSUMER_AL_COMMAND_LINE_PROCESSOR_RUN:
            UsageText := RsApplicationLaunchCommandLineProcessor_Run;
          HID_USAGE_CONSUMER_AL_PROCESS_TASK_MANAGER:
            UsageText := RsApplicationLaunchProcess_TaskManager;
          HID_USAGE_CONSUMER_AL_SELECT_TASK_APP:
            UsageText := RsApplicationLaunchSelectTask_Application;
          HID_USAGE_CONSUMER_AL_NEXT_TASK_APP:
            UsageText := RsApplicationLaunchNextTask_Application;
          HID_USAGE_CONSUMER_AL_PREV_TASK_APP:
            UsageText := RsApplicationLaunchPreviousTask_Application;
          HID_USAGE_CONSUMER_AL_PREEMPTIVE_HALT_TASK_APP:
            UsageText := RsApplicationLaunchPreemptiveHaltTask_Application;
          HID_USAGE_CONSUMER_AL_INTEGRATED_HELP_CENTER:
            UsageText := RsApplicationLaunchIntegratedHelpCenter;
          HID_USAGE_CONSUMER_AL_DOCUMENTS:
            UsageText := RsApplicationLaunchDocuments;
          HID_USAGE_CONSUMER_AL_THESAURUS:
            UsageText := RsApplicationLaunchThesaurus;
          HID_USAGE_CONSUMER_AL_DICTIONARY:
            UsageText := RsApplicationLaunchDictionary;
          HID_USAGE_CONSUMER_AL_DESKTOP:
            UsageText := RsApplicationLaunchDesktop;
          HID_USAGE_CONSUMER_AL_SPELL_CHECK:
            UsageText := RsApplicationLaunchSpellCheck;
          HID_USAGE_CONSUMER_AL_GRAMMAR_CHECK:
            UsageText := RsApplicationLaunchGrammarCheck;
          HID_USAGE_CONSUMER_AL_WIRELESS_STATUS:
            UsageText := RsApplicationLaunchWirelessStatus;
          HID_USAGE_CONSUMER_AL_KEYBOARD_LAYOUT:
            UsageText := RsApplicationLaunchKeyboardLayout;
          HID_USAGE_CONSUMER_AL_VIRUS_PROTECTION:
            UsageText := RsApplicationLaunchVirusProtection;
          HID_USAGE_CONSUMER_AL_ENCRYPTION:
            UsageText := RsApplicationLaunchEncryption;
          HID_USAGE_CONSUMER_AL_SCREENSAVER:
            UsageText := RsApplicationLaunchScreensaver;
          HID_USAGE_CONSUMER_AL_ALARMS:
            UsageText := RsApplicationLaunchAlarms;
          HID_USAGE_CONSUMER_AL_CLOCK:
            UsageText := RsApplicationLaunchClock;
          HID_USAGE_CONSUMER_AL_FILE_BROWSER:
            UsageText := RsApplicationLaunchFileBrowser;
          HID_USAGE_CONSUMER_AL_POWER_STATUS:
            UsageText := RsApplicationLaunchPowerStatus;
          HID_USAGE_CONSUMER_GENERIC_GUI_APP_CONTROLS:
            UsageText := RsGenericGUIApplicationControls;
          HID_USAGE_CONSUMER_AC_NEW:
            UsageText := RsApplicationControlNew;
          HID_USAGE_CONSUMER_AC_OPEN:
            UsageText := RsApplicationControlOpen;
          HID_USAGE_CONSUMER_AC_CLOSE:
            UsageText := RsApplicationControlCose;
          HID_USAGE_CONSUMER_AC_EXIT:
            UsageText := RsApplicationControlExit;
          HID_USAGE_CONSUMER_AC_MAXIMIZE:
            UsageText := RsApplicationControlMaximize;
          HID_USAGE_CONSUMER_AC_MINIMIZE:
            UsageText := RsApplicationControlMinimize;
          HID_USAGE_CONSUMER_AC_SAVE:
            UsageText := RsApplicationControlSave;
          HID_USAGE_CONSUMER_AC_PRINT:
            UsageText := RsApplicationControlPrint;
          HID_USAGE_CONSUMER_AC_PROPERTIES:
            UsageText := RsApplicationControlProperties;
          HID_USAGE_CONSUMER_AC_UNDO:
            UsageText := RsApplicationControlUndo;
          HID_USAGE_CONSUMER_AC_COPY:
            UsageText := RsApplicationControlCopy;
          HID_USAGE_CONSUMER_AC_CUT:
            UsageText := RsApplicationControlCut;
          HID_USAGE_CONSUMER_AC_PASTE:
            UsageText := RsApplicationControlPaste;
          HID_USAGE_CONSUMER_AC_SELECT_ALL:
            UsageText := RsApplicationControlSelectAll;
          HID_USAGE_CONSUMER_AC_FIND:
            UsageText := RsApplicationControlFind;
          HID_USAGE_CONSUMER_AC_FIND_AND_REPLACE:
            UsageText := RsApplicationControlFindandReplace;
          HID_USAGE_CONSUMER_AC_SEARCH:
            UsageText := RsApplicationControlSearch;
          HID_USAGE_CONSUMER_AC_GO_TO:
            UsageText := RsApplicationControlGoTo;
          HID_USAGE_CONSUMER_AC_HOME:
            UsageText := RsApplicationControlHome;
          HID_USAGE_CONSUMER_AC_BACK:
            UsageText := RsApplicationControlBack;
          HID_USAGE_CONSUMER_AC_FORWARD:
            UsageText := RsApplicationControlForward;
          HID_USAGE_CONSUMER_AC_STOP:
            UsageText := RsApplicationControlStop;
          HID_USAGE_CONSUMER_AC_REFRESH:
            UsageText := RsApplicationControlRefresh;
          HID_USAGE_CONSUMER_AC_PREV_LINK:
            UsageText := RsApplicationControlPreviousLink;
          HID_USAGE_CONSUMER_AC_NEXT_LINK:
            UsageText := RsApplicationControlNextLink;
          HID_USAGE_CONSUMER_AC_BOOKMARKS:
            UsageText := RsApplicationControlBookmarks;
          HID_USAGE_CONSUMER_AC_HISTORY:
            UsageText := RsApplicationControlHistory;
          HID_USAGE_CONSUMER_AC_SUBSCRIPTIONS:
            UsageText := RsApplicationControlSubscriptions;
          HID_USAGE_CONSUMER_AC_ZOOM_IN:
            UsageText := RsApplicationControlZoomIn;
          HID_USAGE_CONSUMER_AC_ZOOM_OUT:
            UsageText := RsApplicationControlZoomOut;
          HID_USAGE_CONSUMER_AC_ZOOM:
            UsageText := RsApplicationControlZoom;
          HID_USAGE_CONSUMER_AC_FULL_SCREEN_VIEW:
            UsageText := RsApplicationControlFullScreenView;
          HID_USAGE_CONSUMER_AC_NORMAL_VIEW:
            UsageText := RsApplicationControlNormalView;
          HID_USAGE_CONSUMER_AC_VIEW_TOGGLE:
            UsageText := RsApplicationControlViewToggle;
          HID_USAGE_CONSUMER_AC_SCROLL_UP:
            UsageText := RsApplicationControlScrollUp;
          HID_USAGE_CONSUMER_AC_SCROLL_DOWN:
            UsageText := RsApplicationControlScrollDown;
          HID_USAGE_CONSUMER_AC_SCROLL:
            UsageText := RsApplicationControlScroll;
          HID_USAGE_CONSUMER_AC_PAN_LEFT:
            UsageText := RsApplicationControlPanLeft;
          HID_USAGE_CONSUMER_AC_PAN_RIGHT:
            UsageText := RsApplicationControlPanRight;
          HID_USAGE_CONSUMER_AC_PAN:
            UsageText := RsApplicationControlPan;
          HID_USAGE_CONSUMER_AC_NEW_WINDOW:
            UsageText := RsApplicationControlNewWindow;
          HID_USAGE_CONSUMER_AC_TILE_HORIZONTALLY:
            UsageText := RsApplicationControlTileHorizontally;
          HID_USAGE_CONSUMER_AC_TILE_VERTICALLY:
            UsageText := RsApplicationControlTileVertically;
          HID_USAGE_CONSUMER_AC_FORMAT:
            UsageText := RsApplicationControlFormat;
          HID_USAGE_CONSUMER_AC_EDIT:
            UsageText := RsApplicationControlEdit;
          HID_USAGE_CONSUMER_AC_BOLD:
            UsageText := RsApplicationControlBold;
          HID_USAGE_CONSUMER_AC_ITALICS:
            UsageText := RsApplicationControlItalics;
          HID_USAGE_CONSUMER_AC_UNDERLINE:
            UsageText := RsApplicationControlUnderline;
          HID_USAGE_CONSUMER_AC_STRIKETHROUGH:
            UsageText := RsApplicationControlStrikethrough;
          HID_USAGE_CONSUMER_AC_SUBSCRIPT:
            UsageText := RsApplicationControlSubscript;
          HID_USAGE_CONSUMER_AC_SUPERSCRIPT:
            UsageText := RsApplicationControlSuperscript;
          HID_USAGE_CONSUMER_AC_ALL_CAPS:
            UsageText := RsApplicationControlAllCaps;
          HID_USAGE_CONSUMER_AC_ROTATE:
            UsageText := RsApplicationControlRotate;
          HID_USAGE_CONSUMER_AC_RESIZE:
            UsageText := RsApplicationControlResize;
          HID_USAGE_CONSUMER_AC_FLIP_HORIZONTAL:
            UsageText := RsApplicationControlFlipHorizontal;
          HID_USAGE_CONSUMER_AC_FLIP_VERTICAL:
            UsageText := RsApplicationControlFlipVertical;
          HID_USAGE_CONSUMER_AC_MIRROR_HORIZONTAL:
            UsageText := RsApplicationControlMirrorHorizontal;
          HID_USAGE_CONSUMER_AC_MIRROR_VERTICAL:
            UsageText := RsApplicationControlMirrorVertical;
          HID_USAGE_CONSUMER_AC_FONT_SELECT:
            UsageText := RsApplicationControlFontSelect;
          HID_USAGE_CONSUMER_AC_FONT_COLOR:
            UsageText := RsApplicationControlFontColor;
          HID_USAGE_CONSUMER_AC_FONT_SIZE:
            UsageText := RsApplicationControlFontSize;
          HID_USAGE_CONSUMER_AC_JUSTIFY_LEFT:
            UsageText := RsApplicationControlJustifyLeft;
          HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_H:
            UsageText := RsApplicationControlJustifyCenterHorizontally;
          HID_USAGE_CONSUMER_AC_JUSTIFY_RIGHT:
            UsageText := RsApplicationControlJustifyRight;
          HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_H:
            UsageText := RsApplicationControlJustifyBlockHorizontally;
          HID_USAGE_CONSUMER_AC_JUSTIFY_TOP:
            UsageText := RsApplicationControlJustifyTop;
          HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_V:
            UsageText := RsApplicationControlJustifyCenterVertically;
          HID_USAGE_CONSUMER_AC_JUSTIFY_BOTTOM:
            UsageText := RsApplicationControlJustifyBottom;
          HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_V:
            UsageText := RsApplicationControlJustifyBlockVertically;
          HID_USAGE_CONSUMER_AC_INDENT_DECREASE:
            UsageText := RsApplicationControlIndentDecrease;
          HID_USAGE_CONSUMER_AC_INDENT_INCREASE:
            UsageText := RsApplicationControlIndentIncrease;
          HID_USAGE_CONSUMER_AC_NUMBERED_LIST:
            UsageText := RsApplicationControlNumberedList;
          HID_USAGE_CONSUMER_AC_RESTART_NUMBERING:
            UsageText := RsApplicationControlRestartNumbering;
          HID_USAGE_CONSUMER_AC_BULLETED_LIST:
            UsageText := RsApplicationControlBulletedList;
          HID_USAGE_CONSUMER_AC_PROMOTE:
            UsageText := RsApplicationControlPromote;
          HID_USAGE_CONSUMER_AC_DEMOTE:
            UsageText := RsApplicationControlDemote;
          HID_USAGE_CONSUMER_AC_YES:
            UsageText := RsApplicationControlYes;
          HID_USAGE_CONSUMER_AC_NO:
            UsageText := RsApplicationControlNo;
          HID_USAGE_CONSUMER_AC_CANCEL:
            UsageText := RsApplicationControlCancel;
          HID_USAGE_CONSUMER_AC_CATALOG:
            UsageText := RsApplicationControlCatalog;
          HID_USAGE_CONSUMER_AC_BUY_CHECKOUT:
            UsageText := RsApplicationControlBuyCheckout;
          HID_USAGE_CONSUMER_AC_ADD_TO_CART:
            UsageText := RsApplicationControlAddToCart;
          HID_USAGE_CONSUMER_AC_EXPAND:
            UsageText := RsApplicationControlExpand;
          HID_USAGE_CONSUMER_AC_EXPAND_ALL:
            UsageText := RsApplicationControlExpandAll;
          HID_USAGE_CONSUMER_AC_COLLAPSE:
            UsageText := RsApplicationControlCollapse;
          HID_USAGE_CONSUMER_AC_COLLAPSE_ALL:
            UsageText := RsApplicationControlCollapseAll;
          HID_USAGE_CONSUMER_AC_PRINT_PREVIEW:
            UsageText := RsApplicationControlPrintPreview;
          HID_USAGE_CONSUMER_AC_PASTE_SPECIAL:
            UsageText := RsApplicationControlPasteSpecial;
          HID_USAGE_CONSUMER_AC_INSERT_MODE:
            UsageText := RsApplicationControlInsertMode;
          HID_USAGE_CONSUMER_AC_DELETE:
            UsageText := RsApplicationControlDelete;
          HID_USAGE_CONSUMER_AC_LOCK:
            UsageText := RsApplicationControlLock;
          HID_USAGE_CONSUMER_AC_UNLOCK:
            UsageText := RsApplicationControlUnlock;
          HID_USAGE_CONSUMER_AC_PROTECT:
            UsageText := RsApplicationControlProtect;
          HID_USAGE_CONSUMER_AC_UNPROTECT:
            UsageText := RsApplicationControlUnprotect;
          HID_USAGE_CONSUMER_AC_ATTACH_COMMENT:
            UsageText := RsApplicationControlAttachComment;
          HID_USAGE_CONSUMER_AC_DELETE_COMMENT:
            UsageText := RsApplicationControlDeleteComment;
          HID_USAGE_CONSUMER_AC_VIEW_COMMENT:
            UsageText := RsApplicationControlViewComment;
          HID_USAGE_CONSUMER_AC_SELECT_WORD:
            UsageText := RsApplicationControlSelectWord;
          HID_USAGE_CONSUMER_AC_SELECT_SENTENCE:
            UsageText := RsApplicationControlSelectSentence;
          HID_USAGE_CONSUMER_AC_SELECT_PARAGRAPH:
            UsageText := RsApplicationControlSelectParagraph;
          HID_USAGE_CONSUMER_AC_SELECT_COLUMN:
            UsageText := RsApplicationControlSelectColumn;
          HID_USAGE_CONSUMER_AC_SELECT_ROW:
            UsageText := RsApplicationControlSelectRow;
          HID_USAGE_CONSUMER_AC_SELECT_TABLE:
            UsageText := RsApplicationControlSelectTable;
          HID_USAGE_CONSUMER_AC_SELECT_OBJECT:
            UsageText := RsApplicationControlSelectObject;
          HID_USAGE_CONSUMER_AC_REDO_REPEAT:
            UsageText := RsApplicationControlRedo_Repeat;
          HID_USAGE_CONSUMER_AC_SORT:
            UsageText := RsApplicationControlSort;
          HID_USAGE_CONSUMER_AC_SORT_ASCENDING:
            UsageText := RsApplicationControlSortAscending;
          HID_USAGE_CONSUMER_AC_SORT_DESCENDING:
            UsageText := RsApplicationControlSortDescending;
          HID_USAGE_CONSUMER_AC_FILTER:
            UsageText := RsApplicationControlFilter;
          HID_USAGE_CONSUMER_AC_SET_CLOCK:
            UsageText := RsApplicationControlSetClock;
          HID_USAGE_CONSUMER_AC_VIEW_CLOCK:
            UsageText := RsApplicationControlViewClock;
          HID_USAGE_CONSUMER_AC_SELECT_TIME_ZONE:
            UsageText := RsApplicationControlSelectTimeZone;
          HID_USAGE_CONSUMER_AC_EDIT_TIME_ZONES:
            UsageText := RsApplicationControlEditTimeZones;
          HID_USAGE_CONSUMER_AC_SET_ALARM:
            UsageText := RsApplicationControlSetAlarm;
          HID_USAGE_CONSUMER_AC_CLEAR_ALARM:
            UsageText := RsApplicationControlClearAlarm;
          HID_USAGE_CONSUMER_AC_SNOOZE_ALARM:
            UsageText := RsApplicationControlSnoozeAlarm;
          HID_USAGE_CONSUMER_AC_RESET_ALARM:
            UsageText := RsApplicationControlResetAlarm;
          HID_USAGE_CONSUMER_AC_SYNCHRONIZE:
            UsageText := RsApplicationControlSynchronize;
          HID_USAGE_CONSUMER_AC_SEND_RECEIVE:
            UsageText := RsApplicationControlSend_Receive;
          HID_USAGE_CONSUMER_AC_SEND_TO:
            UsageText := RsApplicationControlSendTo;
          HID_USAGE_CONSUMER_AC_REPLY:
            UsageText := RsApplicationControlReply;
          HID_USAGE_CONSUMER_AC_REPLY_ALL:
            UsageText := RsApplicationControlReplyAll;
          HID_USAGE_CONSUMER_AC_FORWARD_MSG:
            UsageText := RsApplicationControlForwardMessage;
          HID_USAGE_CONSUMER_AC_SEND:
            UsageText := RsApplicationControlSend;
          HID_USAGE_CONSUMER_AC_ATTACH_FILE:
            UsageText := RsApplicationControlAttachFile;
          HID_USAGE_CONSUMER_AC_UPLOAD:
            UsageText := RsApplicationControlUpload;
          HID_USAGE_CONSUMER_AC_DOWNLOAD:
            UsageText := RsApplicationControlDownload;
          HID_USAGE_CONSUMER_AC_SET_BORDERS:
            UsageText := RsApplicationControlSetBordeRs;
          HID_USAGE_CONSUMER_AC_INSERT_ROW:
            UsageText := RsApplicationControlInsertRow;
          HID_USAGE_CONSUMER_AC_INSERT_COLUMN:
            UsageText := RsApplicationControlInsertColumn;
          HID_USAGE_CONSUMER_AC_INSERT_FILE:
            UsageText := RsApplicationControlInsertFile;
          HID_USAGE_CONSUMER_AC_INSERT_PICTURE:
            UsageText := RsApplicationControlInsertPicture;
          HID_USAGE_CONSUMER_AC_INSERT_OBJECT:
            UsageText := RsApplicationControlInsertObject;
          HID_USAGE_CONSUMER_AC_INSERT_SYMBOL:
            UsageText := RsApplicationControlInsertSymbol;
          HID_USAGE_CONSUMER_AC_SAVE_AND_CLOSE:
            UsageText := RsApplicationControlSaveandClose;
          HID_USAGE_CONSUMER_AC_RENAME:
            UsageText := RsApplicationControlRename;
          HID_USAGE_CONSUMER_AC_MERGE:
            UsageText := RsApplicationControlMerge;
          HID_USAGE_CONSUMER_AC_SPLIT:
            UsageText := RsApplicationControlSplit;
          HID_USAGE_CONSUMER_AC_DISTRIBUTE_HORIZONTALLY:
            UsageText := RsApplicationControlDistributeHorizontally;
          HID_USAGE_CONSUMER_AC_DISTRIBUTE_VERTICALLY:
            UsageText := RsApplicationControlDistributeVertically;
        end;
      end;
    HID_USAGE_PAGE_DIGITIZER:
      begin
        UsagePageText := RsDigitizer;
        case Usage of
          HID_USAGE_DIGITIZER_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_DIGITIZER_DIGITIZER:
            UsageText := RsDigitizer;
          HID_USAGE_DIGITIZER_PEN:
            UsageText := RsPen;
          HID_USAGE_DIGITIZER_LIGHT_PEN:
            UsageText := RsLightPen;
          HID_USAGE_DIGITIZER_TOUCH_SCREEN:
            UsageText := RsTouchScreen;
          HID_USAGE_DIGITIZER_TOUCH_PAD:
            UsageText := RsTouchPad;
          HID_USAGE_DIGITIZER_WHITE_BOARD:
            UsageText := RsWhiteBoard;
          HID_USAGE_DIGITIZER_COORDINATE_MEASURING_MACHINE:
            UsageText := RsCoordinateMeasuringMachine;
          HID_USAGE_DIGITIZER_3D_DIGITIZER:
            UsageText := Rs3DDigitizer;
          HID_USAGE_DIGITIZER_STEREO_PLOTTER:
            UsageText := RsStereoPlotter;
          HID_USAGE_DIGITIZER_ARTICULATED_ARM:
            UsageText := RsArticulatedArm;
          HID_USAGE_DIGITIZER_ARMATURE:
            UsageText := RsArmature;
          HID_USAGE_DIGITIZER_MULTIPLE_POINT_DIGITIZER:
            UsageText := RsMultiplePointDigitizer;
          HID_USAGE_DIGITIZER_FREE_SPACE_WAND:
            UsageText := RsFreeSpaceWand;
          HID_USAGE_DIGITIZER_STYLUS:
            UsageText := RsStylus;
          HID_USAGE_DIGITIZER_PUCK:
            UsageText := RsPuck;
          HID_USAGE_DIGITIZER_FINGER:
            UsageText := RsFinger;
          HID_USAGE_DIGITIZER_TIP_PRESSURE:
            UsageText := RsTipPressure;
          HID_USAGE_DIGITIZER_BARREL_PRESSURE:
            UsageText := RsBarrelPressure;
          HID_USAGE_DIGITIZER_IN_RANGE:
            UsageText := RsInRange;
          HID_USAGE_DIGITIZER_TOUCH:
            UsageText := RsTouch;
          HID_USAGE_DIGITIZER_UNTOUCH:
            UsageText := RsUntouch;
          HID_USAGE_DIGITIZER_TAP:
            UsageText := RsTap;
          HID_USAGE_DIGITIZER_QUALITY:
            UsageText := RsQuality;
          HID_USAGE_DIGITIZER_DATA_VALID:
            UsageText := RsDataValid;
          HID_USAGE_DIGITIZER_TRANSDUCER_INDEX:
            UsageText := RsTransducerIndex;
          HID_USAGE_DIGITIZER_TABLET_FUNCTION_KEYS:
            UsageText := RsTabletFunctionKeys;
          HID_USAGE_DIGITIZER_PROGRAM_CHANGE_KEYS:
            UsageText := RsProgramChangeKeys;
          HID_USAGE_DIGITIZER_BATTERY_STRENGTH:
            UsageText := RsBatteryStrength;
          HID_USAGE_DIGITIZER_INVERT:
            UsageText := RsInvert;
          HID_USAGE_DIGITIZER_X_TILT:
            UsageText := RsXTilt;
          HID_USAGE_DIGITIZER_Y_TILT:
            UsageText := RsYTilt;
          HID_USAGE_DIGITIZER_AZIMUTH:
            UsageText := RsAzimuth;
          HID_USAGE_DIGITIZER_ALTITUDE:
            UsageText := RsAltitude;
          HID_USAGE_DIGITIZER_TWIST:
            UsageText := RsTwist;
          HID_USAGE_DIGITIZER_TIP_SWITCH:
            UsageText := RsTipSwitch;
          HID_USAGE_DIGITIZER_SECONDARY_TIP_SWITCH:
            UsageText := RsSecondaryTipSwitch;
          HID_USAGE_DIGITIZER_BARREL_SWITCH:
            UsageText := RsBarrelSwitch;
          HID_USAGE_DIGITIZER_ERASER:
            UsageText := RsEraser;
          HID_USAGE_DIGITIZER_TABLET_PICK:
            UsageText := RsTabletPick;
        end;
      end;
    HID_USAGE_PAGE_PHYSICAL_INPUT_DEVICE:
      begin
        UsagePageText := RsPhysicalInputDeviceForceFeedback;
        case Usage of
          HID_USAGE_PID_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_PID_PHYSICAL_INTERFACE_DEVICE:
            UsageText := RsPhysicalInterfaceDevice;
          HID_USAGE_PID_NORMAL:
            UsageText := RsNormal;
          HID_USAGE_PID_SET_EFFECT_REPORT:
            UsageText := RsSetEffectReport;
          HID_USAGE_PID_EFFECT_BLOCK_INDEX:
            UsageText := RsEffectBlockIndex;
          HID_USAGE_PID_PARAMETER_BLOCK_OFFSET:
            UsageText := RsParameterBlockOffset;
          HID_USAGE_PID_ROM_FLAG:
            UsageText := RsROMFlag;
          HID_USAGE_PID_EFFECT_TYPE:
            UsageText := RsEffectType;
          HID_USAGE_PID_ET_CONSTANT_FORCE:
            UsageText := RsEffectTypeConstantForce;
          HID_USAGE_PID_ET_RAMP:
            UsageText := RsEffectTypeRamp;
          HID_USAGE_PID_ET_CUSTOM_FORCE_DATA:
            UsageText := RsEffectTypeCustomForceData;
          HID_USAGE_PID_ET_SQUARE:
            UsageText := RsEffectTypeSquare;
          HID_USAGE_PID_ET_SINE:
            UsageText := RsEffectTypeSine;
          HID_USAGE_PID_ET_TRIANGLE:
            UsageText := RsEffectTypeTriangle;
          HID_USAGE_PID_ET_SAWTOOTH_UP:
            UsageText := RsEffectTypeSawtoothUp;
          HID_USAGE_PID_ET_SAWTOOTH_DOWN:
            UsageText := RsEffectTypeSawtoothDown;
          HID_USAGE_PID_ET_SPRING:
            UsageText := RsEffectTypeSpring;
          HID_USAGE_PID_ET_DAMPER:
            UsageText := RsEffectTypeDamper;
          HID_USAGE_PID_ET_INERTIA:
            UsageText := RsEffectTypeInertia;
          HID_USAGE_PID_ET_FRICTION:
            UsageText := RsEffectTypeFriction;
          HID_USAGE_PID_DURATION:
            UsageText := RsDuration;
          HID_USAGE_PID_SAMPLE_PERIOD:
            UsageText := RsSamplePeriod;
          HID_USAGE_PID_GAIN:
            UsageText := RsGain;
          HID_USAGE_PID_TRIGGER_BUTTON:
            UsageText := RsTriggerButton;
          HID_USAGE_PID_TRIGGER_REPEAT_INTERVAL:
            UsageText := RsTriggerRepeatInterval;
          HID_USAGE_PID_AXES_ENABLE:
            UsageText := RsAxesEnable;
          HID_USAGE_PID_DIRECTION_ENABLE:
            UsageText := RsDirectionEnable;
          HID_USAGE_PID_DIRECTION:
            UsageText := RsDirection;
          HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_OFFSET:
            UsageText := RsTypeSpecificBlockOffset;
          HID_USAGE_PID_BLOCK_TYPE:
            UsageText := RsBlockType;
          HID_USAGE_PID_SET_ENVELOPE_REPORT:
            UsageText := RsSetEnvelopeReport;
          HID_USAGE_PID_ATTACK_LEVEL:
            UsageText := RsAttackLevel;
          HID_USAGE_PID_ATTACK_TIME:
            UsageText := RsAttackTime;
          HID_USAGE_PID_FADE_LEVEL:
            UsageText := RsFadeLevel;
          HID_USAGE_PID_FADE_TIME:
            UsageText := RsFadeTime;
          HID_USAGE_PID_SET_CONDITION_REPORT:
            UsageText := RsSetConditionReport;
          HID_USAGE_PID_CP_OFFSET:
            UsageText := RsCPOffset;
          HID_USAGE_PID_POSITIVE_COEFFICIENT:
            UsageText := RsPositiveCoefficient;
          HID_USAGE_PID_NEGATIVE_COEFFICIENT:
            UsageText := RsNegativeCoefficient;
          HID_USAGE_PID_POSITIVE_SATURATION:
            UsageText := RsPositiveSaturation;
          HID_USAGE_PID_NEGATIVE_SATURATION:
            UsageText := RsNegativeSaturation;
          HID_USAGE_PID_DEAD_BAND:
            UsageText := RsDeadBand;
          HID_USAGE_PID_DOWNLOAD_FORCE_SAMPLE:
            UsageText := RsDownloadForceSample;
          HID_USAGE_PID_ISOCH_CUSTOM_FORCE_ENABLE:
            UsageText := RsIsochCustomForceEnable;
          HID_USAGE_PID_CUSTOM_FORCE_DATA_REPORT:
            UsageText := RsCustomForceDataReport;
          HID_USAGE_PID_CUSTOM_FORCE_DATA:
            UsageText := RsCustomForceData;
          HID_USAGE_PID_CUSTOM_FORCE_VENDOR_DEFINED_DATA:
            UsageText := RsCustomForceVendorDefinedData;
          HID_USAGE_PID_SET_CUSTOM_FORCE_REPORT:
            UsageText := RsSetCustomForceReport;
          HID_USAGE_PID_CUSTOM_FORCE_DATA_OFFSET:
            UsageText := RsCustomForceDataReport;
          HID_USAGE_PID_SAMPLE_COUNT:
            UsageText := RsSampleCount;
          HID_USAGE_PID_SET_PERIODIC_REPORT:
            UsageText := RsSetPeriodicReport;
          HID_USAGE_PID_OFFSET:
            UsageText := RsOffset;
          HID_USAGE_PID_MAGNITUDE:
            UsageText := RsMagnitude;
          HID_USAGE_PID_PHASE:
            UsageText := RsPhase;
          HID_USAGE_PID_PERIOD:
            UsageText := RsPeriod;
          HID_USAGE_PID_SET_CONSTANT_FORCE_REPORT:
            UsageText := RsSetConstantForceReport;
          HID_USAGE_PID_SET_RAMP_FORCE_REPORT:
            UsageText := RsSetRampForceReport;
          HID_USAGE_PID_RAMP_START:
            UsageText := RsRampStart;
          HID_USAGE_PID_RAMP_END:
            UsageText := RsRampStop;
          HID_USAGE_PID_EFFECT_OPERATION_REPORT:
            UsageText := RsEffectOperationReport;
          HID_USAGE_PID_EFFECT_OPERATION:
            UsageText := RsEffectOperation;
          HID_USAGE_PID_OP_EFFECT_START:
            UsageText := RsOPEffectStart;
          HID_USAGE_PID_OP_EFFECT_START_SOLO:
            UsageText := RsOPEffectStartSolo;
          HID_USAGE_PID_OP_EFFECT_STOP:
            UsageText := RsOPEffectStop;
          HID_USAGE_PID_LOOP_COUNT:
            UsageText := RsLoopCount;
          HID_USAGE_PID_DEVICE_GAIN_REPORT:
            UsageText := RsDeviceGainReport;
          HID_USAGE_PID_DEVICE_GAIN:
            UsageText := RsDeviceGain;
          HID_USAGE_PID_PID_POOL_REPORT:
            UsageText := RsPoolReport;
          HID_USAGE_PID_RAM_POOL_SIZE:
            UsageText := RsRAMPoolSize;
          HID_USAGE_PID_ROM_POOL_SIZE:
            UsageText := RsROMPoolSize;
          HID_USAGE_PID_ROM_EFFECT_BLOCK_COUNT:
            UsageText := RsEffectBlockCount;
          HID_USAGE_PID_SIMULTANEOUS_EFFECTS_MAX:
            UsageText := RsSimultaneousEffectsMax;
          HID_USAGE_PID_POOL_ALIGNMENT:
            UsageText := RsPoolAlignment;
          HID_USAGE_PID_PID_POOL_MOVE_REPORT:
            UsageText := RsPoolMoveReport;
          HID_USAGE_PID_MOVE_SOURCE:
            UsageText := RsMoveSource;
          HID_USAGE_PID_MOVE_DESTINATION:
            UsageText := RsMoveDestination;
          HID_USAGE_PID_MOVE_LENGTH:
            UsageText := RsMoveLength;
          HID_USAGE_PID_PID_BLOCK_LOAD_REPORT:
            UsageText := RsPIDBlockLoadReport;
          HID_USAGE_PID_BLOCK_LOAD_STATUS:
            UsageText := RsBlockLoadStatus;
          HID_USAGE_PID_BLOCK_LOAD_SUCCESS:
            UsageText := RsLoadSuccess;
          HID_USAGE_PID_BLOCK_LOAD_FULL:
            UsageText := RsLoadFull;
          HID_USAGE_PID_BLOCK_LOAD_ERROR:
            UsageText := RsLoadError;
          HID_USAGE_PID_BLOCK_HANDLE:
            UsageText := RsBlockHandle;
          HID_USAGE_PID_PID_BLOCK_FREE_REPORT:
            UsageText := RsPIDBlockFreeReport;
          HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_HANDLE:
            UsageText := RsTypeSpecificBlockHandle;
          HID_USAGE_PID_PID_STATE_REPORT:
            UsageText := RsPIDStateReport;
          HID_USAGE_PID_EFFECT_PLAYING:
            UsageText := RsEffectPlaying;
          HID_USAGE_PID_PID_DEVICE_CONTROL_REPORT:
            UsageText := RsPIDDeviceControlReport;
          HID_USAGE_PID_PID_DEVICE_CONTROL:
            UsageText := RsPIDDeviceControl;
          HID_USAGE_PID_DC_ENABLE_ACTUATORS:
            UsageText := RsDeviceControlEnableActuators;
          HID_USAGE_PID_DC_DISABLE_ACTUATORS:
            UsageText := RsDeviceControlDisableActuators;
          HID_USAGE_PID_DC_STOP_ALL_EFFECTS:
            UsageText := RsDeviceControlStopAllEffects;
          HID_USAGE_PID_DC_DEVICE_RESET:
            UsageText := RsDeviceControlDeviceReset;
          HID_USAGE_PID_DC_DEVICE_PAUSE:
            UsageText := RsDeviceControlDevicePause;
          HID_USAGE_PID_DC_DEVICE_CONTINUE:
            UsageText := RsDeviceControlDeviceContinue;
          HID_USAGE_PID_DEVICE_PAUSED:
            UsageText := RsDevicePaused;
          HID_USAGE_PID_ACTUATORS_ENABLED:
            UsageText := RsActuatorsEnabled;
          HID_USAGE_PID_SAFETY_SWITCH:
            UsageText := RsSafetySwitch;
          HID_USAGE_PID_ACTUATOR_OVERRIDE_SWITCH:
            UsageText := RsActuatorOverrideSwitch;
          HID_USAGE_PID_ACTUATOR_POWER:
            UsageText := RsActuatorPower;
          HID_USAGE_PID_START_DELAY:
            UsageText := RsStartDelay;
          HID_USAGE_PID_PARAMETER_BLOCK_SIZE:
            UsageText := RsParameterBlockSize;
          HID_USAGE_PID_DEVICE_MANAGED_POOL:
            UsageText := RsDeviceManagedPool;
          HID_USAGE_PID_SHARED_PARAMETER_BLOCKS:
            UsageText := RsSharedParameterBlocks;
          HID_USAGE_PID_CREATE_NEW_EFFECT_REPORT:
            UsageText := RsCreateNewEffectReport;
          HID_USAGE_PID_RAM_POOL_AVAILABLE:
            UsageText := RsRAMPoolAvailable;
        end;
      end;
    HID_USAGE_PAGE_UNICODE:
      UsagePageText := RsUnicode;
    HID_USAGE_PAGE_ALPHANUMERIC:
      begin
        UsagePageText := RsAlphanumeric;
        case Usage of
          HID_USAGE_ALNUM_DISPLAY_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_ALNUM_DISPLAY_ALPHANUMERIC_DISPLAY:
            UsageText := RsAlphanumericDisplay;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_ATTRIBUTES_REPORT:
            UsageText := RsDisplayAttributesReport;
          HID_USAGE_ALNUM_DISPLAY_ASCII_CHARSET:
            UsageText := RsASCIICharacterSet;
          HID_USAGE_ALNUM_DISPLAY_DATA_READ_BACK:
            UsageText := RsDataReadBack;
          HID_USAGE_ALNUM_DISPLAY_FONT_READ_BACK:
            UsageText := RsFontReadBack;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTROL_REPORT:
            UsageText := RsDisplayControlReport;
          HID_USAGE_ALNUM_DISPLAY_CLEAR_DISPLAY:
            UsageText := RsClearDisplay;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_ENABLE:
            UsageText := RsDisplayEnable;
          HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_DELAY:
            UsageText := RsScreenSaverDelay;
          HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_ENABLE:
            UsageText := RsScreenSaverEnable;
          HID_USAGE_ALNUM_DISPLAY_VERTICAL_SCROLL:
            UsageText := RsVerticalScroll;
          HID_USAGE_ALNUM_DISPLAY_HORIZONTAL_SCROLL:
            UsageText := RsHorizontalScroll;
          HID_USAGE_ALNUM_DISPLAY_CHARACTER_REPORT:
            UsageText := RsCharacterReport;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_DATA:
            UsageText := RsDisplayData;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_STATUS:
            UsageText := RsDisplayStatus;
          HID_USAGE_ALNUM_DISPLAY_STAT_NOT_READY:
            UsageText := RsStatNotReady;
          HID_USAGE_ALNUM_DISPLAY_STAT_READY:
            UsageText := RsStatReady;
          HID_USAGE_ALNUM_DISPLAY_ERR_NOT_A_LOADABLE_CHAR:
            UsageText := RsErrorNotaLoadableChar;
          HID_USAGE_ALNUM_DISPLAY_ERR_FONT_DATA_CANNOT_BE_READ:
            UsageText := RsErrorFontDataCannotBeRead;
          HID_USAGE_ALNUM_DISPLAY_CURSOR_POSITION_REPORT:
            UsageText := RsCursorPositionReport;
          HID_USAGE_ALNUM_DISPLAY_ROW:
            UsageText := RsRow;
          HID_USAGE_ALNUM_DISPLAY_COLUMN:
            UsageText := RsColumn;
          HID_USAGE_ALNUM_DISPLAY_ROWS:
            UsageText := RsRows;
          HID_USAGE_ALNUM_DISPLAY_COLUMNS:
            UsageText := RsColumns;
          HID_USAGE_ALNUM_DISPLAY_CURSOR_PIXEL_POSITIONING:
            UsageText := RsCursorPixelPositioning;
          HID_USAGE_ALNUM_DISPLAY_CURSOR_MODE:
            UsageText := RsCursorMode;
          HID_USAGE_ALNUM_DISPLAY_CURSOR_ENABLE:
            UsageText := RsCursorEnable;
          HID_USAGE_ALNUM_DISPLAY_CURSOR_BLINK:
            UsageText := RsCursorBlink;
          HID_USAGE_ALNUM_DISPLAY_FONT_REPORT:
            UsageText := RsFontReport;
          HID_USAGE_ALNUM_DISPLAY_FONT_DATA:
            UsageText := RsFontData;
          HID_USAGE_ALNUM_DISPLAY_CHAR_WIDTH:
            UsageText := RsCharacterWidth;
          HID_USAGE_ALNUM_DISPLAY_CHAR_HEIGHT:
            UsageText := RsCharacterHeight;
          HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_HORIZONTAL:
            UsageText := RsCharacterSpacingHorizontal;
          HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_VERTICAL:
            UsageText := RsCharacterSpacingVertical;
          HID_USAGE_ALNUM_DISPLAY_UNICODE_CHARSET:
            UsageText := RsUnicodeCharacterSet;
          HID_USAGE_ALNUM_DISPLAY_FONT_7_SEGMENT:
            UsageText := RsFont7Segment;
          HID_USAGE_ALNUM_DISPLAY_7_SEGMENT_DIRECT_MAP:
            UsageText := Rs7SegmentDirectMap;
          HID_USAGE_ALNUM_DISPLAY_FONT_14_SEGMENT:
            UsageText := RsFont14Segment;
          HID_USAGE_ALNUM_DISPLAY_14_SEGMENT_DIRECT_MAP:
            UsageText := Rs14SegmentDirectMap;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_BRIGHTNESS:
            UsageText := RsDisplayBrightness;
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTRAST:
            UsageText := RsDisplayContrast;
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTRIBUTE:
            UsageText := RsCharacterAttribute;
          HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_READBACK:
            UsageText := RsAttributeReadBack;
          HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_DATA:
            UsageText := RsAttributeData;
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_ENHANCE:
            UsageText := RsCharacterAttributeEnhance;
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_UNDERLINE:
            UsageText := RsCharacterAttributeUnderline;
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_BLINK:
            UsageText := RsCharacterAttributeBlink;
        end;
      end;
    HID_USAGE_PAGE_MEDICAL_INSTRUMENT:
      begin
        UsagePageText := RsMedicalInstrument;
        case Usage of
          HID_USAGE_MEDICAL_INSTRUMENT_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_MEDICAL_INSTRUMENT_MEDICAL_ULTRASOUND:
            UsageText := RsMedicalUltrasound;
          HID_USAGE_MEDICAL_INSTRUMENT_VCR_AQUISITION:
            UsageText := RsVCRAquisition;
          HID_USAGE_MEDICAL_INSTRUMENT_FREEZE_THAW:
            UsageText := RsFreeze_Thaw;
          HID_USAGE_MEDICAL_INSTRUMENT_CLIP_STORE:
            UsageText := RsClipStore;
          HID_USAGE_MEDICAL_INSTRUMENT_UPDATE:
            UsageText := RsUpdate;
          HID_USAGE_MEDICAL_INSTRUMENT_NEXT:
            UsageText := RsNext;
          HID_USAGE_MEDICAL_INSTRUMENT_SAVE:
            UsageText := RsSave;
          HID_USAGE_MEDICAL_INSTRUMENT_PRINT:
            UsageText := RsPrint;
          HID_USAGE_MEDICAL_INSTRUMENT_MICROPHONE_ENABLE:
            UsageText := RsMicrophoneEnable;
          HID_USAGE_MEDICAL_INSTRUMENT_CINE:
            UsageText := RsCine;
          HID_USAGE_MEDICAL_INSTRUMENT_TRANSMIT_POWER:
            UsageText := RsTransmitPower;
          HID_USAGE_MEDICAL_INSTRUMENT_VOLUME:
            UsageText := RsVolume;
          HID_USAGE_MEDICAL_INSTRUMENT_FOCUS:
            UsageText := RsFocus;
          HID_USAGE_MEDICAL_INSTRUMENT_DEPTH:
            UsageText := RsDepth;
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_PRIMARY:
            UsageText := RsSoftStepPrimary;
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_SECONDARY:
            UsageText := RsSoftStepSecondary;
          HID_USAGE_MEDICAL_INSTRUMENT_DEPTH_GAIN_COMPENSATION:
            UsageText := RsDepthGainCompensation;
          HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_SELECT:
            UsageText := RsZoomSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_ADJUST:
            UsageText := RsZoomAdjust;
          HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_MODE_SELECT:
            UsageText := RsSpectralDopplerModeSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_ADJUST:
            UsageText := RsSpectralDopplerAdjust;
          HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_MODE_SELECT:
            UsageText := RsColorDopplerModeSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_ADJUST:
            UsageText := RsColorDopplerAdjust;
          HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_SELECT:
            UsageText := RsMotionModeSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_ADJUST:
            UsageText := RsMotionModeAdjust;
          HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_SELECT:
            UsageText := Rs2DModeSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_ADJUST:
            UsageText := Rs2DModeAdjust;
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_SELECT:
            UsageText := RsSoftControlSelect;
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_ADJUST:
            UsageText := RsSoftControlAdjust;
        end;
      end;
    HID_USAGE_PAGE_USB_MONITOR:
      begin
        UsagePageText := RsUSBMonitor;
        case Usage of
          HID_USAGE_MONITOR_RESERVED:
            UsageText := RsReserved;
          HID_USAGE_MONITOR_MONITOR_CONTROL:
            UsageText := RsMonitorControl;
          HID_USAGE_MONITOR_EDID_INFORMATION:
            UsageText := RsEDIDInformation;
          HID_USAGE_MONITOR_VDIF_INFORMATION:
            UsageText := RsVDIFInformation;
          HID_USAGE_MONITOR_VESA_VERSION:
            UsageText := RsVESAVersion;
        end;
      end;
    HID_USAGE_PAGE_MONITOR_ENUMERATED_VALUES:
      begin
        UsagePageText := RsUSBEnumeratedValues;
        case Usage of
          HID_USAGE_MONITOR_ENUM_VALUE_NO_VALUE:
            UsageText := RsNoEnumeratedValueSelected;
        end;
      end;
    HID_USAGE_PAGE_VESA_VIRTUAL_CONTROLS:
      UsagePageText := RsVESAVirtualControls;
    HID_USAGE_PAGE_RESERVED:
      UsagePageText := RsReserved;
    HID_USAGE_PAGE_POWER_DEVICE:
      begin
        UsagePageText := RsPowerDevice;
        case Usage of
          HID_USAGE_POWER_DEVICE_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_POWER_DEVICE_INAME:
            UsageText := RsiName;
          HID_USAGE_POWER_DEVICE_PRESENT_STATUS:
            UsageText := RsPresentStatus;
          HID_USAGE_POWER_DEVICE_CHANGED_STATUS:
            UsageText := RsChangedStatus;
          HID_USAGE_POWER_DEVICE_UPS:
            UsageText := RsUPS;
          HID_USAGE_POWER_DEVICE_POWER_SUPPLY:
            UsageText := RsPowerSupply;
          HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM:
            UsageText := RsBatterySystem;
          HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM_ID:
            UsageText := RsBatterySystemID;
          HID_USAGE_POWER_DEVICE_BATTERY:
            UsageText := RsBattery;
          HID_USAGE_POWER_DEVICE_BATTERY_ID:
            UsageText := RsBatteryID;
          HID_USAGE_POWER_DEVICE_CHARGER:
            UsageText := RsCharger;
          HID_USAGE_POWER_DEVICE_CHARGER_ID:
            UsageText := RsChargerID;
          HID_USAGE_POWER_DEVICE_POWER_CONVERTER:
            UsageText := RsPowerConverter;
          HID_USAGE_POWER_DEVICE_POWER_CONVERTER_ID:
            UsageText := RsPowerConverterID;
          HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM:
            UsageText := RsOutletSystem;
          HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM_ID:
            UsageText := RsOutletSystemID;
          HID_USAGE_POWER_DEVICE_INPUT:
            UsageText := RsInput;
          HID_USAGE_POWER_DEVICE_INPUT_ID:
            UsageText := RsInputID;
          HID_USAGE_POWER_DEVICE_OUTPUT:
            UsageText := RsOutput;
          HID_USAGE_POWER_DEVICE_OUTPUT_ID:
            UsageText := RsOutputID;
          HID_USAGE_POWER_DEVICE_FLOW:
            UsageText := RsFlow;
          HID_USAGE_POWER_DEVICE_FLOW_ID:
            UsageText := RsFlowID;
          HID_USAGE_POWER_DEVICE_OUTLET:
            UsageText := RsOutlet;
          HID_USAGE_POWER_DEVICE_OUTLET_ID:
            UsageText := RsOutletID;
          HID_USAGE_POWER_DEVICE_GANG:
            UsageText := RsGang;
          HID_USAGE_POWER_DEVICE_GANG_ID:
            UsageText := RsGangID;
          HID_USAGE_POWER_DEVICE_POWER_SUMMARY:
            UsageText := RsPowerSummary;
          HID_USAGE_POWER_DEVICE_POWER_SUMMARY_ID:
            UsageText := RsPowerSummaryID;
          HID_USAGE_POWER_DEVICE_VOLTAGE:
            UsageText := RsVoltage;
          HID_USAGE_POWER_DEVICE_CURRENT:
            UsageText := RsCurrent;
          HID_USAGE_POWER_DEVICE_FREQUENCY:
            UsageText := RsFrequency;
          HID_USAGE_POWER_DEVICE_APPARENT_POWER:
            UsageText := RsApparentPower;
          HID_USAGE_POWER_DEVICE_ACTIVE_POWER:
            UsageText := RsActivePower;
          HID_USAGE_POWER_DEVICE_PERCENT_LOAD:
            UsageText := RsPercentLoad;
          HID_USAGE_POWER_DEVICE_TEMPERATURE:
            UsageText := RsTemperature;
          HID_USAGE_POWER_DEVICE_HUMIDITY:
            UsageText := RsHumidity;
          HID_USAGE_POWER_DEVICE_BAD_COUNT:
            UsageText := RsBadCount;
          HID_USAGE_POWER_DEVICE_CONFIG_VOLTAGE:
            UsageText := RsConfigVoltage;
          HID_USAGE_POWER_DEVICE_CONFIG_CURRENT:
            UsageText := RsConfigCurrent;
          HID_USAGE_POWER_DEVICE_CONFIG_FREQUENCY:
            UsageText := RsConfigFrequency;
          HID_USAGE_POWER_DEVICE_CONFIG_APPARENT_POWER:
            UsageText := RsConfigApparentPower;
          HID_USAGE_POWER_DEVICE_CONFIG_ACTIVE_POWER:
            UsageText := RsConfigActivePower;
          HID_USAGE_POWER_DEVICE_CONFIG_PERCENT_LOAD:
            UsageText := RsConfigPercentLoad;
          HID_USAGE_POWER_DEVICE_CONFIG_TEMPERATURE:
            UsageText := RsConfigTemperature;
          HID_USAGE_POWER_DEVICE_CONFIG_HUMIDITY:
            UsageText := RsConfigHumidity;
          HID_USAGE_POWER_DEVICE_SWITCH_ON_CONTROL:
            UsageText := RsSwitchOnControl;
          HID_USAGE_POWER_DEVICE_SWITCH_OFF_CONTROL:
            UsageText := RsSwitchOffControl;
          HID_USAGE_POWER_DEVICE_TOGGLE_CONTROL:
            UsageText := RsToggleControl;
          HID_USAGE_POWER_DEVICE_LOW_VOLTAGE_TRANSFER:
            UsageText := RsLowVoltageTransfer;
          HID_USAGE_POWER_DEVICE_HIGH_VOLTAGE_TRANSFER:
            UsageText := RsHighVoltageTransfer;
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_REBOOT:
            UsageText := RsDelayBeforeReboot;
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_STARTUP:
            UsageText := RsDelayBeforeStartup;
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_SHUTDOWN:
            UsageText := RsDelayBeforeShutdown;
          HID_USAGE_POWER_DEVICE_TEST:
            UsageText := RsTest;
          HID_USAGE_POWER_DEVICE_MODULE_RESET:
            UsageText := RsModuleReset;
          HID_USAGE_POWER_DEVICE_AUDIBLE_ALARM_CONTROL:
            UsageText := RsAudibleAlarmControl;
          HID_USAGE_POWER_DEVICE_PRESENT:
            UsageText := RsPresent;
          HID_USAGE_POWER_DEVICE_GOOD:
            UsageText := RsGood;
          HID_USAGE_POWER_DEVICE_INTERNAL_FAILURE:
            UsageText := RsInternalFailure;
          HID_USAGE_POWER_DEVICE_VOLTAGE_OUT_OF_RANGE:
            UsageText := RsVoltageOutofRange;
          HID_USAGE_POWER_DEVICE_FREQUENCY_OUT_OF_RANGE:
            UsageText := RsFrequencyOutofRange;
          HID_USAGE_POWER_DEVICE_OVERLOAD:
            UsageText := RsOverload;
          HID_USAGE_POWER_DEVICE_OVERCHARGED:
            UsageText := RsOvercharged;
          HID_USAGE_POWER_DEVICE_OVERTEMPERATURE:
            UsageText := RsOvertemperature;
          HID_USAGE_POWER_DEVICE_SHUTDOWN_REQUESTED:
            UsageText := RsShutdownRequested;
          HID_USAGE_POWER_DEVICE_SHUTDOWN_IMMINENT:
            UsageText := RsShutdownImminent;
          HID_USAGE_POWER_DEVICE_SWITCH_ON_OFF:
            UsageText := RsSwitchOn_Off;
          HID_USAGE_POWER_DEVICE_SWITCHABLE:
            UsageText := RsSwitchable;
          HID_USAGE_POWER_DEVICE_USED:
            UsageText := RsUsed;
          HID_USAGE_POWER_DEVICE_BOOST:
            UsageText := RsBoost;
          HID_USAGE_POWER_DEVICE_BUCK:
            UsageText := RsBuck;
          HID_USAGE_POWER_DEVICE_INITIALIZED:
            UsageText := RsInitialized;
          HID_USAGE_POWER_DEVICE_TESTED:
            UsageText := RsTested;
          HID_USAGE_POWER_DEVICE_AWAITING_POWER:
            UsageText := RsAwaitingPower;
          HID_USAGE_POWER_DEVICE_COMMUNICATION_LOST:
            UsageText := RsCommunicationLost;
          HID_USAGE_POWER_DEVICE_IMANUFACTURER:
            UsageText := RsiManufacturer;
          HID_USAGE_POWER_DEVICE_IPRODUCT:
            UsageText := RsiProduct;
          HID_USAGE_POWER_DEVICE_ISERIALNUMBER:
            UsageText := RsiSerialNumber;
        end;
      end;
    HID_USAGE_PAGE_BATTERY_SYSTEM:
      begin
        UsagePageText := RsBatterySystem;
        case Usage of
          HID_USAGE_BATTERY_SYSTEM_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_MODE:
            UsageText := RsSMBBatteryMode;
          HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_STATUS:
            UsageText := RsSMBBatteryStatus;
          HID_USAGE_BATTERY_SYSTEM_SMB_ALARM_WARNING:
            UsageText := RsSMBAlarmWarning;
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_MODE:
            UsageText := RsSMBChargerMode;
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_STATUS:
            UsageText := RsSMBChargerStatus;
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_SPEC_INFO:
            UsageText := RsSMBChargerSpecInfo;
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_STATE:
            UsageText := RsSMBSelectorstate;
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_PRESETS:
            UsageText := RsSMBSelectorPresets;
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_INFO:
            UsageText := RsSMBSelectorInfo;
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_1:
            UsageText := RsOptionalManufacturerFunction1;
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_2:
            UsageText := RsOptionalManufacturerFunction2;
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_3:
            UsageText := RsOptionalManufacturerFunction3;
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_4:
            UsageText := RsOptionalManufacturerFunction4;
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_5:
            UsageText := RsOptionalManufacturerFunction5;
          HID_USAGE_BATTERY_SYSTEM_CONNECTION_TO_SMBUS:
            UsageText := RsConnectiontoSMBus;
          HID_USAGE_BATTERY_SYSTEM_OUTPUT_CONNECTION:
            UsageText := RsOutputConnection;
          HID_USAGE_BATTERY_SYSTEM_CHARGER_CONNECTION:
            UsageText := RsChargerConnection;
          HID_USAGE_BATTERY_SYSTEM_BATTERY_INSERTION:
            UsageText := RsBatteryInsertion;
          HID_USAGE_BATTERY_SYSTEM_USE_NEXT:
            UsageText := RsUseNext;
          HID_USAGE_BATTERY_SYSTEM_OK_TO_USE:
            UsageText := RsOKtoUse;
          HID_USAGE_BATTERY_SYSTEM_BATTERY_SUPPORTED:
            UsageText := RsBatterySupported;
          HID_USAGE_BATTERY_SYSTEM_SELECTOR_REVISION:
            UsageText := RsSelectorRevision;
          HID_USAGE_BATTERY_SYSTEM_CHARGING_INDICATOR:
            UsageText := RsChargingIndicator;
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_ACCESS:
            UsageText := RsManufacturerAccess;
          HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY_LIMIT:
            UsageText := RsRemainingCapacityLimit;
          HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT:
            UsageText := RsRemainingTimeLimit;
          HID_USAGE_BATTERY_SYSTEM_AT_RATE:
            UsageText := RsAtRate;
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_MODE:
            UsageText := RsCapacityMode;
          HID_USAGE_BATTERY_SYSTEM_BROADCAST_TO_CHARGER:
            UsageText := RsBroadcasttoCharger;
          HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY:
            UsageText := RsPrimaryBattery;
          HID_USAGE_BATTERY_SYSTEM_CHARGE_CONTROLLER:
            UsageText := RsChargeController;
          HID_USAGE_BATTERY_SYSTEM_TERMINATE_CHARGE:
            UsageText := RsTerminateCharge;
          HID_USAGE_BATTERY_SYSTEM_TERMINATE_DISCHARGE:
            UsageText := RsTerminateDischarge;
          HID_USAGE_BATTERY_SYSTEM_BELOW_REMAINING_CAPACITY_LIMIT:
            UsageText := RsBelowRemainingCapacity;
          HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT_EXPIRED:
            UsageText := RsRemainingTimeLimitExpired;
          HID_USAGE_BATTERY_SYSTEM_CHARGING:
            UsageText := RsCharging;
          HID_USAGE_BATTERY_SYSTEM_DISCHARGING:
            UsageText := RsDischarging;
          HID_USAGE_BATTERY_SYSTEM_FULLY_CHARGED:
            UsageText := RsFullyCharged;
          HID_USAGE_BATTERY_SYSTEM_FULLY_DISCHARGED:
            UsageText := RsFullyDischarged;
          HID_USAGE_BATTERY_SYSTEM_CONDITIONING_FLAG:
            UsageText := RsConditioningFlag;
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_OK:
            UsageText := RsAtRateOK;
          HID_USAGE_BATTERY_SYSTEM_SMB_ERROR_CODE:
            UsageText := RsSMBErrorCode;
          HID_USAGE_BATTERY_SYSTEM_NEED_REPLACEMENT:
            UsageText := RsNeedReplacement;
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_FULL:
            UsageText := RsAtRateTimetoFull;
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_EMPTY:
            UsageText := RsAtRateTimetoEmpty;
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_CURRENT:
            UsageText := RsAverageCurrent;
          HID_USAGE_BATTERY_SYSTEM_MAX_ERROR:
            UsageText := RsMaxError;
          HID_USAGE_BATTERY_SYSTEM_RELATIVE_STATE_OF_CHARGE:
            UsageText := RsRelativeStateofCharge;
          HID_USAGE_BATTERY_SYSTEM_ABSOLUTE_STATE_OF_CHARGE:
            UsageText := RsabsoluteStateofcharge;
          HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY:
            UsageText := RsRemainingCapacity;
          HID_USAGE_BATTERY_SYSTEM_FULL_CHARGE_CAPACITY:
            UsageText := RsFullChargeCapacity;
          HID_USAGE_BATTERY_SYSTEM_RUN_TIME_TO_EMPTY:
            UsageText := RsRunTimetoEmpty;
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_EMPTY:
            UsageText := RsAverageTimetoEmpty;
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_FULL:
            UsageText := RsAverageTimetoFull;
          HID_USAGE_BATTERY_SYSTEM_CYCLE_COUNT:
            UsageText := RsCycleCount;
          HID_USAGE_BATTERY_SYSTEM_BATT_PACK_MODEL_LEVEL:
            UsageText := RsBatteryPackModelLevel;
          HID_USAGE_BATTERY_SYSTEM_INTERNAL_CHARGE_CONTROLLER:
            UsageText := RsInternalChargeController;
          HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY_SUPPORT:
            UsageText := RsPrimaryBatterySupport;
          HID_USAGE_BATTERY_SYSTEM_DESIGN_CAPACITY:
            UsageText := RsDesigncapacity;
          HID_USAGE_BATTERY_SYSTEM_SPECIFICATION_INFO:
            UsageText := RsSpecificationInfo;
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATE:
            UsageText := RsManufacturerDate;
          HID_USAGE_BATTERY_SYSTEM_SERIAL_NUMBER:
            UsageText := RsSerialNumber;
          HID_USAGE_BATTERY_SYSTEM_I_MANUFACTURER_NAME:
            UsageText := RsiManufacturerName;
          HID_USAGE_BATTERY_SYSTEM_I_DEVICE_NAME:
            UsageText := RsiDeviceName;
          HID_USAGE_BATTERY_SYSTEM_I_DEVICE_CHEMISTERY:
            UsageText := RsiDeviceChemistery;
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATA:
            UsageText := RsManufacturerData;
          HID_USAGE_BATTERY_SYSTEM_RECHARGABLE:
            UsageText := RsRechargable;
          HID_USAGE_BATTERY_SYSTEM_WARNING_CAPACITY_LIMIT:
            UsageText := RsWarningCapacityLimit;
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_1:
            UsageText := RsCapacityGranularity1;
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_2:
            UsageText := RsCapacityGranularity2;
          HID_USAGE_BATTERY_SYSTEM_I_OEM_INFORMATION:
            UsageText := RsiOEMInformation;
          HID_USAGE_BATTERY_SYSTEM_INHIBIT_CHARGE:
            UsageText := RsInhibitCharge;
          HID_USAGE_BATTERY_SYSTEM_ENABLE_POLLING:
            UsageText := RsEnablePolling;
          HID_USAGE_BATTERY_SYSTEM_RESET_TO_ZERO:
            UsageText := RsResettozero;
          HID_USAGE_BATTERY_SYSTEM_AC_PRESENT:
            UsageText := RsACPresent;
          HID_USAGE_BATTERY_SYSTEM_BATTERY_PRESENT:
            UsageText := RsBatteryPresent;
          HID_USAGE_BATTERY_SYSTEM_POWER_FAIL:
            UsageText := RsPowerFail;
          HID_USAGE_BATTERY_SYSTEM_ALARM_INHIBITED:
            UsageText := RsAlarmInhibited;
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_UNDER_RANGE:
            UsageText := RsThermistorUnderrange;
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_HOT:
            UsageText := RsThermistorHot;
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_COLD:
            UsageText := RsThermistorCold;
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_OVER_RANGE:
            UsageText := RsThermistorOverRange;
          HID_USAGE_BATTERY_SYSTEM_VOLTAGE_OUT_OF_RANGE:
            UsageText := RsVoltageoutofRange;
          HID_USAGE_BATTERY_SYSTEM_CURRENT_OUT_OF_RANGE:
            UsageText := RsCurrentOutofRange;
          HID_USAGE_BATTERY_SYSTEM_CURRENT_NOT_REGULATED:
            UsageText := RsCurrentNotRegulated;
          HID_USAGE_BATTERY_SYSTEM_VOLTAGE_NOT_REGULATED:
            UsageText := RsVoltageNotRegulated;
          HID_USAGE_BATTERY_SYSTEM_MASTER_MODE:
            UsageText := RsMasterMode;
          HID_USAGE_BATTERY_SYSTEM_CHARGER_SELECTOR_SUPPORT:
            UsageText := RsChargerSelectorSupport;
          HID_USAGE_BATTERY_SYSTEM_CHARGER_SPEC:
            UsageText := RsChargerSpec;
          HID_USAGE_BATTERY_SYSTEM_LEVEL_2:
            UsageText := RsLevel2;
          HID_USAGE_BATTERY_SYSTEM_LEVEL_3:
            UsageText := RsLevel3;
        end;
      end;
    HID_USAGE_PAGE_BARCODE_SCANNER:
      begin
        UsagePageText := RsBarcodeScanner;
        case Usage of
          HID_USAGE_BARCODE_SCANNER_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_BADGE_READER:
            UsageText := RsBarCodeBadgeReader;
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER:
            UsageText := RsBarCodeScanner;
          HID_USAGE_BARCODE_SCANNER_DUMB_BAR_CODE_SCANNER:
            UsageText := RsDumbBarCodeScanner;
          HID_USAGE_BARCODE_SCANNER_CORDLESS_SCANNER_BASE:
            UsageText := RsCordlessScannerBase;
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER_CRADLE:
            UsageText := RsBarCodeScannerCradle;
          HID_USAGE_BARCODE_SCANNER_ATTRIBUTE_REPORT:
            UsageText := RsAttributeReport;
          HID_USAGE_BARCODE_SCANNER_SETTINGS_REPORT:
            UsageText := RsSettingsReport;
          HID_USAGE_BARCODE_SCANNER_SCANNED_DATA_REPORT:
            UsageText := RsScannedDataReport;
          HID_USAGE_BARCODE_SCANNER_RAW_SCANNED_DATA_REPORT:
            UsageText := RsRawScannedDataReport;
          HID_USAGE_BARCODE_SCANNER_TRIGGER_REPORT:
            UsageText := RsTriggerReport;
          HID_USAGE_BARCODE_SCANNER_STATUS_REPORT:
            UsageText := RsStatusReport;
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_CONTROL_REPORT:
            UsageText := RsUPC_EANControlReport;
          HID_USAGE_BARCODE_SCANNER_EAN_2_3_LABEL_CONTROL_REPORT:
            UsageText := RsEAN2_3LabelControlReport;
          HID_USAGE_BARCODE_SCANNER_CODE_39_CONTROL_REPORT:
            UsageText := RsCode39ControlReport;
          HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5_CONTROL_REPORT:
            UsageText := RsInterleaved2of5ControlReport;
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_CONTROL_REPORT:
            UsageText := RsStandard2of5ControlReport;
          HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY_CONTROL_REPORT:
            UsageText := RsMSIPlesseyControlReport;
          HID_USAGE_BARCODE_SCANNER_CODABAR_CONTROL_REPORT:
            UsageText := RsCodabarControlReport;
          HID_USAGE_BARCODE_SCANNER_CODE_128_CONTROL_REPORT:
            UsageText := RsCode128ControlReport;
          HID_USAGE_BARCODE_SCANNER_MISC_1D_CONTROL_REPORT:
            UsageText := RsMisc1DControlReport;
          HID_USAGE_BARCODE_SCANNER_2D_CONTROL_REPORT:
            UsageText := Rs2DControlReport;
          HID_USAGE_BARCODE_SCANNER_AIMING_POINTER_MODE:
            UsageText := RsAiming_PointerMode;
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT_SENSOR:
            UsageText := RsBarCodePresentSensor;
          HID_USAGE_BARCODE_SCANNER_CLASS_1A_LASER:
            UsageText := RsClass1ALaser;
          HID_USAGE_BARCODE_SCANNER_CLASS_2_LASER:
            UsageText := RsClass2Laser;
          HID_USAGE_BARCODE_SCANNER_HEATER_PRESENT:
            UsageText := RsHeaterPresent;
          HID_USAGE_BARCODE_SCANNER_CONTACT_SCANNER:
            UsageText := RsContactScanner;
          HID_USAGE_BARCODE_SCANNER_ELECTRONIC_ARTICLE_SURVEILLANCE_NOTIFICATION:
            UsageText := RsElectronicArticleSurveillanceNotification;
          HID_USAGE_BARCODE_SCANNER_CONSTANT_ARTICLE_SURVEILLANCE_NOTIFICATION:
            UsageText := RsConstantArticleSurveillanceNotification;
          HID_USAGE_BARCODE_SCANNER_ERROR_INDICATION:
            UsageText := RsErrorIndication;
          HID_USAGE_BARCODE_SCANNER_FIXED_BEEPER:
            UsageText := RsFixedBeeper;
          HID_USAGE_BARCODE_SCANNER_GOOD_DECODE_INDICATION:
            UsageText := RsGoodDecodeIndication;
          HID_USAGE_BARCODE_SCANNER_HANDS_FREE_SCANNING:
            UsageText := RsHandsFreeScanning;
          HID_USAGE_BARCODE_SCANNER_INTRINSICALLY_SAFE:
            UsageText := RsIntrinsicallySafe;
          HID_USAGE_BARCODE_SCANNER_KLASSE_EINS_LASER:
            UsageText := RsKlasseEinsLaserClass1Laser;
          HID_USAGE_BARCODE_SCANNER_LONG_RANGE_SCANNER:
            UsageText := RsLongRangeScanner;
          HID_USAGE_BARCODE_SCANNER_MIRROR_SPEED_CONTROL:
            UsageText := RsMirrorSpeedControl;
          HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_INDICATION:
            UsageText := RsNotOnFileIndication;
          HID_USAGE_BARCODE_SCANNER_PROGRAMMABLE_BEEPER:
            UsageText := RsProgrammableBeeper;
          HID_USAGE_BARCODE_SCANNER_TRIGGERLESS:
            UsageText := RsTriggerless;
          HID_USAGE_BARCODE_SCANNER_WAND:
            UsageText := RsWand;
          HID_USAGE_BARCODE_SCANNER_WATER_RESISTANT:
            UsageText := RsWaterResistant;
          HID_USAGE_BARCODE_SCANNER_MULTI_RANGE_SCANNER:
            UsageText := RsMulti_RangeScanner;
          HID_USAGE_BARCODE_SCANNER_PROXIMITIY_SENSOR:
            UsageText := RsProximitySensor;
          HID_USAGE_BARCODE_SCANNER_FRAGMENT_DECODING:
            UsageText := RsFragmentDecoding;
          HID_USAGE_BARCODE_SCANNER_SCANNER_READ_CONFIDENCE:
            UsageText := RsScannerReadConfidence;
          HID_USAGE_BARCODE_SCANNER_DATA_PREFIX:
            UsageText := RsDataPrefix;
          HID_USAGE_BARCODE_SCANNER_PREFIX_AIMI:
            UsageText := RsPrefixAIMI;
          HID_USAGE_BARCODE_SCANNER_PREFIX_NODE:
            UsageText := RsPrefixNode;
          HID_USAGE_BARCODE_SCANNER_PREFIX_PROPRIETARY:
            UsageText := RsPrefixProprietary;
          HID_USAGE_BARCODE_SCANNER_ACTIVE_TIME:
            UsageText := RsActiveTime;
          HID_USAGE_BARCODE_SCANNER_AIMING_LASER_PATTERN:
            UsageText := RsAimingLaserPattern;
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT:
            UsageText := RsBarCodePresent;
          HID_USAGE_BARCODE_SCANNER_BEEPER_STATE:
            UsageText := RsBeeperState;
          HID_USAGE_BARCODE_SCANNER_LASER_ON_TIME:
            UsageText := RsLaserOnTime;
          HID_USAGE_BARCODE_SCANNER_LASER_STATE:
            UsageText := RsLaserState;
          HID_USAGE_BARCODE_SCANNER_LOCKOUT_TIME:
            UsageText := RsLockoutTime;
          HID_USAGE_BARCODE_SCANNER_MOTOR_STATE:
            UsageText := RsMotorState;
          HID_USAGE_BARCODE_SCANNER_MOTOR_TIMEOUT:
            UsageText := RsMotorTimeout;
          HID_USAGE_BARCODE_SCANNER_POWER_ON_RESET_SCANNER:
            UsageText := RsPowerOnResetScanner;
          HID_USAGE_BARCODE_SCANNER_PREVENT_READ_OF_BARCODES:
            UsageText := RsPreventReadofBarcodes;
          HID_USAGE_BARCODE_SCANNER_INITIATE_BARCODE_READ:
            UsageText := RsInitiateBarcodeRead;
          HID_USAGE_BARCODE_SCANNER_TRIGGER_STATE:
            UsageText := RsTriggerState;
          HID_USAGE_BARCODE_SCANNER_TRIGGER_MODE:
            UsageText := RsTriggerMode;
          HID_USAGE_BARCODE_SCANNER_TM_BLINKING_LASER_ON:
            UsageText := RsTriggerModeBlinkingLaserOn;
          HID_USAGE_BARCODE_SCANNER_TM_CONTINUOUS_LASER_ON:
            UsageText := RsTriggerModeContinuousLaserOn;
          HID_USAGE_BARCODE_SCANNER_TM_LASER_ON_WHILE_PULLED:
            UsageText := RsTriggerModeLaserOnWhilePulled;
          HID_USAGE_BARCODE_SCANNER_TM_LASER_STAYS_ON_AFTER_TRIGGER_RELEASE:
            UsageText := RsTriggerModeLaserStaysOnAfterTriggerRelease;
          HID_USAGE_BARCODE_SCANNER_COMMIT_PARAMETERS_TO_NVM:
            UsageText := RsCommitParameterstoNVM;
          HID_USAGE_BARCODE_SCANNER_PARAMETER_SCANNING:
            UsageText := RsParameterScanning;
          HID_USAGE_BARCODE_SCANNER_PARAMETERS_CHANGED:
            UsageText := RsParametersChanged;
          HID_USAGE_BARCODE_SCANNER_SET_PARAMETER_DEFAULT_VALUES:
            UsageText := RsSetParameterDefaultValues;
          HID_USAGE_BARCODE_SCANNER_SCANNER_IN_CRADLE:
            UsageText := RsScannerinCradle;
          HID_USAGE_BARCODE_SCANNER_SCANNER_IN_RANGE:
            UsageText := RsScannerinRange;
          HID_USAGE_BARCODE_SCANNER_AIM_DURATION:
            UsageText := RsAimDuration;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_DURATION:
            UsageText := RsGoodReadLampDuration;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_INTENSITY:
            UsageText := RsGoodReadLampIntensity;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LED:
            UsageText := RsGoodReadLED;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_FREQUENCY:
            UsageText := RsGoodReadToneFrequency;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_LENGTH:
            UsageText := RsGoodReadToneLength;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_VOLUME:
            UsageText := RsGoodReadToneVolume;
          HID_USAGE_BARCODE_SCANNER_NO_READ_MESSAGE:
            UsageText := RsNoReadMessage;
          HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_VOLUME:
            UsageText := RsNotOnFileVolume;
          HID_USAGE_BARCODE_SCANNER_POWERUP_BEEP:
            UsageText := RsPowerupBeep;
          HID_USAGE_BARCODE_SCANNER_SOUND_ERROR_BEEP:
            UsageText := RsSoundErrorBeep;
          HID_USAGE_BARCODE_SCANNER_SOUND_GOOD_READ_BEEP:
            UsageText := RsSoundGoodReadBeep;
          HID_USAGE_BARCODE_SCANNER_SOUND_NOT_ON_FILE_BEEP:
            UsageText := RsSoundNotOnFileBeep;
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_WHEN_TO_WRITE:
            UsageText := RsGoodReadWhentoWrite;
          HID_USAGE_BARCODE_SCANNER_GRWTI_AFTER_DECODE:
            UsageText := RsGRWTIAfterDecode;
          HID_USAGE_BARCODE_SCANNER_GRWTI_BEEP_LAMP_AFTER_TRANSMIT:
            UsageText := RsGRWTIBeep_LampAfterTransmit;
          HID_USAGE_BARCODE_SCANNER_GRWTI_NO_BEEP_LAMP_USE_AT_ALL:
            UsageText := RsGRWTINoBeep_LampUseatAll;
          HID_USAGE_BARCODE_SCANNER_BOOKLAND_EAN:
            UsageText := RsBooklandEAN;
          HID_USAGE_BARCODE_SCANNER_CONVERT_EAN_8_TO_13_TYPE:
            UsageText := RsConvertEAN8to13Type;
          HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_A_TO_EAN_13:
            UsageText := RsConvertUPCAtoEAN_13;
          HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_E_TO_A:
            UsageText := RsConvertUPC_EtoA;
          HID_USAGE_BARCODE_SCANNER_EAN_13:
            UsageText := RsEAN_13;
          HID_USAGE_BARCODE_SCANNER_EAN_8:
            UsageText := RsEAN_8;
          HID_USAGE_BARCODE_SCANNER_EAN_99_128_MANDATORY:
            UsageText := RsEAN_99128Mandatory;
          HID_USAGE_BARCODE_SCANNER_EAN_99_P5_128_OPTIONAL:
            UsageText := RsEAN_99P5_128Optional;
          HID_USAGE_BARCODE_SCANNER_UPC_EAN:
            UsageText := RsUPC_EAN;
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_COUPON_CODE:
            UsageText := RsUPC_EANCouponCode;
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_PERIODICALS:
            UsageText := RsUPC_EANPeriodicals;
          HID_USAGE_BARCODE_SCANNER_UPC_A:
            UsageText := RsUPC_A;
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_MANDATORY:
            UsageText := RsUPC_Awith128Mandatory;
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_OPTIONAL:
            UsageText := RsUPC_Awith128Optional;
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_P5_OPTIONAL:
            UsageText := RsUPC_AwithP5Optional;
          HID_USAGE_BARCODE_SCANNER_UPC_E:
            UsageText := RsUPC_E;
          HID_USAGE_BARCODE_SCANNER_UPC_E1:
            UsageText := RsUPC_E1;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL:
            UsageText := RsPeriodical;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_2:
            UsageText := RsPeriodicalAuto_DiscriminatePlus2;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_2:
            UsageText := RsPeriodicalOnlyDecodewithPlus2;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_2:
            UsageText := RsPeriodicalIgnorePlus2;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_5:
            UsageText := RsPeriodicalAuto_DiscriminatePlus5;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_5:
            UsageText := RsPeriodicalOnlyDecodewithPlus5;
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_5:
            UsageText := RsPeriodicalIgnorePlus5;
          HID_USAGE_BARCODE_SCANNER_CHECK:
            UsageText := RsCheck;
          HID_USAGE_BARCODE_SCANNER_CHECK_DISABLE_PRICE:
            UsageText := RsCheckDisablePrice;
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_4_DIGIT_PRICE:
            UsageText := RsCheckEnable4DigitPrice;
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_5_DIGIT_PRICE:
            UsageText := RsCheckEnable5DigitPrice;
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_4_DIGIT_PRICE:
            UsageText := RsCheckEnableEuropean4DigitPrice;
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_5_DIGIT_PRICE:
            UsageText := RsCheckEnableEuropean5DigitPrice;
          HID_USAGE_BARCODE_SCANNER_EAN_TWO_LABEL:
            UsageText := RsEANTwoLabel;
          HID_USAGE_BARCODE_SCANNER_EAN_THREE_LABEL:
            UsageText := RsEANThreeLabel;
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_1:
            UsageText := RsEAN8FlagDigit1;
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_2:
            UsageText := RsEAN8FlagDigit2;
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_3:
            UsageText := RsEAN8FlagDigit3;
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_1:
            UsageText := RsEAN13FlagDigit1;
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_2:
            UsageText := RsEAN13FlagDigit2;
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_3:
            UsageText := RsEAN13FlagDigit3;
          HID_USAGE_BARCODE_SCANNER_ADD_EAN_2_3_LABEL_DEFINITION:
            UsageText := RsAddEAN2_3LabelDefinition;
          HID_USAGE_BARCODE_SCANNER_CLEAR_ALL_EAN_2_3_LABEL_DEFINITIONS:
            UsageText := RsClearAllEAN2_3LabelDefinitions;
          HID_USAGE_BARCODE_SCANNER_CODABAR:
            UsageText := RsCodabar;
          HID_USAGE_BARCODE_SCANNER_CODE_128:
            UsageText := RsCode128;
          HID_USAGE_BARCODE_SCANNER_CODE_39:
            UsageText := RsCode39;
          HID_USAGE_BARCODE_SCANNER_CODE_93:
            UsageText := RsCode93;
          HID_USAGE_BARCODE_SCANNER_FULL_ASCII_CONVERSION:
            UsageText := RsFullASCIIConversion;
          HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5:
            UsageText := RsInterleaved2of5;
          HID_USAGE_BARCODE_SCANNER_ITALIAN_PHARMACY_CODE:
            UsageText := RsItalianPharmacyCode;
          HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY:
            UsageText := RsMSI_Plessey;
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_IATA:
            UsageText := RsStandard2of5IATA;
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5:
            UsageText := RsStandard2of5;
          HID_USAGE_BARCODE_SCANNER_TRANSMIT_START_STOP:
            UsageText := RsTransmitStart_Stop;
          HID_USAGE_BARCODE_SCANNER_TRI_OPTIC:
            UsageText := RsTri_Optic;
          HID_USAGE_BARCODE_SCANNER_UCC_EAN_128:
            UsageText := RsUCC_EAN_128;
          HID_USAGE_BARCODE_SCANNER_CHECK_DIGIT:
            UsageText := RsCheckDigit;
          HID_USAGE_BARCODE_SCANNER_CD_DISABLE:
            UsageText := RsCheckDigitDisable;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_OPCC:
            UsageText := RsCheckDigitEnableInterleaved2of5OPCC;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_USS:
            UsageText := RsCheckDigitEnableInterleaved2of5USS;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_OPCC:
            UsageText := RsCheckDigitEnableStandard2of5OPCC;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_USS:
            UsageText := RsCheckDigitEnableStandard2of5USS;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_ONE_MSI_PLESSEY:
            UsageText := RsCheckDigitEnableOneMSI_Plessey;
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_TWO_MSI_PLESSEY:
            UsageText := RsCheckDigitEnableTwoMSI_Plessey;
          HID_USAGE_BARCODE_SCANNER_CD_CODABAR_ENABLE:
            UsageText := RsCheckDigitCodabarEnable;
          HID_USAGE_BARCODE_SCANNER_CD_CODE_39_ENABLE:
            UsageText := RsCheckDigitCode39Enable;
          HID_USAGE_BARCODE_SCANNER_TRANSMIT_CHECK_DIGIT:
            UsageText := RsTransmitCheckDigit;
          HID_USAGE_BARCODE_SCANNER_DISABLE_CHECK_DIGIT_TRANSMIT:
            UsageText := RsDisableCheckDigitTransmit;
          HID_USAGE_BARCODE_SCANNER_ENABLE_CHECK_DIGIT_TRANSMIT:
            UsageText := RsEnableCheckDigitTransmit;
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_1:
            UsageText := RsSymbologyIdentifier1;
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_2:
            UsageText := RsSymbologyIdentifier2;
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_3:
            UsageText := RsSymbologyIdentifier3;
          HID_USAGE_BARCODE_SCANNER_DECODED_DATA:
            UsageText := RsDecodeData;
          HID_USAGE_BARCODE_SCANNER_DECODED_DATA_CONTINUED:
            UsageText := RsDecodedDataContinued;
          HID_USAGE_BARCODE_SCANNER_BAR_SPACE_DATA:
            UsageText := RsBarSpaceData;
          HID_USAGE_BARCODE_SCANNER_SCANNER_DATA_ACCURACY:
            UsageText := RsScannerDataAccuracy;
          HID_USAGE_BARCODE_SCANNER_RAW_DATA_POLARITY:
            UsageText := RsRawDataPolarity;
          HID_USAGE_BARCODE_SCANNER_POLARITY_INVERTED_BAR_CODE:
            UsageText := RsPolarityInvertedBarCode;
          HID_USAGE_BARCODE_SCANNER_POLARITY_NORMAL_BAR_CODE:
            UsageText := RsPolarityNormalBarCode;
          HID_USAGE_BARCODE_SCANNER_MINIMUM_LENGTH_TO_DECODE:
            UsageText := RsMinimumLengthtoDecode;
          HID_USAGE_BARCODE_SCANNER_MAXIMUM_LENGTH_TO_DECODE:
            UsageText := RsMaximumLengthtoDecode;
          HID_USAGE_BARCODE_SCANNER_FIRST_DISCRETE_LENGTH_TO_DECODE:
            UsageText := RsFirstDiscreteLengthtoDecode;
          HID_USAGE_BARCODE_SCANNER_SECOND_DISCRETE_LENGTH_TO_DECODE:
            UsageText := RsSecondDiscreteLengthtoDecode;
          HID_USAGE_BARCODE_SCANNER_DATA_LENGTH_METHOD:
            UsageText := RsDataLengthMethod;
          HID_USAGE_BARCODE_SCANNER_DLM_READ_ANY:
            UsageText := RsDataLengthMethodReadAny;
          HID_USAGE_BARCODE_SCANNER_DLM_CHECK_IN_RANGE:
            UsageText := RsDataLengthMethodCheckinRange;
          HID_USAGE_BARCODE_SCANNER_DLM_CHECK_FOR_DISCRETE:
            UsageText := RsDataLengthMethodCheckforDiscrete;
          HID_USAGE_BARCODE_SCANNER_AZTEC_CODE:
            UsageText := RsAztecCode;
          HID_USAGE_BARCODE_SCANNER_BC412:
            UsageText := RsBC412;
          HID_USAGE_BARCODE_SCANNER_CHANNEL_CODE:
            UsageText := RsChannelCode;
          HID_USAGE_BARCODE_SCANNER_CODE_16:
            UsageText := RsCode16;
          HID_USAGE_BARCODE_SCANNER_CODE_32:
            UsageText := RsCode32;
          HID_USAGE_BARCODE_SCANNER_CODE_49:
            UsageText := RsCode49;
          HID_USAGE_BARCODE_SCANNER_CODE_ONE:
            UsageText := RsCodeOne;
          HID_USAGE_BARCODE_SCANNER_COLORCODE:
            UsageText := RsColorcode;
          HID_USAGE_BARCODE_SCANNER_DATA_MATRIX:
            UsageText := RsDataMatrix;
          HID_USAGE_BARCODE_SCANNER_MAXICODE:
            UsageText := RsMaxiCode;
          HID_USAGE_BARCODE_SCANNER_MICROPDF:
            UsageText := RsMicroPDF;
          HID_USAGE_BARCODE_SCANNER_PDF_417:
            UsageText := RsPDF_417;
          HID_USAGE_BARCODE_SCANNER_POSICODE:
            UsageText := RsPosiCode;
          HID_USAGE_BARCODE_SCANNER_QR_CODE:
            UsageText := RsQRCode;
          HID_USAGE_BARCODE_SCANNER_SUPERCODE:
            UsageText := RsSuperCode;
          HID_USAGE_BARCODE_SCANNER_ULTRACODE:
            UsageText := RsUltraCode;
          HID_USAGE_BARCODE_SCANNER_USD_5:
            UsageText := RsUSD_5SlugCode;
          HID_USAGE_BARCODE_SCANNER_VERICODE:
            UsageText := RsVeriCode;
        end;
      end;
    HID_USAGE_PAGE_WEIGHING_DEVICE:
      begin
        UsagePageText := RsWeighingDevice;
        case Usage of
          HID_USAGE_SCALE_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_SCALE_WEIGHING_DEVICE:
            UsageText := RsWeighingDevice;
          HID_USAGE_SCALE_SCALE_DEVICE_CLASS:
            UsageText := RsScaleDeviceClass;
          HID_USAGE_SCALE_SCALE_CLASS_I_METRIC_CLASS:
            UsageText := RsScaleClassIMetricClass;
          HID_USAGE_SCALE_SCALE_CLASS_I_METRIC:
            UsageText := RsScaleClassIMetric;
          HID_USAGE_SCALE_SCALE_CLASS_II_METRIC:
            UsageText := RsScaleClassIIMetric;
          HID_USAGE_SCALE_SCALE_CLASS_III_METRIC:
            UsageText := RsScaleClassIIIMetric;
          HID_USAGE_SCALE_SCALE_CLASS_IIIL_METRIC:
            UsageText := RsScaleClassIIILMetric;
          HID_USAGE_SCALE_SCALE_CLASS_IV_METRIC:
            UsageText := RsScaleClassIVMetric;
          HID_USAGE_SCALE_SCALE_CLASS_III_ENGLISH:
            UsageText := RsScaleClassIIIEnglish;
          HID_USAGE_SCALE_SCALE_CLASS_IIIL_ENGLISH:
            UsageText := RsScaleClassIIILEnglish;
          HID_USAGE_SCALE_SCALE_CLASS_IV_ENGLISH:
            UsageText := RsScaleClassIVEnglish;
          HID_USAGE_SCALE_SCALE_CLASS_GENERIC:
            UsageText := RsScaleClassGeneric;
          HID_USAGE_SCALE_SCALE_ATTRIBUTE_REPORT:
            UsageText := RsScaleAttributeReport;
          HID_USAGE_SCALE_SCALE_CONTROL_REPORT:
            UsageText := RsScaleControlReport;
          HID_USAGE_SCALE_SCALE_DATA_REPORT:
            UsageText := RsScaleDataReport;
          HID_USAGE_SCALE_SCALE_STATUS_REPORT:
            UsageText := RsScaleStatusReport;
          HID_USAGE_SCALE_SCALE_WEIGHT_LIMIT_REPORT:
            UsageText := RsScaleWeightLimitReport;
          HID_USAGE_SCALE_SCALE_STATISTICS_REPORT:
            UsageText := RsScaleStatisticsReport;
          HID_USAGE_SCALE_DATA_WEIGHT:
            UsageText := RsDataWeight;
          HID_USAGE_SCALE_DATA_SCALING:
            UsageText := RsDataScaling;
          HID_USAGE_SCALE_WEIGHT_UNIT_CLASS:
            UsageText := RsWeightUnitClass;
          HID_USAGE_SCALE_WEIGHT_UNIT_MILLIGRAM:
            UsageText := RsWeightUnitMilligram;
          HID_USAGE_SCALE_WEIGHT_UNIT_GRAM:
            UsageText := RsWeightUnitGram;
          HID_USAGE_SCALE_WEIGHT_UNIT_KILOGRAM:
            UsageText := RsWeightUnitKilogram;
          HID_USAGE_SCALE_WEIGHT_UNIT_CARATS:
            UsageText := RsWeightUnitCarats;
          HID_USAGE_SCALE_WEIGHT_UNIT_TAELS:
            UsageText := RsWeightUnitTaels;
          HID_USAGE_SCALE_WEIGHT_UNIT_GRAINS:
            UsageText := RsWeightUnitGrains;
          HID_USAGE_SCALE_WEIGHT_UNIT_PENNYWEIGHTS:
            UsageText := RsWeightUnitPennyweights;
          HID_USAGE_SCALE_WEIGHT_UNIT_METRIC_TON:
            UsageText := RsWeightUnitMetricTon;
          HID_USAGE_SCALE_WEIGHT_UNIT_AVOIR_TON:
            UsageText := RsWeightUnitAvoirTon;
          HID_USAGE_SCALE_WEIGHT_UNIT_TROY_OUNCE:
            UsageText := RsWeightUnitTroyOunce;
          HID_USAGE_SCALE_WEIGHT_UNIT_OUNCE:
            UsageText := RsWeightUnitOunce;
          HID_USAGE_SCALE_WEIGHT_UNIT_POUND:
            UsageText := RsWeightUnitPound;
          HID_USAGE_SCALE_CALIBRATION_COUNT:
            UsageText := RsCalibrationCount;
          HID_USAGE_SCALE_RE_ZERO_COUNT:
            UsageText := RsRe_ZeroCount;
          HID_USAGE_SCALE_SCALE_STATUS_CLASS:
            UsageText := RsScaleStatusClass;
          HID_USAGE_SCALE_SCS_FAULT:
            UsageText := RsScaleStatusFault;
          HID_USAGE_SCALE_SCS_STABLE_AT_CENTER_OF_ZERO:
            UsageText := RsScaleStatusStableatCenterofZero;
          HID_USAGE_SCALE_SCS_IN_MOTION:
            UsageText := RsScaleStatusInMotion;
          HID_USAGE_SCALE_SCS_WEIGHT_STABLE:
            UsageText := RsScaleStatusWeightStable;
          HID_USAGE_SCALE_SCS_UNDER_ZERO:
            UsageText := RsScaleStatusUnderZero;
          HID_USAGE_SCALE_SCS_OVER_WEIGHT_LIMIT:
            UsageText := RsScaleStatusOverWeightLimit;
          HID_USAGE_SCALE_SCS_REQUIRES_CALIBRATION:
            UsageText := RsScaleStatusRequiresCalibration;
          HID_USAGE_SCALE_SCS_REQUIRES_REZEROING:
            UsageText := RsScaleStatusRequiresRezeroing;
          HID_USAGE_SCALE_ZERO_SCALE:
            UsageText := RsScaleStatusZeroScale;
          HID_USAGE_SCALE_ENFORCED_ZERO_RETURN:
            UsageText := RsScaleStatusEnforcedZeroReturn;
        end;
      end;
    HID_USAGE_PAGE_MAGNETIC_STRIPE_READER:
      begin
        UsagePageText := RsMagneticStripeReader;
        case Usage of
          HID_USAGE_MSR_UNDEFINED:
            UsageText := RsUndefined;
          HID_USAGE_MSR_MSR_DEVICE_READ_ONLY:
            UsageText := RsMSRDeviceReadOnly;
          HID_USAGE_MSR_TRACK_1_LENGTH:
            UsageText := RsTrack1Length;
          HID_USAGE_MSR_TRACK_2_LENGTH:
            UsageText := RsTrack2Length;
          HID_USAGE_MSR_TRACK_3_LENGTH:
            UsageText := RsTrack3Length;
          HID_USAGE_MSR_TRACK_JIS_LENGTH:
            UsageText := RsTrackJISLength;
          HID_USAGE_MSR_TRACK_DATA:
            UsageText := RsTrackData;
          HID_USAGE_MSR_TRACK_1_DATA:
            UsageText := RsTrack1Data;
          HID_USAGE_MSR_TRACK_2_DATA:
            UsageText := RsTrack2Data;
          HID_USAGE_MSR_TRACK_3_DATA:
            UsageText := RsTrack3Data;
          HID_USAGE_MSR_TRACK_JIS_DATA:
            UsageText := RsTrackJISData;
        end;
      end;
  end;

  if UsagePageText = '' then
    UsagePageText := Format('%x', [UsagePage]);
  if UsageText = '' then
    UsageText := Format('%x', [Usage]);
end;

end.
